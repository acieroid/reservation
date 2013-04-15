%% Author: Quentin Stievenart
%% Created: Apr 15, 2013

%% Description: This is the implementation of the actor that handle
%%              allocations. Per se, allocations are done by multiple
%%              actors that are managed by this one.
-module(allocator).

%%
%% Exported functions
%%
-export([start/3]).

%%
%% API functions
%%
start(GridSize, NActors, FreeCellsActor) ->
    %% How to decompose the grid vertically/horizontally (in subgrid
    %% of equal sizes)
    {M, N} = decompose(NActors),
    %% Width of each subgrid
    W = round(GridSize/N),
    %% Height of each subgrid
    H = round(GridSize/M),
    %% Create grid specifications
    GridsSpecs = create_grids_specs(GridSize, W, H),
    %% Spawn grid actors
    Actors = list:map(fun(GridSpec) ->
                              spawn_link(grid_actor, start, [GridSpec])
                      end,
                      GridsSpecs),
    %% Launch the allocator manager (this actor)
    ActorData = {GridSize, W, H, Actors, FreeCellsActor},
    actor(ActorData).

%%
%% Local functions
%%

%% Main function of this actor
actor(ActorData) ->
    NewActorData =
        receive
            {Pid, get_grid_overview} ->
                get_grid_overview(ActorData, Pid);
            {Pid, request_specific_cells, Ref, ReservationId, Coordinates} ->
                request_specific_cells(ActorData, Pid, Ref, ReservationId, Coordinates);
            Else ->
                erlang:display({unexpected_message, allocator, Else})
        end,
    actor(NewActorData).

%% Decompose the integer N into its two biggest factors
decompose(N) ->
    decompose(N, round(math:sqrt(N))).

decompose(N, M) ->
    case N rem M of
        0 -> {M, N div M};
        _ -> decompose(N, M-1)
    end.

%% Send a message to all the actors
send_to_all([], _) ->
    done;
send_to_all(Actors, Msg) ->
    lists:map(fun (Actor) ->
                     Actor ! Msg
             end, Actors).

%% Decompose the grid into multiple subgrid of size WxH
create_grids_specs(GridSize, W, H) ->
    create_grids_specs(GridSize, 0, 0, W, H, []).

create_grids_specs(GridSize, X, _Y, W, _H, GridsSpecs)
  when X+W > GridSize ->
    GridsSpecs;
create_grids_specs(GridSize, X, Y, W, H, GridsSpecs)
  when Y+H > GridSize ->
    create_grids_specs(GridSize, X+W, 0, W, H, GridsSpecs);
create_grids_specs(GridSize, W, H, X, Y, GridsSpecs)
  when X+W < GridSize, X+2*W > GridSize,
       Y+H < GridSize, Y+2*H > GridSize ->
    DeltaX = GridSize - (X+W),
    DeltaY = GridSize - (Y+H),
    %% Erlang indexes starts at 1, so just do a +1
    NewGridSpec = {X+1, Y+1, W+DeltaX, H+DeltaY},
    [NewGridSpec|GridsSpecs];
create_grids_specs(GridSize, X, Y, W, H, GridsSpecs)
  when X+W < GridSize, X+2*W > GridSize ->
    Delta = GridSize - (X+W),
    NewGridSpec = {X+1, Y+1, W+Delta, H},
    create_grids_specs(GridSize, X, Y+H, W, H, [NewGridSpec|GridsSpecs]);
create_grids_specs(GridSize, X, Y, W, H, GridsSpecs)
  when Y+H < GridSize, Y+2*H > GridSize ->
    Delta = GridSize - (Y+H),
    NewGridSpec = {X+1, Y+1, W, H+Delta},
    create_grids_specs(GridSize, X+W, 0, W, H, [NewGridSpec|GridsSpecs]);
create_grids_specs(GridSize, X, Y, W, H, GridsSpecs) ->
    NewGridSpec = {X+1, Y+1, W, H},
    create_grids_specs(GridSize, X, Y+H, W, H, [NewGridSpec|GridsSpecs]).

%% Create an empty grid
create_empty_grid_content(GridSize) ->
    W = H = GridSize,
    EmptyRow = [ empty || _ <- lists:seq(1, W) ],
    Content = [ EmptyRow || _ <- lists:seq(1, H) ],
    Content.

%% Copy a region of a grid into another grid
copy_region(Content, SubContent, X, Y, W, H) ->
    Row = lists:nth(Y, Content),
    RegionCells = lists:nth(1, SubContent),
    NewSubContent = lists:nthtail(1, SubContent),

    {Begin, Rest} = lists:split(X - 1, Row),
    NewRow = Begin ++ RegionCells ++ lists:nthtail(W, Rest),

    NewContent = lists:sublist(Content, Y - 1) ++
        [NewRow] ++ lists:nthtail(Y, Content),

    if
        H > 1 ->
            copy_region(NewContent, NewSubContent, X, Y+1, W, H-1);
        true ->
            NewContent
    end.


%% Send the overview of the grid
get_grid_overview(ActorData, Pid) ->
    {GridSize, _, _, Actors, _} = ActorData,
    %% Request the overview of each actor
    Ref = make_ref(),
    send_to_all(Actors, {self(), Ref, get_grid_overview}),
    %% Gather the responses
    Content = get_grid_overview_gather_responses(Actors, Ref,
                                                 create_empty_grid_content(GridSize)),
    %% Send the result
    Pid ! {self(), get_grid_overview, Content, {GridSize, GridSize}},
    ActorData.

%% Gather the responses containing the grid overviews sent by the
%% actors
get_grid_overview_gather_responses([], _, Content) ->
    Content;
get_grid_overview_gather_responses(Actors, Ref, Content) ->
    receive
        {Actor, Ref, get_grid_overview, SubContent, {X, Y, W, H}} ->
            NewContent = copy_region(Content, SubContent, X, Y, W, H),
            get_grid_overview_gather_responses(lists:delete(Actor, Actors),
                                               Ref,
                                               NewContent)
    end.

%% Handle the requests of specific cells. Note: at this point, we know
%% that the request is valid, since it has been first validated by the
%% free cells actor before being sent to this actor.
request_specific_cells(ActorData, Pid, Ref, ReservationId, Coordinates) ->
    {GridSize, W, H, Actors, FreeCellsActor} = ActorData,
    %% Send the request to all the grid actors
    send_to_all(Actors,
                {self(), Ref, request_specific_cells,
                 ReservationId, Coordinates}),
    %% Gather the responses of the grid actors
    Status = request_specific_cells_gather_responses(Actors, Ref, success),
    case Status of
        %% Failure
        false ->
            %% if the allocation failed, deallocate all the
            %% sub-allocation that did not fail
            send_to_all(Actors,
                        {self(), Ref, release_specific_cells,
                         ReservationId, Coordinates}),
            release_specific_cells_gather_responses(Actors, Ref);
        %% Success
        _ -> nothing
    end,
    Pid ! {self(),  request_specific_cells, ReservationId, Status},
    {GridSize, W, H, Actors, FreeCellsActor}.

%% Gather the responses after a specific request
request_specific_cells_gather_responses([], _, Res) ->
    Res;
request_specific_cells_gather_responses(Actors, Ref, Res) ->
    {Actor, NewRes} =
        receive
            {Act, Ref, request_specific_cells, success} ->
                {Act, Res};
            {Act, Ref, request_specific_cells, failed} ->
                {Act, failed}
        end,
    request_specific_cells_gather_responses(lists:delete(Actor, Actors),
                                            Ref, NewRes).

%% Gather the responses after a release
release_specific_cells_gather_responses([], _) ->
    done;
release_specific_cells_gather_responses(Actors, Ref) ->
    receive
        {Actor, Ref, release_specific_cells, ok} ->
            release_specific_cells_gather_responses(lists:delete(Actor, Actors), Ref)
    end.
