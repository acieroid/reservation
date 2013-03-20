%% Author: Quentin Stievenart
%% Created: Mar 17, 2013

%% Description: This is the implementation of the manager of a
%%              multiple actor reservation system
-module(reservation_multiple_actors).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported functions
%%
-export([initialize/2, manager/1]).

%%
%% Constants
%%
-define(MAX_REQUEST, 0.02). % A request can be max. 2% of the number of cells

%%
%% API functions
%%
initialize(GridSize, NActors) ->
    {M, N} = decompose(NActors),
    W = round(GridSize/N),
    H = round(GridSize/M),
    UnspecificRequests = {[], 0}, % TODO: also distribute this over the clients
    % Spawn grid actors
    GridsSpecs = create_grids_specs(GridSize, W, H),
    Actors = lists:map(fun(GridSpec) ->
                               spawn_link(grid_actor, start, [GridSpec])
                       end,
                       GridsSpecs),
    % Spawn manager
    ManagerData = {GridSize, GridSize*GridSize, W, H, Actors, UnspecificRequests},
    spawn_link(?MODULE, manager, [ManagerData]).

% TODO: relaunch actors that fails ?
manager(ManagerData) ->
    NewManagerData =
        receive
            {Pid, get_size_of_resource} ->
                %erlang:display({received, get_size_of_resource}),
                get_size_of_resource(ManagerData, Pid);
            {Pid, has_remaining_free_cells} ->
                %erlang:display({received, has_remaining_free_cells}),
                has_remaining_free_cells(ManagerData, Pid);
            {Pid, get_grid_overview} ->
                %erlang:display({received, get_grid_overview}),
                get_grid_overview(ManagerData, Pid);
            {Pid, reserve_cells, NumberOfCells} ->
                %erlang:display({received, reserve_cells}),
                reserve_cells(ManagerData, Pid, NumberOfCells);
            {Pid, request_specific_cells, ReservationId, Coordinates} ->
                %erlang:display({received, reserve_cells}),
                request_specific_cells(ManagerData, Pid, ReservationId, Coordinates);
            Else ->
                erlang:display({unexpected_message, manager, Else})
        end,
    manager(NewManagerData).

%%
%% Local functions
%%
decompose(N) ->
    % Decompose N into two factors
    decompose(N, round(math:sqrt(N))).

decompose(N, M) ->
    case N rem M of
        0 -> {M, N div M};
        _ -> decompose(N, M-1)
    end.

%% Send a message to a random actor of the given list of actors
send_to_random([], Msg) ->
    erlang:display({error, no_more_actors, Msg});
send_to_random(Actors, Msg) ->
    % TODO: actually randomize this
    [Actor|_] = Actors,
    Actor ! Msg,
    Actor.

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

get_size_of_resource(ManagerData, Pid) ->
    % The grid size is a static value and does not change over the
    % execution
    {GridSize, _, _, _, _, _} = ManagerData,
    Pid ! {self(), get_size_of_resource, {GridSize, GridSize}},
    ManagerData.

has_remaining_free_cells(ManagerData, Pid) ->
    {_, FreeCells, _, _, _, _} = ManagerData,
    % The number of free cells is stored in the manager
    Pid ! {self(), has_remaining_free_cells,
           FreeCells > 0},
    ManagerData.

get_grid_overview(ManagerData, Pid) ->
    {GridSize, _, _, _, Actors, _} = ManagerData,
    Ref = make_ref(),
    send_to_all(Actors, {self(), Ref, get_grid_overview}),
    Content = get_grid_overview_gather_responses(Actors, Ref,
                                                 create_empty_grid_content(GridSize)),
    Pid ! {self(), get_grid_overview, Content, {GridSize, GridSize}},
    ManagerData.

create_empty_grid_content(GridSize) ->
    W = H = GridSize,
    EmptyRow = [ empty || _ <- lists:seq(1, W) ],
    Content = [ EmptyRow || _ <- lists:seq(1, H) ],
    Content.

copy_region(Content, SubContent, X, Y, W, H) ->
    Row = lists:nth(Y, Content),
    RegionCells = lists:nth(1, SubContent),
    NewSubContent = lists:nthtail(1, SubContent),

    {Begin, Rest} = lists:split(X - 1, Row),
    NewRow = Begin ++ RegionCells ++ lists:nthtail(W, Rest),

    NewContent = lists:sublist(Content, Y - 1) ++
        [NewRow] ++ lists:nthtail(Y, Content),

    if
        Y < H ->
            copy_region(NewContent, NewSubContent, X, Y+1, W, H);
        true ->
            NewContent
    end.

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

reserve_cells(ManagerData, Pid, NumberOfCells) ->
    {GridSize, FreeCells, W, H, Actors, UnspecificRequests} = ManagerData,
    if
        NumberOfCells > GridSize*GridSize * ?MAX_REQUEST ->
            Pid ! {self(), reserve_cells, failed, request_too_large},
            ManagerData;
        NumberOfCells > FreeCells ->
            Pid ! {self(), reserve_cells, failed, not_enough_cells_available},
            ManagerData;
        true ->
            {UnspecReqList, NextId} = UnspecificRequests,
            Pid ! {self(), reserve_cells, success, {self(), NextId}},
            {GridSize, FreeCells - NumberOfCells,
             W, H, Actors,
             % TODO: don't concatenate: either prepend (but it might
             % starve clients), or use erlang's priority system (with
             % messages)
             {UnspecReqList ++ [{NextId, NumberOfCells}], NextId}}
    end.

request_specific_cells(ManagerData, Pid, ReservationId, Coordinates) ->
    %% TODO: check that the number of cells corresponds
    {GridSize, FreeCells, W, H, Actors, {UnspecificRequests, NextId}} = ManagerData,
    {X, Y, ReserveWidth, ReserveHeight} = Coordinates,
    Request = lists:keyfind(ReservationId, 1, UnspecificRequests),
    if
        not Request;
        X < 1; Y < 1;
        (Y + ReserveHeight - 1) > GridSize; (X + ReserveWidth - 1) > GridSize ->
            % invalid request or request not found
            Pid ! {self(), request_specific_cells, ReservationId, failed},
            ManagerData;
        true ->
            % request found, pass it to all the actors
            Ref = make_ref(),
            send_to_all(Actors,
                        {self(), Ref, request_specific_cells,
                         ReservationId, Coordinates}),
            NewUnspecificRequests = lists:keydelete(ReservationId, 1, UnspecificRequests),
            Status = request_specific_cells_gather_responses(Actors, Ref, success),
            case Status of
                false ->
                    % if the allocation failed, deallocate all the
                    % sub-allocation that did not fail
                    % TODO: should we keep the unspecific request in the list?
                    send_to_all(Actors,
                                {self(), Ref, release_specific_cells,
                                 ReservationId, Coordinates}),
                    release_specific_cells_gather_responses(Actors, Ref);
                _ -> nothing
            end,
            Pid ! {self(), request_specific_cells, ReservationId, Status},
            {GridSize, FreeCells, W, H, Actors, {NewUnspecificRequests, NextId}}
    end.

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

release_specific_cells_gather_responses([], _) ->
    done;
release_specific_cells_gather_responses(Actors, Ref) ->
    receive
        {Actor, Ref, release_specific_cells, ok} ->
            release_specific_cells_gather_responses(lists:delete(Actor, Actors), Ref)
    end.

%%
%% Test functions
%%
initialization_returns_pid_test() ->
    Pid = initialize(100, 4),
    ?assert(is_pid(Pid)).

initialization_consistency_test() ->
    Pid = initialize(100, 4),

    GridDimensions = reservation:get_size_of_resource(Pid),
    {Width, Height} = GridDimensions,
    ?assertMatch(100, (Width = Height)).

empty_has_remaining_free_cells_test() ->
    Pid = initialize(100, 4),
    ?assertMatch(true, reservation:has_remaining_free_cells(Pid)).


allocation_of_too_large_requests_fails_test() ->
    Pid = initialize(100, 4),

    ?assertMatch({failed, request_too_large},
                 reservation:reserve_cells(Pid, 201)),
    ?assertMatch({failed, request_too_large},
                 reservation:reserve_cells(Pid, 401)),
    ?assertMatch({failed, request_too_large},
                 reservation:reserve_cells(Pid, 1000)).

allocation_of_small_requests_ok_test() ->
    Pid = initialize(100, 4),

    ?assertMatch({success, _},
                 reservation:reserve_cells(Pid, 200)),
    ?assertMatch({success, _},
                 reservation:reserve_cells(Pid, 5)),
    ?assertMatch({success, _},
                 reservation:reserve_cells(Pid, 1)).

has_remaining_free_cells_test() ->
    Pid = initialize(100, 4),

    ?assertMatch(true, reservation:has_remaining_free_cells(Pid)),

    I = lists:seq(1, 1999),

    lists:foreach(fun(_) ->
                    ?assertMatch({success, _},
                                 reservation:reserve_cells(Pid, 5))
                          end, I),

    ?assertMatch(true, reservation:has_remaining_free_cells(Pid)),
    reservation:reserve_cells(Pid, 5),

    ?assertMatch(false, reservation:has_remaining_free_cells(Pid)).


no_specific_cells_outside_of_the_grid_test() ->
    Pid = initialize(100, 4),

    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 5),

    % start counting from 1
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {0, 0, 5, 1})),

    % not outside of the board
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {100, 100, 5, 1})),
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {  1, 101, 5, 1})),
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {101,   1, 5, 1})),
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {100,  97, 1, 5})),

    % but at the last row and without leaving the grid it has to work
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {100, 95, 1, 5})).

reserve_cells_between_subgrids_test() ->
    Pid = initialize(100, 4),

    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 16),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {48, 48, 4, 4})).

correctly_release_cells_when_failing_test() ->
    Pid = initialize(100, 4),

    % reserve cells on the corner of a subgrid
    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 16),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {51, 51, 4, 4})),

    % reserve cells between subgrids (should fail)
    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 64),
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {48, 48, 8, 8})),

    % reserve cells on another subgrid
    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 16),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {46, 46, 4, 4})
               ).

reserve_cells_simple_test() ->
    Pid = initialize(20, 4),

    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 4),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {1, 1, 2, 2})),

    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 4),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {5, 5, 2, 2})),

    reservation:write_grid_to_file(Pid, "/tmp/foo.txt"),

    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 4),
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {1, 1, 2, 2})).


grid_overview_test() ->
    Pid = initialize(20, 1),

    {Grid, _} = reservation:get_grid_overview(Pid),

    % Everything should be still empty
    ?assertMatch(20, length(Grid)),
    EmptyRow = [ empty || _ <- lists:seq(1, 20) ],
    lists:foreach(fun(Row) ->
                    ?assertMatch(EmptyRow, Row)
                  end, Grid),


    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 1),
    Cells = {1, 1, 1, 1},
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, Cells)),

    {GridWithOne, _} = reservation:get_grid_overview(Pid),

    reservation:write_grid_to_file(Pid, "/tmp/bar.txt"),

    [FirstRow | RemainingRows] = GridWithOne,
    [First | RemainingRow] = FirstRow,
    ?assertMatch(reserved, First),

    lists:foreach(fun(Cell) ->
                    ?assertMatch(empty, Cell)
                  end, RemainingRow),

    lists:foreach(fun(Row2) ->
                    ?assertMatch(EmptyRow, Row2)
                  end, RemainingRows).
