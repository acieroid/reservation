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
    % Spawn grid actors
    GridsSpecs = create_grids_specs(GridSize, W, H),
    Actors = lists:map(fun(GridSpec) ->
                             spawn_link(grid_actor, start, [GridSpec])
                     end,
                     GridsSpecs),
    % Spawn manager
    ManagerData = {GridSize, GridSize*GridSize, W, H, Actors},
    spawn_link(?MODULE, manager, [ManagerData]).

% TODO: relaunch actors that fails ?
manager(ManagerData) ->
    NewManagerData =
        receive
            {Pid, get_size_of_resource} ->
                erlang:display({received, get_size_of_resource}),
                get_size_of_resource(ManagerData, Pid);
            {Pid, has_remaining_free_cells} ->
                erlang:display({received, has_remaining_free_cells}),
                has_remaining_free_cells(ManagerData, Pid);
            {Pid, get_grid_overview} ->
                erlang:display({received, get_grid_overview}),
                get_grid_overview(ManagerData, Pid);
            {Pid, reserve_cells, NumberOfCells} ->
                erlang:display({received, reserve_cells}),
                reserve_cells(ManagerData, Pid, NumberOfCells);
            {Pid, request_specific_cells, ReservationId, Coordinates} ->
                erlang:display({received, reserve_cells}),
                request_specific_cells(ManagerData, Pid, ReservationId, Coordinates);
            Else ->
                erlang:display({unexpected_message, manager, Else})
        end,
    erlang:display({new_manager_data,NewManagerData}),
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

create_grids_specs(GridSize, W, H) ->
    create_grids_specs(GridSize, W, H, 0, 0, []).

create_grids_specs(GridSize, W, _H, X, _Y, GridsSpecs)
  when X+W > GridSize ->
    GridsSpecs;
create_grids_specs(GridSize, W, H, X, Y, GridsSpecs)
  when Y+H > GridSize ->
    create_grids_specs(GridSize, W, H, X+W, 0, GridsSpecs);
create_grids_specs(GridSize, W, H, X, Y, GridsSpecs)
  when X+W < GridSize, X+2*W > GridSize,
       Y+H < GridSize, Y+2*H > GridSize ->
    DeltaX = GridSize - (X+W),
    DeltaY = GridSize - (Y+H),
    NewGridSpec = {W+DeltaX, H+DeltaY, X, Y},
    [NewGridSpec|GridsSpecs];
create_grids_specs(GridSize, W, H, X, Y, GridsSpecs)
  when X+W < GridSize, X+2*W > GridSize ->
    Delta = GridSize - (X+W),
    NewGridSpec = {W+Delta, H, X, Y},
    create_grids_specs(GridSize, W, H, X, Y+H, [NewGridSpec|GridsSpecs]);
create_grids_specs(GridSize, W, H, X, Y, GridsSpecs)
  when Y+H < GridSize, Y+2*H > GridSize ->
    Delta = GridSize - (Y+H),
    NewGridSpec = {W, H+Delta, X, Y},
    create_grids_specs(GridSize, W, H, X+W, 0, [NewGridSpec|GridsSpecs]);
create_grids_specs(GridSize, W, H, X, Y, GridsSpecs) ->
    NewGridSpec = {W, H, X, Y},
    create_grids_specs(GridSize, W, H, X, Y+H, [NewGridSpec|GridsSpecs]).

get_size_of_resource(ManagerData, Pid) ->
    % The grid size is a static value and does not change over the
    % execution
    erlang:display({get_size_of_resource, Pid}),
    {GridSize, _, _, _, _} = ManagerData,
    Pid ! {self(), get_size_of_resource, {GridSize, GridSize}},
    ManagerData.

has_remaining_free_cells(ManagerData, Pid) ->
    % Ask the remaining free cells of every actor
    {_, _, _, _, Actors} = ManagerData,
    Ref = erlang:make_ref(),
    lists:map(fun(Actor) -> Actor ! {self(), Ref, has_remaining_free_cells} end,
              Actors),
    Pid ! {self(), has_remaining_free_cells,
           has_remaining_free_cells_gather_responses(length(Actors),
                                                     Ref, false)},
    ManagerData.

has_remaining_free_cells_gather_responses(0, _, Res) ->
    Res;
has_remaining_free_cells_gather_responses(N, Ref, Res) ->
    receive
        {_Pid, Ref, has_remaining_free_cells, true} ->
            has_remaining_free_cells_gather_responses(N-1, Ref, true);
        {_Pid, Ref, has_remaining_free_cells, false} ->
            has_remaining_free_cells_gather_responses(N-1, Ref, Res)
    end.

get_grid_overview(ManagerData, _Pid) ->
    ManagerData. % TODO

%% Send a message to a random actor of the given list of actors
send_to_random([], Msg) ->
    erlang:display({error, no_more_actors, Msg});
send_to_random(Actors, Msg) ->
    % TODO: actually randomize this
    [Actor|_] = Actors,
    Actor ! Msg,
    Actor.

reserve_cells({GridSize, TotalCells, W, H, Actors}, Pid, NumberOfCells)
  when NumberOfCells > ?MAX_REQUEST * TotalCells ->
    erlang:display({reserve_cells, fail, request_too_large, NumberOfCells}),
    Pid ! {self(), reserve_cells, failed, request_too_large},
    {GridSize, TotalCells, W, H, Actors};
reserve_cells(ManagerData, Pid, NumberOfCells) ->
    {_, _, _, _, Actors} = ManagerData,
    Ref = erlang:make_ref(),
    reserve_cells(Pid, Ref, Actors, NumberOfCells),
    ManagerData.

reserve_cells(Pid, _, [], _) ->
    Pid ! {self(), reserve_cells, failed, not_enough_cells_available};
reserve_cells(Pid, Ref, Actors, NumberOfCells) ->
    Actor = send_to_random(Actors, {self(), Ref, reserve_cells, NumberOfCells}),
    NewActors = lists:delete(Actor, Actors),
    receive
        {_, reserve_cells, success, Info} ->
            % TODO: UnspecReqList, ReservationId
            Pid ! {self(), reserve_cells, success, Info};
        {_, reserve_cells, partial_success, Allocated} ->
            % 'Allocated' cells have been allocated
            reserve_cells(Pid, Ref, NewActors, NumberOfCells - Allocated);
        {_, reserve_cells, failed, _} ->
            reserve_cells(Pid, Ref, NewActors, NumberOfCells)
    end.

request_specific_cells(ManagerData, _Pid, _ReservationId, _Coordinates) ->
    ManagerData. % TODO

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
