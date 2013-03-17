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
-define(MAX_REQUESTS, 0.02). % A request can be max. 2% of the number of cells

%%
%% API functions
%%
initialize(GridSize, NActors) ->
    {M, N} = decompose(NActors),
    W = round(GridSize/N),
    H = round(GridSize/M),
    % Spawn grid actors
    GridsSpecs = create_grids_specs(GridSize, W, H),
    Pids = lists:map(fun(GridSpec) ->
                             spawn_link(grid_actor, start, [GridSpec])
                     end,
                     GridsSpecs),
    % Spawn manager
    ManagerData = {GridSize, GridSize*GridSize, W, H, Pids},
    spawn_link(?MODULE, manager, [ManagerData]).

% TODO: relaunch actors that fails ?
manager(ManagerData) ->
    NewManagerData =
        receive
            {Pid, get_size_of_resource} ->
                erlang:display(get_size_of_resource),
                get_size_of_resource(ManagerData, Pid);
            {Pid, has_remaining_free_cells} ->
                erlang:display(has_remaining_free_cells),
                has_remaining_free_cells(ManagerData, Pid);
            {Pid, get_grid_overview} ->
                erlang:display(get_grid_overview),
                get_grid_overview(ManagerData, Pid);
            {Pid, reserve_cells, NumberOfCells} ->
                erlang:display(reserve_cells),
                reserve_cells(ManagerData, Pid, NumberOfCells);
            {Pid, request_specific_cells, ReservationId, Coordinates} ->
                erlang:display(reserve_cells),
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
    {GridSize, _, _, _, _} = ManagerData,
    Pid ! {self(), get_size_of_resource, GridSize},
    ManagerData.

has_remaining_free_cells(ManagerData, Pid) ->
    % Ask the remaining free cells of every actor
    {_, _, _, _, Pids} = ManagerData,
    Ref = erlang:make_ref(),
    lists:map(fun(Actor) -> Actor ! {self(), Ref, has_remaining_free_cells} end,
              Pids),
    Pid ! {self(), has_remaining_free_cells,
           has_remaining_free_cells_gather_responses(lists:length(Pids),
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

reserve_cells(ManagerData, _Pid, _NumberOfCells) ->
    ManagerData. % TODO

request_specific_cells(ManagerData, _Pid, _ReservationId, _Coordinates) ->
    ManagerData. % TODO
