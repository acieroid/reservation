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
                             spawn_link(grid_actor, actor, [GridSpec])
                     end,
                     GridsSpecs),
    % Spawn manager
    ManagerData = {GridSize, GridSize*GridSize, W, H, Pids},
    spawn_link(?MODULE, manager, [ManagerData]).

manager(ManagerData) ->
    NewManagerData =
        receive
            {Pid, get_size_of_resource} ->
                get_size_of_resource(ManagerData, Pid);
            {Pid, has_remaining_free_cells} ->
                has_remaining_free_cells(ManagerData, Pid);
            {Pid, get_grid_overview} ->
                get_grid_overview(ManagerData, Pid);
            {Pid, reserve_cells, NumberOfCells} ->
                reserve_cells(ManagerData, Pid, NumberOfCells);
            {Pid, request_specific_cells, ReservationId, Coordinates} ->
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

create_grids_specs(GridSize, _W, _H, GridSize, _Y, GridsSpecs) ->
    GridsSpecs;
create_grids_specs(GridSize, W, H, X, GridSize, GridsSpecs) ->
    create_grids_specs(GridSize, W, H, X+W, 0, GridsSpecs);
create_grids_specs(GridSize, W, H, X, Y, GridsSpecs) ->
    NewGridSpec = {W, H, X, Y},
    create_grids_specs(GridSize, W, H, X, Y+H, [NewGridSpec|GridsSpecs]).

get_size_of_resource(ManagerData, _Pid) ->
    ManagerData. % TODO

has_remaining_free_cells(ManagerData, _Pid) ->
    ManagerData. % TODO

get_grid_overview(ManagerData, _Pid) ->
    ManagerData. % TODO

reserve_cells(ManagerData, _Pid, _NumberOfCells) ->
    ManagerData. % TODO

request_specific_cells(ManagerData, _Pid, _ReservationId, _Coordinates) ->
    ManagerData. % TODO
