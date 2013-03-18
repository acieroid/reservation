%% Author: Quentin Stievenart
%% Created: Mar 17, 2013

%% Description: This is the implementation of the actors that manage
%%              part of the grid for the multiple actor reservation
%%              system
-module(grid_actor).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported functions
%%
-export([start/1]).

%%
%% API functions
%%
start({W, H, X, Y}) ->
    Grid = create_empty_grid(W, H, X, Y),
    actor(Grid).

%%
%% Local functions
%%
create_empty_grid(W, H, X, Y) ->
    FreeCells = W * H,
    EmptyRow = [ empty || _ <- lists:seq(1, W) ],
    Grid = [ EmptyRow || _ <- lists:seq(1, H) ],
    % TODO: what's the purpose of this?
    UnspecificRequests = {[], 0},

    {{W, H, X, Y}, Grid, FreeCells, UnspecificRequests}.

actor(Grid) ->
    NewGrid =
        receive
            {Pid, Ref, has_remaining_free_cells} ->
                has_remaining_free_cells(Grid, Pid, Ref);
            {Pid, get_grid_overview} ->
                get_grid_overview(Grid, Pid);
            {Pid, reserve_cells, NumberOfCells} ->
                reserve_cells(Grid, Pid, NumberOfCells);
            {Pid, request_specific_cells, ReservationId, Coordinates} ->
                request_specific_cells(Grid, Pid, ReservationId, Coordinates);
            Else ->
                erlang:display({unexpected_message, grid_actor, Else})
        end,
    actor(NewGrid).

has_remaining_free_cells(Grid, Pid, Ref) ->
    {_, _, FreeCells, _} = Grid,
    Pid ! {self(), Ref, has_remaining_free_cells, FreeCells > 0},
    Grid.

get_grid_overview(Grid, _Pid) ->
    Grid. % TODO

reserve_cells(Grid, _, _) ->
    Grid.
%% reserve_cells({Spec, Grid, FreeCells, UnspecificRequests}, Pid, NumberOfCells) 
%%   when FreeCells = 0 ->
%%     % No more cells available
%%     Pid ! {self(), failed, not_more_cells_available},
%%     {Spec, Grid, FreeCells, UnspecificRequests};
%% reserve_cells({Spec, Grid, FreeCells, UnspecificRequests}, Pid, NumberOfCells) 
%%   when NumberOfCells > FreeCells ->
%%     % Partial allocation
%%     reserve_cells({Spec, Grid, FreeCells, UnspecificRequests},
%%                   Pid, FreeCells, partial_success);
%% reserve_cells(Grid, Pid, NumberOfCells) ->
%%     % Full allocation
%%     reserve_cells(Grid, Pid, NumberOfCells, success).

%% reserve_cells(Grid, Pid, NumberOfCells, Message) ->
    


request_specific_cells(Grid, _Pid, _ReservationId, _Coordinates) ->
    Grid. % TODO
