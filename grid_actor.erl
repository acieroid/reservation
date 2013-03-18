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

    {{X, Y, W, H}, Grid, FreeCells, UnspecificRequests}.

actor(Grid) ->
    NewGrid =
        receive
            {Pid, Ref, has_remaining_free_cells} ->
                has_remaining_free_cells(Grid, Pid, Ref);
            {Pid, Ref, get_grid_overview} ->
                get_grid_overview(Grid, Pid, Ref);
            {Pid, Ref, reserve_cells, NumberOfCells} ->
                reserve_cells(Grid, Pid, Ref, NumberOfCells);
            {Pid, Ref, request_specific_cells, ReservationId, Coordinates} ->
                request_specific_cells(Grid, Pid, Ref, ReservationId, Coordinates);
            Else ->
                erlang:display({unexpected_message, grid_actor, Else})
        end,
    actor(NewGrid).

has_remaining_free_cells(Grid, Pid, Ref) ->
    {_, _, FreeCells, _} = Grid,
    Pid ! {self(), Ref, has_remaining_free_cells, FreeCells > 0},
    Grid.

get_grid_overview(Grid, _Pid, _Ref) ->
    Grid. % TODO

reserve_cells(Grid, _Pid, _Ref, _NumberOfCells) ->
    Grid. % TODO

%% Return the intersection coordinates in the local coordinate system
intersection(Grid, Coordinates) ->
    {{X, Y, W, H}, _, _, _} = Grid,
    {CX, CY, CW, CH} = Coordinates,
    if
        CX > X + W; CY > Y + H;
        CX + CW < X; CY + CH < Y ->
            none;
        true ->
            IX = max(CX, X),
            IY = max(CY, Y),
            IW = CW - (IX - CX),
            IH = CH - (IY - CY),
            {IX - X, IY - Y, IW, IH}
    end.

region_is_empty({X, Y, Width, Heigth}, GridContent) ->
    Row = lists:nth(Y, GridContent),
    RelevantCells = lists:sublist(Row, X, X + Width),
    AllCellsEmpty = lists:all(fun(E) -> E == empty end, RelevantCells),
    if
        AllCellsEmpty and Y < Heigth ->
            region_is_empty({X, Y + 1, Width, Heigth}, GridContent);
        true ->
            AllCellsEmpty
    end.

mark_region_reserved({X, Y, Width, Height}, GridContent) ->
    Row = lists:nth(Y, GridContent),
    ReservedCells = [ reserved || _ <- lists:seq(1, Width) ],

    {Begin, Rest} = lists:split(X - 1, Row),
    NewRow = Begin ++ ReservedCells ++ lists:nthtail(Width, Rest),

    NewGrid = lists:sublist(GridContent, Y - 1) ++ [NewRow] ++ lists:nthtail(Y, GridContent),

    if
        Y < Height ->
            mark_region_reserved({X, Y + 1, Width, Height}, NewGrid);
        true ->
            NewGrid
    end.

request_specific_cells(Grid, Pid, Ref, _ReservationId, Coordinates) ->
    {Specs, Content, FreeCells, UnspecificRequests} = Grid,
    {NewGrid, Status} =
        case intersection(Grid, Coordinates) of
            none ->
                %% Nothing to allocate in this actor
                {Grid, success};
            IntersectionCoordinates ->
                case region_is_empty(IntersectionCoordinates, Content) of
                    false ->
                        %% Not empty, cannot allocate
                        {Grid, failed};
                    true ->
                        NewContent = mark_region_reserved(IntersectionCoordinates,
                                                          Content),
                        {{Specs, NewContent, FreeCells, UnspecificRequests},
                         success}
                end
        end,
    Pid ! {self(), Ref, request_specific_cells, Status},
    NewGrid.
