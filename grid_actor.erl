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
start({X, Y, W, H}) ->
    Grid = create_empty_grid(X, Y, W, H),
    actor(Grid).

%%
%% Local functions
%%
create_empty_grid(X, Y, W, H) ->
    FreeCells = W * H,
    EmptyRow = [ empty || _ <- lists:seq(1, W) ],
    Grid = [ EmptyRow || _ <- lists:seq(1, H) ],
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
            {Pid, Ref, release_specific_cells, ReservationId, Coordinates} ->
                release_specific_cells(Grid, Pid, Ref, ReservationId, Coordinates);
            Else ->
                erlang:display({self(), unexpected_message, grid_actor, Else})
        end,
    actor(NewGrid).

has_remaining_free_cells(Grid, Pid, Ref) ->
    {_, _, FreeCells, _} = Grid,
    Pid ! {self(), Ref, has_remaining_free_cells, FreeCells > 0},
    Grid.

get_grid_overview(Grid, Pid, Ref) ->
    {Specs, Content, _, _} = Grid,
    Pid ! {self(), Ref, get_grid_overview, Content, Specs},
    Grid.

reserve_cells(Grid, _Pid, _Ref, _NumberOfCells) ->
    Grid. % TODO

%% Return the intersection coordinates in the local coordinate system
intersection(Grid, Coordinates) ->
    {{X, Y, W, H}, _, _, _} = Grid,
    {CX, CY, CW, CH} = Coordinates,
    if
        CX >= X + W; CY >= Y + H;
        CX + CW < X; CY + CH < Y ->
            none;
        true ->
            IX = max(CX, X),
            IY = max(CY, Y),
            IW = CW - (IX - CX) - ((CX + CW) - (X + W)) - 1,
            IH = CH - (IY - CY) - ((CY + CH) - (Y + H)) - 1,
            %% Erlang indexes start at 1
            {IX - X + 1, IY - Y + 1, IW, IH}
    end.

region_is_empty({X, Y, Width, Height}, GridContent) ->
    Row = lists:nth(Y, GridContent),
    RelevantCells = lists:sublist(Row, X, X + Width),
    AllCellsEmpty = lists:all(fun(E) -> E == empty end, RelevantCells),
    if
        AllCellsEmpty and Y < Height ->
            region_is_empty({X, Y + 1, Width, Height}, GridContent);
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

mark_region_empty({X, Y, Width, Height}, GridContent) ->
    Row = lists:nth(Y, GridContent),
    EmptyCells = [ empty || _ <- lists:seq(1, Width) ],

    {Begin, Rest} = lists:split(X - 1, Row),
    NewRow = Begin ++ EmptyCells ++ lists:nthtail(Width, Rest),

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

release_specific_cells(Grid, Pid, Ref, _ReservationId, Coordinates) ->
    {Specs, Content, FreeCells, UnspecificRequests} = Grid,
    {NewGrid, Status} =
        case intersection(Grid, Coordinates) of
            none ->
                %% Nothing to de-allocate in this actor
                {Grid, success};
            IntersectionCoordinates ->
                NewContent = mark_region_empty(IntersectionCoordinates,
                                               Content),
                {{Specs, NewContent, FreeCells, UnspecificRequests},
                 success}
        end,
    Pid ! {self(), Ref, request_specific_cells, Status},
    NewGrid.
