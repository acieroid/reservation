%% Author: Quentin Stievenart
%% Created: Mar 17, 2013

%% Description: This is the implementation of the actors that manage
%%              part of the grid for the multiple actor reservation
%%              system
-module(grid_actor).

%%
%% Exported functions
%%
-export([start/1]).

%%
%% API functions
%%
start({X, Y, W, H}) ->
    {Specs, Grid} = create_empty_grid(X, Y, W, H),
    actor({Specs, Grid, []}).

%%
%% Local functions
%%

%% Create the empty grid specification
create_empty_grid(X, Y, W, H) ->
    EmptyRow = [ empty || _ <- lists:seq(1, W) ],
    Grid = [ EmptyRow || _ <- lists:seq(1, H) ],

    {{X, Y, W, H}, Grid}.

%% Main function for this actor
actor(ActorData) ->
    NewActorData =
        receive
            {Pid, Ref, get_grid_overview} ->
                get_grid_overview(ActorData, Pid, Ref);
            {Pid, Ref, reserve_cells, NumberOfCells} ->
                reserve_cells(ActorData, Pid, Ref, NumberOfCells);
            {Pid, Ref, request_specific_cells, ReservationId, Coordinates} ->
                request_specific_cells(ActorData, Pid, Ref, ReservationId, Coordinates);
            {Pid, Ref, release_specific_cells, ReservationId, Coordinates} ->
                release_specific_cells(ActorData, Pid, Ref, ReservationId, Coordinates)
        end,
    actor(NewActorData).

%% Return the current subgrid to the caller
get_grid_overview(ActorData, Pid, Ref) ->
    {Specs, Content, _} = ActorData,
    Pid ! {self(), Ref, get_grid_overview, Content, Specs},
    ActorData.

%% Reserve cells, not handled here
reserve_cells(ActorData, _Pid, _Ref, _NumberOfCells) ->
    ActorData.

%% Compute and return the intersection coordinates in the local
%% coordinate system
intersection(ActorData, Coordinates) ->
    {{X, Y, W, H}, _, _} = ActorData,
    {CX, CY, CW, CH} = Coordinates,
    if
        CX >= X + W; CY >= Y + H;
        CX + CW < X; CY + CH < Y ->
            none;
        true ->
            IX = max(CX, X),
            IY = max(CY, Y),
            IW = CW - (IX - CX) -
                if
                    CX + CW > X + W -> ((CX + CW) - (X + W));
                    true -> 0
                end,
            IH = CH - (IY - CY) -
                if
                    CY + CH > Y + H -> ((CY + CH) - (Y + H));
                    true -> 0
                end,
            %% Erlang indexes start at 1
            {IX - X + 1, IY - Y + 1, IW, IH}
    end.

%% Check if a region is empty
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

%% Mark a region as reserved
mark_region_reserved({X, Y, Width, Height}, GridContent) ->
    Row = lists:nth(Y, GridContent),
    ReservedCells = [ reserved || _ <- lists:seq(1, Width) ],

    {Begin, Rest} = lists:split(X - 1, Row),
    NewRow = Begin ++ ReservedCells ++ lists:nthtail(Width, Rest),

    NewGrid = lists:sublist(GridContent, Y - 1) ++ [NewRow] ++ lists:nthtail(Y, GridContent),

    if
        Height > 1 ->
            mark_region_reserved({X, Y + 1, Width, Height-1}, NewGrid);
        true ->
            NewGrid
    end.

%% Mark a region as empty (can happen if another grid actor fails
%% to allocate a part of the same request)
mark_region_empty({X, Y, Width, Height}, GridContent) ->
    Row = lists:nth(Y, GridContent),
    EmptyCells = [ empty || _ <- lists:seq(1, Width) ],

    {Begin, Rest} = lists:split(X - 1, Row),
    NewRow = Begin ++ EmptyCells ++ lists:nthtail(Width, Rest),

    NewGrid = lists:sublist(GridContent, Y - 1) ++ [NewRow] ++ lists:nthtail(Y, GridContent),

    if
        Height > 1 ->
            mark_region_reserved({X, Y + 1, Width, Height-1}, NewGrid);
        true ->
            NewGrid
    end.

%% Handle the specific requests
request_specific_cells(ActorData, Pid, Ref, ReservationId, Coordinates) ->
    {Specs, Content, FailedReservations} = ActorData,
    {NewActorData, Status} =
        case intersection(ActorData, Coordinates) of
            none ->
                %% Nothing to allocate in this actor
                {ActorData, success};
            IntersectionCoordinates ->
                case region_is_empty(IntersectionCoordinates, Content) of
                    false ->
                        %% Not empty, cannot allocate
                        {{Specs, Content,
                          %% Remember the ID in order not to release
                          %% the cells when cancelling this
                          %% reservation
                          [ReservationId|FailedReservations]},
                         failed};
                    true ->
                        %% Allocate
                        NewContent = mark_region_reserved(IntersectionCoordinates,
                                                          Content),
                        {{Specs, NewContent, FailedReservations},
                         success}
                end
        end,
    Pid ! {self(), Ref, request_specific_cells, Status},
    NewActorData.

%% Release a region previously allocated (in case another grid
%% actor has failed to allocate the region)
release_specific_cells(ActorData, Pid, Ref, ReservationId, Coordinates) ->
    {Specs, Content, FailedReservations} = ActorData,
    Result = lists:keyfind(ReservationId, 1, FailedReservations),
    {NewActorData, Status} =
        if
            Result ->
                %% The request already failed, so we don't need to
                %% mark its region empty, since it has never been
                %% allocated on this actor
                {ActorData, success};
            true ->
                case intersection(ActorData, Coordinates) of
                    none ->
                        %% Nothing to de-allocate in this actor
                        {ActorData, success};
                    IntersectionCoordinates ->
                        NewContent = mark_region_empty(IntersectionCoordinates,
                                                       Content),
                        {{Specs, NewContent},
                         success}
                end
            end,
    Pid ! {self(), Ref, request_specific_cells, Status},
    NewActorData.
