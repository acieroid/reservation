%% Author: Stefan Marr
%% Created: Feb 12, 2013
%%
%% Description: This is a simple single actor example implementation of the
%%              reservation interfaces.
%%
%%       Tests: This implementation is provided with test cases. However,
%%              those cases are neither complete nor general. They depend on
%%              implementation details of this sequential version. Thus, the
%%              tests will require changes to work with a parallel version.
-module(reservation_single_actor).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([initialize/1, actor/1]).

%%
%% API Functions
%%
initialize(GridSize) ->
    Grid = create_empty_grid(GridSize),
    spawn_link(?MODULE, actor, [Grid]).


actor(Grid) ->
    receive
        {Pid, get_size_of_resource} ->
            get_size_of_resource(Grid, Pid),
            NewGrid = Grid;
        
        {Pid, has_remaining_free_cells} ->
            has_remaining_free_cells(Grid, Pid),
            NewGrid = Grid;
        
        {Pid, get_grid_overview} ->
            get_grid_overview(Grid, Pid),
            NewGrid = Grid;
        
        {Pid, reserve_cells, NumberOfCells} ->
            NewGrid = reserve_cells(Grid, Pid, NumberOfCells);
        
        {Pid, request_specific_cells, ReservationId, Coordinates} ->
            NewGrid = request_specific_cells(Grid, Pid, ReservationId, Coordinates);
        
        Else ->
            erlang:display({unexpectedMessage, actor, Else}),
            NewGrid = Grid
    end,
    actor(NewGrid).

%%
%% Local Functions
%%
create_empty_grid(GridSize) ->
    Width = Height = GridSize,
    FreeCells = Width * Height,
    MaxAllocationSize = FreeCells / 50,
    
    EmptyRow = [ empty || _ <- lists:seq(1, Width) ],
    Grid     = [ EmptyRow || _ <- lists:seq(1, Height) ],
    
    UnspecificRequests = {[], 0},
    
    {{Width, Height}, Grid, MaxAllocationSize, FreeCells, UnspecificRequests}.

get_size_of_resource(Grid, Pid) ->
    {GridSize, _, _, _, _} = Grid,
    Pid ! {self(), get_size_of_resource, GridSize}.

has_remaining_free_cells(Grid, Pid) ->
    {_, _, _, FreeCells, _} = Grid,
    Pid ! {self(), has_remaining_free_cells, FreeCells > 0}.

get_grid_overview(Grid, Pid) ->
    {Dimensions, Content, _, _, _} = Grid,
    GridInfo = {{0,0}, Dimensions},
    Pid ! {self(), get_grid_overview, Content, GridInfo}.


reserve_cells(Grid, Pid, NumberOfCells) ->
    {Dimensions, Content, MaxAllocationSize, FreeCells, UnspecificRequests} = Grid,
    if 
        NumberOfCells > MaxAllocationSize ->
            Pid ! {self(), reserve_cells, failed, requestToLarge},
            Grid;
        NumberOfCells > FreeCells ->
            Pid ! {self(), reserve_cells, failed, not_enough_cells_available},
            Grid;
        true ->
            {UnspecReqList, NextId} = UnspecificRequests,
            Pid ! {self(), reserve_cells, success, {self(), NextId}},
            {Dimensions, 
                Content, 
                MaxAllocationSize, 
                FreeCells - NumberOfCells, 
                {UnspecReqList ++ [{NextId, NumberOfCells}], NextId}}
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
    
    

request_specific_cells(Grid, Pid, ReservationId, Coordinates) ->
    {{Width, Height}, Content, MaxAllocationSize, FreeCells, {UnspecificRequests, NextId}} = Grid,
    {X, Y, ReserveWidth, ReserveHeight} = Coordinates,
    
    Request = lists:keyfind(ReservationId, 1, UnspecificRequests),
    if
        % check that we still know about this request, or that the coordinates are off grid (< 1, > GridSize)
        false == Request; X < 1; Y < 1; (Y + ReserveHeight - 1) > Height; (X + ReserveWidth - 1) > Width ->
            % no, there was no request
            Pid ! {self(), request_specific_cells, ReservationId, failed},
            Grid;
        true ->
            % ok, we still have it
            {ReservationId, _} = Request,

            % now, make sure that all cells are empty
            AllCellsEmpty = region_is_empty(Coordinates, Content),
            if
                false == AllCellsEmpty ->
                    % there are cells which are not empty
                    Pid ! {self(), request_specific_cells, ReservationId, failed},
                    Grid;
                true ->
                    NewUnspecificRequests = lists:keydelete(ReservationId, 1, UnspecificRequests),
                    NewGrid = mark_region_reserved(Coordinates, Content),
                    Pid ! {self(), request_specific_cells, ReservationId, success},
                    {{Width, Height}, NewGrid, MaxAllocationSize, FreeCells, {NewUnspecificRequests, NextId}}
            end
    end.

%%
%% Test Functions For this specific implementation.
%% They are a partial definition of the semantics of the reservation interface
%% but also make certain assumption of its implementation.
%% Thus, they need to be reused with care.
%%
%% initialization_returns_pid_test() ->
%%     Pid = initialize(100),
%%     ?assert(is_pid(Pid)).

initialization_consistency_test() ->
    Pid = initialize(100),
    
    GridDimensions = reservation:get_size_of_resource(Pid),
    {Width, Height} = GridDimensions,
    ?assertMatch(100, (Width = Height)).


allocation_of_too_large_requests_fails_test() ->
    Pid = initialize(100),
    
    ?assertMatch({failed, requestToLarge},
                 reservation:reserve_cells(Pid, 201)),
    ?assertMatch({failed, requestToLarge},
                 reservation:reserve_cells(Pid, 401)),
    ?assertMatch({failed, requestToLarge},
                 reservation:reserve_cells(Pid, 1000)).

allocation_of_small_requests_ok_test() ->
    Pid = initialize(100),
    
    ?assertMatch({success, _},
                 reservation:reserve_cells(Pid, 200)),
    ?assertMatch({success, _},
                 reservation:reserve_cells(Pid, 5)),
    ?assertMatch({success, _},
                 reservation:reserve_cells(Pid, 1)).

has_remaining_free_cells_test() ->
    Pid = initialize(100),
    
    ?assertMatch(true, reservation:has_remaining_free_cells(Pid)),
    
    I = lists:seq(1, 1999),
    
    lists:foreach(fun(_) ->
                    ?assertMatch({success, _},
                                 reservation:reserve_cells(Pid, 5))
                          end, I),
    
    ?assertMatch(true, reservation:has_remaining_free_cells(Pid)),
    reservation:reserve_cells(Pid, 5),
    
    ?assertMatch(false, reservation:has_remaining_free_cells(Pid)).

grid_overview_test() ->
    Pid = initialize(20),
    
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
    
    [FirstRow | RemainingRows] = GridWithOne,
    [First | RemainingRow] = FirstRow,
    ?assertMatch(reserved, First),
    
    lists:foreach(fun(Cell) ->
                    ?assertMatch(empty, Cell)
                  end, RemainingRow),
    
    lists:foreach(fun(Row2) ->
                    ?assertMatch(EmptyRow, Row2)
                  end, RemainingRows).


no_overallocation_test() ->
    Pid = initialize(100),
    
    I = lists:seq(1, 2000),
    
    lists:foreach(fun(_) ->
                    ?assertMatch({success, _},
                                 reservation:reserve_cells(Pid, 5))
                          end, I),
    
    ?assertMatch({failed, not_enough_cells_available},
                 reservation:reserve_cells(Pid, 1)),
    ?assertMatch({failed, not_enough_cells_available},
                 reservation:reserve_cells(Pid, 5)).

no_double_allocation_of_cells_test() ->
    Pid = initialize(100),
    
    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 5),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {1, 1, 5, 1})),
    
    % failed either because it was already allocated or because the range is already taken
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {1, 1, 5, 1})),
    
    % failed because cells were specialized before
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ReservationId, {6, 1, 5, 1})),
    
    {success, {FPid2, RId2}} = reservation:reserve_cells(Pid, 5),
    % no double allocation
    ?assertMatch(failed, reservation:request_specific_cells(FPid2, RId2, {1, 1, 5, 1})),
    % but ok at some other place
   ?assertMatch(success, reservation:request_specific_cells(FPid2, RId2, {6, 1, 5, 1})).
    
    
no_specific_cells_outside_of_the_grid_test() ->
    Pid = initialize(100),
    
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
