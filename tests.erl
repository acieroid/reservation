%% Author: Quentin Stievenart
%% Created: Mar 21, 2013

%% Description: This file contains the tests for the multiple actor
%%              implementation
-module(tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported functions
%%
-export([]).

%%
%% Test functions
%%

initialize(N, P) ->
    reservation_multiple_actors:initialize(N, P).

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
    {success, {FollowUpPid, SecondReservationId}} = reservation:reserve_cells(Pid, 64),
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, SecondReservationId, {48, 48, 8, 8})),

    % reserve cells on another subgrid
    {success, {FollowUpPid, ThirdReservationId}} = reservation:reserve_cells(Pid, 16),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ThirdReservationId, {46, 46, 4, 4})).

reserve_cells_simple_test() ->
    Pid = initialize(20, 4),

    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 4),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {1, 1, 2, 2})),

    {success, {FollowUpPid, SecondReservationId}} = reservation:reserve_cells(Pid, 4),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, SecondReservationId, {5, 5, 2, 2})),

    {success, {FollowUpPid, ThirdReservationId}} = reservation:reserve_cells(Pid, 4),
    ?assertMatch(failed, reservation:request_specific_cells(FollowUpPid, ThirdReservationId, {1, 1, 2, 2})).

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

    [FirstRow | RemainingRows] = GridWithOne,
    [First | RemainingRow] = FirstRow,
    ?assertMatch(reserved, First),

    lists:foreach(fun(Cell) ->
                    ?assertMatch(empty, Cell)
                  end, RemainingRow),

    lists:foreach(fun(Row2) ->
                    ?assertMatch(EmptyRow, Row2)
                  end, RemainingRows).

butlast([_|[]]) ->
    [];
butlast([H|T]) ->
    [H|butlast(T)].

bottom_right_grid_overview_test() ->
    Pid = initialize(20, 4),

    {Grid, _} = reservation:get_grid_overview(Pid),

    % Everything should be still empty
    ?assertMatch(20, length(Grid)),
    EmptyRow = [ empty || _ <- lists:seq(1, 20) ],
    lists:foreach(fun(Row) ->
                    ?assertMatch(EmptyRow, Row)
                  end, Grid),

    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 1),
    Cells = {20, 20, 1, 1},
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, Cells)),

    {GridWithOne, _} = reservation:get_grid_overview(Pid),

    LastRow = lists:last(GridWithOne),
    OtherRows = butlast(GridWithOne),
    Last = lists:last(LastRow),
    OtherRow = butlast(LastRow),
    ?assertMatch(reserved, Last),

    lists:foreach(fun(Cell) ->
                    ?assertMatch(empty, Cell)
                  end, OtherRow),

    lists:foreach(fun(Row2) ->
                    ?assertMatch(EmptyRow, Row2)
                  end, OtherRows).

reserve_corners_test() ->
    Pid = initialize(40, 4),
    {success, {FollowUpPid, ReservationId}} = reservation:reserve_cells(Pid, 16),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ReservationId, {1, 1, 4, 4})),

    {success, {FollowUpPid, SecondReservationId}} = reservation:reserve_cells(Pid, 16),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, SecondReservationId, {37, 1, 4, 4})),

    {success, {FollowUpPid, ThirdReservationId}} = reservation:reserve_cells(Pid, 16),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, ThirdReservationId, {1, 37, 4, 4})),

    {success, {FollowUpPid, FourthReservationId}} = reservation:reserve_cells(Pid, 16),
    ?assertMatch(success, reservation:request_specific_cells(FollowUpPid, FourthReservationId, {37, 37, 4, 4})).
