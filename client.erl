%% Author: Stefan Marr
%% Created: Feb 12, 2013
%%
%% Description: This client is an example user for the reservation API.
%%              It will use a random generator with a hardcoded seed value,
%%              to generate a list of allocations that are then performed in
%%              order on the reservation system.
%%              Since the seed is identical for all clients, this will lead to
%%              identical allocation requests, which is not optimal for testing.
%%
%%     WARNING: This client is not very smart. Depending on a chosen
%%              implementation strategy for the reservation system, it can be
%%              near the worst case scenario of load.
-module(client).

%%
%% Exported Functions
%%
-export([start/4, actor/4]).

%%
%% API Functions
%%

%% @doc start a client to generate load on the given reservation service
-spec start(integer(), 
            pid(), %% @doc the reservation system
            pid(), %% @doc the master actor/program that started the client
            {integer(), integer(), integer()})
        -> pid().
start(NumberOfCellsToBeAllocated,
      EntryPointPid,
      MasterPid,
      {MaxCellsPerRequest, Width, Height}) ->
  GridDetails = {MaxCellsPerRequest, Width, Height},
  spawn_link(?MODULE, actor, [NumberOfCellsToBeAllocated,
                              EntryPointPid, MasterPid, GridDetails]).

%%
%% Local Functions
%%
actor(NumberOfCellsToBeAllocated, EntryPointPid, MasterPid, GridDetails) ->
  AllocationPlan = create_allocation_plan(NumberOfCellsToBeAllocated, GridDetails),
  do_all_allocations(AllocationPlan, EntryPointPid),
  MasterPid ! {self(), done}.

do_all_allocations([], _) ->
    done;
do_all_allocations([Allocation | RemainingAllocs], EntryPointPid) ->
    Result = do_allocation(Allocation, EntryPointPid),
    case Result of
        {success, _} -> ok;
        _Else        -> ignored %erlang:display(Else)
    end,
    do_all_allocations(RemainingAllocs, EntryPointPid).


%% Does the allocation by decoding one of the items from the plan
%% and sending first the reserve_cells request to the reservation
%% system and if necessary also does the reservation of some sepcific
%% cells.
%% If the reservation of specific cells fails, there is does not try
%% to request for other cells, but imediatly aborts
do_allocation({AllocCells}, EntryPointPid) ->
    reservation:has_remaining_free_cells(EntryPointPid),
    Result = reservation:reserve_cells(EntryPointPid, AllocCells),
    case Result of
        {failed, Reason} ->
            erlang:display({basicReservationFailed, shouldNotHappen, Reason}),
            {failed, Reason};
        Else -> Else
    end;
do_allocation({AllocCells, {Row, Column}}, EntryPointPid) ->
    Result = do_allocation({AllocCells}, EntryPointPid),
    case Result of
        {success, {FollowUpPid, ReservationId}} -> 
            reservation:get_grid_overview(FollowUpPid),
            Result2 = reservation:request_specific_cells(FollowUpPid, ReservationId, {Row, Column, AllocCells, 1}),
            {Result2, {ignored, FollowUpPid, ReservationId, {Row, Column}}};
        Else ->
            Else
    end.

%% Create the allocation plan using some hardcoded seed for
%% the random number generator
create_allocation_plan(NumberOfCellsToBeAllocated, GridDetails) ->
    random:seed(42, 42, 42),
    create_reservation_list(NumberOfCellsToBeAllocated, GridDetails, []).

create_reservation_list(0, _, AccList) ->
    AccList;
create_reservation_list(NumberOfCellsToBeAllocated, GridDetails, AccList) ->
    {MaxCellsPerRequest, Width, Height} = GridDetails,
    
    % determine the number of cells
    AllocCells = random:uniform(erlang:min(MaxCellsPerRequest, NumberOfCellsToBeAllocated)),
    
    % determine whether to request specific cells or not
    case random:uniform(3) of
        1     -> % no specialization
            AllocRequest = {AllocCells};
        _Else -> % do specialization in approx. 66% of the cases
            Row    = random:uniform(Height),
            Column = random:uniform(Width - MaxCellsPerRequest),
            AllocRequest = {AllocCells, {Row, Column}}
    end,
    
    NewAccList = AccList ++ [AllocRequest],
    create_reservation_list(NumberOfCellsToBeAllocated - AllocCells,
                            GridDetails,
                            NewAccList).
