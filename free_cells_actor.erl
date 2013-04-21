%% Author: Quentin Stievenart
%% Created: Apr 15, 2013

%% Description: This is the implementation of the actor that manages
%%              the free cells and the reservation IDs. It does not
%%              consists of multiple actors because of its simplicity
-module(free_cells_actor).

%%
%% Exported functions
%%
-export([start/2]).

%%
%% API functions
%%
start(MainPid, GridSize) ->
    prepare_actor({GridSize,          %% grid size
                   GridSize*GridSize, %% free cells
                   [], 0},            %% unspecific requests & next id
                  MainPid).

%%
%% Constants
%%
-define(MAX_REQUEST, 0.02). % A request can be max. 2% of the number of cells

%%
%% Local functions
%%

%% Prepare the actor, wait for the ready signal
prepare_actor(ActorData, MainPid) ->
    {GridSize, FreeCells, UnspecificRequests, NextId} = ActorData,
    receive
        {ready, AllocatorActor} ->
            actor({GridSize, FreeCells, UnspecificRequests, NextId, AllocatorActor},
                  MainPid)
    end.

%% Main function of the free cells actor
actor(ActorData, MainPid) ->
    NewActorData =
        receive
            {Pid, get_size_of_resource} ->
                get_size_of_resource(ActorData, MainPid, Pid);
            {Pid, has_remaining_free_cells} ->
                has_remaining_free_cells(ActorData, MainPid, Pid);
            {Pid, reserve_cells, NumberOfCells} ->
                reserve_cells(ActorData, MainPid, Pid, NumberOfCells);
            {Pid, request_specific_cells, ReservationId, Coordinates} ->
                request_specific_cells(ActorData, MainPid, Pid, ReservationId, Coordinates)
        end,
    actor(NewActorData, MainPid).

%% Send the grid size
get_size_of_resource(ActorData, MainPid, Pid) ->
    {GridSize, _, _, _, _} = ActorData,
    Pid ! {MainPid, get_size_of_resource, {GridSize, GridSize}},
    ActorData.

%% Check if we still have free cells
has_remaining_free_cells(ActorData, MainPid, Pid) ->
    {_, FreeCells, _, _, _} = ActorData,
    Pid ! {MainPid, has_remaining_free_cells, FreeCells > 0},
    ActorData.

%% Reserve unspecific cells
reserve_cells(ActorData, MainPid, Pid, NumberOfCells) ->
    {GridSize, FreeCells, UnspecificRequests, NextId, AllocatorActor} = ActorData,
    if
        NumberOfCells > GridSize*GridSize * ?MAX_REQUEST ->
            %% Too many cells requested
            Pid ! {MainPid, reserve_cells, failed, request_too_large},
            ActorData;
        NumberOfCells > FreeCells ->
            %% Not enough cells remaining
            Pid ! {MainPid, reserve_cells, failed, not_enough_cells_available},
            ActorData;
        true ->
            %% Correct request. Send this actor as the next entry
            %% point, since it will need to validate the specific
            %% request.
            Pid ! {MainPid, reserve_cells, success, {self(), NextId}},
            {GridSize, FreeCells - NumberOfCells,
             %% TODO: concatenating is a bad idea
             UnspecificRequests ++ [{NextId, NumberOfCells}],
             NextId+1,
             AllocatorActor}
    end.

%% Send back the reservation corresponding to the id given, or
%% notifies the client that the id is incorrect. If the ID is
%% incorrect, no response is sent to the main pid.
request_specific_cells(ActorData, MainPid, Pid, ReservationId, Coordinates) ->
    {GridSize, FreeCells, UnspecificRequests, NextId, AllocatorActor} = ActorData,
    {X, Y, W, H} = Coordinates,
    NumberOfCells = W*H,
    {_, Request} = lists:keyfind(ReservationId, 1, UnspecificRequests),
    if
        not Request;
        X < 1; Y < 1;
        (X + W - 1) > GridSize;
        (Y + H - 1) > GridSize;
        not (Request == NumberOfCells) ->
            %% Invalid request or request not found
            Pid ! {self(), request_specific_cells, ReservationId, failed},
            ActorData;
        true ->
            %% Request valid and found, sends it to the allocator
            Ref = make_ref(),
            AllocatorActor ! {Pid, request_specific_cells, Ref, ReservationId, Coordinates},
            NewUnspecificRequests = lists:keydelete(ReservationId, 1, UnspecificRequests),
            {GridSize, FreeCells, NewUnspecificRequests, NextId, AllocatorActor}
    end.
