%% Author: Quentin Stievenart
%% Created: Mar 17, 2013

%% Description: This is the implementation of the manager of a
%%              multiple actor reservation system
-module(reservation_multiple_actors).

%%
%% Exported functions
%%
-export([initialize/2, prepare_actor/0]).


%%
%% API functions
%%
initialize(GridSize, NActors) ->
    %% Spawn the request manager (this actor)
    MainPid = spawn_link(?MODULE, prepare_actor, []),

    %% Launch the free cells actor
    FreeCellsActor = spawn_link(free_cells_actor, start,
                                [MainPid, GridSize]),
    %% Launch the allocator actor
    AllocatorActor = spawn_link(allocator, start,
                                [MainPid, GridSize,
                                 NActors, FreeCellsActor]),

    %% Send the PIDs to the request manager
    MainPid ! {ready, FreeCellsActor, AllocatorActor},
    %% Return the main PID
    MainPid.

%% Waits for the PIDs of the managed actors to start
prepare_actor() ->
    receive
        {ready, FreeCellsActor, AllocatorActor} ->
            actor(FreeCellsActor, AllocatorActor)
    end.

%%
%% Local function
%%

%% The main function of this actor
actor(FreeCellsActor, AllocatorActor) ->
    receive
        {Pid, get_size_of_resource} ->
            FreeCellsActor ! {Pid, get_size_of_resource};
        {Pid, has_remaining_free_cells} ->
            FreeCellsActor ! {Pid, has_remaining_free_cells};
        {Pid, reserve_cells, NumberOfCells} ->
            FreeCellsActor ! {Pid, reserve_cells, NumberOfCells};
        {Pid, request_specific_cells, ReservationId, Coordinates} ->
            %% This request is first validated by the free cells actor
            FreeCellsActor ! {Pid, get_reservation, ReservationId, Coordinates};

        {Pid, get_grid_overview} ->
            AllocatorActor ! {Pid, get_grid_overview};
        {FreeCellsActor, request_specific_cells, Pid, Ref, ReservationId, Coordinates} ->
            %% Message received when a specific request has been validated
            AllocatorActor ! {Pid, request_specific_cells, Ref, ReservationId, Coordinates};

        Else ->
            erlang:display({unexpected_message, manager, Else})
    end,
    actor(FreeCellsActor, AllocatorActor).

