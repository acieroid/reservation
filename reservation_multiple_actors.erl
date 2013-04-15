%% Author: Quentin Stievenart
%% Created: Mar 17, 2013

%% Description: This is the implementation of the manager of a
%%              multiple actor reservation system
-module(reservation_multiple_actors).

%%
%% Exported functions
%%
-export([initialize/2, actor/2]).


%%
%% API functions
%%
initialize(GridSize, NActors) ->
    %% Launch the free cells actor
    FreeCellsActor = spawn_link(free_cells_actor, start,
                                [self(), GridSize]),
    %% Launch the allocator actor
    AllocatorActor = spawn_link(allocator, start,
                                [GridSize, NActors, FreeCellsActor]),
    %% Spawn the request manager (this actor)
    spawn_link(?MODULE, actor, [FreeCellsActor, AllocatorActor]).

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
            AllocatorActor ! {Pid, reserve_specific_cells, Ref, ReservationId, Coordinates};

        Else ->
            erlang:display({unexpected_message, manager, Else})
    end,
    actor(FreeCellsActor, AllocatorActor).

