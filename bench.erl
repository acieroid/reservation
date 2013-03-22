%% Author: Quentin Stievenart
%% Created: Mar 21

%% Description: a simple benchmark to run on a multiple actors
%%              implementation
-module(bench).

%%
%% Exported functions
%%
-export([start/0, fill_with_clients/3, single_actor/2]).

%%
%% API function
%%
start() ->
    benchmarks({300, 50, 350}, %% grid size, not really important
               {4, 2, 6}, %% number of clients, not important neither
               {1, 1, 8}). %% number of actors, important

fill_with_clients(GridSize, Actors, Clients) ->
    Pid = reservation_multiple_actors:initialize(GridSize, Actors),
    spawn_clients(Pid, Clients, GridSize*GridSize),
    wait_clients(Pid, Clients).

single_actor(GridSize, Clients) ->
    Pid = reservation_single_actor:initialize(GridSize),
    spawn_clients(Pid, Clients, GridSize*GridSize),
    wait_clients(Pid, Clients).
%%
%% Local functions
%%
benchmarks({_GridSizeStart, _GridSizeStep, GridSizeStop},
           {_ClientsStart, _ClientsStep, ClientsStop},
           {_ActorsStart, _ActorsStep, ActorsStop},
           GridSizeStop, ClientsStop, ActorsStop) ->
    done;
benchmarks({GridSizeStart, GridSizeStep, GridSizeStop},
           {ClientsStart, ClientsStep, ClientsStop},
           {ActorsStart, ActorsStep, ActorsStop},
           GridSizeStop, ClientsStop, Actors) ->
    benchmarks({GridSizeStart, GridSizeStep, GridSizeStop},
               {ClientsStart, ClientsStep, ClientsStop},
               {ActorsStart, ActorsStep, ActorsStop},
               GridSizeStart,
               ClientsStart,
               Actors + ActorsStep);
benchmarks({GridSizeStart, GridSizeStep, GridSizeStop},
           {ClientsStart, ClientsStep, ClientsStop},
           {ActorsStart, ActorsStep, ActorsStop},
           GridSizeStop, Clients, Actors) ->
    benchmarks({GridSizeStart, GridSizeStep, GridSizeStop},
               {ClientsStart, ClientsStep, ClientsStop},
               {ActorsStart, ActorsStep, ActorsStop},
               GridSizeStart,
               Clients + ClientsStep,
               Actors);
benchmarks({GridSizeStart, GridSizeStep, GridSizeStop},
           {ClientsStart, ClientsStep, ClientsStop},
           {ActorsStart, ActorsStep, ActorsStop},
           GridSize, Clients, Actors) ->
    bench(GridSize, Clients, Actors),
    benchmarks({GridSizeStart, GridSizeStep, GridSizeStop},
               {ClientsStart, ClientsStep, ClientsStop},
               {ActorsStart, ActorsStep, ActorsStop},
               GridSize + GridSizeStep,
               Clients,
               Actors).

benchmarks({GridSizeStart, GridSizeStep, GridSizeStop},
           {ClientsStart, ClientsStep, ClientsStop},
           {ActorsStart, ActorsStep, ActorsStop}) ->
    benchmarks({GridSizeStart, GridSizeStep, GridSizeStop},
               {ClientsStart, ClientsStep, ClientsStop},
               {ActorsStart, ActorsStep, ActorsStop},
               GridSizeStart, ClientsStart, ActorsStart).

bench(GridSize, Clients, Actors) ->
    erlang:display("Grid size: " ++ integer_to_list(GridSize)),
    erlang:display("Number of actors: " ++ integer_to_list(Actors)),
    erlang:display("Number of clients: " ++ integer_to_list(Clients)),
    benchmark(single_actor, [GridSize, Clients]),
    benchmark(fill_with_clients, [GridSize, Actors, Clients]).

spawn_clients(Pid, N, CellsToAllocate) ->
    spawn_clients(Pid, N, CellsToAllocate, N).

spawn_clients(_, 0, _, _) ->
    done;
spawn_clients(Pid, N, CellsToAllocate, Clients) ->
    %% TODO: implement a better client
    client:start(round(CellsToAllocate/Clients), Pid, self(), {5, 100, 100}),
    spawn_clients(Pid, N-1, CellsToAllocate, Clients).

wait_clients(Pid, 0) ->
    exit(Pid, succes);
wait_clients(Pid, Clients) ->
    receive
        {_, done} ->
            wait_clients(Pid, Clients-1)
    end.

benchmark(Name, Args) ->
    erlang:display({starting, Name, Args}),
    {Time, _} = timer:tc(bench, Name, Args),
    erlang:display({time, Name, Args, Time}).
