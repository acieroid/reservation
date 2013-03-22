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
    erlang:display("Starting benchmark"),
    GridSize = argument(gridsize, 300),
    erlang:display("Grid size: " ++ integer_to_list(GridSize)),
    Actors = argument(actors, 1),
    erlang:display("Number of actors: " ++ integer_to_list(Actors)),
    Clients = argument(clients, 4),
    erlang:display("Number of clients: " ++ integer_to_list(Clients)),

    benchmark(single_actor, [GridSize, Clients]),
    benchmark(fill_with_clients, [GridSize, Actors, Clients]),

    erlang:display("Done").

fill_with_clients(GridSize, Actors, Clients) ->
    Pid = reservation_multiple_actors:initialize(GridSize, Actors),
    spawn_clients(Pid, Clients, GridSize*GridSize),
    wait_clients(Clients).

single_actor(GridSize, Clients) ->
    Pid = reservation_single_actor:initialize(GridSize),
    spawn_clients(Pid, Clients, GridSize*GridSize),
    wait_clients(Clients).
%%
%% Local functions
%%
argument(Name, Default) ->
    case init:get_argument(Name) of
        {ok, [[Val]]} -> list_to_integer(Val);
        error -> Default
    end.

spawn_clients(Pid, N, CellsToAllocate) ->
    spawn_clients(Pid, N, CellsToAllocate, N).

spawn_clients(_, 0, _, _) ->
    done;
spawn_clients(Pid, N, CellsToAllocate, Clients) ->
    %% TODO: implement a better client
    client:start(round(CellsToAllocate/Clients), Pid, self(), {5, 100, 100}),
    spawn_clients(Pid, N-1, CellsToAllocate, Clients).

wait_clients(0) ->
    done;
wait_clients(Clients) ->
    receive
        {_, done} ->
            wait_clients(Clients-1)
    end.

benchmark(Name, Args) ->
    {Time, _} = timer:tc(bench, Name, Args),
    erlang:display({time, Name, Time}).
