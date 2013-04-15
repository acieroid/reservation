%% Author: Quentin Stievenart
%% Created: Mar 21

%% Description: a simple benchmark to run on a multiple actors
%%              implementation
-module(bench).

%%
%% Exported functions
%%
-export([start/0, fill_with_clients/5]).

%%
%% API function
%%
start() ->
    %% Grid size (width and height, since it is square)
    GridSize = argument(gridsize, 300, integer),
    %% Number of actors to spawn
    Actors = argument(actors, 1, integer),
    %% Number of clients to spawn
    Clients = argument(clients, 4, integer),
    %% Percentage of the grid to fill with the clients (0-100)
    GridCompletion = argument(grid_completion, 50, integer),
    %% Percent of specific requests (0-100)
    PercentSpecificRequests = argument(percent_specific, 50, integer),
    random:seed(now()),
    Benchmark = argument(benchmark, single_actor, atom),
    benchmark(Benchmark, [GridSize, GridCompletion, PercentSpecificRequests,
                          Actors, Clients]).

fill_with_clients(GridSize, GridCompletion, PercentSpecificRequests, Actors, Clients) ->
    Pid =
        case Actors of
            0 -> reservation_single_actor:initialize(GridSize);
            _ -> reservation_multiple_actors:initialize(GridSize, Actors)
        end,
    spawn_clients(Pid, Clients, (GridCompletion*GridSize*GridSize)/100, PercentSpecificRequests, GridSize),
    wait_clients(Pid, Clients).

%%
%% Local functions
%%
argument(Name, Default, Type) ->
    case init:get_argument(Name) of
        {ok, [[Val]]} ->
            case Type of
                integer -> list_to_integer(Val);
                atom -> list_to_atom(Val)
            end;
        error -> Default
    end.

spawn_clients(Pid, N, CellsToAllocate, PercentSpecificRequests, GridSize) ->
    spawn_clients(Pid, N, CellsToAllocate, PercentSpecificRequests, GridSize, N).

spawn_clients(_, _, _, _, _, 0) ->
    done;
spawn_clients(Pid, N, CellsToAllocate, PercentSpecificRequests, GridSize, Clients) ->
    client:start(round(CellsToAllocate/N), Pid, self(),
                 %% Max cells per request
                 {random:uniform((GridSize * GridSize) div 50),
                  %% Percent of specific request
                  PercentSpecificRequests,
                  %% Grid width and height
                  GridSize, GridSize}),
    spawn_clients(Pid, N, CellsToAllocate, GridSize, Clients-1).

wait_clients(Pid, 0) ->
    exit(Pid, success);
wait_clients(Pid, Clients) ->
    %% TODO: catch errors ?
    receive
        {_, done} ->
            wait_clients(Pid, Clients-1)
    end.

benchmark(Name, Args) ->
    {Time, _} = timer:tc(bench, Name, Args),
    erlang:display(Time).
