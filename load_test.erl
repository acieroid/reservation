%% Author: Stefan Marr
%% Created: Feb 12, 2013
%%
%% Description: A simple rudimentary load test to check for the scaling 
%%              properties of the reservation system implementation.
%%
%%     WARNING: depending on your approach, this test is most-likely
%%              not sufficient, especially the load that the clients are
%%              producing is identical and represents pathologic behavior.
-module(load_test).

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start() ->
    erlang:display("Initialize Reservation System"),
    Pid = reservation_single_actor:initialize(300),
    
    erlang:display("Initialize Clients"),
    _C1 = client:start(20000, Pid, self(), {5, 100, 100}),
    _C2 = client:start(20000, Pid, self(), {5, 100, 100}),
    _C3 = client:start(20000, Pid, self(), {5, 100, 100}),
    _C4 = client:start(30000, Pid, self(), {5, 100, 100}),
    
    erlang:display("Wait till every client is done"),
    % hack to synchronize/join with the actors, not really nice
    receive {_, done} -> done end,
    receive {_, done} -> done end,
    receive {_, done} -> done end,
    receive {_, done} -> done end,
    
    erlang:display("Done").
