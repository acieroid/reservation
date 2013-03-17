%% Author: Quentin Stievenart
%% Created: Mar 17, 2013

%% Description: This is the implementation of the actors that manage
%%              part of the grid for the multiple actor reservation
%%              system
-module(grid_actor).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported functions
%%
-export([actor/1]).

%%
%% API functions
%%
actor(Spec) ->
    erlang:display(Spec).
