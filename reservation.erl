%% Author: Stefan Marr
%% Created: Feb 12, 2013
%%
%% Description: This module provides the interface that is used to interact
%%              with an implementation of the reservation system.
%%
%%              The interface is design to be synchrounous and waits for
%%              the reply of the reservation system.
-module(reservation).

%%
%% Exported Functions
%%
-export([    get_size_of_resource/1, 
         has_remaining_free_cells/1,
                get_grid_overview/1,
               write_grid_to_file/2,
                    reserve_cells/2,
           request_specific_cells/3]).

%%
%% API Functions
%%

%% Query for the size of the grid.
-spec get_size_of_resource(pid())  ->  {integer(),   %% @doc grid width
                                        integer()}.  %% @doc grid height
get_size_of_resource(EntryPointPid) ->
    EntryPointPid ! {self(), get_size_of_resource},
    
    receive
        {EntryPointPid, get_size_of_resource, {Width, Height}} ->
            {Width, Height}
    end.

%% Query whether there are still free cells left.
%%
%% In case this operation is executed by the same client
%% after a reserve_cells function that failed because
%% there are no free values left, this function
%% should also return false.
%% 
%% REMARK: no strong guarantees are provided for this
%%         operation, since it is a racy operation in
%%         cooperation with reserve_cells anyway
-spec has_remaining_free_cells(pid()) -> boolean(). 
has_remaining_free_cells(EntryPointPid) ->
    EntryPointPid ! {self(), has_remaining_free_cells},

    receive
        {EntryPointPid, has_remaining_free_cells, Result} ->
            Result
    end.


%% Allows to query for an overview of the grid.
%% However, the implementation is not specified.
%% The result should enable the client to make decision on where
%% to allocate specific cells on the grid.
-spec get_grid_overview(pid()) ->
          {any(),  %% @doc the grid in some undefined form, specific to implementation 
           any()}. %% @doc some general information on the grid structure
get_grid_overview(SomePid) ->
    SomePid ! {self(), get_grid_overview},
    
    receive
        {SomePid, get_grid_overview, Grid, GridInfo} ->
            {Grid, GridInfo}
    end.


%% Reserves a number of cells, without requiring specific cells to be
%% allocated.
%% It is the first step and returns the necessary information to allow for an
%% additional optional step of requesting specific cells on the grid.
-spec reserve_cells(pid(), integer()) ->
                    {atom(),             %% @doc indicates SuccessOrFailure
                      {pid(), integer()} %% @doc on success, a followup pid, and a ReservationId
                      | atom()}.         %% @doc on failure, an atom indicating the reason
reserve_cells(EntryPointPid, NumberOfCells) ->
    EntryPointPid ! {self(), reserve_cells, NumberOfCells},
    
    receive
        {EntryPointPid, reserve_cells, SuccessOrFailure, Info} ->
            % if SuccessOrFailure = success
            %   Info = {FollowUpPid, ReservationId}
            % else
            %   Info = 'atom indicating reason'
            {SuccessOrFailure, Info};
        Else ->
            erlang:display({unexpectedMessage, reserve_cells, Else}),
            Else
    end.

%% This is the second, optional step to reserve specific cells of the grid.
-spec request_specific_cells(pid(), integer(),
                            {integer(), integer(), integer(), integer()}) %% @doc coordinates of region to be reseted
        -> atom(). %% @doc indicates success or failure
request_specific_cells(FollowUpPid, ReservationId, {X, Y, Width, Height}) ->
    FollowUpPid ! {self(), request_specific_cells, ReservationId, {X, Y, Width, Height}},
    
    receive
        {FollowUpPid, request_specific_cells, ReservationId, SuccessOrFailure} ->
            SuccessOrFailure
    end.

write_grid_to_file(FollowUpPid, Path) ->
    {Grid, _} = get_grid_overview(FollowUpPid),
    {ok, F} = file:open(Path, [write]),
    lists:map(fun (Row) ->
                      lists:map(fun(Cell) ->
                                        case Cell of
                                            empty ->
                                                file:write(F, "0");
                                            reserved ->
                                                file:write(F, "1")
                                        end,
                                        %% TODO: don't add this for the last element
                                        file:write(F, ",")
                                end,
                                Row),
                      file:write(F, "\n")
              end,
              Grid),
    ok = file:close(F).
