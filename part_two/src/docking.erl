%%%-------------------------------------------------------------------
%%% @author ujuezeoke
%%% @copyright (C) 2018
%%% @doc
%%% The docking station contains a number of charging points
%%% @end
%%% Created : 13. Oct 2018 21:26
%%%-------------------------------------------------------------------
-module(docking).
-author("ujuezeoke").
-behaviour(gen_statem).

%% Core docking API
-export([start_link/3, release_moped/1, secure_moped/1, get_info/1]).

%% Exports required for gen_statem behaviour
-export([callback_mode/0, init/1]).

%%Exports required to represent the states of this system
-export([idle/3, empty/3, full/3]).

%%% starts the docking station
%%% Returns {ok, Pid}.
start_link(Total, Occupied, Name) ->
  gen_statem:start_link({local, Name}, ?MODULE, [Name, Total, Occupied], []).

%%% The user uses this function to receive a moped from the docking station
%%% Returns ok or {error, empty}.
release_moped(Name) ->
  gen_statem:call(Name, release_moped).

%%% The user uses this function to return a moped to the docking station
%%% Returns ok or {error, full}.
secure_moped(Name) ->
  gen_statem:call(Name, secure_moped).

%%% This is used to retrieve the state of the docking station.
%%% It is assumed that get_info should return information irrespective of the state the system is in.
%%% {Name, [{total, Total}, {occupied, Occupied}, {free, Total - Occupied}]}}].
get_info(Name) ->
  gen_statem:call(Name, get_info).

callback_mode() ->
  state_functions.

init([Name, Total, Occupied]) ->
  {ok, idle, [Name, Total, Occupied]}.

idle({call, From}, release_moped, [Name, Total, 1]) ->
  {next_state, empty, [Name, Total, 0], [{reply, From, ok}]};
idle({call, From}, secure_moped, [Name, Total, Occupied]) when Total == Occupied + 1 ->
  {next_state, full, [Name, Total, Total], [{reply, From, ok}]};
idle({call, From}, release_moped, [Name, Total, Occupied]) ->
  {keep_state, [Name, Total, Occupied - 1], [{reply, From, ok}]};
idle({call, From}, secure_moped, [Name, Total, Occupied]) ->
  {keep_state, [Name, Total, Occupied + 1], [{reply, From, ok}]};
idle({call, From}, get_info, [Name, Total, Occupied]) ->
  get_info(From, Name, Total, Occupied).

empty({call, From}, release_moped, [Name, Total, 0]) ->
  {next_state, empty, [Name, Total, 0], [{reply, From, {error, empty}}]};
empty({call, From}, secure_moped, [Name, Total, 0]) ->
  {next_state, idle, [Name, Total, 1], [{reply, From, ok}]};
empty({call, From}, get_info, [Name, Total, Occupied]) ->
  get_info(From, Name, Total, Occupied).

full({call, From}, release_moped, [Name, Total, Total]) ->
  {next_state, idle, [Name, Total, Total - 1], [{reply, From, ok}]};
full({call, From}, secure_moped, [Name, Total, Total]) ->
  {next_state, full, [Name, Total, Total], [{reply, From, {error, full}}] };
full({call, From}, get_info, [Name, Total, Occupied]) ->
  get_info(From, Name, Total, Occupied).

get_info(From, Name, Total, Occupied) ->
  {keep_state, [Name, Total, Occupied],
    [{reply, From, {Name, [{total, Total}, {occupied, Occupied}, {free, Total - Occupied}]}}] }.
