%%%-------------------------------------------------------------------
%%% @author ujuezeoke
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2018 15:17
%%%-------------------------------------------------------------------
-module(ev_supervisor).
-author("ujuezeoke").
-behaviour(supervisor).

%% Core API
-export([start_link/0, start_child/3]).

%% Exports required for supervisor behaviour
-export([init/1]).

start_link() ->
  ets:new(children_names, [set, named_table]),
  supervisor:start_link({local, ?MODULE},?MODULE, []).

%% TODO Lock the table as the write is happening.
start_child(Total, Occupied, Name) ->
  ChildSpec = #{id => Name,
    start => {docking, start_link, [Total, Occupied, Name]},
    restart => permanent,
    shutdown => 3000,
    type => worker,
    modules => []},
  ets:insert(children_names, {Name}),
  supervisor:start_child(?MODULE, ChildSpec).

init(_Args) ->
  process_flag(trap_exit, true),
  SupFlags = #{strategy => one_for_one},
  {ok, {SupFlags, []}}.