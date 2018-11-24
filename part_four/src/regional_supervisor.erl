%%%-------------------------------------------------------------------
%%% @author ujuezeoke
%%% @copyright (C) 2018, Uju Ezeoke
%%% @doc
%%% This monitors the ev_supervisors started up in other Nodes.
%%% I clearly cannot use behaviours for this. I may need to implement the supervisor behaviour
%%% By hand.
%%% @end
%%% Created : 04. Nov 2018 13:39
%%%-------------------------------------------------------------------
-module(regional_supervisor).
-author("ujuezeoke").
-behaviour(supervisor).

%% API
-export([init/1]).
-export([start_link/0]).


init(Args) ->
  process_flag(trap_exit, true),
  SupFlags = #{strategy => simple_one_for_one},
  {ok, {SupFlags, []}}.

start_link() ->
  supervisor:start_link({global, ?MODULE, ?MODULE},?MODULE, []).