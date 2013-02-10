%%%----------------------------------------------------------------------
%%% File    : echo_sup.erl
%%% Author  : Pete Kazmier <pete-trapexit@kazmier.com>
%%% Purpose : Port Tutorial
%%% Created : Fri Jan 13 12:39:27 EST 2006
%%%----------------------------------------------------------------------

-module(echo_sup).
-author('pete-trapexit@kazmier.com').

-behavior(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(ExtProg) ->
    supervisor:start_link(echo_sup, ExtProg).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

init(ExtProg) ->
    {ok, {{one_for_one, 3, 10},
          [{echo, {echo, start_link, [ExtProg]},
            permanent, 10, worker, [echo]}]}}.
