-module(etiny_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(Conifg) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Conifg]).

init([Conifg]) ->
  {ok, {{one_for_one, 1, 60},
    [{etiny, {etiny, start_link, [Conifg]},
    permanent, brutal_kill, worker, [etiny]}]}}.