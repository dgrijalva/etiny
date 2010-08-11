-module(etiny_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    start(etiny).

start(App) ->
    start_ok(App, application:start(App)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) -> 
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) -> 
    erlang:error({app_start_failed, App, Reason}).

start(_Type, _Args) ->
	error_logger:info_msg("~p starting with options ~p~n", [?MODULE, application:get_all_env(?MODULE)]),
	etiny_sup:start_link().

stop(_State) ->
  ok.
