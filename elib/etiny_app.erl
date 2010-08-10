-module(etiny_app).
-behaviour(application).
-export([boot/0, start/2, stop/1]).

boot() ->
  application:start(etiny_app).

start(_Type, _Args) ->
	error_logger:info_msg("~p starting with options ~p~n", [?MODULE, application:get_all_env(?MODULE)]),
	inets:start(),
	inets:start(httpd, [
		{port, 8000},
		{bind_address, {127,0,0,1}},
		{server_name, "localhost"},
		{server_root, "/tmp"},
		{document_root, "public"}
	]).

stop(_State) ->
  ok.
