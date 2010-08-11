-module(etiny_app).
-behaviour(application).
-export([boot/0, start/2, stop/1]).

boot() ->
  application:start(etiny_app).

start(_Type, _Args) ->
	error_logger:info_msg("~p starting with options ~p~n", [?MODULE, application:get_all_env(?MODULE)]),
	inets:start(),
	Defaults = orddict:from_list([
		{port, 8000},
		{bind_address, {127,0,0,1}},
		{server_name, "localhost"},
		{server_root, "/tmp"},
		{document_root, "public"},
		{directory_index, ["index.html"]},
		{modules, [mod_etiny, mod_alias, mod_esi, mod_actions, mod_cgi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log]}
	]),
	HttpdArgs = case application:get_env(?MODULE, httpd) of
		{ok, Overrides} -> orddict:merge(fun(_K, A, _B) -> A end, orddict:from_list(Overrides), Defaults);
		undefined -> Defaults
	end,
	{ok, CassandraOpts} = application:get_env(?MODULE, cassandra),
	etiny_sup:start_link(CassandraOpts),
	inets:start(httpd, HttpdArgs).

stop(_State) ->
  ok.
