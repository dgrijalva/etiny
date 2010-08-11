-module(etiny_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_httpd/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Config) ->
  {ok, {{one_for_one, 1, 60},
    [
		{etiny, {etiny, start_link, []},
		    permanent, brutal_kill, worker, []},
		{etiny_httpd, {?MODULE, start_httpd, []},
		    permanent, brutal_kill, worker, []}
	]}}.
	
start_httpd() ->
	Defaults = orddict:from_list([
		{port, 8000},
		{bind_address, {127,0,0,1}},
		{server_name, "localhost"},
		{server_root, "/tmp"},
		{document_root, "public"},
		{directory_index, ["index.html"]},
		{modules, [mod_etiny, mod_alias, mod_esi, mod_actions, mod_cgi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log]}
	]),
	HttpdArgs = case application:get_env(etiny, httpd) of
		{ok, Overrides} -> orddict:merge(fun(_K, A, _B) -> A end, orddict:from_list(Overrides), Defaults);
		undefined -> Defaults
	end,
	inets:start(httpd, HttpdArgs).
	