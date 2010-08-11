-module(mod_etiny).

-include_lib("inets/src/http_server/httpd.hrl").

% HTTPD MODULE API
-export([do/1, remove/1]).

do(ReqData) ->
	error_logger:info_msg("Request: ~p~n", [ReqData]),
	case ReqData#mod.method of
		"GET" ->
			case ReqData#mod.request_uri of
				% Index page
				"/" -> {proceed, [ReqData]};
				% Tag
				"/" ++ Tag ->
					case etiny:get_url(Tag) of
						{ok, Url} -> 
							Head = [{code, 301}, {location, Url}],
							{proceed, [{response,{response,Head,nobody}}]};
						{error, _} -> {proceed, [ReqData]}
					end;
				% Something else
				_ -> {proceed, [ReqData]}
			end;
		"POST" -> {proceed, [ReqData]};
		_ -> {proceed, [ReqData]}
	end.

remove(_DB) ->
	ok.