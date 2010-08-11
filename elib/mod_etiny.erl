-module(mod_etiny).

-include_lib("inets/src/http_server/httpd.hrl").

% HTTPD MODULE API
-export([do/1, remove/1]).


do(ReqData = #mod{method="GET", request_uri="/" ++ Tag}) when Tag =/= "" ->
	case etiny:get_url(Tag) of
		{ok, Url} -> 
			Head = [{code, 301}, {location, Url}],
			{proceed, [{response,{response,Head,nobody}}]};
		{error, _} -> {proceed, [ReqData]}
	end;
do(ReqData = #mod{method="POST", entity_body=Body}) ->
	Args = httpd:parse_query(Body),
	error_logger:info_msg("Parsed arg data: ~p~n", [Args]),
	case proplists:get_value("url", Args) of
		undefined ->
			Head = [{code, 301}, {location, "/#error=\"bad_request\""}],
			{proceed, [{response,{response,Head,nobody}}]};
		Url ->
			{ok, Tag} = etiny:store_url(Url),
			Head = [{code, 301}, {location, "/#saved="++Tag++""}],
			{proceed, [{response,{response,Head,nobody}}]}
	end;
do(ReqData) ->
	{proceed, [ReqData]}.


remove(_DB) ->
	ok.