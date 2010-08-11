-module(etiny).
-behavior(gen_server).

%% api
-export([start_link/1, get_url/1, store_url/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cassandra/cassandra_types.hrl").

-record(state, {connection = undefined, keyspace = undefined, host = undefined, port = undefined}).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start(Args) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

get_url(Token) ->
  gen_server:call(?MODULE, {get_url, Token}, timer:seconds(5)).

store_url(Url) ->
  gen_server:call(?MODULE, {store_url, Url}, timer:seconds(5)).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Options) ->
	error_logger:info_msg("~p starting with options ~p~n", [?MODULE, Options]),
	{ok, connect(#state{
		host = proplists:get_value(host, Options),
		port = proplists:get_value(port, Options),
		keyspace = proplists:get_value(keyspace, Options)
	})}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_url, Token}, _From, State) ->
	{reply, url_for_token(Token, State), State};
handle_call({store_url, Url}, _From, State) ->
	{reply, token_for_stored_url(Url, State), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(Msg, State) ->
  error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%====================================================================
%% Internal
%%====================================================================

% Connect to cassandra and return an updated state record.
connect(State) ->
	{ok, C} = thrift_client:start_link(State#state.host, State#state.port, cassandra_thrift),
	State#state{connection = C}.
	
% Microsecond timestamps for cassandra
timestamp() ->
	{MegaSecs, Secs, Microsec} = now(),
	MegaSecs*1000000000000 + Secs*1000000 + Microsec.

% Lookup a url from a token
% Uses cassandra 'get' method: http://wiki.apache.org/cassandra/API
% See cassandra/cassandra_types.hrl for record definitions
url_for_token(Token, State) ->
	% 'get' takes (Keyspace, Key, ColumnRecord, ConsistencyLevel)
	Args = [State#state.keyspace, Token, 
		#columnPath{column_family = "TinyUrls", column = "url"}, 
		?cassandra_ONE],
	% get will throw an exception if the record isn't found
	try thrift_client:call(State#state.connection, 'get', Args) of
		% Success returns a columnOrSuperColumn record.  Since we queried for
		% a column, it contains a column record.
		{ok, ColumnOrSuperColumn} -> 
			Column = ColumnOrSuperColumn#columnOrSuperColumn.column,
			{ok, binary_to_list(Column#column.value)};
		% get misses throw exceptions.  I don't know what error cases might
		% cause this.  Catching it just incase.
		R -> {error, R}
	catch
		% Record wasn't found.  Stupid that this is an exception.
		_:{notFoundException} -> {error, not_found};
		% Some other exception
		_:E -> {error, E}
	end.
	
gen_token()	->
	% Letters that are easy to distinguish visually (no zero or capital o, etc)
	Letters = "ABCDEFGHJKLMNPQRSTWXYZ23456789abcdefghijkmnpqrstwxyz",
	Size = length(Letters),
	lists:map(fun(_A)-> lists:nth(random:uniform(Size), Letters) end, "123456").

% Store a url and return the token
% Uses cassandra 'batch_mutate' method: http://wiki.apache.org/cassandra/API
% See cassandra/cassandra_types.hrl for record definitions
token_for_stored_url(Url, State) ->
	token_for_stored_url(Url, State, 1).
	
% Try at most 5 times to find an available token
token_for_stored_url(Url, State, Try) when Try < 5 ->
	Token = gen_token(),
	case url_for_token(Token, State) of
		% Token is available
		{error, not_found} ->
			% mutations can be writes or deletes
			Mutation = #mutation{
				column_or_supercolumn = #columnOrSuperColumn{
					column = #column{name = "url", value = Url, timestamp = timestamp()}
				}
			},
			% 'batch_mutate' takes (Keyspace, MutationDict, ConsistencyLevel)
			% The mutation dict is a nested dictionary, described in json like this:
			% {key: {columnFamily: [mutations]}}
			% In erlang, it expects the dict record type.
			Args = [State#state.keyspace, 
				dict:store(Token, 
					dict:store("TinyUrls", [Mutation], dict:new()), 
				dict:new()), 
			?cassandra_ONE],
			Response = thrift_client:call(State#state.connection, 'batch_mutate', Args),
			case Response of
				% Success returns {ok, ok}.  Awesome.
				{ok,ok} -> {ok, Token};
				R -> {error, R}
			end;
		% Token is taken
		{ok, _MatchedUrl} ->
			token_for_stored_url(Url, State, Try + 1);
		% Something broke
		OtherError -> OtherError
	end;
% Out of tries.  Giving up.
token_for_stored_url(Url, State, Try) ->
	{error, could_not_find_available_token}.
