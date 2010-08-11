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
	{ok, #state{
		host = proplists:get_value(host, Options),
		port = proplists:get_value(port, Options),
		keyspace = proplists:get_value(keyspace, Options)
	}}.

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
	{Response, NewState} = url_for_token(Token, State),
	{reply, Response, NewState};
handle_call({store_url, Url}, _From, State) ->
	{Response, NewState} = token_for_stored_url(Url, State),
	{reply, Response, NewState};
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

timestamp() ->
	{MegaSecs, Secs, Microsec} = now(),
	MegaSecs*1000000000000 + Secs*1000000 + Microsec.

cass_call(Method, Args, State) ->
	case State#state.connection of 
		undefined -> 
			{ok, C} = thrift_client:start_link(State#state.host, State#state.port, cassandra_thrift),
			cass_call(Method, Args, State#state{connection = C});
		C -> 
			try thrift_client:call(C, Method, Args) of
				{ok, Response} -> {{ok, Response}, State};
				ok -> {ok, State}
			catch
				_:{notFoundException} -> {{error, not_found}, State};
				_:E -> {{error, E}, State}
			end
	end.
	
url_for_token(Token, State) ->
	Response = cass_call('get', [State#state.keyspace, Token, 
		#columnPath{column_family = "TinyUrls", column = "url"}, 
		?cassandra_ONE], State),
	error_logger:info_msg("Got response from cassandra: ~p~n", [Response]),
	case Response of
		{{ok, ColumnOrSuperColumn}, NewState} -> 
			Column = ColumnOrSuperColumn#columnOrSuperColumn.column,
			{{ok, Column#column.value}, NewState};
		_ -> Response
	end.
	
gen_token()	->
	"ASDFGH".

token_for_stored_url(Url, State) ->
	Token = gen_token(),
	Mutation = #mutation{
		column_or_supercolumn = #columnOrSuperColumn{
			column = #column{name = "url", value = Url, timestamp = timestamp()}
		}
	},
	Response = cass_call('batch_mutate', [State#state.keyspace, dict:store(Token, dict:store("TinyUrls", [Mutation], dict:new()), dict:new()), ?cassandra_ONE], State),
	error_logger:info_msg("Got response from cassandra: ~p~n", [Response]),
	case Response of
		{{ok,ok}, NewState} -> {{ok, Token}, NewState};
		_ -> Response
	end.
	