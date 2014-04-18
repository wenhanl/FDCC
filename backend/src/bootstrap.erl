%%%-------------------------------------------------------------------
%%% @author ajaltade
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Mar 2014 6:38 PM
%%%-------------------------------------------------------------------
-module(bootstrap).
-author("ajaltade").

-behaviour(gen_server).

%%-include("../include/mysql.hrl").

%% API
-export([start/0]).
-export([add_node/1, get_list/0, remove_node/1,
  begin_job/1, update_state/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {jobId = 1, set = ets:new(?MODULE, [set])}).

%%%===================================================================
%%% API
%%%===================================================================
add_node(HostInfo) ->
  io:format("Called~n"),
  {Name, IpAddr} = HostInfo,
  gen_server:call(?MODULE, {add_node, Name, IpAddr}).

remove_node(Name)->
  gen_server:call(?MODULE, {remove_node, Name}).

update_state(Name, Status) ->
  gen_server:call(?MODULE, {update_state, Name, Status}).

begin_job(Args) ->
  gen_server:call(?MODULE, {begin_job, Args}).

get_list() ->
  gen_server:call(?MODULE, {get_list}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
  case gen_server:start_link({global, bsn}, ?MODULE, [], []) of
    {ok, Pid} -> [];
    {error, _Why} -> {ok, Pid} = gen_server:start_link({global, bsn1},
      ?MODULE, [], [])
  end,
  {ok, Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Args) ->
  %% TODO - This needs to be a replicated database - Mnesia
  data:sql_init(),

  %% TODO - This is only for now. Using State as a counter for JobID. We should
  %% TODO - be taking this from what the DB thinks.

  {ok, #state{jobId = 1, set = ets:new(?MODULE, [set])}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  case _Request of

    %% A new node joins the network
    {add_node, Name, IpAddr} ->
      io:format("Trying to add ~p~n", [Name]),
      Reply = case data:lookup(Name) of
                [] -> data:add_node(Name, IpAddr),
                  {welcome, data:get_hosts_name()};
                [_Data] -> _Data %% TODO - What should we do in case it exists? Update
              end,
      {reply, Reply, State};

    %% A node leaves the network
    {remove_node, Name} ->
      Set = State#state.set,
      Reply = case ets:lookup(Set, Name) of
                []  -> not_a_customer;
                [_] -> ets:delete(Set, Name),
                  {deleted, ets:tab2list(Set)}
              end,
      {reply, Reply, State#state{set = Set}};

    %% Status change of a node
    %% TODO - If it goes down, should we remove it instead?
    {update_state, Name, Status} ->
      %% Since we init this as a set, we can just re-insert and it gets
      %% replaced.
      Set = State#state.set,
      case ets:insert(Set, {Name, {Status}}) of
        true -> Reply = {updated, ets:tab2list(Set)};
        false -> Reply = {update_failed, ets:tab2list(Set)}
      end,
      {reply, Reply, State#state{set = Set}};

    %% Return a list of all nodes
    {get_list} ->
      Set = State#state.set,
      Reply = {list, ets:tab2list(Set)},
      {reply, Reply, State};

    %% Initiate a job
    {begin_job, Args} ->

      JobId = State#state.jobId,
      HostList = data:get_hosts_ip(),
      HostNames = data:get_hosts_name(),
      io:format("Hostlist is - ~p~n", [HostList]),

      %% TODO  {msg, Tar} = Args -- From Django
      %% TODO - Args will directly have the Tar bytestream??
      %{ok, Tar} = file:read_file("/tmp/test.tar"),
      Tar = Args,

      TempList = lists:append(HostList),
      TempHostList = lists:append(HostNames),
      HostStrList = lists:map(fun erlang:binary_to_list/1, TempList),
      HostNameStrList = lists:map(fun erlang:binary_to_list/1, TempHostList),

      %% TODO - This will come from the DB selection process.
      %% TODO - Currently we just select the first one. But this does not ensure load balancing.
      [ScnHost | Tail] = HostNameStrList,

      io:format(" Job id is -- ~p~n", [JobId]),
      rpc:call(list_to_atom(ScnHost), compile_node, act_as_scn,
        [{JobId, HostStrList, Tar}]),
      Reply = {this_is_a_TODO, Args},
      {reply, Reply, State#state{jobId = JobId + 1}}

    %% TODO - Get a list of down nodes

    %% TODO - Get a list of available nodes

    %% TODO - Other stuff
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



