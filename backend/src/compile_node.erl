%%%-------------------------------------------------------------------
%%% @author ajaltade
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2014 12:12 AM
%%%-------------------------------------------------------------------
-module(compile_node).
-author("ajaltade").

-behaviour(gen_server).

%% API
-export([start_link/2, act_as_scn/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
act_as_scn(Args) ->
  gen_server:call(?MODULE, {act_as_scn, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Name :: term(), Interface :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Bootstrap, Interface) ->
  net_kernel:connect_node(Bootstrap),
  global:sync(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Interface], []).

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
init([Interface]) ->
  process_flag(trap_exit, true),
  join_network(Interface),
  {ok, #state{}}.

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

    {act_as_scn, Args} ->
      %% TODO - Parse Args to get a list of hosts and a tarball.
      %% 1. Untar the tarball.
      %% 2. cd into the directory.
      %% 2. Invoke make as - 'make CC=distcc DISTCC_HOSTS=<List>'
      {JobId, HostList, Tar} = Args,
      HostListStr = string:join(HostList, " "),
      io:format("Yay! I am chosen to be a super compile node ~n"),
      {ok, Dir} = file:get_cwd(),

      %% TODO - This will be replaced by untar
      case file:make_dir(integer_to_list(JobId)) of
        ok -> [];
        {error, Reason} -> io:format("ERR! ~p~n", [Reason])
      end,
      c:cd(integer_to_list(JobId)),
      erl_tar:extract({binary, Tar}),

      %c:cd("/home/ajaltade/CMU/Sem4/18-845/IP"),
      %% End of TODO.

      os:putenv("DISTCC_HOSTS", HostListStr),
      Output = os:cmd("make -j12 CC=distcc"),
      io:format("~s~n", [Output]),
      c:cd(Dir)
  end,
  {reply, ok, State}.

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
-spec(join_network(Interface :: term()) ->
  {welcome, [_]} | {error, Reason :: term()}).
join_network(Interface) ->
  {ok, [{addr, Address}]} = inet:ifget(Interface, [addr]),
  AddressStr = inet_parse:ntoa(Address),
  gen_server:call({global, bsn}, {add_node, node(), AddressStr}).
