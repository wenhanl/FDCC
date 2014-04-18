%%%-------------------------------------------------------------------
%%% @author ajaltade
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2014 1:55 AM
%%%-------------------------------------------------------------------
-module(compile_node_super).
-author("ajaltade").

-behaviour(supervisor).

%% API
-export([start/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(MainArgs :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start(MainArgs) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [MainArgs]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([MainArgs]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  {BSNode, Interface, AllowList, LogFile} = MainArgs,
  CNMain = {cnMain, {compile_node, start_link, [BSNode, Interface]},
    Restart, Shutdown, Type, [compile_node]},

  CNJobMgr = {jobMgr, {cn_job_mgr, start_link, [{AllowList, LogFile}]},
    Restart, Shutdown, Type, [cn_job_mgr]},

  CNStatusMgr = {cnStatusMgr, {cn_status_mgr, start_link, [LogFile]},
    Restart, Shutdown, Type, [cn_status_mgr]},

%%  {ok, {SupFlags, [CNMain]}}.
  {ok, {SupFlags, [CNMain, CNJobMgr, CNStatusMgr]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
