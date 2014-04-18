%%%-------------------------------------------------------------------
%%% @author ajaltade
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Mar 2014 8:52 PM
%%%-------------------------------------------------------------------
-module(backend).
-author("ajaltade").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(TCP_OPTIONS,[binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  spawn(fun() -> listen() end),
  {ok, #state{}}.

listen() ->
	case gen_tcp:listen(18842, ?TCP_OPTIONS) of 
		{ok, LSocket} ->
			accept(LSocket);
		{error, Reason} ->
			{error, Reason}
	end.

accept(LSocket) ->
	case gen_tcp:accept(LSocket) of
   	 	{ok, Socket} -> 
			spawn(fun() -> do_echo(Socket,[],[],[]) end),
			%%listen_file(Socket),
			accept(LSocket);
		{error, Reason} ->
			[error, Reason]
    end.

	
do_echo(Socket, Filename , Filedata, Length) ->
	File_name_sig = list_to_binary("FFDT:FileName"),
	File_length_sig = list_to_binary("FFDT:FileLength"),
	File_data_sig = list_to_binary("FFDT:FileData"),
%%	io:format("print out length~p~n",[Length]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
			
			case Data of
				File_name_sig ->
					put(status,filename),
					do_echo(Socket, Filename , Filedata, Length),
					io:format("get file name~n");
				File_length_sig ->
					put(status,length),
          FilePath = "/tmp/backend/" ++ lists:reverse(Filename),
					do_echo(Socket,FilePath , Filedata, Length),
					io:format("get file len~n");
				File_data_sig ->
					put(status,data),
					do_echo(Socket,Filename , Filedata, Length),
					io:format("get file data~n");
				Other ->
					case get(status) of
						filename -> 
							do_echo(Socket, [binary_to_list(Data)|Filename], Filedata, Length);
						length ->
							do_echo(Socket, Filename, Filedata, list_to_integer(binary_to_list(Data)));
						data when Length > byte_size(Data)->
							NewLen  = Length - byte_size(Data),
							do_echo(Socket, Filename, [binary_to_list(Data)|Filedata], NewLen);
						data when Length == byte_size(Data) ->
							io:format("get file ~s~n",[Filename]),
              FileData = list_to_binary(lists:reverse([binary_to_list(Data)
                |Filedata])),
							case file:write_file(Filename, FileData) of
								ok ->
									io:format("write file success"),
                  gen_server:call({global, bsn}, {begin_job, FileData}),
									gen_tcp:close(Socket);
								{error, Reason} ->
									io:format("error when writing ~p~n",[Reason])
							end;
            Other2 ->
              io:format("Wrong case : ~p ~p ~n", [Length, byte_size(Data)])
					end
			end;
        {error, closed} ->
            ok
    end.

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