-module(eadc_listener).
-author('jlarky@gmail.com').

-behaviour(gen_server).

%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(lstate, {
	  client_started,
	  listeners,
	  sockets,
	  client,         % Client
	  module          % FSM handling module
	 }).

-include("eadc.hrl").

%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------
init([Port, Module]) ->
    process_flag(trap_exit, true),
    io:format("~p\n",[{self()}]),
    {ok,Pid}=sockroute:start(self()),
    register(listen_sockroute,Pid),
    Options=[list, {packet, line},{active, false}],
    case sockroute:listen(Pid,{Port,Options}) of
	{ok,Socket} ->
	    {ok, #lstate{client_started=false,
			listeners = [Pid],
			sockets   = [Socket],
			module    = Module}};
	Err ->
	    'oh shi~',
	    io:format("Fatal error: ~p\n",[Err]),
	    halt()
    end.

%%    Opts = [binary, {packet, 2}, {reuseaddr, true},
%%            {keepalive, true}, {backlog, 30}, {active, false}],


%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast({sr,#sender{pid=Pid}=Sender,Msg}, #lstate{listeners=Pids}=State) ->
    true=lists:member(Pid,Pids),
    NewState=sockroute(Sender,Msg,State),
    {noreply,NewState};
handle_cast(_Msg, State) ->
    io:format("~p\n",[_Msg]),
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({'EXIT',Client,Ded}, #lstate{client=Client}=State) ->
    error_logger:error_msg("Client process (~p) was killed (or something) ~p",
			   [Client,Ded]),
    %% wait for respawn
    NClient=wait_client(_Time=10000),
    case is_process_alive(NClient) of
	true ->
	    link(NClient),
	    {noreply, State#lstate{client=NClient,client_started=true}};
	Error ->
	    error_logger:error_msg("Error (~p) with starting client ~p\n",[Error,NClient]),
	    {stop, {error, 'client not started'}, State}
    end;
handle_info(_Info, State) ->
    ?DEBUG(error, "Unhabded info in eadc_listener: ~p\n",[_Info]),
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
    lists:foreach(fun(P)->sockroute:stop(P)end,State#lstate.listeners),
    ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

sockroute(Sender,accept,#lstate{client_started=true,client=Client}=State) ->
    gen_server:cast(Client,{accept,Sender}),
    State;
sockroute(Sender,accept,State) ->
    OK=supervisor:start_child(eadc_client_sup,[]),
    error_logger:info_msg("eadc_cl started, ~p\n",[OK]),
    case OK of
	{ok, Client} ->
	    link(Client),
	    gen_server:cast(Client,{accept,Sender}),
	    State#lstate{client=Client,client_started=true};
	Error ->
	    error_logger:error_msg("Error with starting client ~p\n",[Error]),
	    State#lstate{client=error,client_started=false}
    end;
sockroute(Sender,{received,Data}, #lstate{client=Client}=State) ->
    gen_server:cast(Client,{received, Sender, Data}),
    State;
sockroute(Sender,closed, #lstate{client=Client}=State) ->
    gen_server:cast(Client,{closed, Sender}),
    State;
sockroute(Sender, Msg,State) ->
    io:format("other ~p\n",[{Sender,Msg,State}]),
    State.

wait_client(Timeout) when is_integer(Timeout) andalso (Timeout >= 0) ->
    Pid=whereis(eadc_client),
    case catch is_process_alive(Pid) of
	true ->
	    Pid;
	_ ->
	    timer:sleep(100),
	    wait_client(Timeout-100)
    end;
wait_client(_) ->
    timeout.
