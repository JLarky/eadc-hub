%%%-------------------------------------------------------------------
%%% File    : socket routine
%%% Author  : JLarky <jlarky@gmail.com>
%%% Description : ???
%%%
%%% Created :  20 Mar 2010 by JLarky <jlarky@gmail.com>
%%%-------------------------------------------------------------------
-module(sockroute).

-behaviour(gen_server).

%% API
-export([start/1,stop/1,connect/3,aconnect/3,listen/2,send/3,send/2,sendn/2,close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {type,protocol,worker,socket}).
-record(sender, {socket,pid}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Pid) ->
    gen_server:start_link(?MODULE, Pid, []).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
start(Pid) ->
    start(Pid, [async,tcp]).

start(Pid,Options) when is_list(Options) ->
    start_link({Pid,Options}).

stop(Pid) ->
    gen_server:cast(Pid,stop).

connect(Pid,Host,Port) ->
    gen_server:call(Pid,{connect, Host, Port}).

aconnect(Pid,Host,Port) ->
    gen_server:call(Pid,{aconnect, Host, Port}).

listen(Pid,Port) ->
    gen_server:call(Pid,{listen, Port}).
    
send(Pid, Socket, Thing) ->
    gen_server:call(Pid,{send, Socket, Thing}).

send(#sender{pid=Pid, socket=Socket}, Thing) ->
    gen_server:call(Pid,{send, Socket, Thing}).

sendn(#sender{pid=Pid, socket=Socket}, Thing) ->
    gen_server:call(Pid,{send, Socket, [Thing,"\n"]}).

close(#sender{pid=Pid, socket=Socket}) ->
    gen_server:call(Pid,{close, Socket}).




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
init({Pid,Options}) when is_pid(Pid) and is_list(Options) ->
    io:format("Started !!\n"),
    Types    =fun(E)->lists:member(E,[sync,async])end,
    Protocols=fun(E)->lists:member(E,[tcp,ssl])   end,
    Type    =lists:last([async|lists:filter(Types,Options)]),
    Protocol=lists:last([tcp  |lists:filter(Protocols,Options)]),
    {ok, #state{type=Type,protocol=Protocol,worker=Pid,
		socket=undefined}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%%% API
%% see connect/3
handle_call({connect, Host, Port}, _From, #state{protocol=tcp}=State)->
    #state{worker=Worker,type=Type,protocol=Protocol}=State,
    case (catch gen_tcp:connect(Host, Port,[binary, {packet, 0},{active, false}])) of
	{ok, Connection} ->
	    Me=self(),spawn(fun()->do_recv(Type,Protocol,Me,Connection,Worker)end),	    
	    case (random:uniform(100)==1) of
		true  -> spawn(erlang,garbage_collect,[Me]);
		false -> ok
	    end,
	    {reply, {ok,Connection}, State#state{socket=connect}};
	Error ->
	    error_logger:error_info({?FILE,?LINE, {"Error in connect ", Error}}),
	    {reply, {error,Error}, State#state{socket=connect}}
    end;
%%% API
%% see aconnect/3
handle_call({aconnect, Host, Port}, _From, #state{protocol=tcp}=State)->
    #state{worker=Worker,type=Type,protocol=Protocol}=State,
    Me=self(),
    spawn(fun() ->
		  case (catch gen_tcp:connect(Host, Port,[binary, {packet, 0},{active, false}])) of
		      {ok, Connection} ->
			  gen_tcp:controlling_process(Connection, Me),
			  gen_server:cast(Worker,{sr,#sender{pid=Me,socket=Connection},aconnect}),
			  spawn(fun()->do_recv(Type,Protocol,Me,Connection,Worker)end);
		      Error ->
			  error_logger:error_info({?FILE,?LINE, {"Error in connect ", Error}}),
			  {reply, {error,Error}, State#state{socket=connect}}
		  end
	  end),
    case (random:uniform(100)==1) of
	true  -> spawn(erlang,garbage_collect,[Me]);
	false -> ok
    end,
    {reply, async, State#state{socket=connect}};

%%% API
%% see listen/2
handle_call({listen, {Port, Options}}, _From, #state{protocol=tcp}=State)->
    #state{socket=Socket,worker=Worker,type=Type,protocol=Protocol}=State,
    (catch gen_tcp:close(Socket)),%% in case of connection before
    case (catch gen_tcp:listen(Port, Options)) of
	{ok, ListenSocket} ->
	    Me=self(),spawn(fun()->accept(Type,Protocol,Me,ListenSocket,Worker) end),
	    {reply, {ok,ListenSocket}, State#state{socket=ListenSocket}};
	Error ->
	    error_logger:error_info({?FILE,?LINE, {"Error in connect ", Error}}),
	    {reply, {error,Error}, State#state{socket=undefined}}
    end;
handle_call({listen, Port}, _From, #state{protocol=tcp}=State)->
    #state{socket=Socket,worker=Worker,type=Type,protocol=Protocol}=State,
    (catch gen_tcp:close(Socket)),%% in case of connection before
    case (catch gen_tcp:listen(Port, [binary, {packet, 0},{active, false}])) of
	{ok, ListenSocket} ->
	    Me=self(),spawn(fun()->accept(Type,Protocol,Me,ListenSocket,Worker) end),
	    {reply, {ok,ListenSocket}, State#state{socket=ListenSocket}};
	Error ->
	    error_logger:error_info({?FILE,?LINE, {"Error in connect ", Error}}),
	    {reply, {error,Error}, State#state{socket=undefined}}
    end;
%%% API
%% see send/3
handle_call({send, Socket, Thing}, _From, #state{protocol=tcp}=State) ->
    Reply=
	case (catch gen_tcp:send(Socket, Thing)) of
	    {error,einval} -> %% may be because of utf8?
		(catch gen_tcp:send(Socket, unicode:characters_to_binary(Thing)));
	    Other ->
		Other
	end,
    case (random:uniform(1000)==1) of % one of N cases
	true ->
	    %%garbage_collect();
	    {reply, Reply, State,hibernate};
	false ->
	    {reply, Reply, State}
    end;

%%% API
%% see close/1
handle_call({close, Socket}, _From, #state{protocol=tcp}=State) ->
    Reply=(catch gen_tcp:close(Socket)),
    case (random:uniform(1000)==1) of % one of N cases
	true ->
	    %%garbage_collect();
	    {reply, Reply, State,hibernate};
	false ->
	    {reply, Reply, State}
    end;


handle_call(Request, From, State)->
    error_logger:error_info({?FILE,?LINE,{unhandled,Request,From}}),
    {reply, unhandled, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({closed,Socket}, #state{socket=Socket}=State)-> % close listen socket
    {noreply,State#state{socket=undefined}};
handle_cast(stop, State) ->
    handle_cast({stop, 'stop signal'}, State);
handle_cast({stop, Why}, #state{worker=Worker}=State) ->
    gen_server:cast(Worker,{sr,#sender{pid=self()},stopped}),
    io:format("!! Stop: ~p\n",[Why]),
    {stop, normal, State};
handle_cast(Msg, State) ->
    io:format("!! unhandled cast ~p\n", [{Msg, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("!! unhandled info ~p\n", [{Info, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=Socket}=_State) ->
    io:format("Terminated !!\n"),
    %% for listener
    (catch gen_tcp:close(Socket)),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_recv(async,tcp,Pid,Sock,Worker) -> %% it must catch 100% error without crash itself
    case (catch gen_tcp:recv(Sock, 0)) of
	{ok, B} ->
	    gen_server:cast(Worker, {sr,#sender{pid=Pid,socket=Sock},{received,B}}),
	    do_recv(async,tcp,Pid,Sock,Worker);
	{error, closed} ->
	    gen_server:cast(Worker, {sr,#sender{pid=Pid,socket=Sock},closed});
	Error ->
	    error_logger:error_msg("! Fatal error in recv ~p \n", [Error]),
	    Ok=(catch gen_tcp:close(Sock)),
	    gen_server:cast(Worker, {sr,#sender{pid=Pid,socket=Sock},closed}),
	    error_logger:info_msg("! Trying to close connection ~p \n", [Ok])
    end.

accept(async,tcp,Pid,LSocket,Worker) -> %% it must catch 100% error without crash itself
    case (catch gen_tcp:accept(LSocket)) of
	{ok, Socket} ->
	    gen_server:cast(Worker,{sr,#sender{pid=Pid,socket=Socket},accept}),
	    spawn(fun()->do_recv(async,tcp,Pid,Socket,Worker)end),
	    accept(async,tcp,Pid,LSocket,Worker);%%loop
	{error,closed} ->
	    gen_server:cast(Worker,{sr,#sender{pid=Pid,socket=LSocket},closed}),
	    gen_server:cast(Pid, {closed,LSocket});
	Error ->
	    error_logger:error_msg("Error in accept ~p\n", [self(),Error]),
	    accept(async,tcp,Pid,LSocket,Worker)%%loop
    end.
