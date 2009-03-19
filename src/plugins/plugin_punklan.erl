%%%-------------------------------------------------------------------
%%% Author : JLarky <jlarky@jlarky.info>
%%% Description : Some plugin for something
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(plugin_punklan).
-author('jlarky@gmail.com').
 
-include("eadc.hrl").
-include("eadc_plugin.hrl").
-include("plugin_punklan.hrl").
 
-export([ctm/1, chat_msg/1]). %% some hook that you want to catch
 
chat_msg(Args) ->
    ?GET_VAL(msg, Msg),
    case Msg of
	[$., $i, $p | Tail] ->
	    case Tail of
		_ ->
		    ?GET_VAL(state, State),
		    ?GET_VAL(client, Client),
		    #state{addr=Addr, sid=Sid}=State,Nick=Client#client.nick,
		    Place=whois(Addr)
	    end,
	    Place_m =  case Place of
			   inet -> "интернетах. Осторожно! трафик!";
			   _    -> f("~w общаге.",[Place])
		       end,
	    eadc_utils:info_to_pid(self(), f("Пользователь ~s находится в ~s", [Nick, Place_m])),
	    lists:keyreplace(pids, 1, Args, {pids, []});
	_ ->
	    Args
    end.
 
ctm(Args) ->
    ?GET_VAL(pids, [Pid]=_Pids),
    ?GET_VAL(state, State_f),
    ?GET_VAL(client, Client),
    State_t=gen_fsm:sync_send_all_state_event(Pid, get_state, 10000),
    Nick_f=Client#client.nick,
    Client_t=eadc_utils:client_get(State_t#state.sid),
    Nick_t=Client_t#client.nick,
    Direction=lists:concat([whois(State_f#state.addr), "\\sи\\s", whois(State_t#state.addr)]),
 
    %% send message to sender
    eadc_utils:info_to_pid(self(), io_lib:format("~s только что попробовал приконнектиться к ~s. (~s)",[Nick_f, Nick_t,Direction])),
    case {whois(State_f#state.addr), whois(State_t#state.addr)} of
	{F, T} when (F == inet) or (T == inet) ->
	    eadc_utils:info_to_pid(self(), "Но нифига из этого не выйдет."),
	    lists:keyreplace(pids, 1, Args, {pids, []});
	_ ->
	    Args
    end.
    

 
    %% if we want see Erlang term in chat we have do like that
    %%Out='gen_fsm:sync_send_all_state_event(self(), get_state, 1000)',
    
 
    %%Out={whois(State_f#state.addr), whois(State_t#state.addr)},
    %%Out="",
 
    %%Test=lists:flatten(io_lib:format("~w", [Out])),
    %%eadc_utils:error_to_pid(self(), Test),
 
    %% YOU ALWAYS MAST DO THAT
    %%Args.
 
 
whois({N1, N2, N3, N4}) ->
    ?GET_CAMPUS_NAME(N4+256*(N3+256*(N2+256*N1))).


f(F, A) ->
    lists:flatten(io_lib:format(F, A)).
