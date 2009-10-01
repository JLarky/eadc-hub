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

-export([init/0,terminate/0]).

-export([ctm/1, rcm/1, chat_msg/1]). %% some hook that you want to catch

init() -> ok.
terminate() -> ok.

chat_msg(Args) ->
    ?GET_VAL(msg, Msg),
    case Msg of
	[$., $i, $p, $\  | Tail] ->
	    try
		[Client|_]=eadc_user:client_find(#client{nick=Tail,_='_'}),
		Addr=Client#client.addr,
		Place=whois(Addr),
		Place_m =  case Place of
			       inet -> "интернетах. Осторожно! трафик!";
			       _    -> f("~w общаге.",[Place])
			   end,
		eadc_utils:info_to_pid(self(), f("Пользователь ~s находится в ~s",
						 [Tail, Place_m]))
	    catch error:Error ->
		    eadc_utils:info_to_pid(self(), io_lib:format("Error: ~p\n",[Error]))
	    end,
	    eadc_utils:set_val(pids, [], Args);
	_ ->
	    Args
    end.

rcm(Args) ->
    ctm(eadc_utils:set_val(rcm, true,Args)).
 
ctm(Args) ->
    try
	[Pid]=eadc_utils:get_val(pids, Args),
	%%State_f=eadc_utils:get_val(state, Args),
	Client=eadc_utils:get_val(client, Args),
	Nick_f=Client#client.nick,
	
	[Client_t]=eadc_user:client_find(#client{pid=Pid, _='_'}),
	Nick_t=Client_t#client.nick,
	_Direction=lists:concat([whois(Client_t#client.addr), "\\sи\\s", 
				 whois(Client#client.addr)]),
	%% send message to sender

	case {whois(Client#client.addr),whois(Client_t#client.addr)} of
	    {F,G} when (F==inet)or(G==inet) ->
		PA={F,G,eadc_utils:get_val(rcm, false, Args)},
		?DEBUG(debug,"~p\n",[PA]),
		Direction=lists:concat([whois(Client#client.addr), " и ",
					whois(Client_t#client.addr)]),
		eadc_utils:info_to_pid(self(), "Соединенние между "++Nick_f++" и "++Nick_t++
				       " ("++Direction++") не состоится. Ваш Хаб."),
		eadc_utils:set_val(pids, [], Args);
	    _ ->
		Args
	end

    catch error:Error ->
	    eadc_utils:info_to_pid(self(), io_lib:format("Error: ~p\n",[Error])),
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
