%%%-------------------------------------------------------------------
%%% Author  : JLarky <jlarky@jlarky.info>
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
    %%?GET_VAL(pids, Pids),
    %%?GET_VAL(state, State),
    %%eadc_utils:info_to_pid(self(), io_lib:format("Кастую! ~s",[whois(State#state.addr)])),
    Args.

ctm(Args) ->
    ?GET_VAL(pids, [Pid]=_Pids),
    ?GET_VAL(state, State_f),
    State_t=gen_fsm:sync_send_all_state_event(Pid, get_state),
    Nick_f=State_f#state.nick, 
    Nick_t=State_t#state.nick,
    Direction=lists:concat([whois(State_f#state.addr), "\\sи\\s", whois(State_t#state.addr)]),

    %% send message to sender
    eadc_utils:info_to_pid(self(), io_lib:format("~s только что попробовал приконнектится к ~s. (~s)",[Nick_f, Nick_t,Direction])),

    %% if we want see Erlang term in chat we have do like that
    %%Out='gen_fsm:sync_send_all_state_event(self(), get_state, 1000)',
    

    %%Out={whois(State_f#state.addr), whois(State_t#state.addr)},
    %%Out="",

    %%Test=lists:flatten(io_lib:format("~w", [Out])),
    %%eadc_utils:error_to_pid(self(), Test),

    %% YOU ALWAYS MAST DO THAT
    Args.


whois({N1, N2, N3, N4}) ->
    ?GET_CAMPUS_NAME(N4+256*(N3+256*(N2+256*N1))).

