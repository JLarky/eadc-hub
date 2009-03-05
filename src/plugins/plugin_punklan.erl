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

-export([ctm/1]). %% some hook that you want to catch

ctm(Args) ->
    ?GET_VAL(pids, [Pid]), %% macro for extracting 'msg' param from 'Args'

    %% send message to sender
    eadc_utils:info_to_pid(self(), "Ты только что попробовал приконнектится"),

    %% if we want see Erlang term in chat we have do like that
    %%Out='gen_fsm:sync_send_all_state_event(self(), get_state, 1000)',
    Out=gen_fsm:sync_send_all_state_event(Pid, get_state),
    Test=lists:flatten(io_lib:format("~w", [Out])),
    eadc_utils:error_to_pid(self(), Test),

    %% YOU ALWAYS MAST DO THAT
    Args.
