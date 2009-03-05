%%%-------------------------------------------------------------------
%%% Author  : JLarky <jlarky@jlarky.info>
%%% Description : Some plugin for something
%%%
%%% Created :  
%%%-------------------------------------------------------------------
-module(plugin_skeleton).
-author('jlarky@gmail.com').

-include("eadc.hrl").
-include("eadc_plugin.hrl").

-export([chat_msg/1]). %% some hook that you want to catch

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]), %% it's appear in debug
    ?GET_VAL(msg, Msg), %% macro for extracting 'msg' param from 'Args'

    %% send message to sender
    eadc_utils:info_to_pid(self(), "I see your message! You just wrote: "
			   ++eadc_utils:unquote(Msg)), %% unquote becose info_to_pid do quote

    %% if we want see Erlang term in chat we have do like that
    Out=[1,2,3,{1,2,3}], 
    Test=lists:flatten(io_lib:format("~w", [Out])),
    eadc_utils:error_to_pid(self(), Test),

    %% YOU ALWAYS MAST DO THAT
    Args.
