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

-export([init/0,terminate/0]). %% required

-export([chat_msg/1]). %% some hook that you want to catch

%% @spec init() -> ok | {error, String}
%% @doc do init stuff and returns ok or {error, Message} if something's going wrong
init() ->
    ok.
%% @spec terminate() -> ok | {error, String}
%% @doc do terminate stuff and returns ok or {error, Message} if something's going wrong
terminate() ->
    ok.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]), %% it's appear in debug
    Msg=eadc_utils:get_val(msg, Args), %% extracting 'msg' param from 'Args'

    %% send message to sender
    eadc_utils:info_to_pid(self(), "I see your message! You just wrote: "
			   ++eadc_utils:unquote(Msg)), %% unquote becose info_to_pid do quote

    %% if we want see Erlang term in chat we have do like that
    Out=[1,2,3,{1,2,3}], 
    Test=lists:flatten(io_lib:format("~w", [Out])),
    eadc_utils:error_to_pid(self(), Test),

    %% YOU ALWAYS MAST DO THAT
    Args.
