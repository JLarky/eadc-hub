-module(eadc_plugin).
-author('jlarky@gmail.com').

%% hook
-export([hook/2]).

-export([get_plugins/0, set_plugins/1]).

%% plugin functions
-export([bot_add/4, bot_del/1]).

-define(PLUGINS, [plugin_bot]).

-include("eadc.hrl").

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

hook(Hook, Args) ->
    F=fun(P,A) -> hook(Hook,P,A) end,
    lists:foldl(F, Args, get_plugins()).

hook(Hook, Plugin, Args) ->
    case (catch Plugin:Hook(Args)) of
	{'EXIT',{undef,[{Plugin, Hook, _}|_]}} ->
	    Args; %% don't change pids and data
	{'EXIT', Error} ->
	    Format="Error in plugin ~s, hook ~s(~p)\n~p\nPlease report to admin.",
	    Msg=lists:flatten(io_lib:format(Format,[Plugin, Hook, Args, Error])),
	    ?DEBUG(error, Msg, []),
	    Client=eadc_utils:get_val(client,Args),
	    catch eadc_utils:info_to_client(Client, Msg),
	    Args; %% don't change pids and data
	New_Args ->
	    ?DEBUG(debug, "~s:~s\n~w\n", [Plugin, Hook, New_Args]),
	    New_Args
    end.

%% @spec set_plugins() -> PluginList
%% PluginList = [Plugin_module]
%% @doc gets plugin lists
get_plugins() ->
    eadc_utils:get_option(plugins, allowed, ?PLUGINS).

%% @spec set_plugins(PluginList) -> ok
%% PluginList = [Plugin_module]
%% @doc save plugin lists
set_plugins(Plugins) when is_list(Plugins) ->
    eadc_utils:set_option(plugins, allowed, Plugins).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PLUGIN FUNCTIONS
%%%%%%%%%%

bot_add(Id, Nick, Desc, Level) when is_integer(Level) ->
    Cid=eadc_client_fsm:get_unical_cid(),
    Sid=eadc_client_fsm:get_unical_SID(),
    Inf="BINF "++eadc_utils:sid_to_s(Sid)++" CT"++integer_to_list(Level)++" ID"++Cid++" NI"++
	eadc_utils:quote(Nick)++" DE"++eadc_utils:quote(Desc),
    eadc_client_fsm:client_write(#client{cid=Cid, sid=Sid, nick=Nick, inf=Inf, pid=Id}),
    eadc_utils:broadcast({string, Inf}),
    Inf.

bot_del(Id) ->
    BotList=eadc_user:client_find(#client{pid=Id, _='_'}),
    lists:foreach(fun(#client{sid=Sid}=_Bot) ->
			  eadc_client_fsm:client_delete(Sid),
			  Qui="IQUI "++eadc_utils:sid_to_s(Sid),
			  eadc_utils:broadcast({string, Qui})
		  end, BotList).
