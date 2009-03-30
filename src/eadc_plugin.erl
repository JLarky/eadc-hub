-module(eadc_plugin).
-author('jlarky@gmail.com').

%% hook
-export([hook/2]).

-export([get_plugins/0, set_plugins/1]).

-define(PLUGINS, [plugin_bot, plugin_hub_merge, plugin_punklan, plugin_jabber_gate]).

-include("eadc.hrl").

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

hook(Hook, Args) ->
    F=fun(Plugin, Acc_Args) ->
	      case (catch Plugin:Hook(Acc_Args)) of
		  {'EXIT',{undef,[{Plugin, Hook, _}|_]}} ->
		      Acc_Args; %% don't change pids and data
		  {'EXIT', Error} ->
		      ?DEBUG(error, "Error in module ~s with hook ~s - ~w",
			     [Plugin, Hook, Error]),
		      Acc_Args; %% don't change pids and data
		  New_Args ->
		      ?DEBUG(debug, "~s:~s\n~w\n", [Plugin, Hook, New_Args]),
		      New_Args
	      end
      end,    
    lists:foldl(F, Args, get_plugins()).

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
