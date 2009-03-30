-module(eadc_plugin).
-author('jlarky@gmail.com').

%% hook
-export([hook/2]).

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
get_plugins() ->
    ?PLUGINS.
