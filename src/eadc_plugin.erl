-module(eadc_plugin).
-author('jlarky@gmail.com').

%% hook
-export([hook/2]).

-define(PLUGINS, [plugin_bot, plugin_hub_merge, plugin_ten_lines, plugin_punklan, plugin_jabber_gate]).

-include("eadc.hrl").

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

hook(Hook, Args) -> %% применяем все плагины
    %% если кто-то вернёт true то прерываемся  
    F=fun(Plugin, Acc_Args) ->
	      A= case (catch Plugin:Hook(Acc_Args)) of
		     {'EXIT',{undef,[{Plugin, Hook, _}|_]}} ->
			 Acc_Args; %% don't change pids and data
		     {'EXIT', Error} ->
			 ?DEBUG(error, "Error in module ~s with hook ~s - ~w",
				[Plugin, Hook, Error]),
			 Acc_Args; %% don't change pids and data
		     New_Args ->
			 New_Args
		 end,
	      ?DEBUG(debug, "~s:~s\n~w\n", [Plugin, Hook, A]),
	      A
      end,    
    New_Args=lists:foldl(F, Args, get_plugins()),
    {value,{data, Data}} = lists:keysearch(data, 1, New_Args),
    {value,{pids, Pids}} = lists:keysearch(pids, 1, New_Args),
    {Pids, Data}.

get_plugins() ->
    ?PLUGINS.
