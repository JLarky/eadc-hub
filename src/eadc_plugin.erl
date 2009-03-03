-module(eadc_plugin).
-author('jlarky@gmail.com').

%% hook
-export([hook/2]).

-define(PLUGINS, [plugin_bot, plugin_hub_merge]).
-include("eadc.hrl").

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

hook(Hook, Args) -> %% применяем все плагины
    lists:any(      %% если кто-то вернёт true то прерываемся  
      fun(Plugin) ->
	      A= case (catch Plugin:Hook(Args)) of
		     {'EXIT',{undef,_}} ->
			 false;
		     {'EXIT', Error} ->
			 ?DEBUG(error, "Error in module ~s with hook ~s - ~w",
				[Plugin, Hook, Error]),
			 false;
		     Other ->
			 Other
		 end,
	      ?DEBUG(debug, "~s:~s\n~w\n", [Plugin, Hook, A]),
	      A
      end, get_plugins()).


get_plugins() ->
    ?PLUGINS.
