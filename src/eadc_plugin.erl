-module(eadc_plugin).
-author('jlarky@gmail.com').

%% hook
-export([hook/2]).

-define(PLUGINS, [plugin_bot]).
-include("eadc.hrl").

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

hook(Hook, Args) ->
    lists:foreach(
      fun(Plugin) ->
	      A= case (catch Plugin:Hook(Args)) of
		     {'EXIT',{undef,_}} ->
			 undef;
		     {'EXIT', Error} ->
			 {error, Error};
		     Other ->
			 Other
		 end,		 
	      ?DEBUG(debug, "~s:~s\n~w\n", [Plugin, Hook, A])
      end, get_plugins()).


get_plugins() ->
    ?PLUGINS.
