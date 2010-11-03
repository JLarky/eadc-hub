%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% eadc-hub - ADC hub software written using Erlang/OTP.
%%% Copyright (c) 2010, JLarky <jlarky@gmail.com>
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%

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

%% @spec get_plugins() -> PluginList
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
    Cid=eadc_client:get_uniq_cid(),
    Sid=eadc_client:get_uniq_sid(),
    Inf="BINF "++eadc_utils:sid_to_s(Sid)++" CT"++integer_to_list(Level)++" ID"++Cid++" NI"++
	eadc_utils:quote(Nick)++" DE"++eadc_utils:quote(Desc),
    eadc_client:client_write(#client{cid=Cid, sid=Sid, nick=Nick, inf=Inf, pid=Id}),
    eadc_utils:broadcast({string, Inf}),
    Inf.

bot_del(Id) ->
    BotList=eadc_user:client_find(#client{pid=Id, _='_'}),
    lists:foreach(fun(#client{sid=Sid}=_Bot) ->
			  eadc_client:client_delete(Sid),
			  Qui="IQUI "++eadc_utils:sid_to_s(Sid),
			  eadc_utils:broadcast({string, Qui})
		  end, BotList).
