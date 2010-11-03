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

%%%-------------------------------------------------------------------
%%% Author : JLarky <jlarky@jlarky.info>
%%% Description : Some plugin for something
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(plugin_punklan).
-author('jlarky@gmail.com').
 
-include("eadc.hrl").

-export([init/0,terminate/0]).

-export([user_login/1, ctm/1, rcm/1, chat_msg/1]). %% some hook that you want to catch

-export([network_update/0, whois/1]).

init() -> ok.
terminate() -> ok.

user_login(Args) ->
    Client=eadc_utils:get_val(client, Args),
    eadc_utils:send_to_client(Client, {args, ["ICMD", "IP",
					      "TTBMSG\s%[mySID]\s.ip\\s%[userNI]\n", "CT15"]}),
    Args.

chat_msg(Args) ->
    Client=eadc_utils:get_val(client, Args),
    Msg=eadc_utils:get_val(msg,Args),
    case Msg of
	[$., $i, $p, $\  | Tail] ->
	    try
		[Cl|_]=eadc_user:client_find(#client{nick=Tail,_='_'}),
		Addr=Cl#client.addr,
		Place=whois(Addr),
		Place_m =  case Place of
			       inet -> "интернетах. Осторожно! трафик!";
			       _    -> f("~w общаге.",[Place])
			   end,
		eadc_utils:info_to_client(Client, f("Пользователь ~s находится в ~s",
						 [Tail, Place_m]))
	    catch error:Error ->
		    eadc_utils:info_to_client(Client, eadc_utils:format("2Error: ~p\n",[Error]))
	    end,
	    eadc_utils:set_val(senders, [], Args);
	_ ->
	    Args
    end.

rcm(Args) ->
    ctm(eadc_utils:set_val(rcm, true,Args)).
 
ctm(Args) ->
    Client=eadc_utils:get_val(client, Args),
    try
	Nick_f=Client#client.nick,
	TarSid=eadc_utils:unbase32(eadc_utils:get_val(tar_sid, Args)),
	Client_t=eadc_client:client_get(TarSid),
	Nick_t=Client_t#client.nick,
	_Direction=lists:concat([whois(Client_t#client.addr), "\\sи\\s", 
				 whois(Client#client.addr)]),
	%% send message to sender

	case {whois(Client#client.addr),whois(Client_t#client.addr)} of
	    {F,G} when (F==inet)or(G==inet) ->
		PA={F,G,eadc_utils:get_val(rcm, false, Args)},
		?DEBUG(debug,"~p\n",[PA]),
		Direction=lists:concat([whois(Client#client.addr), " и ",
					whois(Client_t#client.addr)]),
		eadc_utils:info_to_client(Client, "Соединенние между "++Nick_f++" и "++Nick_t++
					  " ("++Direction++") не состоится. Ваш Хаб."),
		eadc_utils:set_val(senders, [], Args);
	    _ ->
		Args
	end

    catch error:Error ->
	    eadc_utils:info_to_client(Client, eadc_utils:format("1Error: ~p\n",[Error])),
	    Args
    end.
 
whois({N1,N2,N3,N4}) ->
    IP=N4+256*(N3+256*(N2+256*N1)),
    Update_time=eadc_utils:get_option(punklan, lastupdate, {{0,0,0},{0,0,0}}),
    case (catch calendar:time_difference(Update_time,calendar:local_time())) of
	{0,_} ->
	    ok;
	{_Day,_} ->
	    network_update();
	_ -> %% error
	    network_update()
    end,
    Networks=eadc_utils:get_option(punklan, networks, ""),
    Camp=lists:foldl(fun({IP1,IP2,C},C_so_far) ->
			     case ((IP>=IP1)and(IP=<IP2)) of
				 true -> C;
				 false-> C_so_far
			     end
		     end, inet, Networks),
   Camp.


f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


network_update() ->
    inets:start(),
    {ok, {_,_, A}}=http:request("http://spb.edu/campus/networks.txt"),
    B=string:tokens(A,"\n"),
    %% C - list ok {start_ip, end_ip, campus}
    C=lists:foldl(fun(String, Acc) ->
			  case String of
			      [$#|_] ->
				  Acc;
			      [] ->
				  Acc;
			      _ ->
				  [Ip_range,Campus] = string:tokens(String, "\tcampus"),
				  [Ip,Range] = string:tokens(Ip_range, "/"),
				  {ok, {N1,N2,N3,N4}}=inet_parse:address(Ip),
				  IP=N4+256*(N3+256*(N2+256*N1)),
				  RANGE=1 bsl (32-list_to_integer(Range)),
				  [{IP, IP+RANGE, list_to_integer(Campus)}|Acc]
			  end end, [], B),
    eadc_utils:set_option(punklan, lastupdate, calendar:local_time()),
    eadc_utils:set_option(punklan, networks, C).
