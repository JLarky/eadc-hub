%%%-------------------------------------------------------------------
%%% File    : plugin_ten_lines.erl
%%% Author  : JLarky <jlarky@gmail.com>
%%% Description : 
%%%
%%% Created :  3 Mar 2009 by JLarky <jlarky@gmail.com>
%%%-------------------------------------------------------------------
-module(plugin_ten_lines).

-include("eadc.hrl").
-include("eadc_plugin.hrl").

-export([chat_msg/1, user_login/1, init/1]).

-export([init/0,terminate/0]).

%debug
-export([ten_lines/0]).

init() ->
    ok.
init(Args) ->
    Args.
terminate() ->
    ok.


chat_msg(Args) ->
    Senders=eadc_utils:get_val(senders, Args),    
    case Senders of 
	[] ->
	    command_or_something;
	_Else ->
	    Client=eadc_utils:get_val(client, Args),
	    Msg=eadc_utils:get_val(msg, Args),
	    Nick=Client#client.nick,Sid=Client#client.sid,
	    New_Msg=[{{date(),time()}, Sid, eadc_utils:unquote(Msg), Nick}],
	    case eadc_utils:get_option(ten_lines, tenlines, []) of
		[_|Tail]=Msgs ->
		    if length(Msgs) > 10 ->
			    Old_Msgs=Tail;
		       true ->
			    Old_Msgs=Msgs
		    end,
		    New_Msgs=Old_Msgs++New_Msg;
		[] ->
		    New_Msgs=New_Msg
	    end,
	    eadc_utils:set_option(ten_lines, tenlines, New_Msgs)
    end,
    Args.

user_login(Args) ->
    Client=eadc_utils:get_val(client, Args),
    ?DEBUG(debug, "~w: user_login ~w", [?MODULE, Client]),

    {ok,Ten_lines}=(catch ten_lines()),
    eadc_utils:info_to_client(Client, lists:concat([Ten_lines])),
    Args.


ten_lines() ->
    case eadc_utils:get_option(ten_lines, tenlines, []) of
	Msgs when is_list(Msgs) ->
	    Ten_lines = lists:foldl(fun({Time, _Sid, Msg, Nick}, Acc) ->
					    {_,{A,S,D}}=Time,
					    lists:concat([Acc, "[",A,":",S,":",D,"] <",Nick, "> ", Msg, "\n"])
				    end, "Последние сообщения хаба:\n", Msgs),
	    {ok,Ten_lines};
	HZ ->
	    HZ
    end.
    


time_diff(Time) ->
    {N, {N1, N2, N3}}=calendar:time_difference(Time, {date(),time()}),
	time_diff2(N, {"", " день ", " дня ", " дней "})++
	time_diff2(N1, {"", " час ", " часа ", " часов "})++
	time_diff2(N2, {"", " минута ", " минуты ", " минут "})++
	time_diff2(N3, {"0 секунд ", " секунда ", " секунды ", " секунд "})++"назад".

time_diff2(N, {M0,M1,M2,M3}) ->
    case skl(N) of
	0 -> M0;
	1 -> integer_to_list(N)++M1;
	2 -> integer_to_list(N)++M2;
	3 -> integer_to_list(N)++M3
    end. 

skl(N) ->
    case N of
	0 -> 0;
	N when (N rem 10 ==1) and (N =/= 11) -> 1;
	N when ((N rem 10 ==2) or (N rem 10 == 3) or (N rem 10 == 4)) and (N div 10 =/=1) -> 2;
	N -> 3
    end.
