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
    eadc_utils:send_to_pid(self(), {args, ["ICMD", "IP",
					   "TTBMSG\s%[mySID]\s.ip\\s%[userNI]\n", "CT15"]}),
    Args.

chat_msg(Args) ->
    Msg=eadc_utils:get_val(msg,Args),
    case Msg of
	[$., $i, $p, $\  | Tail] ->
	    try
		[Client|_]=eadc_user:client_find(#client{nick=Tail,_='_'}),
		Addr=Client#client.addr,
		Place=whois(Addr),
		Place_m =  case Place of
			       inet -> "интернетах. Осторожно! трафик!";
			       _    -> f("~w общаге.",[Place])
			   end,
		eadc_utils:info_to_pid(self(), f("Пользователь ~s находится в ~s",
						 [Tail, Place_m]))
	    catch error:Error ->
		    eadc_utils:info_to_pid(self(), io_lib:format("Error: ~p\n",[Error]))
	    end,
	    eadc_utils:set_val(pids, [], Args);
	_ ->
	    Args
    end.

rcm(Args) ->
    ctm(eadc_utils:set_val(rcm, true,Args)).
 
ctm(Args) ->
    try
	[Pid]=eadc_utils:get_val(pids, Args),
	%%State_f=eadc_utils:get_val(state, Args),
	Client=eadc_utils:get_val(client, Args),
	Nick_f=Client#client.nick,
	
	[Client_t]=eadc_user:client_find(#client{pid=Pid, _='_'}),
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
		eadc_utils:info_to_pid(self(), "Соединенние между "++Nick_f++" и "++Nick_t++
				       " ("++Direction++") не состоится. Ваш Хаб."),
		eadc_utils:set_val(pids, [], Args);
	    _ ->
		Args
	end

    catch error:Error ->
	    eadc_utils:info_to_pid(self(), io_lib:format("Error: ~p\n",[Error])),
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
    {ok, B}=regexp:split(A, "\n"),
    %% C - list ok {start_ip, end_ip, campus}
    C=lists:foldl(fun(String, Acc) ->
			  case String of
			      [$#|_] ->
				  Acc;
			      [] ->
				  Acc;
			      _ ->
				  {ok,[Ip_range,Campus]} = regexp:split(String, "\tcampus"),
				  {ok,[Ip,Range]} = regexp:split(Ip_range, "/"),
				  {ok, {N1,N2,N3,N4}}=inet_parse:address(Ip),
				  IP=N4+256*(N3+256*(N2+256*N1)),
				  RANGE=1 bsl (32-list_to_integer(Range)),
				  [{IP, IP+RANGE, list_to_integer(Campus)}|Acc]
			  end end, [], B),
    eadc_utils:set_option(punklan, lastupdate, calendar:local_time()),
    eadc_utils:set_option(punklan, networks, C).
