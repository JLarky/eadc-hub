-module(eadc_utils).
-author('jlarky@gmail.com').

-export([convert/1, quote/1, unquote/1, cuteol/1]).
-export([random_base32/1, base32/1]).

-export([code_reload/1]).
-export([parse_inf/1]).

-export([broadcast/1, send_to_pids/2, send_to_pid/2, error_to_pid/2, info_to_pid/2]).

-include("eadc.hrl").

convert({string, String}) ->
    convert_string (String);
convert({list, List}) when is_list(List) ->
    convert_list(_Out=[], List);
convert({args, List}) when is_list(List) ->
    convert_args(_Out=[], List).

convert_string(String) ->
    {Bu, Ac} = lists:foldr(fun(Char, {Buf, Acc}) ->
			 case Char of
			     $\  -> {[], [Buf|Acc]};
			     _ -> {[Char | Buf], Acc}
			 end
		 end, {[],[]}, String), {list, [Bu|Ac]}.


convert_list(Out, []) ->
    {string, Out};
convert_list( [], [H|T]) when is_list(H)->
    convert_list(H, T);
convert_list(Out, [H|T]) when is_list(H)->
    convert_list(Out++" "++H, T).

convert_args(Out, []) ->
    {string, Out};
convert_args( [], [H|T]) when is_list(H)->
    convert_args(quote(H), T);
convert_args(Out, [H|T]) when is_list(H)->
    convert_args(Out++" "++quote(H), T).

quote(String) ->
    lists:foldl(fun(Char, Acc) -> case Char of
				      $\ ->
					  Acc++"\\s";
				      $\\ ->
					  Acc++"\\\\";
				      $\n ->
					  Acc++"\\n";
				      _ ->
					  Acc++[Char]
				  end end, [], String).

unquote(String) ->
    {Buf, U_String} =
	lists:foldl(
	  fun(Char, {Prev, Acc}) -> case {Prev, Char} of
					{$\\,$\\} ->
					    {[], Acc++"\\"};
					{$\\,$s} ->
					    {[], Acc++" "};
					{$\\,$n} ->
					    {[], Acc++"\n"};
					{[], Char} ->
					    {Char, Acc};
					_ ->
					    {Char, lists:append(Acc,[Prev])}
				    end end, {[],[]}, String),
    lists:append(U_String, [Buf]).

random_base32(Count) ->
    {A,B,C}=time(), 
    {D,E,F}=random:seed(),
    random:seed(A+D+erlang:crc32(pid_to_list(self())),B+E, C+F),
    random_base32(Count, []).
random_base32(0, Output) ->
    Output;
random_base32(Count, Output) ->
    random_base32(Count-1, Output)++base32(random:uniform(32)-1).


base32(V) ->
    if
	V < 0 -> error;
	V < 26 -> [V+65];
	V > 25 -> [V+24]; % V-26+48+2
	true -> V
    end.

code_reload(Module) ->
    error_logger:info_msg("~s", [os:cmd("cd .. && make")]),
    true = code:soft_purge(Module),
    code:load_file(Module).

cuteol(String) ->
    String.


parse_inf(Inf) ->
    {list, List} = convert({string, Inf}),
    lists:map(fun([H1,H2|T]) ->
		      {list_to_atom([H1,H2]), T}
	      end, List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comunication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_to_pids(Pids, Param) when is_list(Pids) ->
    lists:foreach(fun(Pid) ->
			  send_to_pid(Pid, Param)
		  end, Pids).

send_to_pid(Pid, {list, List}) when is_list(List) ->
    {string, String} = eadc_utils:convert({list, List}),
    eadc_utils:send_to_pid(Pid, String);
send_to_pid(Pid, {args, List}) when is_list(List) ->
    {string, String} = eadc_utils:convert({args, List}),
    eadc_utils:send_to_pid(Pid, String);
send_to_pid(Pid, String) when is_pid(Pid) and is_list(String) ->
    gen_fsm:send_event(Pid, {send_to_socket, String}).

broadcast({string,String}) when is_list(String) -> 
    broadcast(fun(Client) -> send_to_pid(Client, String) end);
broadcast(F) when is_function(F) ->
    lists:foreach(F, eadc_client_fsm:all_pids()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% messaging functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

error_to_pid(Pid, Message) ->
    send_to_pid(Pid, {args, ["ISTA", "100", Message]}).
info_to_pid(Pid, Message) ->
    send_to_pid(Pid, {args, ["ISTA", "000", Message]}).
