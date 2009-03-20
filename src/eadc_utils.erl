-module(eadc_utils).
-author('jlarky@gmail.com').

-export([convert/1, quote/1, unquote/1]).
-export([random_base32/1, base32/1, base32_encode/1,
	unbase32/1, base32_decode/1]).

-export([code_reload/1]).
-export([parse_inf/1, deparse_inf/1, get_required_field/2, get_val/2, set_val/3]).

-export([broadcast/1, send_to_pids/2, send_to_pid/2, error_to_pid/2, info_to_pid/2,
	 redirect_to/3]).

-export([account_write/1, account_all/0, account_get/1, account_get_login/2]).

-include("eadc.hrl").

%% @spec convert({FromType::atom(), Arg::term()}) -> {Type::atom(), Val::term()}
%% FromType = string | list | args
%% Type = list | string
%% @doc Convert <pre>
%% {string, "some litle string"} to {list, ["some", "litle", "string"]}
%% {list, ["some", "litle string"}] to {string, "some litle string"}
%% {args, ["some", "litle string"}] to {string, "some litle\\sstring"}
%% </pre>
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

%% @spec quote(string()) -> QutedString::string()
%% @doc escape space, newline and '\' charaters. Like "Hello World" -> "Hello\\sWorld"
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

%% @spec unquote(string()) -> UnqutedString::string()
%% @doc remove escape character for space, newline and '\' charaters. Like "Hello\\sWorld" -> "Hello World"
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

%% @type base32string() = [base32char()]
%% @type base32char() = ABCDEFGHIJKLMNOPQRSTUVWXYZ234567
%% @spec random_base32(integer()) -> base32string()
%% @doc return base32 encoded string with length of Count
random_base32(Count) ->
    {A,B,C}=time(), 
    {D,E,F}=random:seed(),
    random:seed(A+D+erlang:crc32(pid_to_list(self())),B+E, C+F),
    random_base32(Count, []).
random_base32(0, Output) ->
    Output;
random_base32(Count, Output) ->
    random_base32(Count-1, Output)++base32(random:uniform(32)-1).

%% @spec base32(integer()) -> [base32char()]
%% @doc return base32 character corresponding with V like 1 -> 'B', 31 -> '7'.
%% V is integer from 0 to 31
%% @see unbase32/1
base32(V) when (V >=0) and (V < 32)->
    if
	V < 0 -> error;
	V < 26 -> [V+65];
	V > 25 -> [V+24] % V-26+48+2
    end;
base32(V) ->
    base32(V bsr 5)++base32(V rem 32).

%% @spec base32_encode(string()) -> base32string()
%% @doc return base32 encoded string of String
%% @see base32_decode/1
base32_encode(String) ->
    base32_encode_(list_to_binary(String), _Out=[]).

base32_encode_(Bin, Out) ->
    case Bin of
	<<>> ->
	    Out;
	<<A:1>> ->
	    Out++base32(A bsl 4);
	<<A:2>> ->
	    Out++base32(A bsl 3);
	<<A:3>> ->
	    Out++base32(A bsl 2);
	<<A:4>> ->
	    Out++base32(A bsl 1);
	Bin ->
	    <<A:5, T/bitstring>>=Bin,
	    base32_encode_(T, Out++base32(A))
    end.

%% @spec unbase32(base32char()) -> integer()
%% @doc A=unbase32(base32(A))
%% @see base32/1
unbase32([V]) when ((V>64) and (V <91)) or ((V > 49) and (V < 56)) ->
    if
	V < 56 -> V-24;
	V > 64 -> V-65
    end;
unbase32(String) ->
    lists:foldl(fun(Char, Acc) ->
			Acc*32+unbase32([Char])
		end, 0, String).

%% @spec base32_decode(base32string()) -> string()
%% @doc return base32 decoded string of String
%% @see base32_encode/1
base32_decode(String) ->
    Bits=lists:foldl(fun(Elem, Acc) ->
			     A= unbase32([Elem]),
			     New= <<Acc/bitstring, A:5>>,
			     New
		     end, <<>>, String),
    base32_decode_(Bits, _Out=[]).

base32_decode_(<<>>, Out) ->
    Out;
base32_decode_(Bits, Out) ->
    case Bits of
	<<Head:8, Rest/bitstring>> ->
	    base32_decode_(Rest, Out++[Head]);
	<<0:1>> -> Out;
	<<0:2>> -> Out;
	<<0:3>> -> Out;
	<<0:4>> -> Out;
	<<0:5>> -> Out;
	<<0:6>> -> Out;
	<<0:7>> -> Out;
	<<H:1>> -> Out++[H bsl 7];
	<<H:2>> -> Out++[H bsl 6];
	<<H:3>> -> Out++[H bsl 5];
	<<H:4>> -> Out++[H bsl 4];
	<<H:5>> -> Out++[H bsl 3];
	<<H:6>> -> Out++[H bsl 2];
	<<H:7>> -> Out++[H bsl 1]
    end.

%% @spec code_reload(atom()) -> {module, Module} | {error, Error}
%% @doc do 'make' and load Module if it's possible
code_reload(Module) ->
    io:format("~s\n", [os:cmd("cd .. && make")]),
    true = code:soft_purge(Module),
    code:load_file(Module).

%% @spec parse_inf(string()) -> TupleList
%% TupleList = [Tuple]
%% Tuple = {Key, Val}
%% Key = atom()
%% Val = string()
%% @doc parse inf message like "NItest DEdesc" into List=[{'NI', "test"}, {'DE', "desc"}].
%% In this case "test"=get_val('NI', List), 'NO KEY' = get_val('VRONGKEY', List)
%% @see get_val/2
%% @see deparse_inf/1
parse_inf(Inf) ->
    {list, List} = convert({string, Inf}),
    lists:map(fun([H1,H2|T]) ->
		      {list_to_atom([H1,H2]), T}
	      end, List).

%% @spec deparse_inf(TupleList) -> string()
%% TupleList = [Tuple]
%% Tuple = {Key, Val}
%% Key = atom()
%% Val = string()
%% @doc combine parsed inf message like [{'NI', "test"}, {'DE', "desc"}] into "NItest DEdesc"
%% @see parse_inf/1
deparse_inf(Parsed_Inf) ->
    lists:foldl(fun({Key, Val}, Acc) ->
			lists:concat([Acc," ",Key,Val])
		end, "", Parsed_Inf).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comunication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec send_to_pids(Pids, Message::term()) -> ok
%% Pids = [pid()]
%% @doc apply send_to_pid/2 with every pid of Pids
%% @see send_to_pid/2
send_to_pids(Pids, Param) when is_list(Pids) ->
    lists:foreach(fun(Pid) ->
			  send_to_pid(Pid, Param)
		  end, Pids).

%% @spec send_to_pid(pid(), Param::term()) -> ok
%% Param = {list, List} | {args, List} | List
%% List = string()
%% @doc apply {@link convert/1} to Param if needed and sends String to socket controled by gen_fsm with pid Pid
send_to_pid(Pid, {list, List}) when is_list(List) ->
    {string, String} = eadc_utils:convert({list, List}),
    eadc_utils:send_to_pid(Pid, String);
send_to_pid(Pid, {args, List}) when is_list(List) ->
    {string, String} = eadc_utils:convert({args, List}),
    eadc_utils:send_to_pid(Pid, String);
send_to_pid(Pid, String) when is_pid(Pid) and is_list(String) ->
    gen_fsm:send_event(Pid, {send_to_socket, String});
send_to_pid(undefined, String) when is_list(String) ->
    ok;
send_to_pid(Unknown1, Unknown2) ->
    ?DEBUG(error, "send_to_pid(~w, ~w)\n", [Unknown1, Unknown2]).

%% @spec broadcast(Argument::term()) -> ok
%% Argument = {string, String::string} | fun()
broadcast({string,String}) when is_list(String) -> 
    broadcast(fun(Client) -> send_to_pid(Client, String) end);
broadcast(F) when is_function(F) ->
    lists:foreach(F, eadc_client_fsm:all_pids()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% messaging functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec error_to_pid(pid(), string()) -> ok
%% @doc send error message to Pid by {@link send_to_pid/2}
error_to_pid(Pid, Message) ->
    send_to_pid(Pid, {args, ["ISTA", "100", Message]}).
%% @spec info_to_pid(pid(), string()) -> ok
%% @doc send info message to Pid by {@link send_to_pid/2}
info_to_pid(Pid, Message) ->
    send_to_pid(Pid, {args, ["ISTA", "000", Message]}).

redirect_to(Pid, Sid, Hub) ->
    R_msg="You are redirected to dchub://jlarky.punklan.net",
    eadc_utils:info_to_pid(Pid, R_msg),
    SID= if
	     is_atom(Sid) -> atom_to_list(Sid);
	     is_list(Sid) -> Sid
	 end,
    eadc_utils:send_to_pid(Pid, {args, ["IQUI", SID, "RDdchub://jlarky.punklan.net", "MS"++R_msg]}),
    gen_fsm:send_event(Pid, kill_your_self).

get_required_field(Key, PInf) ->
    case (catch lists:keysearch(Key, 1, PInf)) of
	{value,{Key, Val}} ->
	    Val;
	Not_found_or_error ->
	    ?DEBUG(error, "~w not fount required_field ~w: ~w", [self(), Key, Not_found_or_error]),
	    error_to_pid(self(), lists:concat(["Required field ", Key, " not found"])),
	    gen_fsm:send_event(self(), kill_your_self)
    end.

%% @spec get_val(atom(), TupleList1) -> Val::term() | 'NO KEY'
%% TupleLis = [{Key::atom(), Val::term()}]
%% @doc return value with key Key or 'NO KEY'
get_val(Key, Args) -> 
    case lists:keysearch(Key, 1, Args) of
	{value,{Key, Val}} -> Val;
	_ -> 'NO KEY'
    end.

%% @spec set_val(atom(), term(), TupleList1) -> TupleList2
%% TupleLis1 = [{Key::atom(), OldVal::term()}]
%% TupleLis2 = [{Key::atom(), Val::term()}]
%% @doc replace tuple {Key, OldVar} with {Key, Val} in list Args 
set_val(Key, Val, Args) -> 
    lists:keyreplace(Key, 1, Args, {Key, Val}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% account functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @type account() = record(account)
%% @spec account_write(account()) -> {atomic, ok} | {aborted, Reason}
%% @doc writes account in mnesia table  
account_write(Account) when is_record(Account, account)->
    F=fun() ->
	      mnesia:write(Account)
      end,
    mnesia:transaction(F).

%% @spec account_all() -> Accounts | {error, Error}
%% Accounts = [account()]
%% @doc return list of all account
account_all() ->
    F = fun()->
		mnesia:match_object(#account{_='_'})
	end,
    case (catch mnesia:transaction(F)) of
	{atomic, List} when is_list(List) ->
	    List;
	Error ->
	    {error, Error}
    end.

%% @spec account_get(string()) -> Account
%% Account = account()
%% @doc return account record with login Login
account_get(Login) ->
    F = fun()->
		mnesia:match_object(#account{login=Login,_='_'})
	end,

    case (catch mnesia:transaction(F)) of
	{atomic, [Account]} ->
	    Account#account{};
	_ ->
	    false
    end.

%% @spec account_get_login(string(), string()) -> {login, Ligin::string()} | false
%% @doc find account with this Nick or Cid.
account_get_login(Nick, Cid) ->
    MatchHead = #account{cid='$1', nick='$2', _='_', login='$3'},
    Guard = [{'or',{'==','$2',Nick},{'==','$1',Cid}}], Result = '$3',
    F = fun() ->
		mnesia:select(account,[{MatchHead, Guard, [Result]}])
	end,
    A=(catch mnesia:transaction(F)),
    case A of
	{atomic, [Log]} ->
	    {login, Log};
	_ ->
	    false
    end.
