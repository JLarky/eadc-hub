-module(eadc_utils).
-author('jlarky@gmail.com').

-export([quote/1, unquote/1,s2a/1, a2s/1]).
-export([base32/1, base32_encode/1,unbase32/1, base32_decode/1, 
	 random/1, random_string/1, sid_to_s/1, cid_to_s/1]).

-export([code_reload/1]).
-export([parse_inf/1, deparse_inf/1, get_required_field/2, get_val/2, set_val/3]).

-export([broadcast/1, send_to_pids/2, send_to_pid/2, error_to_pid/2, info_to_pid/2,
	 redirect_to/3]).

-export([account_write/1, account_all/0, account_get/1, account_get_login/2]).

-export([get_option/3, set_option/3, get_options/1]).

-include("eadc.hrl").

%% @doc Converts <code>"some little\sstring" -> ["some", "little string"]</code>
s2a(String) -> _Args=lists:map(fun unquote/1, string:tokens(String," ")).
%% @doc Converts <code>%% ["some", "little string"] -> "some little\sstring"</code>
a2s(Args) -> _String=string:join(lists:map(fun quote/1, Args), " ").

%% @spec quote(string()) -> QutedString::string()
%% @doc quotes space, newline and '\' characters. Like "Hello World" -> "Hello\sWorld"
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
%% @doc removes escape character for space, newline and '\' charaters. Like "Hello\sWorld" -> "Hello World"
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

random(Max) ->
    {A,B,C}=time(),
    {D,E,F}=random:seed(),
    random:seed(A+D+erlang:crc32(pid_to_list(self())),B+E, C+F),
    random:uniform(Max).

random_string(Length) ->
    random_string_(Length, "").

random_string_(Length, Acc) when Length < 1->
    Acc;
random_string_(Length, Acc) ->
    {A,B,C}=time(),
    {D,E,F}=random:seed(),
    random:seed(A+D+erlang:crc32(pid_to_list(self())),B+E, C+F),
    random_string_(Length-1, [random:uniform(255)|Acc]).

%% @spec base32(integer()) -> [base32char()]
%% @doc returns base32 character corresponding to V like 1 -> 'B', 31 -> '7'.
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
%% @doc returns base32 encoded string of String
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
unbase32([V]) when ((V >= $A) and (V =< $Z)) ->
    V-$A;
unbase32([V]) when ((V >= $2) and (V =< $7)) ->
    V-$2+26;
unbase32([V]) ->
    throw({badarg, [V]});
unbase32(String=[_|_]) ->
    lists:foldl(fun(Char, Acc) ->
			Acc*32+unbase32([Char])
		end, 0, String).

%% @spec base32_decode(base32string()) -> string()
%% @doc returns base32 decoded string of String
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

fillA(I, String) when I < 1 ->
    String;
fillA(Count, String) ->
    fillA(Count-1, [$A|String]).

sid_to_s(Sid) ->
    Sid_string=base32(Sid),
    fillA(4-length(Sid_string), Sid_string).

cid_to_s(Sid) ->
    Sid_string=base32(Sid),
    fillA(39-length(Sid_string), Sid_string).

%% @spec code_reload(atom()) -> {module, Module} | {error, Error}
%% @doc does 'make' and loads Module if it's possible.
code_reload(Module) ->
    io:format("~s\n", [os:cmd("cd .. && make")]),
    true = code:soft_purge(Module),
    code:load_file(Module).

%% @spec parse_inf(string()) -> TupleList
%% TupleList = [Tuple]
%% Tuple = {Key, Val}
%% Key = atom()
%% Val = string()
%% @doc parses inf message like "NItest DEdesc" into List=[{'NI', "test"}, {'DE', "desc"}].
%% In this case "test"=get_val('NI', List), 'NO KEY' = get_val('WRONGKEY', List)
%% @see get_val/2
%% @see deparse_inf/1
parse_inf(Inf) ->
    List = s2a(Inf),
    lists:map(fun([H1,H2|T]) ->
		      {list_to_atom([H1,H2]), T}
	      end, List).

%% @spec deparse_inf(TupleList) -> string()
%% TupleList = [Tuple]
%% Tuple = {Key, Val}
%% Key = atom()
%% Val = string()
%% @doc combines parsed inf message like [{'NI', "test"}, {'DE', "desc"}] into "NItest DEdesc"
%% @see parse_inf/1
deparse_inf(Parsed_Inf) ->
    lists:foldl(fun({Key, Val}, Acc) ->
			lists:concat([Acc," ",Key,Val])
		end, "", Parsed_Inf).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec send_to_pids(Pids, Message::term()) -> ok
%% Pids = [pid()]
%% @doc applies send_to_pid/2 to every pid of Pids
%% @see send_to_pid/2
send_to_pids(Pids, Param) when is_list(Pids) ->
    lists:foreach(fun(Pid) ->
			  send_to_pid(Pid, Param)
		  end, Pids).

%% @spec send_to_pid(pid(), Param::term()) -> ok
%% Param = {list, List} | {args, List} | List
%% List = string()
%% @doc applies {@link convert/1} to Param if needed and sends String to socket controlled by gen_fsm with pid Pid
%%send_to_pid(Pid, {list, List}) when is_list(List) ->
%%    {string, String} = eadc_utils:convert({list, List}),
%%    eadc_utils:send_to_pid(Pid, String);
send_to_pid(Pid, {args, List}) when is_list(List) ->
    String = eadc_utils:a2s(List),
    eadc_utils:send_to_pid(Pid, String);
send_to_pid(Pid, String) when is_pid(Pid) and is_list(String) ->
    gen_fsm:send_event(Pid, {send_to_socket, String});
send_to_pid(Atom, String) when is_atom(Atom) andalso is_list(String) ->
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
%% Messaging functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec error_to_pid(pid(), string()) -> ok
%% @doc sends error message to Pid by {@link send_to_pid/2}
error_to_pid(Pid, Message) ->
    send_to_pid(Pid, {args, ["ISTA", "100", Message]}).
%% @spec info_to_pid(pid(), string()) -> ok
%% @doc sends info message to Pid by {@link send_to_pid/2}
info_to_pid(Pid, Message) ->
    send_to_pid(Pid, {args, ["ISTA", "000", Message]}).

redirect_to(Pid, Sid, Hub) ->
    R_msg="You are redirected to "++Hub,
    eadc_utils:info_to_pid(Pid, R_msg),
    SID= if
	     is_integer(Sid) -> unbase32(Sid);
	     is_list(Sid) -> Sid
	 end,
    io:format("!~w\n", [SID]),
    eadc_utils:send_to_pid(Pid, {args, ["IQUI", SID, "RD"++Hub, "MS"++R_msg]}),
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
%% @doc returns value with key Key or 'NO KEY'
get_val(Key, Args) -> 
    case lists:keysearch(Key, 1, Args) of
	{value,{Key, Val}} -> Val;
	_ -> 'NO KEY'
    end.

%% @spec set_val(atom(), term(), TupleList1) -> TupleList2
%% TupleLis1 = [{Key::atom(), OldVal::term()}]
%% TupleLis2 = [{Key::atom(), Val::term()}]
%% @doc replaces tuple {Key, OldVar} with {Key, Val} in list Args 
set_val(Key, Val, Args) -> 
    lists:keyreplace(Key, 1, Args, {Key, Val}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Account functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @type account() = record(account)
%% @spec account_write(account()) -> {atomic, ok} | {aborted, Reason}
%% @doc writes account into mnesia table  
account_write(Account) when is_record(Account, account)->
    F=fun() ->
	      mnesia:write(Account)
      end,
    mnesia:transaction(F).

%% @spec account_all() -> Accounts | {error, Error}
%% Accounts = [account()]
%% @doc returns list of all accounts
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
%% @doc returns account record with Login
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
%% @doc finds account with this Nick or Cid.
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


get_option(Ns, Key, Default) ->
    F = fun()->
		mnesia:match_object(#option{id={Ns, Key}, _='_'})
	end,
    case (catch mnesia:transaction(F)) of
	{atomic, [Val|_]=List} when is_list(List) ->
	    Val#option.val;
	_ ->
	    Default
    end.

set_option(Ns, Key, Val) ->
    F = fun()->
		mnesia:write(#option{id={Ns, Key}, val=Val})
	end,
    (catch mnesia:transaction(F)).

get_options(OptionTemplate) when is_record(OptionTemplate, option)->
    F = fun()->
		mnesia:match_object(OptionTemplate)
	end,
    case (catch mnesia:transaction(F)) of
	{atomic, List} when is_list(List) ->
	    List;
	Error ->
	    {error, Error}
    end.
