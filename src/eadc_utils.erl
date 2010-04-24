-module(eadc_utils).
-author('jlarky@gmail.com').

-export([quote/1, unquote/1,s2a/1, a2s/1, thing_to_string/1,format/2]).
-export([base32/1, base32_encode/1,unbase32/1, base32_decode/1, 
	 random/1, random_string/1, sid_to_s/1, cid_to_s/1]).

-export([debug/3, code_reload/1, code_update/0, code_update/1, make_script/0, make_tar/1]).
-export([parse_inf/1, deparse_inf/2, get_required_field/3, get_nick_field/1,get_cid_field/2,
	 get_val/2, get_val/3, set_val/3]).

-export([broadcast/1, send_to_pids/2, send_to_pid/2, error_to_pid/2, info_to_pid/2,
	 redirect_to/2]).

-export([send_to_senders/2,send_to_sender/2,error_to_sender/2, info_to_sender/2,
	 send_to_clients/2,send_to_client/2,error_to_client/2, info_to_client/2]).

-export([get_option/3, set_option/3, get_options/1]).

-export([get_unix_timestamp/1]).

-export([test/0,profile/1]).

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

thing_to_string(Thing) ->
    lists:flatten(io_lib:format("~p",[Thing])).

format(Format, Thing) ->
    lists:flatten(io_lib:format(Format,Thing)).

random(Max) ->
    random_p:uniform(Max).

random_string(Length) ->
    random_string_(Length, "").

random_string_(Length, Acc) when Length < 1->
    Acc;
random_string_(Length, Acc) ->
    random_string_(Length-1, [random_p:uniform(255)|Acc]).

%% @spec base32(integer()) -> [base32char()]
%% @doc returns base32 character corresponding to V like 1 -> 'B', 31 -> '7'.
%% V is integer from 0 to 31
%% @see unbase32/1
base32(V) when (V >=0) and (V < 32)->
    if
        V < 26 ->
	    [V+65];
        V > 25 ->
	    [V+24] % V-26+48+2
    end;

base32(V) ->
    base32(V bsr 5)++base32(V rem 32).

%% @spec base32_encode(string()) -> base32string()
%% @doc returns base32 encoded string of String
%% @see base32_decode/1
base32_encode(String) ->
    lists:reverse(base32_encode_(list_to_binary(String), _Out=[])).

base32_encode_(Bin, Out) ->
    case Bin of
	<<>> ->
	    Out;
	<<A:1>> ->
	    [B]=base32(A bsl 4),[B|Out];
	<<A:2>> ->
	    [B]=base32(A bsl 3),[B|Out];
	<<A:3>> ->
	    [B]=base32(A bsl 2),[B|Out];
	<<A:4>> ->
	    [B]=base32(A bsl 1),[B|Out];
	Bin ->
	    <<A:5, T/bitstring>>=Bin,
	    [B]=base32(A),base32_encode_(T, [B|Out])
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

%% @spec debug(atom(), Format::string(), Data::list()) -> ok
debug(debug, _Format, _Data) ->
    %%error_logger:info_msg(Format, Data);
    ok;
debug(info, Format, Data) ->
    error_logger:info_msg(Format, Data);
debug(error, Format, Data) ->
    error_logger:error_msg(Format, Data);
debug(_Type, Format, Data) ->
    error_logger:info_msg(Format, Data).


%% @spec code_reload(atom()) -> {module, Module} | {error, Error}
%% @doc does 'make' and loads Module if it's possible.
code_reload(Module) ->
    io:format("~s\n", [os:cmd("cd .. && make")]),
    true = code:soft_purge(Module),
    code:load_file(Module).

code_update() ->
    code_update(compiled).

code_update(Mode) ->
    A=os:cmd("cd .. && make"),
    io:format("~ts\n",[A]),
    case string:str(A, "Error") of
	0 -> %% no error in make
	    case file:consult("eadc.app") of
		{ok, [{application,eadc,D}]} -> Info=D;
		Other -> Info=throw({Other})
	    end,
	    Modules=get_val(modules, Info),
	    F=fun(Module) ->
		      case (Mode/=compiled) or (string:str(A, atom_to_list(Module))>0) of
			  false -> %% module was not changed
			      ok;
			  true -> %% module was compiled
			      P=code:soft_purge(Module),
			      C=code:load_file(Module),
			      M=case P of
				    true -> "";
				    false -> "Warning!: "
				end,
			      io:format("~25s: ~p\n",[M++atom_to_list(Module),{P,C}])
		      end
	      end, lists:foreach(F, Modules);
	_ -> error
    end.

make_script() ->
    systools:make_script("eadc",[{outdir, "ebin"},{path,["deps/*/ebin"]}]).

make_tar(Dir) ->
    Name="eadc",TarFileName = io_lib:fwrite("~s/~s.tar.gz", [Dir,Name]),
    ok=systools:make_tar(Name, [{erts,code:root_dir()},{outdir,Dir}]),
    TmpDir=Dir++"/tmp",file:make_dir(TmpDir),
    %%os:cmd("(cd "++TmpDir++"&& tar xf "++TarFileName++")"),
    erl_tar:extract(TarFileName,[compressed,{cwd,TmpDir}]),file:delete(TarFileName),
    Start=TmpDir++"/start.sh",
    file:write_file(Start,
		    <<"#/bin/sh\nerts-*/bin/erl -boot releases/ADC\\ Hub/start -pa lib/*/*\n">>),
    os:cmd("chmod +x "++Start),
    %%os:cmd("(cd "++TmpDir++"&& tar cf "++TarFileName++" .)"),
    {ok, Cwd} = file:get_cwd(),file:set_cwd(TmpDir),
    (catch erl_tar:create(TarFileName,["."],[compressed])),
    file:set_cwd(Cwd),
    os:cmd("rm -r "++TmpDir),
    ok.

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
    ["BINF", _SID |List] = s2a(Inf),
    lists:map(fun([H1,H2|T]) ->
		      {list_to_atom([H1,H2]), T}
	      end, List).

%% @spec deparse_inf(SID::string(),TupleList) -> string()
%% TupleList = [Tuple]
%% Tuple = {Key, Val}
%% Key = atom()
%% Val = string()
%% @doc combines parsed inf message like [{'NI', "test"}, {'DE', "desc"}] into "NItest DEdesc"
%% @see parse_inf/1
deparse_inf(SID, Parsed_Inf) ->
    lists:foldl(fun({Key, Val}, Acc) ->
			lists:concat([Acc," ",Key,Val])
		end, "BINF "++SID, Parsed_Inf).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_to_clients(Clients, Param) ->
    lists:foreach(fun(Client) ->
                          send_to_client(Client, Param)
                  end, Clients).

send_to_client(Client, Param) ->
    send_to_sender(Client#client.sender,Param).

%% @spec send_to_senders(Senders, Message::term()) -> ok
%% Senders = [sender()]
%% @doc applies send_to_sender/2 to every sender of Senders
%% @see send_to_sender/2
send_to_senders(Senders, Param) when is_list(Senders) ->
    lists:foreach(fun(Sender) ->
			  send_to_sender(Sender, Param)
		  end, Senders).

%% @spec send_to_sender(sender(), Param::term()) -> ok
%% Param = {list, List} | {args, List} | List
%% List = string()
%% @doc applies {@link convert/1} to Param if needed and sends String to socket controlled by sockroute
%%send_to_sender(Sender, {list, List}) when is_list(List) ->
%%    {string, String} = eadc_utils:convert({list, List}),
%%    eadc_utils:send_to_sender(Sender, String);
send_to_sender(Sender, {args, List}) when is_list(List) ->
    String = eadc_utils:a2s(List),
    send_to_sender(Sender, String);
send_to_sender(Sender, String) when is_record(Sender,sender) and is_list(String) ->
    wait_sockroute(Sender#sender.pid, 100, 10),
    sockroute:asendn(Sender, String);

send_to_sender(Unknown1, Unknown2) ->
    ?DEBUG(error, "send_to_pid(~w, ~w)\n", [Unknown1, Unknown2]).

wait_sockroute(_Pid, N, M) when (N < M) ->
    ok;
wait_sockroute(Pid, N, M) ->
    case get_queue_len(Pid) > N of
	false -> ok;
	true -> 
	    %%sockroute:send(Sender, "\n"),
	    timer:sleep(0),
	    ok
	    %%wait_sockroute(Pid, N-1, M)
    end.

get_queue_len(Pid) ->
    case process_info(Pid) of
	undefined -> 0;
	Info ->
	    MQ=get_val(message_queue_len, Info),
	    case MQ of
		I when is_integer(I) -> I;
		_ -> 0
	    end
    end.



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
    broadcast(fun(Client) -> send_to_sender(Client, String) end);
broadcast({info,String}) when is_list(String) -> 
    broadcast(fun(Client) -> info_to_sender(Client, String) end);
broadcast(F) when is_function(F) ->
    lists:foreach(F, eadc_client:all_senders()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Messaging functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec error_to_pid(pid(), string()) -> ok
%% @doc sends error message to Pid by {@link send_to_pid/2}
error_to_client(Client, Message) when is_record(Client,client) ->
    send_to_sender(Client#client.sender, {args, ["ISTA", "100", Message]}).
%% @spec info_to_pid(pid(), string()) -> ok
%% @doc sends info message to Pid by {@link send_to_pid/2}
info_to_client(Client, Message) when is_record(Client,client) ->
    send_to_sender(Client#client.sender, {args, ["ISTA", "000", Message]}).

%% @spec error_to_pid(pid(), string()) -> ok
%% @doc sends error message to Pid by {@link send_to_pid/2}
error_to_sender(Sender, Message) when is_record(Sender,sender) ->
    send_to_sender(Sender, {args, ["ISTA", "100", Message]}).
%% @spec info_to_pid(pid(), string()) -> ok
%% @doc sends info message to Pid by {@link send_to_pid/2}
info_to_sender(Sender, Message) when is_record(Sender,sender) ->
    send_to_sender(Sender, {args, ["ISTA", "000", Message]}).



%% @spec error_to_pid(pid(), string()) -> ok
%% @doc sends error message to Pid by {@link send_to_pid/2}
error_to_pid(Pid, Message) ->
    send_to_pid(Pid, {args, ["ISTA", "100", Message]}).
%% @spec info_to_pid(pid(), string()) -> ok
%% @doc sends info message to Pid by {@link send_to_pid/2}
info_to_pid(Pid, Message) ->
    send_to_pid(Pid, {args, ["ISTA", "000", Message]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%

redirect_to(Client, Hub) ->
    R_msg="You are redirected to "++Hub,
    SID=sid_to_s(Client#client.sid),
    eadc_client:logoff(Client,{string, a2s(["IQUI", SID, "RD"++Hub, "MS"++R_msg])}).

get_required_field(Key, PInf, Client) ->
    case (catch lists:keysearch(Key, 1, PInf)) of
	{value,{Key, Val}} ->
	    Val;
	Not_found_or_error ->
	    ?DEBUG(error, "~w not fount required_field ~w: ~w", [self(), Key, Not_found_or_error]),
	    error_to_client(Client,lists:concat(["Required field ", Key, " not found"])),
	    throw({stop,Client})
    end.

get_nick_field(P_Inf) ->
    case eadc_utils:get_val('NI', "", P_Inf) of
	"" -> "not_set:"++base32_encode(random_string(5));
	N -> N
    end.

get_cid_field(P_Inf,#client{nick=Nick}=Client) ->
    PID=get_required_field('PD', P_Inf, Client),
    Cid_f=get_required_field('ID', P_Inf, Client),
    Cid_u=eadc_client:get_uniq_cid(Cid_f),
    Cid_p=(catch eadc_utils:base32_encode(tiger:hash(eadc_utils:base32_decode(PID)))),
    case Cid_p==Cid_u of
	true ->
	    Cid_u;
	false ->
	    _Msg=lists:concat(["User '", Nick, "' has wrong CID (",Cid_f,"). Be aware"]),
	    %%eadc_utils:broadcast({info, _Msg}),
	    eadc_utils:info_to_client(Client,"Your CID isn't corresponding to PID. "
				      "You are cheater."),
	    eadc_client:get_uniq_cid("NOTSET"++Cid_u)%% user can't take CID with incorrect PID
    end.


%% @spec get_val(atom(), Default::term(), TupleList1) -> Val::term() | Default::term()
%% TupleList = [{Key::atom(), Val::term()}]
%% @doc returns value with key Key or Default if not found
get_val(Key, Default, Args) ->
    case (catch lists:keysearch(Key, 1, Args)) of
	{value,{Key, Val}} -> Val;
	_ -> Default
    end.

%% @spec get_val(atom(), TupleList1) -> Val::term() | 'NO KEY'
%% TupleList = [{Key::atom(), Val::term()}]
%% @doc returns value with key Key or 'NO KEY'
get_val(Key, Args) -> 
    get_val(Key, 'NO KEY', Args).

%% @spec set_val(atom(), term(), TupleList1) -> TupleList2
%% TupleLis1 = [{Key::atom(), OldVal::term()}]
%% TupleLis2 = [{Key::atom(), Val::term()}]
%% @doc replaces tuple {Key, OldVar} with {Key, Val} in list Args 
set_val(Key, Val, Args) -> 
    lists:keystore(Key, 1, Args, {Key, Val}).



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

%% @doc get_unix_timestamp
%% @spec
%% @output
get_unix_timestamp({_MegaSecs, _Secs, _MicroSecs}=TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
	calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}}).


profile({M,F,A}) ->
    fprof:apply(M, F, A),fprof:profile(),fprof:analyse();

profile(Module) ->
    code_reload(Module),fprof:apply(Module, test, []),fprof:profile(),fprof:analyse().

test() ->
    "MFZWIMJSGMYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYTCMJRGEYQ"=base32_encode("asd12311111111111111111111111111111111111111111111111111111111111111111111111111111111111111"),
    ok.
