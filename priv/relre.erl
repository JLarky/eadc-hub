#!/usr/bin/escript

main([RelPath]) ->
    try
        case file:consult(RelPath) of
            {ok, [{release, ROrig, Erts, Apps}]} ->
                NewRel = {release, ROrig, refr(Erts), [refr(A) || A <- Apps]},
		file:write_file(RelPath,io_lib:format("~p.~n", [NewRel])),
		io:format("File ~s was updated\n~p.~n", [RelPath,NewRel]);
            _ ->
                ok
        end
    catch
        C:W ->
            io:format("Error: ~p:~p~n~p", [C, W, erlang:get_stacktrace()])
    end;
main(_) ->
    usage().

usage() ->
    io:format("Usage: relre.erl path_to.rel~n"),
    halt(1).

refr({AppName, _AppVsn} = S) ->
    SAppN = atom_to_list(AppName),
    case code:lib_dir(SAppN) of
        Path when is_list(Path) ->
            BN = filename:basename(Path),
            {AppName, string:sub_string(BN, length(SAppN)+2)};
        {error, bad_name} ->
            S
    end;
refr({AppName, AppVsn, TA}) ->
    {NAppName, NVsn} = refr({AppName, AppVsn}),
    {NAppName, NVsn, TA};
refr({AppName, AppVsn, Type, IncApps}) ->
    {NAppName, NVsn} = refr({AppName, AppVsn}),
    {NAppName, NVsn, Type, IncApps}.
