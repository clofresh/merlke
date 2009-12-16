-module(merlke).
-export([
    clean/0,
    compile/0,
    generate/0,
    edoc/0,
    dialyzer/0,
    test/0,
    start/0,
    dist/0,
    ebin_dir/0,
    edoc_dir/0
]).

-include("include/merlke.hrl").

clean() -> 
    DeleteFun = fun
        ({regular, File}) ->
            io:format("Removing ~s~n", [File]),
            ok = file:delete(File);
        ({directory, Dir}) ->
            io:format("Removing ~s~n", [Dir]),
            ok = file:del_dir(Dir)
    end,
    
    lists:foreach(
        fun(D) -> 
            merlke_files:traversal(D, DeleteFun)
        end, 
        [merlkefile_api:ebin_dir(), 
         merlkefile_api:edoc_dir()]
    ).

compile() ->
    
    ToCompile = [lists:concat([merlkefile_api:src_dir(), "/", Module, ".erl"])
                    || Module <- merlkefile_api:modules()],

    case make:files(ToCompile, ?MAKE_OPTIONS) of
        up_to_date ->
            ok;
        error ->
            io:format("~n**** Compilation failed.~n"),
            halt(1)
    end.

generate() ->
    EbinDir = filename:absname(merlkefile_api:ebin_dir()),
    merlke_files:traversal(
        ".", 
        fun generate/1,
        fun
            ({regular, _}) ->
                allow;
            ({directory, Dir}) ->
                case filename:absname(Dir) of
                    EbinDir -> skip;
                    _ -> allow
                end
        end
    ).

generate({regular, File}) ->
    case filename:extension(File) of
        ".xrl" -> 
            io:format("Generating leex file: ~s~n", [File]),
            {ok, Generated} = leex:file(File),
            up_to_date = make:files([Generated], ?MAKE_OPTIONS);
        ".yrl" -> 
            io:format("Generating yecc file: ~s~n", [File]),
            {ok, Generated} = yecc:file(File),
            up_to_date = make:files([Generated], ?MAKE_OPTIONS);
        ".app" ->
            EbinDir = merlkefile_api:ebin_dir(),
            io:format("Copying ~s to ~s~n", [File, EbinDir]),
            {ok, _ByteCount} = file:copy(
                File, 
                filename:join(EbinDir, filename:basename(File))
            );
        _      -> 
            nope
    end;

generate(_) ->
    nope.

edoc() ->
    io:format("Generating edocs from ~p to ~p with options: ~p~n", 
              [merlkefile_api:src_dir(), 
               merlkefile_api:edoc_dir(),
               ?EDOC_OPTIONS]),
    
    merlke_files:prepare_overview(),
    
    edoc:files([lists:concat([merlkefile_api:src_dir(), "/", Module, ".erl"]) 
                    || Module <- merlkefile_api:modules()],
               ?EDOC_OPTIONS).

dialyzer() ->
    io:format("dialyzer~n").

test() ->
    case eunit:test([list_to_atom(M) 
                    || M <- merlkefile_api:modules()], [verbose]) of
        ok -> ok;
        _ -> halt(1)
    end.

start() ->
    EbinDir = merlkefile_api:ebin_dir(),
    {ok, NewFiles} = file:list_dir(EbinDir),
    lists:map(
        fun(F) ->
            case filename:extension(F) of
                ".app" ->
                    case file:consult(F) of
                        {ok, [{application, AppName, _Config}]} ->
                            spawn_link(fun() -> 
                                    io:format("Starting ~s~n", [AppName]),
                                    application:start(AppName)
                                  end);
                        _ ->
                            io:format("Error: ~s is invalid~n", [F]),
                            invalid_app_file
                    end;
                _ ->
                    not_app_file
            end
        end,
        [filename:join(EbinDir, NF) || NF <- NewFiles]                    
    ),
    
    wait(),

    io:format("Done.~n").
    
wait() ->
    receive 
        Result ->
            io:format("~n**** Received: ~p~n", [Result])
    end,
    wait().


dist() ->
    io:format("dist~n").

ebin_dir() -> make_dir(merlkefile_api:ebin_dir()).
edoc_dir() -> make_dir(merlkefile_api:edoc_dir()).

make_dir(Dir) ->
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        Error -> Error
    end.


    
