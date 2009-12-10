-module(merlke).
-export([
    clean/0,
    compile/0,
    generate/0,
    edoc/0,
    dialyzer/0,
    test/0,
    start/0,
    dist/0
]).

-define(MAKE_OPTIONS, [{outdir, merlkefile_api:ebin_dir()}]).
-define(EDOC_OPTIONS, [{dir, merlkefile_api:edoc_dir()}]).

clean() -> 
    lists:foreach(fun(Module) -> 
                    File = lists:concat([merlkefile_api:ebin_dir(), "/", Module, ".beam"]),
                    io:format("Removing ~s~n", [File]),
                    file:delete(File)
                  end,
                  merlkefile_api:modules()).

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
        {directory, "."}, 
        [], 
        fun generate/1,
        fun(FT) ->
            case FT of
                {regular, _} -> 
                    allow;
                {directory, Dir} ->
                    case filename:absname(Dir) of
                        EbinDir -> skip;
                        _ -> allow
                    end
            end                    
        end
    ).

generate(File) ->
    case filename:extension(File) of
        ".xrl" -> 
            io:format("Generating leex file: ~s~n", [File]),
            leex:file(File);
        ".yrl" -> 
            io:format("Generating yecc file: ~s~n", [File]),
            yecc:file(File);
        ".app" ->
            EbinDir = merlkefile_api:ebin_dir(),
            io:format("Copying ~s to ~s~n", [File, EbinDir]),
            file:copy(File, EbinDir);
        _      -> 
            nope
    end.

edoc() ->
    io:format("Generating edocs from ~p to ~p~n", [merlkefile_api:src_dir(), 
                                                   merlkefile_api:edoc_dir()]),
    edoc:files([lists:concat([merlkefile_api:src_dir(), "/", Module, ".erl"]) 
                    || Module <- merlkefile_api:modules()],
               ?EDOC_OPTIONS).

dialyzer() ->
    io:format("dialyzer~n").

test() ->
    eunit:test([list_to_atom(M) 
                    || M <- merlkefile_api:modules()]).

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
        [string:join([EbinDir, NF], "/") || NF <- NewFiles]                    
    ),

    receive 
        Result ->
            io:format("Result: ~p~n", [Result])
    end,

    io:format("Done.~n").

dist() ->
    io:format("dist~n").


