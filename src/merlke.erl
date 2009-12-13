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

-include("include/merlke.hrl").

clean() -> 
    merlke_files:traversal(
        merlkefile_api:ebin_dir(), 
        fun
            ({regular, File}) ->
                DeleteFun = fun(F) ->
                    io:format("Removing ~s~n", [F]),
                    file:delete(F)
                end,
                case filename:extension(File) of
                    ".beam" -> DeleteFun(File);
                    ".app"  -> DeleteFun(File);
                    _       -> nope
                end;    
            (_) ->
                nope
        end).

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
            leex:file(File);
        ".yrl" -> 
            io:format("Generating yecc file: ~s~n", [File]),
            yecc:file(File);
        ".app" ->
            EbinDir = merlkefile_api:ebin_dir(),
            io:format("Copying ~s to ~s~n", [File, EbinDir]),
            {ok, _ByteCount} = file:copy(
                File, 
                string:join([EbinDir, filename:basename(File)], "/")
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


