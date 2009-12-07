-module(merlke).
-export([
    clean/0,
    compile/0,
    edoc/0,
    dialyzer/0,
    app/0,
    leex/0,
    yecc/0,
    test/0,
    start/0,
    dist/0,
    dependencies/1
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
    ToCompile = lists:map(fun(Module) -> 
                            lists:concat([merlkefile_api:src_dir(), "/", Module, ".erl"])
                          end,
                          merlkefile_api:modules()),

    make:files(ToCompile, ?MAKE_OPTIONS).

edoc() ->
    io:format("Generating edocs from ~p to ~p~n", [merlkefile_api:src_dir(), 
                                                   merlkefile_api:edoc_dir()]),
    edoc:files([lists:concat([merlkefile_api:src_dir(), "/", Module, ".erl"]) 
                    || Module <- merlkefile_api:modules()],
               ?EDOC_OPTIONS).

dialyzer() ->
    io:format("dialyzer~n").

app() ->
    io:format("Copying ~s to ~s~n", [merlkefile_api:app(), merlkefile_api:ebin_dir()]),
    file:copy(merlkefile_api:app(), merlkefile_api:ebin_dir()).

leex() ->
    ToCompile = lists:map(fun(F) -> 
                            leex:file(F),
                            re:replace(F, "\\.xrl", "", [{return, list}])
                          end,
                          merlkefile_api:leex()),

    make:files(ToCompile, ?MAKE_OPTIONS).

yecc() ->
    ToCompile = lists:map(fun(F) -> 
                            yecc:file(F),
                            re:replace(F, "\\.yrl", "", [{return, list}])
                          end,
                          merlkefile_api:yecc()),

    make:files(ToCompile, ?MAKE_OPTIONS).


test() ->
    io:format("test~n").

start() ->
    io:format("start~n").

dist() ->
    io:format("dist~n").

dependencies(Target) ->
    case Target of
        "start" -> ["test"];
        "test"  -> ["app"];
        "app"   -> ["compile", "leex", "yecc"];
        "dist"  -> ["test", "edoc"];
        _       -> []
    end.

