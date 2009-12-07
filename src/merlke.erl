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

-define(MAKE_OPTIONS, [{outdir, merlkefile:ebin_dir()}]).

modules() ->
    [re:replace(M, "\\.erl", "", [{return, list}]) || M <- merlkefile:modules()].

clean() -> 
    lists:foreach(fun(Module) -> 
                    File = lists:concat([merlkefile:ebin_dir(), "/", Module, ".beam"]),
                    io:format("Removing ~s~n", [File]),
                    file:delete(File)
                  end,
                  modules()).

compile() ->
    ToCompile = lists:map(fun(Module) -> 
                            lists:concat([merlkefile:src_dir(), "/", Module, ".erl"])
                          end,
                          modules()),

    make:files(ToCompile, ?MAKE_OPTIONS).

edoc() ->
    io:format("edoc~n").

dialyzer() ->
    io:format("dialyzer~n").

app() ->
    io:format("app~n").

leex() ->
    ToCompile = lists:map(fun(F) -> 
                            leex:file(F),
                            re:replace(F, "\\.xrl", "", [{return, list}])
                          end,
                          merlkefile:leex()),

    make:files(ToCompile, ?MAKE_OPTIONS).

yecc() ->
    ToCompile = lists:map(fun(F) -> 
                            yecc:file(F),
                            re:replace(F, "\\.yrl", "", [{return, list}])
                          end,
                          merlkefile:yecc()),

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

