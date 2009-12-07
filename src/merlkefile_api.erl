-module(merlkefile_api).
-export([src_dir/0, ebin_dir/0, modules/0, leex/0, yecc/0, app/0]).

-define(BUILDFILE, merlkefile).

run_safely(F) ->
    code:purge(?BUILDFILE),
    {module, ?BUILDFILE} = code:load_file(?BUILDFILE),
    case erlang:function_exported(?BUILDFILE, F, 0) of
        true ->
            apply(?BUILDFILE, F, []);
        false ->
            io:format("Error: ~p.erl does not export ~p/0~n", [?BUILDFILE , F]),
            halt(1)
    end.

src_dir() -> run_safely(src_dir).
ebin_dir() -> run_safely(ebin_dir).
modules() ->
    [re:replace(M, "\\.erl", "", [{return, list}]) || M <- run_safely(modules)].
leex() -> run_safely(leex).
yecc() -> run_safely(yecc).
app() -> run_safely(app).
