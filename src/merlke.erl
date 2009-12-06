-module(merlke).
-export([
    execute/1,
    compile/0,
    edoc/0,
    dialyzer/0,
    app/0,
    leex/0,
    yecc/0,
    test/0,
    start/0,
    dist/0
]).

execute("") ->
    io:format("Must specify a task~n");
    
execute(Targets) ->
    io:format("Executing ~p~n~n", [Targets]),
    execute(hd(Targets), tl(Targets), sets:new()).

remove_duplicates(Elements) ->
    remove_duplicates(hd(Elements), tl(Elements), sets:new(), []).

remove_duplicates(Element, [], SeenAlready, Result) ->
    case sets:is_element(Element, SeenAlready) of
        true ->
            lists:reverse(Result);
        false ->
            lists:reverse([Element] ++ Result)
    end;

remove_duplicates(Element, [Next | Rest], SeenAlready, Result) ->
    case sets:is_element(Element, SeenAlready) of
        true ->
            remove_duplicates(Next, Rest, SeenAlready, Result);
        false ->
            remove_duplicates(Next, Rest, sets:add_element(Element, SeenAlready), [Element] ++ Result)
    end.

execute(Target, ExecutedAlready) ->
    % Execute a single target if it hasn't been executed already
    
    case sets:is_element(Target, ExecutedAlready) of
        true -> 
            % Already executed, don't do anything
            ExecutedAlready;
        false -> 
            Command = string:join([atom_to_list(?MODULE), ":", Target, "()."], ""),
            io:format("==== ~s~n", [Command]),
            meta:eval(Command, []),
            io:format("~n"),

            sets:add_element(Target, ExecutedAlready)
    end.
    
execute(CurrentTarget, [], ExecutedTargets) ->
    execute(CurrentTarget, ExecutedTargets);
    
execute(CurrentTarget, TargetsToExecute, ExecutedTargets) ->
    [Next | Rest] = remove_duplicates(lists:append(dependencies(CurrentTarget), TargetsToExecute)),
    ExecutedTargets2 = execute(CurrentTarget, ExecutedTargets),
    execute(Next, Rest, ExecutedTargets2).

compile() ->
    io:format("compile~n").

edoc() ->
    io:format("edoc~n").

dialyzer() ->
    io:format("dialyzer~n").

app() ->
    io:format("app~n").

leex() ->
    io:format("leex~n").

yecc() ->
    io:format("yecc~n").

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

