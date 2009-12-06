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

execute(CurrentTarget, TargetsToExecute, ExecutedTargets) ->
    ExecutedTargets2 = case dependencies(CurrentTarget) of
        [] -> 
            ExecutedTargets;
        Dependencies ->
            % Execute dependencies first
            sets:union(ExecutedTargets, execute(hd(Dependencies), tl(Dependencies), ExecutedTargets))
    end,
    
    ExecutedTargets3 = case sets:is_element(CurrentTarget, ExecutedTargets2) of
        true -> 
            % Already executed, don't do anything
            ExecutedTargets2;
        false -> 
            Command = string:join([atom_to_list(?MODULE), ":", CurrentTarget, "()."], ""),
            io:format("==== ~s~n", [Command]),
            meta:eval(Command, []),
            io:format("~n"),

            sets:add_element(CurrentTarget, ExecutedTargets2)
    end,
        
    case TargetsToExecute of 
        [] -> ExecutedTargets3;
        _  -> execute(hd(TargetsToExecute), tl(TargetsToExecute), ExecutedTargets3)
    end.



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

