-module(merlke_runner).
-export([execute/0, execute/1]).

execute() ->
    execute(["compile"]).
    
execute(Targets) ->
    io:format("Executing ~p~n~n", [Targets]),
    execute(hd(Targets), tl(Targets), sets:new()).

execute(CurrentTarget, TargetsToExecute, ExecutedTargets) ->
    %io:format("CurrentTarget: ~p, TargetsToExecute: ~p, ExecutedTargets: ~p ~n", [CurrentTarget, TargetsToExecute, sets:to_list(ExecutedTargets)]),   
    [Next | Rest] = remove_duplicates(lists:append([merlke:dependencies(CurrentTarget), [CurrentTarget], TargetsToExecute]), ExecutedTargets),
    case Next of 
        CurrentTarget ->
            ExecutedTargets2 = execute(CurrentTarget, ExecutedTargets),
    
            case Rest of
                [] -> ExecutedTargets2;
                _  -> execute(hd(Rest), tl(Rest), ExecutedTargets2)
            end;
        _ -> 
            execute(Next, Rest, ExecutedTargets)
    end.

execute(Target, ExecutedAlready) ->
    % Execute a single target if it hasn't been executed already
    
    case sets:is_element(Target, ExecutedAlready) of
        true -> 
            % Already executed, don't do anything
            ExecutedAlready;
        false -> 
            Command = string:join(["merlke:", Target, "()."], ""),
            io:format("==== ~s~n", [Command]),
            meta:eval(Command, []),
            io:format("~n"),

            sets:add_element(Target, ExecutedAlready)
    end.
    
remove_duplicates(Elements, Exclude) ->
    remove_duplicates(hd(Elements), tl(Elements), Exclude, []).

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


