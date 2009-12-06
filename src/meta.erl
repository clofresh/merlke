% Copied from http://nutrun.com/weblog/erlang-eval-and-dynamic-dispatch/

-module (meta).
-export ([eval/2]).

eval(Code, Args) ->
  {ok, Scanned, _} = erl_scan:string(Code),
  {ok, Parsed} = erl_parse:parse_exprs(Scanned),
  Bindings = lists:foldl(fun ({Key, Val}, BindingsAccumulator) ->
    erl_eval:add_binding(Key, Val, BindingsAccumulator)
  end, erl_eval:new_bindings(), Args),
  {value, Result, _} = erl_eval:exprs(Parsed, Bindings),
  Result.
 