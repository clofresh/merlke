-module(merlke_dependencies).
-export([dependencies/1]).

dependencies(Target) ->
    case Target of
        "start"   -> ["test"];
        "test"    -> ["compile"];
        "compile" -> ["generate"];
        "dist"    -> ["test", "edoc"];
        _         -> []
    end.

