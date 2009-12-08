-module(merlke_dependencies).
-export([dependencies/1]).

dependencies(Target) ->
    case Target of
        "start"   -> ["test"];
        "test"    -> ["app"];
        "compile" -> ["generate"];
        "app"     -> ["compile"];
        "dist"    -> ["test", "edoc"];
        _         -> []
    end.

