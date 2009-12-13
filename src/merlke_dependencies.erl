-module(merlke_dependencies).
-export([dependencies/1]).

dependencies(Target) ->
    case Target of
        "start"    -> ["test"];
        "test"     -> ["compile"];
        "compile"  -> ["ebin_dir", "generate"];
        "generate" -> ["ebin_dir"];
        "dist"     -> ["test", "edoc"];
        "edoc"     -> ["edoc_dir"];
        _          -> []
    end.

