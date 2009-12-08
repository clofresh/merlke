-module(merlke_files).
-export([traversal/4]).

-include_lib("kernel/include/file.hrl").

traversal(FT = {regular, File}, [], FileFun, FilterFun) ->
    case FilterFun(FT) of
        allow -> FileFun(File);
        _     -> skipped
    end;

traversal(FT = {directory, Dir}, [], FileFun, FilterFun) ->
    case FilterFun(FT) of
        allow ->
            case file:list_dir(Dir) of
                {ok, NewFiles} -> 
                    [Next | Rest] = [lists:concat([Dir, "/", F]) 
                                        || F <- NewFiles],
                    traversal(get_file_info(Next), Rest, FileFun, FilterFun);
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    {error, Reason}
            end;
        _ ->
            skipped
    end;

traversal({_Other, _File}, [], _FileFun, _FilterFun) ->
    nope;

traversal(FT = {regular, File}, [Next | Rest], FileFun, FilterFun) ->
    case FilterFun(FT) of
        allow -> FileFun(File);
        _     -> skipped
    end,
    traversal(get_file_info(Next), Rest, FileFun, FilterFun);
    
traversal(FT = {directory, Dir}, [Next | Rest], FileFun, FilterFun) ->
    case FilterFun(FT) of
        allow ->
            case file:list_dir(Dir) of
                {ok, NewFiles} -> 
                    traversal(get_file_info(Next), 
                                lists:append(
                                    Rest, 
                                    [lists:concat([Dir, "/", F]) 
                                        || F <- NewFiles]),
                              FileFun, FilterFun);
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    traversal(get_file_info(Next), Rest, FileFun, FilterFun)
            end;
        _ ->
            traversal(get_file_info(Next), Rest, FileFun, FilterFun)
    end;

traversal({_Other, _File}, [Next | Rest], FileFun, FilterFun) ->
    traversal(get_file_info(Next), Rest, FileFun, FilterFun).

get_file_info(File) ->
    case filename:basename(File) of
        "." -> 
            {directory, "."};
        "." ++ _Other ->
            {hidden, File};
        _ ->
            case file:read_file_info(File) of
                {ok, FileInfo}  -> 
                    {FileInfo#file_info.type, File};
                {error, Reason} -> 
                    io:format("Error: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.
    