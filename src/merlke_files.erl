-module(merlke_files).
-export([traversal/2, traversal/3, prepare_overview/0]).

-include("include/merlke.hrl").
-include_lib("kernel/include/file.hrl").

prepare_overview() ->
    TargetOverviewFile = proplists:get_value(overview, ?EDOC_OPTIONS),
    case get_file_info("overview.edoc") of 
        {regular, F} -> file:copy(F, TargetOverviewFile);
        _ -> 
            case get_file_info(string:join([merlkefile_api:src_dir(), "overview.edoc"], "/")) of
                {regular, F} -> file:copy(F, TargetOverviewFile);
                _ -> 
                    MarkdownFilesSet = sets:from_list(
                                         ["README." ++ Ext 
                                            || Ext <- ["markdown",
                                                       "md",
                                                       "mdown",
                                                       "mkd",
                                                       "mkdn"]]),
                    {ok, RootFiles} = file:list_dir("."),
                    RootFilesSet = sets:from_list(RootFiles),
                    case sets:to_list(sets:union(MarkdownFilesSet, RootFilesSet)) of
                        [] -> got_nothing;
                        [F|_] -> 
                            case os:find_executable(?MARKDOWN_SCRIPT) of
                                false ->
                                    io:format(
                                        "Warning: Could not translate markdown file ~s to ~s (~s not in PATH)~n",
                                        [F, TargetOverviewFile, ?MARKDOWN_SCRIPT]
                                    );
                                MarkdownFullPath -> 
                                    Html = os:cmd(string:join([MarkdownFullPath, F], " ")),
                                    file:make_dir(merlkefile_api:edoc_dir()),
                                    {ok, IoDevice} = file:open(TargetOverviewFile, write),
                                    io:put_chars(IoDevice, "@doc "),
                                    io:put_chars(IoDevice, Html),
                                    ok = file:close(IoDevice)
                            end
                    end
            end
    end.

traversal(Root, FileFun) ->
    traversal(get_file_info(Root), [], FileFun, fun(_) -> allow end).

traversal(Root, FileFun, FilterFun) ->
    traversal(get_file_info(Root), [], FileFun, FilterFun).

traversal(FT = {regular, _File}, [], FileFun, FilterFun) ->
    case FilterFun(FT) of
        allow -> FileFun(FT);
        _     -> skipped
    end;

traversal(FT = {directory, Dir}, [], FileFun, FilterFun) ->
    case FilterFun(FT) of
        allow ->
            FileFun(FT),
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

traversal(FT = {regular, _File}, [Next | Rest], FileFun, FilterFun) ->
    case FilterFun(FT) of
        allow -> FileFun(FT);
        _     -> skipped
    end,
    traversal(get_file_info(Next), Rest, FileFun, FilterFun);
    
traversal(FT = {directory, Dir}, [Next | Rest], FileFun, FilterFun) ->
    case FilterFun(FT) of
        allow ->
            FileFun(FT),
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
                    {error, Reason}
            end
    end.
    