-module(merlke).
-export([
    clean/0,
    compile/0,
    generate/0,
    edoc/0,
    dialyzer/0,
    test/0,
    start/0,
    dist/0
]).

-include_lib("kernel/include/file.hrl").

-define(MAKE_OPTIONS, [{outdir, merlkefile_api:ebin_dir()}]).
-define(EDOC_OPTIONS, [{dir, merlkefile_api:edoc_dir()}]).

clean() -> 
    lists:foreach(fun(Module) -> 
                    File = lists:concat([merlkefile_api:ebin_dir(), "/", Module, ".beam"]),
                    io:format("Removing ~s~n", [File]),
                    file:delete(File)
                  end,
                  merlkefile_api:modules()).

compile() ->
    
    ToCompile = [lists:concat([merlkefile_api:src_dir(), "/", Module, ".erl"])
                    || Module <- merlkefile_api:modules()],

    make:files(ToCompile, ?MAKE_OPTIONS).

generate() ->
    generate({directory, "."}, []).

generate(File) ->
    case filename:extension(File) of
        ".xrl" -> 
            io:format("Generating leex file: ~s~n", [File]),
            leex:file(File);
        ".yrl" -> 
            io:format("Generating yecc file: ~s~n", [File]),
            yecc:file(File);
        ".app" ->
            EbinDir = merlkefile_api:ebin_dir(),
            io:format("Copying ~s to ~s~n", [File, EbinDir]),
            file:copy(File, EbinDir);
        _      -> 
            nope
    end.

generate({regular, File}, []) ->
    generate(File);

generate({directory, Dir}, []) ->
    EbinDir = filename:absname(merlkefile_api:ebin_dir()),
    case filename:absname(Dir) of
        EbinDir ->
            skip;
        _ ->
            case file:list_dir(Dir) of
                {ok, NewFiles} -> 
                    [Next | Rest] = [lists:concat([Dir, "/", F]) 
                                        || F <- NewFiles],
                    generate(get_file_info(Next), Rest);
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    {error, Reason}
            end
    end;

generate({_Other, _File}, []) ->
    nope;

generate({regular, File}, [Next | Rest]) ->
    generate(File),
    generate(get_file_info(Next), Rest);
    
generate({directory, Dir}, [Next | Rest]) ->
    EbinDir = filename:absname(merlkefile_api:ebin_dir()),
    case filename:absname(Dir) of
        EbinDir ->
            generate(get_file_info(Next), Rest);
        _ ->
            case file:list_dir(Dir) of
                {ok, NewFiles} -> 
                    generate(get_file_info(Next), 
                                lists:append(
                                    Rest, 
                                    [lists:concat([Dir, "/", F]) 
                                        || F <- NewFiles]
                                )
                            );
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    generate(get_file_info(Next), Rest)
            end
    end;

generate({_Other, _File}, [Next | Rest]) ->
    generate(get_file_info(Next), Rest).

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
    
edoc() ->
    io:format("Generating edocs from ~p to ~p~n", [merlkefile_api:src_dir(), 
                                                   merlkefile_api:edoc_dir()]),
    edoc:files([lists:concat([merlkefile_api:src_dir(), "/", Module, ".erl"]) 
                    || Module <- merlkefile_api:modules()],
               ?EDOC_OPTIONS).

dialyzer() ->
    io:format("dialyzer~n").

test() ->
    eunit:test([list_to_atom(M) 
                    || M <- merlkefile_api:modules()]).

start() ->
    EbinDir = merlkefile_api:ebin_dir(),
    {ok, NewFiles} = file:list_dir(EbinDir),
    lists:map(
        fun(F) ->
            case filename:extension(F) of
                ".app" ->
                    case file:consult(F) of
                        {ok, [{application, AppName, _Config}]} ->
                            spawn_link(fun() -> 
                                    io:format("Starting ~s~n", [AppName]),
                                    application:start(AppName)
                                  end);
                        _ ->
                            io:format("Error: ~s is invalid~n", [F]),
                            invalid_app_file
                    end;
                _ ->
                    not_app_file
            end
        end,
        [string:join([EbinDir, NF], "/") || NF <- NewFiles]                    
    ),

    receive 
        Result ->
            io:format("Result: ~p~n", [Result])
    end,

    io:format("Done.~n").

dist() ->
    io:format("dist~n").


