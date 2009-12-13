-define(MAKE_OPTIONS, [{outdir, merlkefile_api:ebin_dir()}]).
-define(EDOC_OPTIONS, [{dir, merlkefile_api:edoc_dir()},
                       {overview, string:join([merlkefile_api:edoc_dir(), 
                                               "overview.edoc"], 
                                              "/")}]).
-define(MARKDOWN_SCRIPT, "markdown.py").
