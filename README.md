merlke - A native Erlang build tool
===================================

Merlke is a command line build tool that aims to automate common Erlang build tasks. It's a dependency build language in the vein of Make but is written in Erlang to provide nicer hooks into built-in Erlang modules.

Installation
------------
Run:

    make

Then put scripts/merlke in your PATH

Usage
-----
Assuming you follow the standard Erlang file conventions, you can just run:

    merlke task1 [task2 [task3 [...]]]

Standard directory structure
----------------------------
    doc/      -> generated edocs directory   (edoc_dir/0)
    ebin/     -> .beam directory             (ebin_dir/0)
    src/      -> module source directory     (src_dir/0)
    src/*.erl -> module source files         (modules/0)
    *.app     -> application files
    *.xrl     -> leex files
    *.yrl     -> yecc files 

To override a directory convention, implement the callback in parenthesis 
in a module called merlkefile.erl in your project's base directory.


Tasks
-----
* clean
* compile
* generate
* edoc
* dialyzer (not yet implemented)
* test
* start
* dist  (not yet implemented)

Task dependencies
-----------------
    start   -> test
    test    -> compile
    compile -> generate
    dist    -> test, edoc

Sample merlkefile.erl
-----------------
Merlke will work fine without a merlkefile, but if you want to override the standard files, you can do so by export certain callbacks:

    -module(merlkefile).
    -export([src_dir/0, ebin_dir/0, edoc_dir/0, modules/0]).

    src_dir() -> "src".
    ebin_dir() -> "ebin".
    edoc_dir() -> "doc".
    modules() -> 
        {ok, Filenames} = file:list_dir(src_dir()),
        Filenames.



