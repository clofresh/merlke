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
Create a file called merlkefile.erl in the base directory of your project and define the appropriate callbacks. Then from the command line, run:

    merlke task1 [task2 [task3 [...]]]

Merlkefile callbacks
--------------------
* src_dir/0
* ebin_dir/0
* edoc_dir/0
* modules/0
* leex/0
* yecc/0
* app_file/0

Tasks
-----

* compile
* edoc
* dialyzer
* app
* leex
* yecc
* test
* start
* dist

Task Dependencies
-----------------

* start -> test
* test -> app
* app -> compile, leex, yecc
* dist -> test, edoc

Sample merlkefile
-----------------

    -module(merlkefile).
    -export([src_dir/0, ebin_dir/0, edoc_dir/0, modules/0, leex/0, yecc/0, app_file/0]).

    src_dir() -> "src".
    ebin_dir() -> "ebin".
    edoc_dir() -> "doc".
    modules() -> 
        {ok, Filenames} = file:list_dir(src_dir()),
        Filenames.
    leex() -> ["parser/esyslog_config_lexer.xrl"].
    yecc() -> ["parser/esyslog_config_parser.yrl"].
    app_file() -> "etc/esyslog.app".



