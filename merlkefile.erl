-module(merlkefile).
-export([src_dir/0, ebin_dir/0, modules/0]).

src_dir() -> "src".
ebin_dir() -> "ebin".
edoc_dir() -> "doc".
modules() -> ["merlke", "merlke_runner", "meta"].
leex() -> ["parser/esyslog_config_lexer.xrl"].
yecc() -> ["parser/esyslog_config_lexer.yrl"].
app() -> "etc/esyslog.app".

