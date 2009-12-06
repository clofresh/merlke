-module(merlkefile).
-export([src_dir/0]).

src_dir() -> "src".
edoc_dir() -> "doc".
modules() -> ["esyslog"].
leex() -> ["parser/esyslog_config_lexer.xrl"].
yecc() -> ["parser/esyslog_config_lexer.yrl"].
app() -> "etc/esyslog.app".

