-module(b).
-compile(export_all).

-record(br, { cell=undefined }).

b() -> #br{ cell=specified }.