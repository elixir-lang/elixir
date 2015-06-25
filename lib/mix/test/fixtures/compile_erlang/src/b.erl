-module(b).
-compile(export_all).

-callback c() -> term().
-record(br, {cell=undefined}).

b() -> #br{cell=specified}.
