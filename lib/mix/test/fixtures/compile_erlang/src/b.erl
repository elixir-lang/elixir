-module(b).
-export([b/0]).

-callback c() -> term().
-record(br, {cell=undefined}).

b() -> #br{cell=specified}.
