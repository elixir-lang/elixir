-module(b).
-export([z/0]).

-callback b() -> term().
-record(br, {cell=undefined}).

z() -> #br{cell=specified}.
