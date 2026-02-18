-module(b).
-export([z/0]).
-compile({nowarn_unused_function, []}).

-callback b() -> term().
-record(br, {cell=undefined}).

z() -> #br{cell=specified}.
