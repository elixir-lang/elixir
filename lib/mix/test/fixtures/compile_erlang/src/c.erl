-module(c).
-export([b/0]).
-compile([{nowarn_unused_function, []}]).

-include("r.hrl").
-behaviour(b).

b() -> #r{cell=specified}.
