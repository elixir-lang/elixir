-module(c).
-export([b/0]).

-include("r.hrl").
-behaviour(b).

b() -> #r{cell=specified}.
