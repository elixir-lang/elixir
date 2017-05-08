-module(c).
-export([c/0]).

-include("r.hrl").
-behaviour(b).

c() -> #r{cell=specified}.
