-module(c).
-compile(export_all).

-include("r.hrl").
-behaviour(b).

c() -> #r{cell=specified}.