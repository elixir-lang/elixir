-module(c).
-compile(export_all).

-include("r.hrl").

c() -> #r{ cell=specified }.