-module(escripttest).

-export([start/0, main/1]).


start() ->
    ok = application:start(escripttest).

main(_Args) ->
    {ok, Val} = application:get_env(escripttest, erlval),
    io:put_chars(Val).
