-module(escript_test).

-export([start/0, main/1]).


start() ->
    ok = application:start(escript_test).

main(_Args) ->
    {ok, Val} = application:get_env(escript_test, erl_val),
    io:put_chars(Val).
