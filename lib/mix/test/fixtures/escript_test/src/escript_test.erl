-module(escript_test).

-export([start/0, main/1]).


start() ->
    ok = application:start(escript_test).

main(Args) ->
    io:format("~p", [Args]).
