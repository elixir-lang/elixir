module Elixir::Code

# # Usage
#
# Elixir is still in development but ready to try out! First, you need to
# clone this repository to your machine, compile and test it:
#
#     $ git clone https://github.com/josevalim/elixir.git
#     $ cd elixir
#     $ make test
#
#     $ bin/elixir -v
#     Elixir 0.3.1.dev
#
# If tests fail, it is likely you have an outdated Erlang version. You can
# check your Erlang version by calling `erl` in the command line. You will see # some information as follow:
#
#     Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]
#
# Elixir requires Erlang R14B03 or later. If you have the correct version and
# tests still fail, feel free to open an issue in the issues tracker on
# Github. If all tests pass, you are ready to go.

def version, do: "0.4.0.dev"