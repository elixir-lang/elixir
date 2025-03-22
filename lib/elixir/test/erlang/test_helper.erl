%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

-module(test_helper).
-export([test/0, run_and_remove/2, throw_elixir/1, throw_erlang/1]).
-define(TESTS, [
  atom_test,
  control_test,
  function_test,
  string_test,
  tokenizer_test
]).

test() ->
  application:ensure_all_started(elixir),
  enable_coverage_report(),
  ExitCode = case eunit:test(?TESTS) of
    error -> 1;
    _Res  -> 0
  end,
  write_report_coverage(),
  erlang:halt(ExitCode).

% Execute a piece of code and purge given modules right after
run_and_remove(Fun, Modules) ->
  try
    Fun()
  after
    [code:purge(Module)  || Module <- Modules],
    [code:delete(Module) || Module <- Modules]
  end.

% Throws an error with the Erlang Abstract Form from the Elixir string
throw_elixir(String) ->
  Forms = elixir:'string_to_quoted!'(String, 1, 1, <<"nofile">>, []),
  {Expr, _, _, _} = elixir:quoted_to_erl(Forms, elixir:env_for_eval([])),
  erlang:error(io:format("~p~n", [Expr])).

% Throws an error with the Erlang Abstract Form from the Erlang string
throw_erlang(String) ->
  {ok, Tokens, _} = erl_scan:string(String),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  erlang:error(io:format("~p~n", [Form])).

enable_coverage_report() ->
  case os:getenv("COVER_FILE") of
    false -> ok;
    _File ->
      _ = cover:stop(),
      {ok, _Pid} = cover:start(),
      Ebin = filename:dirname(filename:absname(?FILE)) ++ "/../../ebin",
      % TODO: Check Result
      cover:compile_beam_directory(Ebin)
  end.

write_report_coverage() ->
  case os:getenv("COVER_FILE") of
    false -> ok;
    File -> cover:export(File)
  end.