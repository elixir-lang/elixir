-module(elixir).
-behaviour(application).
-export([main/1, start_cli/0, start_app/0,
  scope_for_eval/1, eval/2, eval/3, eval/4,
  eval_quoted/2, eval_quoted/3, eval_quoted/4,
  eval_forms/3]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).


% OTP APPLICATION API

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  %% Set the shell to unicode so printing inside files work
  io:setopts(standard_io, [{encoding,unicode}]),
  io:setopts(standard_error, [{encoding,unicode}]),
  elixir_sup:start_link([]).

stop(_S) ->
  ok.

config_change(_Changed, _New, _Remove) ->
  ok.

%% escript entry point

main(Args) ->
  start_app(),
  'Elixir.Kernel.CLI':process_argv(Args).

%% ELIXIR ENTRY POINTS

% Start the Elixir app. This is the proper way to boot Elixir from
% inside an Erlang process.

start_app() ->
  case lists:keyfind(?MODULE, 1, application:loaded_applications()) of
    false -> application:start(?MODULE);
    _ -> ok
  end.

% Boot and process given options. Invoked by Elixir's script.

start_cli() ->
  start_app(),
  'Elixir.Kernel.CLI':process_argv(init:get_plain_arguments()).

%% EVAL HOOKS

scope_for_eval(Opts) ->
  File = case lists:keyfind(file, 1, Opts) of
    { file, F } -> to_binary(F);
    false -> <<"nofile">>
  end,

  Local = case lists:keyfind(delegate_locals_to, 1, Opts) of
    { delegate_locals_to, L } -> L;
    false -> nil
  end,

  #elixir_scope{file=File,local=Local}.

%% String evaluation

eval(String, Binding) -> eval(String, Binding, []).

eval(String, Binding, Opts) ->
  case lists:keyfind(line, 1, Opts) of
    false -> Line = 1;
    { line, Line } -> []
  end,
  eval(String, Binding, Line, scope_for_eval(Opts)).

eval(String, Binding, Line, #elixir_scope{file=File} = S) when
    is_list(String), is_list(Binding), is_integer(Line), is_binary(File) ->
  Forms = elixir_translator:'forms!'(String, Line, File, []),
  eval_forms(Forms, Binding, S).

%% Quoted evaluation

eval_quoted(Tree, Binding) -> eval_quoted(Tree, Binding, []).

eval_quoted(Tree, Binding, Opts) ->
  case lists:keyfind(line, 1, Opts) of
    { line, Line } -> [];
    false -> Line = 1
  end,
  eval_quoted(Tree, Binding, Line, scope_for_eval(Opts)).

eval_quoted(Tree, Binding, Line, #elixir_scope{} = S) ->
  eval_forms(elixir_quote:linify(Line, Tree), Binding, S).

%% Handle forms evaluation internally, it is an
%% internal API not meant for external usage.

eval_forms(Tree, Binding, RawScope) ->
  Scope = RawScope#elixir_scope{
    vars=binding_dict(Binding),
    temp_vars=[],
    quote_vars=[],
    clause_vars=[],
    counter=0
  },
  { ParseTree, NewScope } = elixir_translator:translate(Tree, Scope),
  case ParseTree of
    [] -> { nil, Binding, NewScope };
    _  ->
      {value, Value, NewBinding} = erl_eval:exprs(ParseTree, normalize_binding(Binding)),
      {Value, final_binding(NewBinding, NewScope#elixir_scope.vars), NewScope }
  end.

%% INTERNAL HELPERS

to_binary(Bin)  when is_binary(Bin) -> Bin;
to_binary(List) when is_list(List) -> list_to_binary(List).

binding_dict(List) -> binding_dict(List, orddict:new()).
binding_dict([{H,_}|T], Dict) -> binding_dict(T, orddict:store(H, H, Dict));
binding_dict([], Dict) -> Dict.

final_binding(Binding, Vars) -> final_binding(Binding, [], Binding, Vars).
final_binding([{Var,_}|T], Acc, Binding, Vars) ->
  case atom_to_list(Var) of
    "_@" ++ _ -> final_binding(T, Acc, Binding, Vars);
    _ ->
      RealName = orddict:fetch(Var, Vars),
      RealValue = proplists:get_value(RealName, Binding, nil),
      final_binding(T, [{Var, RealValue}|Acc], Binding, Vars)
  end;

final_binding([], Acc, _Binding, _Vars) -> lists:reverse(Acc).

normalize_binding(Binding) ->
  Keyword = orddict:from_list(Binding),
  case orddict:find('_@MODULE', Keyword) of
    { ok, _ } -> Keyword;
    _ -> orddict:store('_@MODULE', nil, Keyword)
  end.
