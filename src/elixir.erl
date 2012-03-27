-module(elixir).
-behaviour(application).
-export([start/0, start_app/0, scope_for_eval/0,
  eval/2, eval/3, eval/4, eval/5,
  eval_quoted/4, eval_forms/3]).
-include("elixir.hrl").

% OTP APPLICATION API

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  (catch code:add_pathz(code:lib_dir(?MODULE, exbin))),
  elixir_sup:start_link([]).

stop(_S) ->
  ok.

config_change(_Changed, _New, _Remove) ->
  ok.

%% ELIXIR ENTRY POINTS

% Start the Elixir app. This is the proper way to boot Elixir from
% inside an Erlang process.

start_app() ->
  case lists:keyfind(?MODULE, 1, application:loaded_applications()) of
    false -> application:start(?MODULE);
    _ -> ok
  end.

% Boot and process given options. Invoked by Elixir's script.

start() ->
  start_app(),
  '__MAIN__.Elixir.CLI':process_argv(init:get_plain_arguments()).

%% EVAL HOOKS

scope_for_eval() -> #elixir_scope{}.

%% String evaluation

eval(String, Binding) -> eval(String, Binding, "nofile").
eval(String, Binding, Filename) -> eval(String, Binding, Filename, 1).
eval(String, Binding, Filename, Line) -> eval(String, Binding, Filename, Line, #elixir_scope{}).
eval(String, Binding, Filename, Line, Scope) when is_list(Filename) ->
  Forms = elixir_translator:forms(String, Line, Filename),
  eval_forms(Forms, Binding, Scope#elixir_scope{filename=Filename}).

%% Quoted evaluation

eval_quoted(Tree, Binding, Line, Filename) when is_list(Filename) ->
  eval_quoted(Tree, Binding, Line, #elixir_scope{filename=Filename});

eval_quoted(Tree, Binding, Line, #elixir_scope{} = S) ->
  eval_forms(elixir_quote:linify(Line, Tree), Binding, S).

%% Handle forms evaluation internally, it is an
%% internal API not meant for external usage.

eval_forms(Tree, Binding, RawScope) ->
  Scope = RawScope#elixir_scope{vars=binding_dict(Binding)},
  { ParseTree, NewScope } = elixir_translator:translate(Tree, Scope),
  case ParseTree of
    [] -> { nil, Binding, NewScope };
    _  ->
      {value, Value, NewBinding} = erl_eval:exprs(ParseTree, normalize_binding(Binding)),
      {Value, final_binding(NewBinding, NewScope#elixir_scope.vars), NewScope }
  end.

%% INTERNAL HELPERS

binding_dict(List) -> binding_dict(List, dict:new()).
binding_dict([{H,_}|T], Dict) -> binding_dict(T, dict:store(H, H, Dict));
binding_dict([], Dict) -> Dict.

final_binding(Binding, Vars) -> final_binding(Binding, [], Binding, Vars).
final_binding([{Var,_}|T], Acc, Binding, Vars) ->
  case atom_to_list(Var) of
    "_EX" ++ _ -> final_binding(T, Acc, Binding, Vars);
    _ ->
      RealName = dict:fetch(Var, Vars),
      RealValue = proplists:get_value(RealName, Binding, nil),
      final_binding(T, [{Var, RealValue}|Acc], Binding, Vars)
  end;

final_binding([], Acc, _Binding, _Vars) -> lists:reverse(Acc).

normalize_binding(Binding) ->
  Orddict = orddict:from_list(Binding),
  case orddict:find('_EXMODULE', Orddict) of
    { ok, _ } -> Orddict;
    _ -> orddict:store('_EXMODULE', nil, Orddict)
  end.