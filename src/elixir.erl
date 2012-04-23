-module(elixir).
-behaviour(application).
-export([start_cli/0, start_app/0,
  scope_for_eval/1, eval/2, eval/3, eval/4,
  eval_quoted/2, eval_quoted/3, eval_quoted/4,
  eval_forms/3]).
-include("elixir.hrl").

% OTP APPLICATION API

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  (catch code:add_pathz(code:lib_dir(?MODULE, exbin))),
  %% Set the shell to unicode so printing inside files work
  io:setopts([{encoding,unicode}]),
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

start_cli() ->
  start_app(),
  '__MAIN__.Elixir.CLI':process_argv(init:get_plain_arguments()).

%% EVAL HOOKS

scope_for_eval(Opts) ->
  Filename = case orddict:find(file, Opts) of
    { ok, F } -> to_char_list(F);
    error -> "nofile"
  end,

  Local = case orddict:find(delegate_locals_to, Opts) of
    { ok, L } -> L;
    error -> []
  end,

  #elixir_scope{filename=Filename,local=Local}.

%% String evaluation

eval(String, Binding) -> eval(String, Binding, []).

eval(String, Binding, Opts) ->
  case orddict:find(line, Opts) of
    { ok, Line } -> [];
    error -> Line = 1
  end,
  eval(String, Binding, Line, scope_for_eval(Opts)).

eval(String, Binding, Line, #elixir_scope{filename=Filename} = S) ->
  Forms = elixir_translator:forms(String, Line, Filename),
  eval_forms(Forms, Binding, S).

%% Quoted evaluation

eval_quoted(Tree, Binding) -> eval_quoted(Tree, Binding, []).

eval_quoted(Tree, Binding, Opts) ->
  case orddict:find(line, Opts) of
    { ok, Line } -> [];
    error -> Line = 1
  end,
  eval_quoted(Tree, Binding, Line, scope_for_eval(Opts)).

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

to_char_list(Bin)  when is_binary(Bin) -> binary_to_list(Bin);
to_char_list(List) when is_list(List) -> List.

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
  Keyword = orddict:from_list(Binding),
  case orddict:find('_EXMODULE', Keyword) of
    { ok, _ } -> Keyword;
    _ -> orddict:store('_EXMODULE', nil, Keyword)
  end.