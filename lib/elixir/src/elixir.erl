%% Main entry point for Elixir functions. All of those functions are
%% private to the Elixir compiler and reserved to be used by Elixir only.
-module(elixir).
-behaviour(application).
-export([main/1, start_cli/0,
  string_to_quoted/4, 'string_to_quoted!'/4,
  env_for_eval/1, env_for_eval/2, quoted_to_erl/2, quoted_to_erl/3,
  eval/2, eval/3, eval_forms/3, eval_forms/4, eval_quoted/3]).
-include("elixir.hrl").

%% Top level types
-export_type([char_list/0, as_boolean/1]).
-type char_list() :: string().
-type as_boolean(T) :: T.

%% OTP Application API

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  %% Set the shell to unicode so printing inside scripts work
  %% Those can take a while, so let's do it in a new process
  spawn(fun() ->
    io:setopts(standard_io, [binary,{encoding,utf8}]),
    io:setopts(standard_error, [{unicode,true}])
  end),
  elixir_sup:start_link().

stop(_S) ->
  ok.

config_change(_Changed, _New, _Remove) ->
  ok.

%% escript entry point

main(Args) ->
  application:start(?MODULE),
  'Elixir.Kernel.CLI':main(Args).

%% Boot and process given options. Invoked by Elixir's script.

start_cli() ->
  application:start(?MODULE),
  'Elixir.Kernel.CLI':main(init:get_plain_arguments()).

%% EVAL HOOKS

env_for_eval(Opts) ->
  env_for_eval(#elixir_env{
    local = nil,
    requires = elixir_dispatch:default_requires(),
    functions = elixir_dispatch:default_functions(),
    macros = elixir_dispatch:default_macros()
  }, Opts).

env_for_eval(Env, Opts) ->
  Line = case lists:keyfind(line, 1, Opts) of
    { line, RawLine } when is_integer(RawLine) -> RawLine;
    false -> Env#elixir_env.line
  end,

  File = case lists:keyfind(file, 1, Opts) of
    { file, RawFile } when is_binary(RawFile) -> RawFile;
    false -> Env#elixir_env.file
  end,

  Local = case lists:keyfind(delegate_locals_to, 1, Opts) of
    { delegate_locals_to, LocalOpt } -> LocalOpt;
    false -> Env#elixir_env.local
  end,

  Aliases = case lists:keyfind(aliases, 1, Opts) of
    { aliases, AliasesOpt } -> AliasesOpt;
    false -> Env#elixir_env.aliases
  end,

  Requires = case lists:keyfind(requires, 1, Opts) of
    { requires, List } -> ordsets:from_list(List);
    false -> Env#elixir_env.requires
  end,

  Functions = case lists:keyfind(functions, 1, Opts) of
    { functions, FunctionsOpt } -> FunctionsOpt;
    false -> Env#elixir_env.functions
  end,

  Macros = case lists:keyfind(macros, 1, Opts) of
    { macros, MacrosOpt } -> MacrosOpt;
    false -> Env#elixir_env.macros
  end,

  Module = case lists:keyfind(module, 1, Opts) of
    { module, ModuleOpt } when is_atom(ModuleOpt) -> ModuleOpt;
    false -> nil
  end,

  Env#elixir_env{
    file=File, local=Local, module=Module,
    macros=Macros, functions=Functions,
    requires=Requires, aliases=Aliases, line=Line
  }.

%% String evaluation

eval(String, Binding) ->
  eval(String, Binding, []).

eval(String, Binding, Opts) when is_list(Opts) ->
  eval(String, Binding, env_for_eval(Opts));
eval(String, Binding, #elixir_env{line=Line,file=File} = E) when
    is_list(String), is_list(Binding), is_integer(Line), is_binary(File) ->
  Forms = 'string_to_quoted!'(String, Line, File, []),
  eval_forms(Forms, Binding, E).

%% Quoted evaluation

eval_quoted(Tree, Binding, Opts) when is_list(Opts) ->
  eval_quoted(Tree, Binding, env_for_eval(Opts));
eval_quoted(Tree, Binding, #elixir_env{line=Line} = E) ->
  eval_forms(elixir_quote:linify(Line, Tree), Binding, E).

%% Handle forms evaluation. The main difference to
%% to eval_quoted is that it does not linefy the given
%% args.

eval_forms(Tree, Binding, #elixir_env{} = E) ->
  eval_forms(Tree, Binding, E, elixir_env:env_to_scope(E));
eval_forms(Tree, Binding, Opts) when is_list(Opts) ->
  eval_forms(Tree, Binding, env_for_eval(Opts)).
eval_forms(Tree, Binding, Env, Scope) ->
  { ParsedBinding, ParsedScope } = elixir_scope:load_binding(Binding, Scope),
  ParsedEnv = Env#elixir_env{vars=[K || {K,_} <- ParsedScope#elixir_scope.vars]},
  { Erl, NewEnv, NewScope } = quoted_to_erl(Tree, ParsedEnv, ParsedScope),

  case Erl of
    { atom, _, Atom } ->
      { Atom, Binding, NewEnv, NewScope };
    _  ->
      { value, Value, NewBinding } = erl_eval:expr(Erl, ParsedBinding),
      { Value, elixir_scope:dump_binding(NewBinding, NewScope), NewEnv, NewScope }
  end.

%% Converts a quoted expression to erlang abstract format

quoted_to_erl(Quoted, Env) ->
  quoted_to_erl(Quoted, Env, elixir_env:env_to_scope(Env)).

quoted_to_erl(Quoted, Env, Scope) ->
  { Expanded, NewEnv } = elixir_exp:expand(Quoted, Env),
  { Erl, NewScope }    = elixir_translator:translate(Expanded, Scope),
  { Erl, NewEnv, NewScope }.

%% Converts a given string (char list) into quote expression

string_to_quoted(String, StartLine, File, Opts) when is_integer(StartLine), is_binary(File) ->
  case elixir_tokenizer:tokenize(String, StartLine, [{ file, File }|Opts]) of
    { ok, _Line, Tokens } ->
      try elixir_parser:parse(Tokens) of
        { ok, Forms } -> { ok, Forms };
        { error, { Line, _, [Error, Token] } } -> { error, { Line, to_binary(Error), to_binary(Token) } }
      catch
        { error, { Line, _, [Error, Token] } } -> { error, { Line, to_binary(Error), to_binary(Token) } }
      end;
    { error, { Line, Error, Token }, _Rest, _SoFar } -> { error, { Line, to_binary(Error), to_binary(Token) } }
  end.

'string_to_quoted!'(String, StartLine, File, Opts) ->
  case string_to_quoted(String, StartLine, File, Opts) of
    { ok, Forms } ->
      Forms;
    { error, { Line, Error, Token } } ->
      elixir_errors:parse_error(Line, File, Error, Token)
  end.

to_binary(List) when is_list(List) -> elixir_utils:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).
