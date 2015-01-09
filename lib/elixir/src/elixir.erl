%% Main entry point for Elixir functions. All of those functions are
%% private to the Elixir compiler and reserved to be used by Elixir only.
-module(elixir).
-behaviour(application).
-export([start_cli/0,
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
  %% In case there is a shell, we can't really change its
  %% encoding, so we just set binary to true. Otherwise
  %% we must set the encoding as the user with no shell
  %% has encoding set to latin1.
  Opts =
    case init:get_argument(noshell) of
      {ok, _} -> [binary,{encoding,utf8}];
      error   -> [binary]
    end,

  ok = io:setopts(standard_io, Opts),
  %% Must use undocument {unicode, true} to set unicode on standard_error, more
  %% info: http://erlang.org/pipermail/erlang-bugs/2014-April/004310.html
  ok = io:setopts(standard_error, [{unicode,true}]),
  case file:native_name_encoding() of
    latin1 ->
      io:format(standard_error,
        "warning: the VM is running with native name encoding of latin1 which may cause "
        "Elixir to malfunction as it expects utf8. Please ensure your locale is set to UTF-8 "
        "(which can be verified by running \"locale\" in your shell)~n", []);
    _ ->
      ok
  end,

  elixir_sup:start_link().

stop(_S) ->
  ok.

config_change(_Changed, _New, _Remove) ->
  ok.

%% Boot and process given options. Invoked by Elixir's script.

start_cli() ->
  {ok, _} = application:ensure_all_started(?MODULE),

  %% We start the Logger so tools that depend on Elixir
  %% always have the Logger directly accessible. However
  %% Logger is not a dependency of the Elixir application,
  %% which means releases that want to use Logger must
  %% always list it as part of its applications.
  _ = case code:ensure_loaded('Elixir.Logger') of
    {module, _} -> application:start(logger);
    {error, _}  -> ok
  end,

  'Elixir.Kernel.CLI':main(init:get_plain_arguments()).

%% EVAL HOOKS

env_for_eval(Opts) ->
  env_for_eval((elixir_env:new())#{
    local := nil,
    requires := elixir_dispatch:default_requires(),
    functions := elixir_dispatch:default_functions(),
    macros := elixir_dispatch:default_macros()
 }, Opts).

env_for_eval(Env, Opts) ->
  Line = case lists:keyfind(line, 1, Opts) of
    {line, LineOpt} when is_integer(LineOpt) -> LineOpt;
    false -> ?m(Env, line)
  end,

  File = case lists:keyfind(file, 1, Opts) of
    {file, FileOpt} when is_binary(FileOpt) -> FileOpt;
    false -> ?m(Env, file)
  end,

  Local = case lists:keyfind(delegate_locals_to, 1, Opts) of
    {delegate_locals_to, LocalOpt} when is_atom(LocalOpt) -> LocalOpt;
    false -> ?m(Env, local)
  end,

  Aliases = case lists:keyfind(aliases, 1, Opts) of
    {aliases, AliasesOpt} when is_list(AliasesOpt) -> AliasesOpt;
    false -> ?m(Env, aliases)
  end,

  Requires = case lists:keyfind(requires, 1, Opts) of
    {requires, RequiresOpt} when is_list(RequiresOpt) -> ordsets:from_list(RequiresOpt);
    false -> ?m(Env, requires)
  end,

  Functions = case lists:keyfind(functions, 1, Opts) of
    {functions, FunctionsOpt} when is_list(FunctionsOpt) -> FunctionsOpt;
    false -> ?m(Env, functions)
  end,

  Macros = case lists:keyfind(macros, 1, Opts) of
    {macros, MacrosOpt} when is_list(MacrosOpt) -> MacrosOpt;
    false -> ?m(Env, macros)
  end,

  Module = case lists:keyfind(module, 1, Opts) of
    {module, ModuleOpt} when is_atom(ModuleOpt) -> ModuleOpt;
    false -> nil
  end,

  Env#{
    file := File, local := Local, module := Module,
    macros := Macros, functions := Functions,
    requires := Requires, aliases := Aliases, line := Line
  }.

%% String evaluation

eval(String, Binding) ->
  eval(String, Binding, []).

eval(String, Binding, Opts) when is_list(Opts) ->
  eval(String, Binding, env_for_eval(Opts));
eval(String, Binding, #{line := Line, file := File} = E) when
    is_list(String), is_list(Binding), is_integer(Line), is_binary(File) ->
  Forms = 'string_to_quoted!'(String, Line, File, []),
  eval_forms(Forms, Binding, E).

%% Quoted evaluation

eval_quoted(Tree, Binding, Opts) when is_list(Opts) ->
  eval_quoted(Tree, Binding, env_for_eval(Opts));
eval_quoted(Tree, Binding, #{line := Line} = E) ->
  eval_forms(elixir_quote:linify(Line, Tree), Binding, E).

%% Handle forms evaluation. The main difference to
%% to eval_quoted is that it does not linefy the given
%% args.

eval_forms(Tree, Binding, Opts) when is_list(Opts) ->
  eval_forms(Tree, Binding, env_for_eval(Opts));
eval_forms(Tree, Binding, E) ->
  eval_forms(Tree, Binding, E, elixir_env:env_to_scope(E)).
eval_forms(Tree, Binding, Env, Scope) ->
  {ParsedBinding, ParsedScope} = elixir_scope:load_binding(Binding, Scope),
  ParsedEnv = Env#{vars := [K || {K,_} <- ParsedScope#elixir_scope.vars]},
  {Erl, NewEnv, NewScope} = quoted_to_erl(Tree, ParsedEnv, ParsedScope),

  case Erl of
    {atom, _, Atom} ->
      {Atom, Binding, NewEnv, NewScope};
    _  ->
      {value, Value, NewBinding} = erl_eval(Erl, ParsedBinding, Env),
      {Value, elixir_scope:dump_binding(NewBinding, NewScope), NewEnv, NewScope}
  end.

erl_eval(Erl, ParsedBinding, E) ->
  case erl_eval:check_command([Erl], ParsedBinding) of
    ok -> ok;
    {error, Desc} -> elixir_errors:handle_file_error(?m(E, file), Desc)
  end,

  % Below must be all one line for locations to be the same when the stacktrace
  % needs to be extended to the full stacktrace.
  try erl_eval:expr(Erl, ParsedBinding, none, none, none) catch Class:Exception -> erlang:raise(Class, Exception, get_stacktrace()) end.

get_stacktrace() ->
  Stacktrace = erlang:get_stacktrace(),
  % eval_eval and eval_bits can call :erlang.raise/3 without the full
  % stacktrace. When this occurs re-add the current stacktrace so that no
  % stack information is lost.
  try
    throw(stack)
  catch
    throw:stack ->
      % Ignore stack item for current function.
      [_ | CurrentStack] = erlang:get_stacktrace(),
      get_stacktrace(Stacktrace, CurrentStack)
  end.

% The stacktrace did not include the current stack, re-add it.
get_stacktrace([], CurrentStack) ->
  CurrentStack;
% The stacktrace includes the current stack.
get_stacktrace(CurrentStack, CurrentStack) ->
  CurrentStack;
get_stacktrace([StackItem | Stacktrace], CurrentStack) ->
  [StackItem | get_stacktrace(Stacktrace, CurrentStack)].

%% Converts a quoted expression to erlang abstract format

quoted_to_erl(Quoted, Env) ->
  quoted_to_erl(Quoted, Env, elixir_env:env_to_scope(Env)).

quoted_to_erl(Quoted, Env, Scope) ->
  {Expanded, NewEnv} = elixir_exp:expand(Quoted, Env),
  {Erl, NewScope}    = elixir_translator:translate(Expanded, Scope),
  {Erl, NewEnv, NewScope}.

%% Converts a given string (char list) into quote expression

string_to_quoted(String, StartLine, File, Opts) when is_integer(StartLine), is_binary(File) ->
  case elixir_tokenizer:tokenize(String, StartLine, [{file, File}|Opts]) of
    {ok, _Line, _Column, Tokens} ->
      try elixir_parser:parse(Tokens) of
        {ok, Forms} -> {ok, Forms};
        {error, {{Line, _, _}, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}};
        {error, {Line, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}}
      catch
        {error, {{Line, _, _}, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}};
        {error, {Line, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}}
      end;
    {error, {Line, {ErrorPrefix, ErrorSuffix}, Token}, _Rest, _SoFar} ->
      {error, {Line, {to_binary(ErrorPrefix), to_binary(ErrorSuffix)}, to_binary(Token)}};
    {error, {Line, Error, Token}, _Rest, _SoFar} ->
      {error, {Line, to_binary(Error), to_binary(Token)}}
  end.

'string_to_quoted!'(String, StartLine, File, Opts) ->
  case string_to_quoted(String, StartLine, File, Opts) of
    {ok, Forms} ->
      Forms;
    {error, {Line, Error, Token}} ->
      elixir_errors:parse_error(Line, File, Error, Token)
  end.

to_binary(List) when is_list(List) -> elixir_utils:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).
