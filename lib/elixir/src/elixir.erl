%% Main entry point for Elixir functions. All of those functions are
%% private to the Elixir compiler and reserved to be used by Elixir only.
-module(elixir).
-behaviour(application).
-export([start_cli/0,
  string_to_tokens/4, tokens_to_quoted/3, 'string_to_quoted!'/4,
  env_for_eval/1, env_for_eval/2, quoted_to_erl/2,
  eval_forms/3, eval_forms/4, eval_quoted/3]).
-include("elixir.hrl").
-define(system, 'Elixir.System').

%% Top level types
%% TODO: Remove char_list type on v2.0
-export_type([charlist/0, char_list/0, nonempty_charlist/0, struct/0, as_boolean/1, keyword/0, keyword/1]).
-type charlist() :: string().
-type char_list() :: string().
-type nonempty_charlist() :: nonempty_string().
-type as_boolean(T) :: T.
-type keyword() :: [{atom(), any()}].
-type keyword(T) :: [{atom(), T}].
-type struct() :: #{'__struct__' := atom(), atom() => any()}.

%% OTP Application API

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  _ = parse_otp_release(),
  Encoding = file:native_name_encoding(),

  preload_common_modules(),
  set_stdio_and_stderr_to_binary_and_maybe_utf8(),
  check_file_encoding(Encoding),

  case application:get_env(elixir, check_endianness, true) of
    true  -> check_endianness();
    false -> ok
  end,

  Tokenizer = case code:ensure_loaded('Elixir.String.Tokenizer') of
    {module, Mod} -> Mod;
    _ -> elixir_tokenizer
  end,

  URIConfig = [
    {{uri, <<"ftp">>}, 21},
    {{uri, <<"sftp">>}, 22},
    {{uri, <<"tftp">>}, 69},
    {{uri, <<"http">>}, 80},
    {{uri, <<"https">>}, 443},
    {{uri, <<"ldap">>}, 389}
  ],

  {ok, [[Home] | _]} = init:get_argument(home),

  Config = [
    {at_exit, []},
    {argv, []},
    {no_halt, false},

    %% Static options
    {bootstrap, false},
    {home, unicode:characters_to_binary(Home, Encoding, Encoding)},
    {identifier_tokenizer, Tokenizer},

    %% Compiler options
    {docs, true},
    {ignore_module_conflict, false},
    {parser_options, []},
    {debug_info, true},
    {warnings_as_errors, false},
    {relative_paths, true},
    {no_warn_undefined, []},
    {tracers, []}
    | URIConfig
  ],

  Tab = elixir_config:new(Config),
  case elixir_sup:start_link() of
    {ok, Sup} ->
      {ok, Sup, Tab};
    {error, _Reason} = Error ->
      elixir_config:delete(Tab),
      Error
  end.

stop(Tab) ->
  elixir_config:delete(Tab).

config_change(_Changed, _New, _Remove) ->
  ok.

set_stdio_and_stderr_to_binary_and_maybe_utf8() ->
  %% In case there is a shell, we can't really change its
  %% encoding, so we just set binary to true. Otherwise
  %% we must set the encoding as the user with no shell
  %% has encoding set to latin1.
  Opts =
    case init:get_argument(noshell) of
      {ok, _} -> [binary, {encoding, utf8}];
      error   -> [binary]
    end,

  ok = io:setopts(standard_io, Opts),
  ok = io:setopts(standard_error, [{encoding, utf8}]),
  ok.

preload_common_modules() ->
  %% We attempt to load those modules here so throughout
  %% the codebase we can avoid code:ensure_loaded/1 checks.
  _ = code:ensure_loaded('Elixir.Kernel'),
  _ = code:ensure_loaded('Elixir.Macro.Env'),
  ok.

parse_otp_release() ->
  %% Whenever we change this check, we should also change Makefile.
  case string:to_integer(erlang:system_info(otp_release)) of
    {Num, _} when Num >= 20 ->
      Num;
    _ ->
      io:format(standard_error, "ERROR! Unsupported Erlang/OTP version, expected Erlang/OTP 20+~n", []),
      erlang:halt(1)
  end.

check_endianness() ->
  case code:ensure_loaded(?system) of
    {module, ?system} ->
      Endianness = ?system:endianness(),
      case ?system:compiled_endianness() of
        Endianness ->
          ok;
        _ ->
          io:format(standard_error,
            "warning: Elixir is running in a system with a different endianness than the one its "
            "source code was compiled in. Please make sure Elixir and all source files were compiled "
            "in a machine with the same endianness as the current one: ~ts~n", [Endianness])
      end;
    {error, _} ->
      ok
  end.

check_file_encoding(Encoding) ->
  case Encoding of
    latin1 ->
      io:format(standard_error,
        "warning: the VM is running with native name encoding of latin1 which may cause "
        "Elixir to malfunction as it expects utf8. Please ensure your locale is set to UTF-8 "
        "(which can be verified by running \"locale\" in your shell)~n", []);
    _ ->
      ok
  end.

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
  env_for_eval(elixir_env:new(), Opts).

env_for_eval(Env, Opts) ->
  Line = case lists:keyfind(line, 1, Opts) of
    {line, LineOpt} when is_integer(LineOpt) -> LineOpt;
    false -> ?key(Env, line)
  end,

  File = case lists:keyfind(file, 1, Opts) of
    {file, FileOpt} when is_binary(FileOpt) -> FileOpt;
    false -> ?key(Env, file)
  end,

  Aliases = case lists:keyfind(aliases, 1, Opts) of
    {aliases, AliasesOpt} when is_list(AliasesOpt) -> AliasesOpt;
    false -> ?key(Env, aliases)
  end,

  Requires = case lists:keyfind(requires, 1, Opts) of
    {requires, RequiresOpt} when is_list(RequiresOpt) -> ordsets:from_list(RequiresOpt);
    false -> ?key(Env, requires)
  end,

  Functions = case lists:keyfind(functions, 1, Opts) of
    {functions, FunctionsOpt} when is_list(FunctionsOpt) -> FunctionsOpt;
    false -> ?key(Env, functions)
  end,

  Macros = case lists:keyfind(macros, 1, Opts) of
    {macros, MacrosOpt} when is_list(MacrosOpt) -> MacrosOpt;
    false -> ?key(Env, macros)
  end,

  Module = case lists:keyfind(module, 1, Opts) of
    {module, ModuleOpt} when is_atom(ModuleOpt) -> ModuleOpt;
    false -> nil
  end,

  LexicalTracker = case lists:keyfind(lexical_tracker, 1, Opts) of
    {lexical_tracker, Pid} when is_pid(Pid) ->
      case is_process_alive(Pid) of
        true -> Pid;
        false -> nil
      end;
    {lexical_tracker, nil} ->
      nil;
    false ->
      nil
  end,

  FA = case lists:keyfind(function, 1, Opts) of
    {function, {Function, Arity}} when is_atom(Function), is_integer(Arity) -> {Function, Arity};
    {function, nil} -> nil;
    false -> nil
  end,

  Env#{
    file := File, module := Module, function := FA,
    macros := Macros, functions := Functions, lexical_tracker := LexicalTracker,
    requires := Requires, aliases := Aliases, line := Line
  }.

%% Quoted evaluation

eval_quoted(Tree, Binding, Opts) when is_list(Opts) ->
  eval_quoted(Tree, Binding, env_for_eval(Opts));
eval_quoted(Tree, Binding, #{line := Line} = E) ->
  eval_forms(elixir_quote:linify(Line, line, Tree), Binding, E).

%% Handle forms evaluation. The main difference to
%% eval_quoted is that it does not linify the given
%% args.

eval_forms(Tree, Binding, Opts) when is_list(Opts) ->
  eval_forms(Tree, Binding, env_for_eval(Opts));
eval_forms(Tree, Binding, E) ->
  eval_forms(Tree, Binding, E, elixir_env:env_to_scope(E)).
eval_forms(Tree, Binding, Env, Scope) ->
  {ParsedBinding, ParsedVars, ParsedScope} = elixir_erl_var:load_binding(Binding, Scope),
  ParsedEnv = elixir_env:with_vars(Env, ParsedVars),
  {Erl, NewEnv, NewScope} = quoted_to_erl(Tree, ParsedEnv, ParsedScope),

  case Erl of
    {atom, _, Atom} ->
      {Atom, Binding, NewEnv, NewScope};
    _  ->
      Exprs =
        case Erl of
          {block, _A, E} -> E;
          _ -> [Erl]
        end,

      % Below must be all one line for locations to be the same
      % when the stacktrace is extended to the full stacktrace.
      {value, Value, NewBinding} = recur_eval(Exprs, ParsedBinding, Env),
      {Value, elixir_erl_var:dump_binding(NewBinding, NewScope), NewEnv, NewScope}
  end.

recur_eval([Expr | Exprs], Binding, Env) ->
  {value, Value, NewBinding} =
    try erl_eval:expr(Expr, Binding, none, none, none) catch ?WITH_STACKTRACE(Class, Exception, Stacktrace) erlang:raise(Class, rewrite_exception(Exception, Stacktrace, Expr, Env), rewrite_stacktrace(Stacktrace)) end,

  case Exprs of
    [] -> {value, Value, NewBinding};
    _ -> recur_eval(Exprs, NewBinding, Env)
  end.

rewrite_exception(badarg, [{Mod, _, _, _} | _], Erl, #{file := File}) when Mod == erl_eval; Mod == eval_bits ->
  {Min, Max} =
    erl_parse:fold_anno(fun(Anno, {Min, Max}) ->
      case erl_anno:line(Anno) of
        Line when Line > 0 -> {min(Min, Line), max(Max, Line)};
        _ -> {Min, Max}
      end
    end, {999999, -999999}, Erl),

  'Elixir.ArgumentError':exception(
    erlang:iolist_to_binary(
      ["argument error while evaluating", badarg_file(File), badarg_line(Min, Max)]
    )
  );
rewrite_exception(Other, _, _, _) ->
  Other.

badarg_file(<<"nofile">>) -> "";
badarg_file(Path) -> [$\s, 'Elixir.Path':relative_to_cwd(Path)].

badarg_line(999999, -999999) -> [];
badarg_line(Line, Line) -> [" at line ", integer_to_binary(Line)];
badarg_line(Min, Max) -> [" between lines ", integer_to_binary(Min), " and ", integer_to_binary(Max)].

rewrite_stacktrace(Stacktrace) ->
  % eval_eval and eval_bits can call :erlang.raise/3 without the full
  % stacktrace. When this occurs re-add the current stacktrace so that no
  % stack information is lost.
  {current_stacktrace, CurrentStack} = erlang:process_info(self(), current_stacktrace),
  merge_stacktrace(Stacktrace, tl(CurrentStack)).

% The stacktrace did not include the current stack, re-add it.
merge_stacktrace([], CurrentStack) ->
  CurrentStack;
% The stacktrace includes the current stack.
merge_stacktrace(CurrentStack, CurrentStack) ->
  CurrentStack;
merge_stacktrace([StackItem | Stacktrace], CurrentStack) ->
  [StackItem | merge_stacktrace(Stacktrace, CurrentStack)].

%% Converts a quoted expression to Erlang abstract format

quoted_to_erl(Quoted, Env) ->
  quoted_to_erl(Quoted, Env, elixir_env:env_to_scope(Env)).

quoted_to_erl(Quoted, Env, Scope) ->
  {Expanded, NewEnv} = elixir_expand:expand(Quoted, Env),
  {Erl, NewScope} = elixir_erl_pass:translate(Expanded, Scope),
  {Erl, NewEnv, NewScope}.

%% Converts a given string (charlist) into quote expression

string_to_tokens(String, StartLine, File, Opts) when is_integer(StartLine), is_binary(File) ->
  case elixir_tokenizer:tokenize(String, StartLine, [{file, File} | Opts]) of
    {ok, _Tokens} = Ok ->
      Ok;
    {error, {Line, _, {ErrorPrefix, ErrorSuffix}, Token}, _Rest, _SoFar} ->
      {error, {Line, {to_binary(ErrorPrefix), to_binary(ErrorSuffix)}, to_binary(Token)}};
    {error, {Line, _, Error, Token}, _Rest, _SoFar} ->
      {error, {Line, to_binary(Error), to_binary(Token)}}
  end.

tokens_to_quoted(Tokens, File, Opts) ->
  handle_parsing_opts(File, Opts),

  try elixir_parser:parse(Tokens) of
    {ok, Forms} ->
      {ok, Forms};
    {error, {Line, _, [{ErrorPrefix, ErrorSuffix}, Token]}} ->
      {error, {parser_line(Line), {to_binary(ErrorPrefix), to_binary(ErrorSuffix)}, to_binary(Token)}};
    {error, {Line, _, [Error, Token]}} ->
      {error, {parser_line(Line), to_binary(Error), to_binary(Token)}}
  after
    erase(elixir_parser_file),
    erase(elixir_parser_columns),
    erase(elixir_token_metadata),
    erase(elixir_wrap_literals)
  end.

parser_line({Line, _, _}) ->
  Line;
parser_line(Meta) ->
  case lists:keyfind(line, 1, Meta) of
    {line, L} -> L;
    false -> 0
  end.

'string_to_quoted!'(String, StartLine, File, Opts) ->
  case string_to_tokens(String, StartLine, File, Opts) of
    {ok, Tokens} ->
      case tokens_to_quoted(Tokens, File, Opts) of
        {ok, Forms} ->
          Forms;
        {error, {Line, Error, Token}} ->
          elixir_errors:parse_error(Line, File, Error, Token)
      end;
    {error, {Line, Error, Token}} ->
      elixir_errors:parse_error(Line, File, Error, Token)
  end.

to_binary(List) when is_list(List) -> elixir_utils:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).

handle_parsing_opts(File, Opts) ->
  LiteralEncoder =
    case lists:keyfind(literal_encoder, 1, Opts) of
      {literal_encoder, Fun} -> Fun;
      false -> false
    end,
  TokenMetadata = lists:keyfind(token_metadata, 1, Opts) == {token_metadata, true},
  Columns = lists:keyfind(columns, 1, Opts) == {columns, true},
  put(elixir_parser_file, File),
  put(elixir_parser_columns, Columns),
  put(elixir_token_metadata, TokenMetadata),
  put(elixir_literal_encoder, LiteralEncoder).
