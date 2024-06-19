%% Main entry point for Elixir functions. All of those functions are
%% private to the Elixir compiler and reserved to be used by Elixir only.
-module(elixir).
-behaviour(application).
-export([start_cli/0, start/0]).
-export([start/2, stop/1, config_change/3]).
-export([
  string_to_tokens/5, tokens_to_quoted/3, 'string_to_quoted!'/5,
  env_for_eval/1, quoted_to_erl/2, eval_forms/3, eval_quoted/3,
  eval_quoted/4, eval_local_handler/2, eval_external_handler/3,
  format_token_error/1
]).
-include("elixir.hrl").
-define(system, 'Elixir.System').
-define(elixir_eval_env, {elixir, eval_env}).

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
%% TODO: Remove Erlang/OTP 26+ checks

load_paths(OTP, Paths) when OTP >= 26 -> code:add_pathsa(Paths, cache);
load_paths(_OTP, Paths) -> code:add_pathsa(Paths).

start(_Type, _Args) ->
  OTP = parse_otp_release(),
  preload_common_modules(),
  set_stdio_and_stderr_to_binary_and_maybe_utf8(OTP),
  check_file_encoding(file:native_name_encoding()),

  case init:get_argument(elixir_root) of
    {ok, [[Root]]} ->
      load_paths(OTP, [
        Root ++ "/eex/ebin",
        Root ++ "/ex_unit/ebin",
        Root ++ "/iex/ebin",
        Root ++ "/logger/ebin",
        Root ++ "/mix/ebin",
        Root ++ "/elixir/ebin"
      ]);
    _ ->
      ok
  end,

  case application:get_env(elixir, check_endianness, true) of
    true  -> check_endianness();
    false -> ok
  end,

  case application:get_env(elixir, ansi_enabled) of
    {ok, _} -> ok;
    undefined ->
      %% Remove prim_tty module check as well as checks from scripts on Erlang/OTP 26
      ANSIEnabled = erlang:module_loaded(prim_tty) andalso (prim_tty:isatty(stdout) == true),
      application:set_env(elixir, ansi_enabled, ANSIEnabled)
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
    {{uri, <<"ldap">>}, 389},
    {{uri, <<"ws">>}, 80},
    {{uri, <<"wss">>}, 443}
  ],

  Config = [
    %% ARGV options
    {at_exit, []},
    {argv, []},
    {no_halt, false},

    %% Compiler options
    {docs, true},
    {ignore_already_consolidated, false},
    {ignore_module_conflict, false},
    {on_undefined_variable, raise},
    {parser_options, [{columns, true}]},
    {debug_info, true},
    {warnings_as_errors, false},
    {relative_paths, true},
    {no_warn_undefined, []},
    {tracers, []}
    | URIConfig
  ],

  elixir_config:static(#{bootstrap => false, identifier_tokenizer => Tokenizer}),
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

set_stdio_and_stderr_to_binary_and_maybe_utf8(OTP) when OTP >= 26 ->
  ok = io:setopts(standard_io, [binary]),
  ok;
set_stdio_and_stderr_to_binary_and_maybe_utf8(_OTP) ->
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
    {Num, _} when Num >= 25 ->
      Num;
    _ ->
      io:format(standard_error, "ERROR! Unsupported Erlang/OTP version, expected Erlang/OTP 25+~n", []),
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
        "(which can be verified by running \"locale\" in your shell) or set the "
        "ELIXIR_ERL_OPTIONS=\"+fnu\" environment variable~n", []);
    _ ->
      ok
  end.

%% Boot and process given options. Invoked by Elixir's script.
%% TODO: Delete prim_tty branches on Erlang/OTP 26.

start() ->
  case code:ensure_loaded(prim_tty) of
    {module, _} ->
      user_drv:start(#{initial_shell => iex:shell()});
    {error, _} ->
      case init:get_argument(elixir_root) of
        {ok, [[Root]]} -> code:add_patha(Root ++ "/iex/ebin");
        _ -> ok
      end,
      'Elixir.IEx.CLI':deprecated()
  end.

start_cli() ->
  {ok, _} = application:ensure_all_started(elixir),

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

env_for_eval(#{lexical_tracker := Pid} = Env) ->
  NewEnv = Env#{
    context := nil,
    context_modules := [],
    macro_aliases := [],
    versioned_vars := #{}
  },

  case is_pid(Pid) of
    true ->
      case is_process_alive(Pid) of
        true ->
          NewEnv;
        false ->
          'Elixir.IO':warn(
            <<"an __ENV__ with outdated compilation information was given to eval, "
              "call Macro.Env.prune_compile_info/1 to prune it">>
          ),
          NewEnv#{lexical_tracker := nil, tracers := []}
      end;
    false ->
      NewEnv#{tracers := []}
  end;
env_for_eval(Opts) when is_list(Opts) ->
  Env = elixir_env:new(),

  Line = case lists:keyfind(line, 1, Opts) of
    {line, LineOpt} when is_integer(LineOpt) -> LineOpt;
    false -> ?key(Env, line)
  end,

  File = case lists:keyfind(file, 1, Opts) of
    {file, FileOpt} when is_binary(FileOpt) -> FileOpt;
    false -> ?key(Env, file)
  end,

  Module = case lists:keyfind(module, 1, Opts) of
    {module, ModuleOpt} when is_atom(ModuleOpt) -> ModuleOpt;
    false -> nil
  end,

  FA = case lists:keyfind(function, 1, Opts) of
    {function, {Function, Arity}} when is_atom(Function), is_integer(Arity) -> {Function, Arity};
    {function, nil} -> nil;
    false -> nil
  end,

  TempTracers = case lists:keyfind(tracers, 1, Opts) of
    {tracers, TracersOpt} when is_list(TracersOpt) -> TracersOpt;
    false -> []
  end,

  Aliases = case lists:keyfind(aliases, 1, Opts) of
    {aliases, AliasesOpt} when is_list(AliasesOpt) ->
      'Elixir.IO':warn(<<":aliases option in eval is deprecated">>),
      AliasesOpt;
    false ->
      ?key(Env, aliases)
  end,

  Requires = case lists:keyfind(requires, 1, Opts) of
    {requires, RequiresOpt} when is_list(RequiresOpt) ->
      'Elixir.IO':warn(<<":requires option in eval is deprecated">>),
      ordsets:from_list(RequiresOpt);
    false ->
      ?key(Env, requires)
  end,

  Functions = case lists:keyfind(functions, 1, Opts) of
    {functions, FunctionsOpt} when is_list(FunctionsOpt) ->
      'Elixir.IO':warn(<<":functions option in eval is deprecated">>),
      FunctionsOpt;
    false ->
      ?key(Env, functions)
  end,

  Macros = case lists:keyfind(macros, 1, Opts) of
    {macros, MacrosOpt} when is_list(MacrosOpt) ->
      'Elixir.IO':warn(<<":macros option in eval is deprecated">>),
      MacrosOpt;
    false ->
      ?key(Env, macros)
  end,

  %% If there is a dead PID or lexical tracker is nil,
  %% we assume the tracers also cannot be (re)used.
  {LexicalTracker, Tracers} = case lists:keyfind(lexical_tracker, 1, Opts) of
    {lexical_tracker, Pid} when is_pid(Pid) ->
      'Elixir.IO':warn(<<":lexical_tracker option in eval is deprecated">>),
      case is_process_alive(Pid) of
        true -> {Pid, TempTracers};
        false -> {nil, []}
      end;
    {lexical_tracker, nil} ->
      'Elixir.IO':warn(<<":lexical_tracker option in eval is deprecated">>),
      {nil, []};
    false ->
      {nil, TempTracers}
  end,

  Env#{
    file := File, module := Module, function := FA, tracers := Tracers,
    macros := Macros, functions := Functions, lexical_tracker := LexicalTracker,
    requires := Requires, aliases := Aliases, line := Line
  }.

%% Quoted evaluation

eval_quoted(Tree, Binding, E) ->
  eval_quoted(Tree, Binding, E, []).
eval_quoted(Tree, Binding, #{line := Line} = E, Opts) ->
  eval_forms(elixir_quote:linify(Line, line, Tree), Binding, E, Opts).

eval_forms(Tree, Binding, OrigE) ->
  eval_forms(Tree, Binding, OrigE, []).
eval_forms(Tree, Binding, OrigE, Opts) ->
  Prune = proplists:get_value(prune_binding, Opts, false),
  {ExVars, ErlVars, ErlBinding} = elixir_erl_var:load_binding(Binding, Prune),
  E = elixir_env:with_vars(OrigE, ExVars),
  ExS = elixir_env:env_to_ex(E),
  ErlS = elixir_erl_var:from_env(E, ErlVars),
  {Erl, NewErlS, NewExS, NewE} = quoted_to_erl(Tree, ErlS, ExS, E),

  case Erl of
    {Literal, _, Value} when Literal == atom; Literal == float; Literal == integer ->
      if
        Prune -> {Value, [], NewE#{versioned_vars := #{}}};
        true -> {Value, Binding, NewE}
      end;

    _  ->
      Exprs =
        case Erl of
          {block, _, BlockExprs} -> BlockExprs;
          _ -> [Erl]
        end,

      %% We use remote names so eval works across Elixir versions.
      LocalHandler = {value, fun ?MODULE:eval_local_handler/2},
      ExternalHandler = {value, fun ?MODULE:eval_external_handler/3},

      {value, Value, NewBinding} =
        try
          %% ?elixir_eval_env is used by the external handler.
          %%
          %% The reason why we use the process dictionary to pass the environment
          %% is because we want to avoid passing closures to erl_eval, as that
          %% would effectively tie the eval code to the Elixir version and it is
          %% best if it depends solely on Erlang/OTP.
          %%
          %% The downside is that functions that escape the eval context will no
          %% longer have the original environment they came from.
          erlang:put(?elixir_eval_env, NewE),
          erl_eval:exprs(Exprs, ErlBinding, LocalHandler, ExternalHandler)
        after
          erlang:erase(?elixir_eval_env)
        end,

      PruneBefore = if Prune -> length(Binding); true -> -1 end,

      {DumpedBinding, DumpedVars} =
        elixir_erl_var:dump_binding(NewBinding, NewErlS, NewExS, PruneBefore),

      {Value, DumpedBinding, NewE#{versioned_vars := DumpedVars}}
  end.

eval_local_handler(FunName, Args) ->
  {current_stacktrace, Stack} = erlang:process_info(self(), current_stacktrace),
  Opts = [{module, nil}, {function, FunName}, {arity, length(Args)}, {reason, 'undefined local'}],
  Exception = 'Elixir.UndefinedFunctionError':exception(Opts),
  erlang:raise(error, Exception, Stack).

eval_external_handler(Ann, FunOrModFun, Args) ->
  try
    case FunOrModFun of
      {Mod, Fun} -> apply(Mod, Fun, Args);
      Fun -> apply(Fun, Args)
    end
  catch
    Kind:Reason:Stacktrace ->
      %% Take everything up to the Elixir module
      Pruned =
        lists:takewhile(fun
          ({elixir,_,_,_}) -> false;
          (_) -> true
        end, Stacktrace),

      Caller =
        lists:dropwhile(fun
          ({elixir,_,_,_}) -> false;
          (_) -> true
        end, Stacktrace),

      %% Now we prune any shared code path from erl_eval
      {current_stacktrace, Current} =
        erlang:process_info(self(), current_stacktrace),

      %% We need to make sure that we don't generate more
      %% frames than supported. So we do our best to drop
      %% from the Caller, but if the caller has no frames,
      %% we need to drop from Pruned.
      {DroppedCaller, ToDrop} =
        case Caller of
          [] -> {[], true};
          _ -> {lists:droplast(Caller), false}
        end,

      Reversed = drop_common(lists:reverse(Current), lists:reverse(Pruned), ToDrop),

      %% Add file+line information at the bottom
      Bottom =
        case erlang:get(?elixir_eval_env) of
          #{file := File} ->
            [{elixir_eval, '__FILE__', 1,
             [{file, elixir_utils:characters_to_list(File)}, {line, erl_anno:line(Ann)}]}];

          _ ->
            []
        end,

      Custom = lists:reverse(Bottom ++ Reversed, DroppedCaller),
      erlang:raise(Kind, Reason, Custom)
  end.

%% We need to check if we have dropped any frames.
%% If we have not dropped frames, then we need to drop one
%% at the end so we can put the elixir_eval frame in. If
%% we have more traces then depth, Erlang would discard
%% the whole stacktrace.
drop_common([H | T1], [H | T2], _ToDrop) -> drop_common(T1, T2, false);
drop_common([_ | T1], T2, ToDrop) -> drop_common(T1, T2, ToDrop);
drop_common([], [{?MODULE, _, _, _} | T2], _ToDrop) -> T2;
drop_common([], [_ | T2], true) -> T2;
drop_common([], T2, _) -> T2.

%% Converts a quoted expression to Erlang abstract format

quoted_to_erl(Quoted, E) ->
  {_, ErlS} = elixir_erl_var:from_env(E),
  ExS = elixir_env:env_to_ex(E),
  quoted_to_erl(Quoted, ErlS, ExS, E).

quoted_to_erl(Quoted, ErlS, ExS, Env) ->
  {Expanded, NewExS, NewEnv} =
    elixir_expand:expand(Quoted, ExS, Env),
  {Erl, NewErlS} = elixir_erl_pass:translate(Expanded, erl_anno:new(?key(Env, line)), ErlS),
  {Erl, NewErlS, NewExS, NewEnv}.

%% Converts a given string (charlist) into quote expression

string_to_tokens(String, StartLine, StartColumn, File, Opts) when is_integer(StartLine), is_binary(File) ->
  case elixir_tokenizer:tokenize(String, StartLine, StartColumn, Opts) of
    {ok, _Line, _Column, [], Tokens, Terminators} ->
      {ok, lists:reverse(Tokens, Terminators)};
    {ok, _Line, _Column, Warnings, Tokens, Terminators} ->
      (lists:keyfind(emit_warnings, 1, Opts) /= {emit_warnings, false}) andalso
        [elixir_errors:erl_warn(L, File, M) || {L, M} <- lists:reverse(Warnings)],
      {ok, lists:reverse(Tokens, Terminators)};
    {error, Info, _Rest, _Warnings, _SoFar} ->
      {error, format_token_error(Info)}
  end.

format_token_error({Location, {ErrorPrefix, ErrorSuffix}, Token}) ->
  {Location, {to_binary(ErrorPrefix), to_binary(ErrorSuffix)}, to_binary(Token)};
format_token_error({Location, Error, Token}) ->
  {Location, to_binary(Error), to_binary(Token)}.

tokens_to_quoted(Tokens, WarningFile, Opts) ->
  handle_parsing_opts(WarningFile, Opts),

  try elixir_parser:parse(Tokens) of
    {ok, Forms} ->
      {ok, Forms};
    {error, {Line, _, [{ErrorPrefix, ErrorSuffix}, Token]}} ->
      {error, {parser_location(Line), {to_binary(ErrorPrefix), to_binary(ErrorSuffix)}, to_binary(Token)}};
    {error, {Line, _, [Error, Token]}} ->
      {error, {parser_location(Line), to_binary(Error), to_binary(Token)}}
  after
    erase(elixir_parser_warning_file),
    erase(elixir_parser_columns),
    erase(elixir_token_metadata),
    erase(elixir_literal_encoder)
  end.

parser_location({Line, Column, _}) ->
  [{line, Line}, {column, Column}];
parser_location(Meta) ->
  Line =
    case lists:keyfind(line, 1, Meta) of
      {line, L} -> L;
      false -> 0
    end,

  case lists:keyfind(column, 1, Meta) of
    {column, C} -> [{line, Line}, {column, C}];
    false -> [{line, Line}]
  end.

'string_to_quoted!'(String, StartLine, StartColumn, File, Opts) ->
  case string_to_tokens(String, StartLine, StartColumn, File, Opts) of
    {ok, Tokens} ->
      case tokens_to_quoted(Tokens, File, Opts) of
        {ok, Forms} ->
          Forms;
        {error, {Meta, Error, Token}} ->
          elixir_errors:parse_error(Meta, File, Error, Token, {String, StartLine, StartColumn})
      end;
    {error, {Meta, Error, Token}} ->
      elixir_errors:parse_error(Meta, File, Error, Token, {String, StartLine, StartColumn})
  end.

to_binary(List) when is_list(List) -> elixir_utils:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom).

handle_parsing_opts(File, Opts) ->
  WarningFile =
    case lists:keyfind(emit_warnings, 1, Opts) of
      {emit_warnings, false} -> nil;
      _ -> File
    end,
  LiteralEncoder =
    case lists:keyfind(literal_encoder, 1, Opts) of
      {literal_encoder, Fun} -> Fun;
      false -> false
    end,
  TokenMetadata = lists:keyfind(token_metadata, 1, Opts) == {token_metadata, true},
  Columns = lists:keyfind(columns, 1, Opts) == {columns, true},
  put(elixir_parser_warning_file, WarningFile),
  put(elixir_parser_columns, Columns),
  put(elixir_token_metadata, TokenMetadata),
  put(elixir_literal_encoder, LiteralEncoder).
