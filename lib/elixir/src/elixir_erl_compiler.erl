-module(elixir_erl_compiler).
-export([spawn/1, forms/3, noenv_forms/3, erl_to_core/2]).
-include("elixir.hrl").

spawn(Fun) ->
  CompilerInfo = get(elixir_compiler_info),

  CodeDiagnostics =
    case get(elixir_code_diagnostics) of
      undefined -> undefined;
      {_Tail, Log} -> {[], Log}
    end,

  {_, Ref} =
    spawn_monitor(fun() ->
      put(elixir_compiler_info, CompilerInfo),
      put(elixir_code_diagnostics, CodeDiagnostics),

      try Fun() of
        Result -> exit({ok, Result, get(elixir_code_diagnostics)})
      catch
        Kind:Reason:Stack ->
          exit({Kind, Reason, Stack, get(elixir_code_diagnostics)})
      end
    end),

  receive
    {'DOWN', Ref, process, _, {ok, Result, Diagnostics}} ->
      copy_diagnostics(Diagnostics),
      Result;
    {'DOWN', Ref, process, _, {Kind, Reason, Stack, Diagnostics}} ->
      copy_diagnostics(Diagnostics),
      erlang:raise(Kind, Reason, Stack)
  end.

copy_diagnostics(undefined) ->
  ok;
copy_diagnostics({Head, _}) ->
  case get(elixir_code_diagnostics) of
    undefined -> ok;
    {Tail, Log} -> put(elixir_code_diagnostics, {Head ++ Tail, Log})
  end.

forms(Forms, File, Opts) ->
  compile(Forms, File, Opts ++ compile:env_compiler_options()).

noenv_forms(Forms, File, Opts) ->
  compile(Forms, File, Opts).

erl_to_core(Forms, Opts) ->
  %% TODO: Remove parse transform handling on Elixir v2.0
  case [M || {parse_transform, M} <- Opts] of
    [] ->
      v3_core:module(Forms, Opts);
    _ ->
      case compile:noenv_forms(Forms, [no_spawn_compiler_process, to_core0, return, no_auto_import | Opts]) of
        {ok, _Module, Core, Warnings} -> {ok, Core, Warnings};
        {error, Errors, Warnings} -> {error, Errors, Warnings}
      end
  end.

compile(Forms, File, Opts) when is_list(Forms), is_list(Opts), is_binary(File) ->
  Source = elixir_utils:characters_to_list(File),

  case erl_to_core(Forms, Opts) of
    {ok, CoreForms, CoreWarnings} ->
      format_warnings(Opts, CoreWarnings),
      CompileOpts = [no_spawn_compiler_process, from_core, no_core_prepare,
                     no_auto_import, return, {source, Source} | Opts],

      case compile:noenv_forms(CoreForms, CompileOpts) of
        {ok, Module, Binary, Warnings} when is_binary(Binary) ->
          format_warnings(Opts, Warnings),
          {Module, Binary};

        {ok, Module, _Binary, _Warnings} ->
          Message = io_lib:format(
            "could not compile module ~ts. We expected the compiler to return a .beam binary but "
            "got something else. This usually happens because ERL_COMPILER_OPTIONS or @compile "
            "was set to change the compilation outcome in a way that is incompatible with Elixir",
            [elixir_aliases:inspect(Module)]
          ),

          elixir_errors:compile_error([], File, Message);

        {error, Errors, Warnings} ->
          format_warnings(Opts, Warnings),
          format_errors(Errors)
      end;

    {error, CoreErrors, CoreWarnings} ->
      format_warnings(Opts, CoreWarnings),
      format_errors(CoreErrors)
  end.

format_errors([]) ->
  exit({nocompile, "compilation failed but no error was raised"});
format_errors(Errors) ->
  lists:foreach(fun
    ({File, Each}) when is_list(File) ->
      BinFile = elixir_utils:characters_to_binary(File),
      lists:foreach(fun(Error) -> handle_file_error(BinFile, Error) end, Each);
    ({Mod, Each}) when is_atom(Mod) ->
      lists:foreach(fun(Error) -> handle_file_error(elixir_aliases:inspect(Mod), Error) end, Each)
  end, Errors).

format_warnings(Opts, Warnings) ->
  NoWarnNoMatch = proplists:get_value(nowarn_nomatch, Opts, false),
  lists:foreach(fun ({File, Each}) ->
    BinFile = elixir_utils:characters_to_binary(File),
    lists:foreach(fun(Warning) ->
      handle_file_warning(NoWarnNoMatch, BinFile, Warning)
    end, Each)
  end, Warnings).

%% Handle warnings from Erlang land

%% Those we implement ourselves
handle_file_warning(_, _File, {_Line, v3_core, {map_key_repeated, _}}) -> ok;
handle_file_warning(_, _File, {_Line, sys_core_fold, {ignored, useless_building}}) -> ok;

%% Ignore all linting errors (only come up on parse transforms)
handle_file_warning(_, _File, {_Line, erl_lint, _}) -> ok;

handle_file_warning(_, File, {Line, Module, Desc}) ->
  Message = custom_format(Module, Desc),
  elixir_errors:erl_warn(Line, File, Message).

%% Handle warnings

handle_file_error(File, {beam_validator, Desc}) ->
  elixir_errors:compile_error([{line, 0}], File, beam_validator:format_error(Desc));
handle_file_error(File, {Line, Module, Desc}) ->
  Message = custom_format(Module, Desc),
  elixir_errors:compile_error([{line, Line}], File, Message).

%% Mention the capture operator in make_fun
custom_format(sys_core_fold, {ignored, {no_effect, {erlang, make_fun, 3}}}) ->
  "the result of the capture operator & (Function.capture/3) is never used";

%% Make no_effect clauses pretty
custom_format(sys_core_fold, {ignored, {no_effect, {erlang, F, A}}}) ->
  {Fmt, Args} = case erl_internal:comp_op(F, A) of
    true -> {"use of operator ~ts has no effect", [elixir_utils:erlang_comparison_op_to_elixir(F)]};
    false ->
      case erl_internal:bif(F, A) of
        false -> {"the call to :erlang.~ts/~B has no effect", [F, A]};
        true ->  {"the call to ~ts/~B has no effect", [F, A]}
      end
  end,
  io_lib:format(Fmt, Args);

%% Rewrite nomatch to be more generic, it can happen inside if, unless, and the like
custom_format(sys_core_fold, {nomatch, X}) when X == guard; X == no_clause ->
  "this check/guard will always yield the same result";

custom_format(sys_core_fold, {nomatch, {shadow, Line, {ErlName, ErlArity}}}) ->
  {Name, Arity} = elixir_utils:erl_fa_to_elixir_fa(ErlName, ErlArity),

  io_lib:format(
    "this clause for ~ts/~B cannot match because a previous clause at line ~B always matches",
    [Name, Arity, Line]
  );

custom_format([], Desc) ->
  io_lib:format("~p", [Desc]);

custom_format(Module, Desc) ->
  Module:format_error(Desc).
