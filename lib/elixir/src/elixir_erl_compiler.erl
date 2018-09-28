-module(elixir_erl_compiler).
-export([forms/3, noenv_forms/3]).

forms(Forms, File, Opts) ->
  compile(fun compile:forms/2, Forms, File, Opts).

noenv_forms(Forms, File, Opts) ->
  compile(fun compile:noenv_forms/2, Forms, File, Opts).

compile(Fun, Forms, File, Opts) when is_list(Forms), is_list(Opts), is_binary(File) ->
  Source = elixir_utils:characters_to_list(File),
  case Fun(Forms, [return, {source, Source} | Opts]) of
    {ok, Module, Binary, Warnings} ->
      format_warnings(Opts, Warnings),
      {Module, Binary};
    {error, Errors, Warnings} ->
      format_warnings(Opts, Warnings),
      format_errors(Errors)
  end.

format_errors([]) ->
  exit({nocompile, "compilation failed but no error was raised"});
format_errors(Errors) ->
  lists:foreach(fun ({File, Each}) ->
    BinFile = elixir_utils:characters_to_binary(File),
    lists:foreach(fun(Error) -> handle_file_error(BinFile, Error) end, Each)
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

%% Ignore nomatch warnings
handle_file_warning(true, _File, {_Line, sys_core_fold, nomatch_guard}) -> ok;
handle_file_warning(true, _File, {_Line, sys_core_fold, {nomatch_shadow, _}}) -> ok;

%% Ignore always
handle_file_warning(_, _File, {_Line, sys_core_fold, useless_building}) -> ok;

%% This is an Erlang bug, it considers {tuple, _}.call to always fail
handle_file_warning(_, _File, {_Line, v3_kernel, bad_call}) -> ok;

%% Those we handle them ourselves
handle_file_warning(_, _File, {_Line, erl_lint, {unused_function, _}}) -> ok;
handle_file_warning(_, _File, {_Line, erl_lint, {unused_var, _}}) -> ok;
handle_file_warning(_, _File, {_Line, erl_lint, {shadowed_var, _, _}}) -> ok;
handle_file_warning(_, _File, {_Line, erl_lint, {exported_var, _, _}}) -> ok;
handle_file_warning(_, _File, {_Line, v3_core, {map_key_repeated, _}}) -> ok;

%% Ignore behaviour warnings as we check for these problem ourselves
handle_file_warning(_, _File, {_Line, erl_lint, {conflicting_behaviours, _, _, _, _}}) -> ok;
handle_file_warning(_, _File, {_Line, erl_lint, {undefined_behaviour_func, _, _}}) -> ok;
handle_file_warning(_, _File, {_Line, erl_lint, {undefined_behaviour, _}}) -> ok;
handle_file_warning(_, _File, {_Line, erl_lint, {ill_defined_behaviour_callbacks, _}}) -> ok;
handle_file_warning(_, _File, {_Line, erl_lint, {ill_defined_optional_callbacks, _}}) -> ok;

handle_file_warning(_, File, {Line, Module, Desc}) ->
  Message = format_error(Module, Desc),
  elixir_errors:warn(Line, File, Message).

%% Handle warnings

handle_file_error(File, {beam_validator, Rest}) ->
  elixir_errors:form_error([{line, 0}], File, beam_validator, Rest);
handle_file_error(File, {Line, Module, Desc}) ->
  Message = format_error(Module, Desc),
  elixir_errors:compile_error([{line, Line}], File, Message).

%% Custom formatting

%% Normalize formatting of functions
format_error(erl_lint, {undefined_function, {F, A}}) ->
  io_lib:format("undefined function ~ts/~B", [F, A]);

%% Normalize formatting of specs
format_error(erl_lint, {spec_fun_undefined, {M, F, A}}) ->
  io_lib:format("spec for undefined function ~ts.~ts/~B", [elixir_aliases:inspect(M), F, A]);

%% Mention the capture operator in make_fun
format_error(sys_core_fold, {no_effect, {erlang, make_fun, 3}}) ->
  "the result of the capture operator & (:erlang.make_fun/3) is never used";

%% Make no_effect clauses pretty
format_error(sys_core_fold, {no_effect, {erlang, F, A}}) ->
  {Fmt, Args} = case erl_internal:comp_op(F, A) of
    true -> {"use of operator ~ts has no effect", [elixir_utils:erlang_comparison_op_to_elixir(F)]};
    false ->
      case erl_internal:bif(F, A) of
        false -> {"the call to :erlang.~ts/~B has no effect", [F, A]};
        true ->  {"the call to ~ts/~B has no effect", [F, A]}
      end
  end,
  io_lib:format(Fmt, Args);

%% Rewrite nomatch_guard to be more generic it can happen inside if, unless, etc
format_error(sys_core_fold, nomatch_guard) ->
  "this check/guard will always yield the same result";

%% Handle literal eval failures
format_error(sys_core_fold, {eval_failure, Error}) ->
  #{'__struct__' := Struct} = 'Elixir.Exception':normalize(error, Error),
  ["this expression will fail with ", elixir_aliases:inspect(Struct)];

format_error([], Desc) ->
  io_lib:format("~p", [Desc]);

format_error(Module, Desc) ->
  Module:format_error(Desc).
