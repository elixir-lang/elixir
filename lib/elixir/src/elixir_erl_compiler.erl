-module(elixir_erl_compiler).
-export([forms/3, noenv_forms/3]).

forms(Forms, File, Opts) ->
  compile(fun compile:forms/2, Forms, File, Opts).

noenv_forms(Forms, File, Opts) ->
  compile(fun compile:noenv_forms/2, Forms, File, Opts).

compile(Fun, Forms, File, Opts) when is_list(Forms), is_list(Opts), is_binary(File) ->
  Source = elixir_utils:characters_to_list(File),
  case Fun([no_auto_import() | Forms], [return, {source, Source} | Opts]) of
    {ok, Module, Binary, Warnings} ->
      format_warnings(Opts, Warnings),
      {Module, Binary};
    {error, Errors, Warnings} ->
      format_warnings(Opts, Warnings),
      format_errors(Errors)
  end.

no_auto_import() ->
  {attribute, 0, compile, no_auto_import}.

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

%% We handle unused local warnings ourselves
handle_file_warning(_, _File, {_Line, erl_lint, {unused_function, _}}) -> ok;

%% Ignore unused vars at "weird" lines (<= 0)
handle_file_warning(_, _File, {Line, erl_lint, {unused_var, _Var}}) when Line =< 0 -> ok;

%% Ignore shadowed and exported vars as we guarantee no conflicts ourselves
handle_file_warning(_, _File, {_Line, erl_lint, {shadowed_var, _Var, _Where}}) -> ok;
handle_file_warning(_, _File, {_Line, erl_lint, {exported_var, _Var, _Where}}) -> ok;

handle_file_warning(_, File, {Line, erl_lint, {undefined_behaviour, Module}}) ->
  case elixir_compiler:get_opt(internal) of
    true ->
      ok;
    false ->
      elixir_errors:warn(Line, File, ["behaviour ", elixir_aliases:inspect(Module), " is undefined"])
  end;

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

%% TODO: Remove this clause when we depend only on Erlang 19.
format_error(erl_lint, {bittype_mismatch, Val1, Val2, Kind}) ->
  Desc = "conflict in ~s specification for bit field: \"~p\" and \"~p\"",
  io_lib:format(Desc, [Kind, Val1, Val2]);

%% Make no_effect clauses pretty
format_error(sys_core_fold, {no_effect, {erlang, F, A}}) ->
  {Fmt, Args} = case erl_internal:comp_op(F, A) of
    true -> {"use of operator ~ts has no effect", [translate_comp_op(F)]};
    false ->
      case erl_internal:bif(F, A) of
        false -> {"the call to :erlang.~ts/~B has no effect", [F, A]};
        true ->  {"the call to ~ts/~B has no effect", [F, A]}
      end
  end,
  io_lib:format(Fmt, Args);

%% Rewrite undefined behaviour to check for protocols
format_error(erl_lint, {undefined_behaviour_func, {Fun, Arity}, Module}) ->
  {DefKind, Def, DefArity} =
    case atom_to_list(Fun) of
      "MACRO-" ++ Rest -> {macro, list_to_atom(Rest), Arity - 1};
      _ -> {function, Fun, Arity}
    end,

  Kind    = protocol_or_behaviour(Module),
  Raw     = "undefined ~ts ~ts ~ts/~B (for ~ts ~ts)",
  io_lib:format(Raw, [Kind, DefKind, Def, DefArity, Kind, elixir_aliases:inspect(Module)]);

%% Rewrite nomatch_guard to be more generic it can happen inside if, unless, etc
format_error(sys_core_fold, nomatch_guard) ->
  "this check/guard will always yield the same result";

%% Properly format other unused vars
format_error(erl_lint, {unused_var, Var}) ->
  ["variable \"", format_var(Var), "\" is unused"];

%% Handle literal eval failures
format_error(sys_core_fold, {eval_failure, Error}) ->
  #{'__struct__' := Struct} = 'Elixir.Exception':normalize(error, Error),
  ["this expression will fail with ", elixir_aliases:inspect(Struct)];

format_error([], Desc) ->
  io_lib:format("~p", [Desc]);

format_error(Module, Desc) ->
  Module:format_error(Desc).

%% Helpers

format_var(Var) ->
  lists:takewhile(fun(X) -> X /= $@ end, atom_to_list(Var)).

protocol_or_behaviour(Module) ->
  case is_protocol(Module) of
    true  -> protocol;
    false -> behaviour
  end.

is_protocol(Module) ->
  case code:ensure_loaded(Module) of
    {module, _} ->
      erlang:function_exported(Module, '__protocol__', 1) andalso
        Module:'__protocol__'(module) == Module;
    {error, _} ->
      false
  end.

translate_comp_op('/=') -> '!=';
translate_comp_op('=<') -> '<=';
translate_comp_op('=:=') -> '===';
translate_comp_op('=/=') -> '!==';
translate_comp_op(Other) -> Other.
