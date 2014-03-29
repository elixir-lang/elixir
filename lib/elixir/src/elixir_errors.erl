% A bunch of helpers to help to deal with errors in Elixir source code.
% This is not exposed in the Elixir language.
-module(elixir_errors).
-export([compile_error/3, compile_error/4,
  form_error/4, parse_error/4, warn/2, warn/3,
  handle_file_warning/2, handle_file_warning/3, handle_file_error/2,
  deprecation/3, deprecation/4]).
-include("elixir.hrl").

-type line_or_meta() :: integer() | list().

warn(Warning) ->
  CompilerPid = get(elixir_compiler_pid),
  if
    CompilerPid =/= undefined ->
      elixir_code_server:cast({ register_warning, CompilerPid });
    true -> false
  end,
  io:put_chars(standard_error, Warning).

warn(Caller, Warning) ->
  warn([Caller, "warning: ", Warning]).

warn(Line, File, Warning) ->
  warn(file_format(Line, File, "warning: " ++ Warning)).

%% Raised during expansion/translation/compilation.

-spec form_error(line_or_meta(), binary(), module(), any()) -> no_return().

form_error(Meta, File, Module, Desc) ->
  compile_error(Meta, File, format_error(Module, Desc)).

-spec compile_error(line_or_meta(), binary(), iolist()) -> no_return().
-spec compile_error(line_or_meta(), binary(), iolist(), list()) -> no_return().

compile_error(Meta, File, Message) when is_list(Message) ->
  raise(Meta, File, 'Elixir.CompileError', elixir_utils:characters_to_binary(Message)).

compile_error(Meta, File, Format, Args) when is_list(Format)  ->
  compile_error(Meta, File, io_lib:format(Format, Args)).

%% Raised on tokenizing/parsing

-spec parse_error(line_or_meta(), binary(), iolist() | atom(), [] | iolist()) -> no_return().

parse_error(Meta, File, Error, <<>>) ->
  Message = case Error of
    <<"syntax error before: ">> -> <<"syntax error: expression is incomplete">>;
    _ -> Error
  end,
  raise(Meta, File, 'Elixir.TokenMissingError', Message);

parse_error(Meta, File, <<"syntax error before: ">>, <<"'end'">>) ->
  raise(Meta, File, 'Elixir.SyntaxError', <<"unexpected token: end">>);

parse_error(Meta, File, Error, Token) when is_binary(Error), is_binary(Token) ->
  Message = <<Error / binary, Token / binary >>,
  raise(Meta, File, 'Elixir.SyntaxError', Message).

%% Shows a deprecation message

deprecation(Meta, File, Message) -> deprecation(Meta, File, Message, []).

deprecation(Meta, File, Message, Args) ->
  io:format(file_format(?line(Meta), File, io_lib:format(Message, Args))).

%% Handle warnings and errors (called during module compilation)

%% Ignore on bootstrap
handle_file_warning(true, _File, { _Line, sys_core_fold, nomatch_guard }) -> [];
handle_file_warning(true, _File, { _Line, sys_core_fold, { nomatch_shadow, _ } }) -> [];

%% Ignore always
handle_file_warning(_, _File, { _Line, sys_core_fold, useless_building }) -> [];

%% This is an Erlang bug, it considers { tuple, _ }.call to always fail
handle_file_warning(_, _File, { _Line, v3_kernel, bad_call }) -> [];

%% We handle unused local warnings ourselves
handle_file_warning(_, _File, { _Line, erl_lint, { unused_function, _ } }) -> [];

%% Make no_effect clauses pretty
handle_file_warning(_, File, { Line, sys_core_fold, { no_effect, { erlang, F, A } } }) ->
  { Fmt, Args } = case erl_internal:comp_op(F, A) of
    true -> { "use of operator ~ts has no effect", [translate_comp_op(F)] };
    false ->
      case erl_internal:bif(F, A) of
        false -> { "the call to :erlang.~ts/~B has no effect", [F,A] };
        true -> { "the call to ~ts/~B has no effect", [F,A] }
      end
  end,
  Message = io_lib:format(Fmt, Args),
  warn(Line, File, Message);

%% Rewrite undefined behaviour to check for protocols
handle_file_warning(_, File, {Line,erl_lint,{undefined_behaviour_func,{Fun,Arity},Module}}) ->
  { DefKind, Def, DefArity } =
    case atom_to_list(Fun) of
      "MACRO-" ++ Rest -> { macro, list_to_atom(Rest), Arity - 1 };
      _ -> { function, Fun, Arity }
    end,

  Kind    = protocol_or_behaviour(Module),
  Raw     = "undefined ~ts ~ts ~ts/~B (for ~ts ~ts)",
  Message = io_lib:format(Raw, [Kind, DefKind, Def, DefArity, Kind, elixir_aliases:inspect(Module)]),
  warn(Line, File, Message);

handle_file_warning(_, File, {Line,erl_lint,{undefined_behaviour,Module}}) ->
  case elixir_compiler:get_opt(internal) of
    true  -> [];
    false ->
      Message = io_lib:format("behaviour ~ts undefined", [elixir_aliases:inspect(Module)]),
      warn(Line, File, Message)
  end;

%% Ignore unused vars at "weird" lines (<= 0)
handle_file_warning(_, _File, {Line,erl_lint,{unused_var,_Var}}) when Line =< 0 ->
  [];

%% Ignore shadowed vars as we guarantee no conflicts ourselves
handle_file_warning(_, _File, {_Line,erl_lint,{shadowed_var,_Var,_Where}}) ->
  [];

%% Properly format other unused vars
handle_file_warning(_, File, {Line,erl_lint,{unused_var,Var}}) ->
  Message = format_error(erl_lint, { unused_var, format_var(Var) }),
  warn(Line, File, Message);

%% Default behaviour
handle_file_warning(_, File, {Line,Module,Desc}) ->
  Message = format_error(Module, Desc),
  warn(Line, File, Message).

handle_file_warning(File, Desc) ->
  handle_file_warning(false, File, Desc).

-spec handle_file_error(file:filename_all(), {non_neg_integer(), module(), any()}) -> no_return().

handle_file_error(File, {Line,erl_lint,{unsafe_var,Var,{In,_Where}}}) ->
  Translated = case In of
    'orelse'  -> 'or';
    'andalso' -> 'and';
    _ -> In
  end,
  Message = io_lib:format("cannot define variable ~ts inside ~ts", [format_var(Var), Translated]),
  raise(Line, File, 'Elixir.CompileError', iolist_to_binary(Message));

handle_file_error(File, {Line,erl_lint,{spec_fun_undefined,{M,F,A}}}) ->
  Message = io_lib:format("spec for undefined function ~ts.~ts/~B", [elixir_aliases:inspect(M), F, A]),
  raise(Line, File, 'Elixir.CompileError', iolist_to_binary(Message));

handle_file_error(File, {Line,Module,Desc}) ->
  form_error(Line, File, Module, Desc).

%% Helpers

raise(Meta, File, Kind, Message) when is_list(Meta) ->
  raise(?line(Meta), File, Kind, Message);

raise(none, File, Kind, Message) ->
  raise(0, File, Kind, Message);

raise(Line, File, Kind, Message) when is_integer(Line), is_binary(File) ->
  %% Populate the stacktrace so we can raise it
  try
    throw(ok)
  catch
    ok -> ok
  end,
  Stacktrace = erlang:get_stacktrace(),
  Exception = Kind:new([{description, Message}, {file, File}, {line, Line}]),
  erlang:raise(error, Exception, tl(Stacktrace)).

file_format(0, File, Message) when is_binary(File) ->
  io_lib:format("~ts: ~ts~n", [elixir_utils:relative_to_cwd(File), Message]);

file_format(Line, File, Message) when is_binary(File) ->
  io_lib:format("~ts:~w: ~ts~n", [elixir_utils:relative_to_cwd(File), Line, Message]).

format_var(Var) ->
  list_to_atom(lists:takewhile(fun(X) -> X /= $@ end, atom_to_list(Var))).

format_error([], Desc) ->
  io_lib:format("~p", [Desc]);

format_error(Module, Desc) ->
  Module:format_error(Desc).

protocol_or_behaviour(Module) ->
  case is_protocol(Module) of
    true  -> protocol;
    false -> behaviour
  end.

is_protocol(Module) ->
  case code:ensure_loaded(Module) of
    { module, _ } ->
      erlang:function_exported(Module, '__protocol__', 1) andalso
        Module:'__protocol__'(name) == Module;
    { error, _ } ->
      false
  end.

translate_comp_op('/=') -> '!=';
translate_comp_op('=<') -> '<=';
translate_comp_op('=:=') -> '===';
translate_comp_op('=/=') -> '!==';
translate_comp_op(Other) -> Other.
