% A bunch of helpers to help to deal with errors in Elixir source code.
% This is not exposed in the Elixir language.
-module(elixir_errors).
-export([compile_error/3, compile_error/4,
  form_error/4, form_warn/4, parse_error/4, warn/2, warn/3,
  handle_file_warning/2, handle_file_warning/3, handle_file_error/2]).
-include("elixir.hrl").

-spec warn(non_neg_integer(), binary(), unicode:chardata()) -> ok.

warn(Line, File, Warning) when is_integer(Line), is_binary(File) ->
  warn(file_format(Line, File), Warning).

-spec warn(unicode:chardata(), unicode:chardata()) -> ok.

warn(Caller, Warning) ->
  do_warn([Caller, "warning: ", Warning, $\n]).

%% General forms handling.

-spec form_error(list(), binary(), module(), any()) -> no_return().

form_error(Meta, File, Module, Desc) ->
  compile_error(Meta, File, format_error(Module, Desc)).

-spec form_warn(list(), binary(), module(), any()) -> ok.

form_warn(Meta, File, Module, Desc) when is_list(Meta) ->
  {MetaFile, MetaLine} = meta_location(Meta, File),
  warn(MetaLine, MetaFile, format_error(Module, Desc)).

%% Compilation error.

-spec compile_error(list(), binary(), unicode:charlist()) -> no_return().
-spec compile_error(list(), binary(), string(), list()) -> no_return().

compile_error(Meta, File, Message) when is_list(Message) ->
  raise(Meta, File, 'Elixir.CompileError', elixir_utils:characters_to_binary(Message)).

compile_error(Meta, File, Format, Args) when is_list(Format)  ->
  compile_error(Meta, File, io_lib:format(Format, Args)).

%% Tokenization parsing/errors.

-spec parse_error(non_neg_integer(), binary() | {binary(), binary()}, binary(), binary()) -> no_return().

parse_error(Line, File, Error, <<>>) ->
  Message = case Error of
    <<"syntax error before: ">> -> <<"syntax error: expression is incomplete">>;
    _ -> Error
  end,
  do_raise(Line, File, 'Elixir.TokenMissingError', Message);

%% Show a nicer message for missing end tokens
parse_error(Line, File, <<"syntax error before: ">>, <<"'end'">>) ->
  do_raise(Line, File, 'Elixir.SyntaxError', <<"unexpected token: end">>);

%% Produce a human-readable message for errors before a sigil
parse_error(Line, File, <<"syntax error before: ">>, <<"{sigil,", _Rest/binary>> = Full) ->
  {sigil, _, Sigil, [Content|_], _} = parse_erl_term(Full),
  Content2 = case is_binary(Content) of
    true -> Content;
    false -> <<>>
  end,
  Message = <<"syntax error before: sigil ~", Sigil," starting with content '", Content2/binary, "'">>,
  do_raise(Line, File, 'Elixir.SyntaxError', Message);

%% Aliases are wrapped in ['']
parse_error(Line, File, Error, <<"['", _/binary>> = Full) when is_binary(Error) ->
  [AliasAtom] = parse_erl_term(Full),
  Alias = atom_to_binary(AliasAtom, utf8),
  do_raise(Line, File, 'Elixir.SyntaxError', <<Error/binary, Alias/binary>>);

%% Binaries (and interpolation) are wrapped in [<<...>>]
parse_error(Line, File, Error, <<"[", _/binary>> = Full) when is_binary(Error) ->
  Term = case parse_erl_term(Full) of
    [H|_] when is_binary(H) -> <<$", H/binary, $">>;
    _ -> <<$">>
  end,
  do_raise(Line, File, 'Elixir.SyntaxError', <<Error/binary, Term/binary>>);

%% Given a string prefix and suffix to insert the token inside the error message rather than append it
parse_error(Line, File, {ErrorPrefix, ErrorSuffix}, Token) when is_binary(ErrorPrefix), is_binary(ErrorSuffix), is_binary(Token) ->
  Message = <<ErrorPrefix/binary, Token/binary, ErrorSuffix/binary >>,
  do_raise(Line, File, 'Elixir.SyntaxError', Message);

%% Everything else is fine as is
parse_error(Line, File, Error, Token) when is_binary(Error), is_binary(Token) ->
  Message = <<Error/binary, Token/binary >>,
  do_raise(Line, File, 'Elixir.SyntaxError', Message).

%% Helper to parse terms which have been converted to binaries
parse_erl_term(Term) ->
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Term)),
  {ok, Parsed} = erl_parse:parse_term(Tokens ++ [{dot, 1}]),
  Parsed.


%% Handle warnings and errors from Erlang land (called during module compilation)

%% Ignore on bootstrap
handle_file_warning(true, _File, {_Line, sys_core_fold, nomatch_guard}) -> ok;
handle_file_warning(true, _File, {_Line, sys_core_fold, {nomatch_shadow, _}}) -> ok;

%% Ignore always
handle_file_warning(_, _File, {_Line, sys_core_fold, useless_building}) -> ok;

%% This is an Erlang bug, it considers {tuple, _}.call to always fail
handle_file_warning(_, _File, {_Line, v3_kernel, bad_call}) -> ok;

%% We handle unused local warnings ourselves
handle_file_warning(_, _File, {_Line, erl_lint, {unused_function, _}}) -> ok;

%% Make no_effect clauses pretty
handle_file_warning(_, File, {Line, sys_core_fold, {no_effect, {erlang, F, A}}}) ->
  {Fmt, Args} = case erl_internal:comp_op(F, A) of
    true -> {"use of operator ~ts has no effect", [translate_comp_op(F)]};
    false ->
      case erl_internal:bif(F, A) of
        false -> {"the call to :erlang.~ts/~B has no effect", [F, A]};
        true ->  {"the call to ~ts/~B has no effect", [F, A]}
      end
  end,
  Message = io_lib:format(Fmt, Args),
  warn(Line, File, Message);

%% Rewrite undefined behaviour to check for protocols
handle_file_warning(_, File, {Line, erl_lint, {undefined_behaviour_func, {Fun, Arity}, Module}}) ->
  {DefKind, Def, DefArity} =
    case atom_to_list(Fun) of
      "MACRO-" ++ Rest -> {macro, list_to_atom(Rest), Arity - 1};
      _ -> {function, Fun, Arity}
    end,

  Kind    = protocol_or_behaviour(Module),
  Raw     = "undefined ~ts ~ts ~ts/~B (for ~ts ~ts)",
  Message = io_lib:format(Raw, [Kind, DefKind, Def, DefArity, Kind, elixir_aliases:inspect(Module)]),
  warn(Line, File, Message);

handle_file_warning(_, File, {Line, erl_lint, {undefined_behaviour, Module}}) ->
  case elixir_compiler:get_opt(internal) of
    true  -> ok;
    false ->
      Message = io_lib:format("behaviour ~ts undefined", [elixir_aliases:inspect(Module)]),
      warn(Line, File, Message)
  end;

%% Ignore unused vars at "weird" lines (<= 0)
handle_file_warning(_, _File, {Line, erl_lint, {unused_var, _Var}}) when Line =< 0 ->
  ok;

%% Ignore shadowed and exported vars as we guarantee no conflicts ourselves
handle_file_warning(_, _File, {_Line, erl_lint, {shadowed_var, _Var, _Where}}) ->
  ok;

handle_file_warning(_, _File, {_Line, erl_lint, {exported_var, _Var, _Where}}) ->
  ok;

%% Rewrite nomatch_guard to be more generic it can happen inside if, unless, etc
handle_file_warning(_, File, {Line, sys_core_fold, nomatch_guard}) ->
  warn(Line, File, "this check/guard will always yield the same result");

%% Properly format other unused vars
handle_file_warning(_, File, {Line, erl_lint, {unused_var, Var}}) ->
  Message = format_error(erl_lint, {unused_var, format_var(Var)}),
  warn(Line, File, Message);

%% Handle literal eval failures
handle_file_warning(_, File, {Line, sys_core_fold, {eval_failure, Error}}) ->
  #{'__struct__' := Struct} = 'Elixir.Exception':normalize(error, Error),
  warn(Line, File, ["this expression will fail with ", elixir_aliases:inspect(Struct)]);

%% Default behaviour
handle_file_warning(_, File, {Line, Module, Desc}) ->
  Message = format_error(Module, Desc),
  warn(Line, File, Message).

handle_file_warning(File, Desc) ->
  handle_file_warning(false, File, Desc).

-spec handle_file_error(file:filename_all(), {non_neg_integer(), module(), any()}) -> no_return().

handle_file_error(File, {Line, erl_lint, {unsafe_var, Var, {In, _Where}}}) ->
  Translated = case In of
    'orelse'  -> 'or';
    'andalso' -> 'and';
    _ -> In
  end,
  Message = io_lib:format("cannot define variable ~ts inside ~ts", [format_var(Var), Translated]),
  do_raise(Line, File, 'Elixir.CompileError', elixir_utils:characters_to_binary(Message));

handle_file_error(File, {Line, erl_lint, {spec_fun_undefined, {M, F, A}}}) ->
  Message = io_lib:format("spec for undefined function ~ts.~ts/~B", [elixir_aliases:inspect(M), F, A]),
  do_raise(Line, File, 'Elixir.CompileError', elixir_utils:characters_to_binary(Message));

handle_file_error(File, {Line, Module, Desc}) ->
  Message = format_error(Module, Desc),
  do_raise(Line, File, 'Elixir.CompileError', elixir_utils:characters_to_binary(Message)).

%% Helpers

raise(Meta, File, Kind, Message) when is_list(Meta) ->
  {MetaFile, MetaLine} = meta_location(Meta, File),
  do_raise(MetaLine, MetaFile, Kind, Message).

file_format(0, File) ->
  io_lib:format("~ts: ", [elixir_utils:relative_to_cwd(File)]);

file_format(Line, File) ->
  io_lib:format("~ts:~w: ", [elixir_utils:relative_to_cwd(File), Line]).

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

meta_location(Meta, File) ->
  case elixir_utils:meta_location(Meta) of
    {F, L} -> {F, L};
    nil    -> {File, ?line(Meta)}
  end.

do_warn(Warning) ->
  CompilerPid = get(elixir_compiler_pid),
  if
    CompilerPid =/= undefined ->
      elixir_code_server:cast({register_warning, CompilerPid});
    true -> false
  end,
  io:put_chars(standard_error, Warning),
  ok.

do_raise(none, File, Kind, Message) ->
  do_raise(0, File, Kind, Message);
do_raise({Line, _, _}, File, Kind, Message) when is_integer(Line) ->
  do_raise(Line, File, Kind, Message);
do_raise(Line, File, Kind, Message) when is_integer(Line), is_binary(File), is_binary(Message) ->
  try
    throw(ok)
  catch
    ok -> ok
  end,
  Stacktrace = erlang:get_stacktrace(),
  Exception = Kind:exception([{description, Message}, {file, File}, {line, Line}]),
  erlang:raise(error, Exception, tl(Stacktrace)).
