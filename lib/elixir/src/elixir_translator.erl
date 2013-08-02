%% Main entry point for translations. Are macros that cannot be
%% overriden are defined in this file.
-module(elixir_translator).
-export([forms/4, 'forms!'/4]).
-export([translate/2, translate_each/2, translate_arg/2,
         translate_args/2, translate_apply/7]).
-import(elixir_scope, [umergev/2, umergec/2, umergea/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4,
  compile_error/3, compile_error/4,
  assert_function_scope/3, assert_module_scope/3,
  assert_no_guard_scope/3, assert_no_match_or_guard_scope/3]).
-include("elixir.hrl").

forms(String, StartLine, File, Opts) ->
  try elixir_tokenizer:tokenize(String, StartLine, [{ file, File }|Opts]) of
    { ok, Tokens } ->
      try elixir_parser:parse(Tokens) of
        { ok, Forms } -> { ok, Forms };
        { error, { Line, _, [Error, Token] } } -> { error, { Line, Error, Token } }
      catch
        { error, { Line, _, [Error, Token] } } -> { error, { Line, Error, Token } }
      end;
    { error, { _, _, _ } } = Else -> Else
  catch
    { interpolation_error, { _, _, _ } = Tuple} -> { error, Tuple }
  end.

'forms!'(String, StartLine, File, Opts) ->
  case forms(String, StartLine, File, Opts) of
    { ok, Forms } ->
      Forms;
    { error, { Line, Error, Token } } ->
      elixir_errors:parse_error(Line, File, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Those macros are "low-level". They are the basic mechanism
%% that makes the language work and cannot be partially applied
%% nor overwritten.

%% Assignment operator

translate_each({ '=', Meta, [Left, Right] }, S) ->
  assert_no_guard_scope(Meta, '=', S),
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL } = elixir_clauses:assigns(fun translate_each/2, Left, SR),
  { { match, ?line(Meta), TLeft, TRight }, SL };

%% Containers

translate_each({ C, _, _ } = Original, S) when C == '[]'; C == '{}'; C == '<<>>' ->
  elixir_literal:translate(Original, S);

%% Blocks and scope rewriters

translate_each({ '__block__', Meta, [] }, S) ->
  { { atom, ?line(Meta), nil }, S };

translate_each({ '__block__', _Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ '__block__', Meta, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, ?line(Meta), TArgs }, NS };

translate_each({ '__scope__', _Meta, [[{file,File}],[{do,Expr}]] }, S) ->
  Old = S#elixir_scope.file,
  { TExpr, TS } = translate_each(Expr, S#elixir_scope{file=File}),
  { TExpr, TS#elixir_scope{file=Old} };

%% Erlang op

translate_each({ '__op__', Meta, [Op, Expr] }, S) when is_atom(Op) ->
  { TExpr, NS } = translate_each(Expr, S),
  { { op, ?line(Meta), convert_op(Op), TExpr }, NS };

translate_each({ '__op__', Meta, [Op, Left, Right] }, S) when is_atom(Op) ->
  { [TLeft, TRight], NS }  = translate_args([Left, Right], S),
  { { op, ?line(Meta), convert_op(Op), TLeft, TRight }, NS };

%% Lexical

translate_each({ alias, Meta, [Ref] }, S) ->
  translate_each({ alias, Meta, [Ref,[]] }, S);

translate_each({ alias, Meta, [Ref, KV] }, S) ->
  assert_no_match_or_guard_scope(Meta, alias, S),
  { TRef, SR } = translate_each(Ref, S),
  { TKV, ST }  = translate_opts(Meta, alias, [as], no_alias_opts(KV), SR),

  case TRef of
    { atom, _, Old } ->
      translate_alias(Meta, true, Old, TKV, ST);
    _ ->
      compile_error(Meta, S#elixir_scope.file, "invalid args for alias, expected a compile time atom or alias as argument")
  end;

translate_each({ require, Meta, [Ref] }, S) ->
  translate_each({ require, Meta, [Ref, []] }, S);

translate_each({ require, Meta, [Ref, KV] }, S) ->
  assert_no_match_or_guard_scope(Meta, require, S),

  { TRef, SR } = translate_each(Ref, S),
  { TKV, ST }  = translate_opts(Meta, require, [as], no_alias_opts(KV), SR),

  case TRef of
    { atom, _, Old } ->
      elixir_aliases:ensure_loaded(Meta, Old, ST),
      translate_require(Meta, Old, TKV, ST);
    _ ->
      compile_error(Meta, S#elixir_scope.file, "invalid args for require, expected a compile time atom or alias as argument")
  end;

translate_each({ import, Meta, [Left] }, S) ->
  translate_each({ import, Meta, [Left, []]}, S);

translate_each({ import, Meta, [Left,Opts] }, S) when is_list(Opts) ->
  translate_each({ import, Meta, [default, Left, Opts]}, S);

translate_each({ import, Meta, [Left,Right] }, S) ->
  %% Second argument is ambiguous, translate it and take a peek
  case translate_each(Right, S) of
    { { atom, _, _ }, _ } ->
      translate_each({ import, Meta, [Left, Right, []]}, S);
    _ ->
      translate_each({ import, Meta, [default, Left, Right]}, S)
  end;

translate_each({ import, Meta, [Left, Right, KV] }, S) ->
  assert_no_match_or_guard_scope(Meta, import, S),
  { TSelector, SL } = translate_each(Left, S),
  { TRef, SR } = translate_each(Right, SL),
  { TKV, ST }  = translate_opts(Meta, import, [as, only, except, warn], no_alias_opts(KV), SR),

  Selector = case TSelector of
    { atom, _,  SelectorAtom } -> SelectorAtom;
    _ -> compile_error(Meta, S#elixir_scope.file, "invalid selector for import, expected a compile time atom")
  end,

  case TRef of
    { atom, _, Old } ->
      elixir_aliases:ensure_loaded(Meta, Old, ST),
      SF = elixir_import:import(Meta, Old, TKV, Selector, ST),
      translate_require(Meta, Old, TKV, SF);
    _ ->
      compile_error(Meta, S#elixir_scope.file, "invalid name for import, expected a compile time atom or alias")
  end;

%% Pseudo variables

translate_each({ '__MODULE__', Meta, Atom }, S) when is_atom(Atom) ->
  { { atom, ?line(Meta), S#elixir_scope.module }, S };

translate_each({ '__FILE__', _Meta, Atom }, S) when is_atom(Atom) ->
  translate_each(S#elixir_scope.file, S);

translate_each({ '__DIR__', _Meta, Atom }, S) when is_atom(Atom) ->
  translate_each(filename:dirname(S#elixir_scope.file), S);

translate_each({ '__ENV__', Meta, Atom }, S) when is_atom(Atom) ->
  Env = elixir_scope:to_ex_env({ ?line(Meta), S }),
  { elixir_tree_helpers:elixir_to_erl(Env), S };

translate_each({ '__CALLER__', Meta, Atom }, S) when is_atom(Atom) ->
  { { var, ?line(Meta), '__CALLER__' }, S#elixir_scope{caller=true} };

%% Aliases

translate_each({ '__aliases__', Meta, _ } = Alias, S) ->
  case elixir_aliases:expand(Alias, S#elixir_scope.aliases, S#elixir_scope.macro_aliases) of
    Receiver when is_atom(Receiver) ->
      elixir_tracker:record_alias(Receiver, S#elixir_scope.module),
      { { atom, ?line(Meta), Receiver }, S };
    Aliases ->
      { TAliases, SA } = translate_args(Aliases, S),

      case lists:all(fun is_atom_tuple/1, TAliases) of
        true ->
          Atoms = [Atom || { atom, _, Atom } <- TAliases],
          Receiver = elixir_aliases:concat(Atoms),
          elixir_tracker:record_alias(Receiver, S#elixir_scope.module),
          { { atom, ?line(Meta), Receiver }, SA };
        false ->
          Args = [elixir_tree_helpers:list_to_cons(?line(Meta), TAliases)],
          { ?wrap_call(?line(Meta), elixir_aliases, concat, Args), SA }
      end
  end;

%% Quoting

translate_each({ Unquote, Meta, [_|_] }, S) when Unquote == unquote; Unquote == unquote_splicing ->
  compile_error(Meta, S#elixir_scope.file, "~p called outside quote", [Unquote]);

translate_each({ quote, Meta, [Opts] }, S) when is_list(Opts) ->
  case lists:keyfind(do, 1, Opts) of
    { do, Do } ->
      translate_each({ quote, Meta, [lists:keydelete(do, 1, Opts), [{do,Do}]] }, S);
    false ->
      syntax_error(Meta, S#elixir_scope.file, "missing do keyword in quote")
  end;

translate_each({ quote, Meta, [_] }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid args for quote");

translate_each({ quote, Meta, [KV, Do] }, S) when is_list(Do) ->
  Exprs =
    case lists:keyfind(do, 1, Do) of
      { do, E } -> E;
      false -> syntax_error(Meta, S#elixir_scope.file, "missing do keyword in quote")
    end,

  ValidOpts   = [hygiene, context, var_context, location, line, file, unquote, bind_quoted],
  { TKV, ST } = translate_opts(Meta, quote, ValidOpts, KV, S),

  Hygiene = case lists:keyfind(hygiene, 1, TKV) of
    { hygiene, List } when is_list(List) ->
      List;
    false ->
      []
  end,

  Context = case lists:keyfind(context, 1, TKV) of
    { context, Atom } when is_atom(Atom) ->
      Atom;
    { context, _ } ->
      compile_error(Meta, S#elixir_scope.file, "invalid :context for quote, expected a compile time atom or an alias");
    false ->
      case S#elixir_scope.module of
        nil -> 'Elixir';
        Mod -> Mod
      end
  end,

  Vars    = lists:keyfind(vars, 1, Hygiene) /= { vars, false },
  Aliases = lists:keyfind(aliases, 1, Hygiene) /= { aliases, false },
  Imports = lists:keyfind(imports, 1, Hygiene) /= { imports, false },

  { DefaultLine, DefaultFile } = case lists:keyfind(location, 1, TKV) of
    { location, keep } -> { keep, keep };
    false -> { nil, nil }
  end,

  Line = proplists:get_value(line, TKV, DefaultLine),
  File = proplists:get_value(file, TKV, DefaultFile),

  Scope = case File of
    keep -> S#elixir_scope.file;
    _    -> File
  end,

  QExprs = if
    is_binary(Scope) ->
      { '__scope__', Meta, [[{file,Scope}],[{do,Exprs}]] };
    File == nil ->
      Exprs;
    true ->
      compile_error(Meta, S#elixir_scope.file, "invalid :file for quote, expected a compile time binary")
  end,

  { Binding, DefaultUnquote } = case lists:keyfind(bind_quoted, 1, TKV) of
    { bind_quoted, BQ } -> { BQ, false };
    false -> { nil, true }
  end,

  Unquote = case lists:keyfind(unquote, 1, TKV) of
    { unquote, Bool } when is_boolean(Bool) -> Bool;
    false -> DefaultUnquote
  end,

  Q = #elixir_quote{vars_hygiene=Vars, line=Line, unquote=Unquote,
        aliases_hygiene=Aliases, imports_hygiene=Imports, context=Context},

  { TExprs, _TQ, TS } = elixir_quote:erl_quote(QExprs, Binding, Q, ST),
  { TExprs, TS };

translate_each({ quote, Meta, [_, _] }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid args for quote");

translate_each({ 'alias!', _Meta, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ 'var!', Meta, [Arg] }, S) ->
  translate_each({ 'var!', Meta, [Arg, nil] }, S);

translate_each({ 'var!', Meta, [{Name, _, Atom}, Kind] }, S) when is_atom(Name), is_atom(Atom) ->
  translate_each({ 'var!', Meta, [Name, Kind] }, S);

translate_each({ 'var!', Meta, [Name, Kind] }, S) when is_atom(Name) ->
  Expanded = expand_quote_context(Meta, Kind, "invalid second argument for var!", S),
  elixir_scope:translate_var(Meta, Name, Expanded, S, fun() ->
    compile_error(Meta, S#elixir_scope.file, "expected var!(~ts) to expand to an existing "
                  "variable or be a part of a match", [Name])
  end);

translate_each({ 'var!', Meta, [_, _] }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid first argument for var!, expected a var or an atom");

%% Functions

translate_each({ '&', Meta, [Arg] }, S) ->
  assert_no_match_or_guard_scope(Meta, '&', S),
  elixir_fn:capture(Meta, Arg, S);

translate_each({ fn, Meta, [[{do, { '->', _, Pairs }}]] }, S) ->
  assert_no_match_or_guard_scope(Meta, 'fn', S),
  elixir_fn:fn(Meta, Pairs, S);

%% Comprehensions

translate_each({ Kind, Meta, Args }, S) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  translate_comprehension(Meta, Kind, Args, S);

%% Super (exceptionally supports partial application)

translate_each({ super, Meta, Args } = Original, S) when is_list(Args) ->
  case elixir_partials:handle(Original, S) of
    error ->
      assert_no_match_or_guard_scope(Meta, super, S),
      Module = assert_module_scope(Meta, super, S),
      Function = assert_function_scope(Meta, super, S),
      elixir_def_overridable:ensure_defined(Meta, Module, Function, S),

      { _, Arity } = Function,

      { TArgs, TS } = if
        length(Args) == Arity ->
          translate_args(Args, S);
        true ->
          syntax_error(Meta, S#elixir_scope.file, "super must be called with the same number of "
                       "arguments as the current function")
      end,

      Super = elixir_def_overridable:name(Module, Function),
      { { call, ?line(Meta), { atom, ?line(Meta), Super }, TArgs }, TS#elixir_scope{super=true} };
    Else -> Else
  end;

translate_each({ 'super?', Meta, [] }, S) ->
  Module = assert_module_scope(Meta, 'super?', S),
  Function = assert_function_scope(Meta, 'super?', S),
  Bool = elixir_def_overridable:is_defined(Module, Function),
  { { atom, ?line(Meta), Bool }, S };

%% Variables

translate_each({ '^', Meta, [ { Name, _, Kind } = Var ] },
               #elixir_scope{extra=fn_match, extra_guards=Extra} = S) when is_atom(Name), is_atom(Kind) ->
  case orddict:find({ Name, Kind }, S#elixir_scope.backup_vars) of
    { ok, Value } ->
      Line = ?line(Meta),
      { TVar, TS } = translate_each(Var, S),
      Guard = { op, Line, '=:=', { var, ?line(Meta), Value }, TVar },
      { TVar, TS#elixir_scope{extra_guards=[Guard|Extra]} };
    error ->
      compile_error(Meta, S#elixir_scope.file, "unbound variable ^~ts", [Name])
  end;

translate_each({ '^', Meta, [ { Name, _, Kind } ] }, #elixir_scope{context=match} = S) when is_atom(Name), is_atom(Kind) ->
  case orddict:find({ Name, Kind }, S#elixir_scope.backup_vars) of
    { ok, Value } ->
      { { var, ?line(Meta), Value }, S };
    error ->
      compile_error(Meta, S#elixir_scope.file, "unbound variable ^~ts", [Name])
  end;

translate_each({ '^', Meta, [ { Name, _, Kind } ] }, S) when is_atom(Name), is_atom(Kind) ->
  compile_error(Meta, S#elixir_scope.file,
    "cannot use ^~ts outside of match clauses", [Name]);

translate_each({ '^', Meta, [ Expr ] }, S) ->
  syntax_error(Meta, S#elixir_scope.file,
    "the unary operator ^ can only be used with variables, invalid expression ^~ts", ['Elixir.Macro':to_string(Expr)]);

translate_each({ Name, Meta, Kind }, S) when is_atom(Name), is_atom(Kind) ->
  elixir_scope:translate_var(Meta, Name, Kind, S, fun() ->
    translate_each({ Name, Meta, [] }, S)
  end);

%% Local calls

translate_each({ '->', Meta, _Args }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "unhandled operator ->");

translate_each({ Atom, Meta, Args } = Original, S) when is_atom(Atom) ->
  assert_no_ambiguous_op(Atom, Meta, Args, S),

  case elixir_partials:is_sequential(Args) andalso
       elixir_dispatch:import_function(Meta, Atom, length(Args), S) of
    false ->
      case elixir_partials:handle(Original, S) of
        error ->
          Callback = fun() ->
            case S#elixir_scope.context of
              match ->
                compile_error(Meta, S#elixir_scope.file,
                              "cannot invoke function ~ts/~B inside match", [Atom, length(Args)]);
              guard ->
                Arity = length(Args),
                File  = S#elixir_scope.file,
                case Arity of
                  0 -> compile_error(Meta, File, "unknown variable ~ts or cannot invoke "
                                     "function ~ts/~B inside guard", [Atom, Atom, Arity]);
                  _ -> compile_error(Meta, File, "cannot invoke local ~ts/~B inside guard",
                                     [Atom, Arity])
                end;
              _ ->
                translate_local(Meta, Atom, Args, S)
            end
          end,

          elixir_dispatch:dispatch_import(Meta, Atom, Args, S, Callback);
        Else  -> Else
      end;
    Else -> Else
  end;

%% Remote calls

translate_each({ { '.', _, [Left, Right] }, Meta, Args } = Original, S) when is_atom(Right) ->
  { TLeft,  SL } = translate_each(Left, S),

  Fun = (element(1, TLeft) == atom) andalso elixir_partials:is_sequential(Args) andalso
    elixir_dispatch:require_function(Meta, element(3, TLeft), Right, length(Args), SL),

  case Fun of
    false ->
      case elixir_partials:handle(Original, S) of
        error ->
          { TRight, SR } = translate_each(Right, umergec(S, SL)),
          Callback = fun() -> translate_apply(Meta, TLeft, TRight, Args, S, SL, SR) end,

          case TLeft of
            { atom, _, Receiver } ->
              elixir_dispatch:dispatch_require(Meta, Receiver, Right, Args, umergev(SL, SR), fun() ->
                case S#elixir_scope.context of
                  Context when Receiver /= erlang, (Context == match) orelse (Context == guard) ->
                    compile_error(Meta, S#elixir_scope.file, "cannot invoke remote function ~ts.~ts/~B inside ~ts",
                      [elixir_errors:inspect(Receiver), Right, length(Args), Context]);
                  _ ->
                    Callback()
                end
              end);
            _ ->
              case S#elixir_scope.context of
                Context when Context == match; Context == guard ->
                  compile_error(Meta, S#elixir_scope.file, "cannot invoke remote function ~ts/~B inside ~ts",
                    [Right, length(Args), Context]);
                _ ->
                  Callback()
              end
          end;
        Else -> Else
      end;
    Else -> Else
  end;

%% Anonymous function calls

translate_each({ { '.', _, [Expr] }, Meta, Args } = Original, S) ->
  { TExpr, SE } = translate_each(Expr, S),
  case TExpr of
    { atom, _, Atom } ->
      syntax_error(Meta, S#elixir_scope.file, "invalid function call :~ts.()", [Atom]);
    _ ->
      case elixir_partials:handle(Original, S) of
        error ->
          { TArgs, SA } = translate_args(Args, umergec(S, SE)),
          { {call, ?line(Meta), TExpr, TArgs}, umergev(SE, SA) };
        Else -> Else
      end
  end;

%% Invalid calls

translate_each({ Invalid, Meta, _Args }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "unexpected parenthesis after ~ts",
    ['Elixir.Macro':to_string(Invalid)]);

%% Literals

translate_each(Literal, S) ->
  elixir_literal:translate(Literal, S).

%% Helpers

%% Opts

translate_opts(Meta, Kind, Allowed, Opts, S) ->
  { Expanded, TS } = case literal_opts(Opts) orelse skip_expansion(S#elixir_scope.module) of
    true  -> { Opts, S };
    false -> 'Elixir.Macro':expand_all(Opts, elixir_scope:to_ex_env({ ?line(Meta), S }), S)
  end,
  validate_opts(Meta, Kind, Allowed, Expanded, TS),
  { Expanded, TS }.

skip_expansion('Elixir.Kernel') -> true;
skip_expansion('Elixir.Kernel.Typespec') -> true;
skip_expansion(_) -> false.

literal_opts({ X, Y }) -> literal_opts(X) andalso literal_opts(Y);
literal_opts(Opts) when is_list(Opts) -> lists:all(fun literal_opts/1, Opts);
literal_opts(Opts) when is_atom(Opts); is_number(Opts); is_bitstring(Opts) -> true;
literal_opts(_) -> false.

validate_opts(Meta, Kind, Allowed, Opts, S) when is_list(Opts) ->
  [begin
    compile_error(Meta, S#elixir_scope.file,
                  "unsupported option ~ts given to ~s", ['Elixir.Kernel':inspect(Key), Kind])
  end || { Key, _ } <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, _Opts, S) ->
  compile_error(Meta, S#elixir_scope.file, "invalid options for ~s, expected a keyword list", [Kind]).

%% Quote

expand_quote_context(_Meta, Atom, _Msg, _S) when is_atom(Atom) -> Atom;
expand_quote_context(Meta, Alias, Msg, S) ->
  case translate_each(Alias, S) of
    { { atom, _, Atom }, _ } ->
      Atom;
    _ ->
      compile_error(Meta, S#elixir_scope.file, "~ts, expected a compile time available alias or an atom", [Msg])
  end.

%% Require

translate_require(Meta, Old, TKV, S) ->
  SF = S#elixir_scope{
    requires=ordsets:add_element(Old, S#elixir_scope.requires)
  },
  translate_alias(Meta, false, Old, TKV, SF).

%% Aliases

translate_alias(Meta, IncludeByDefault, Old, TKV, S) ->
  New = case lists:keyfind(as, 1, TKV) of
    { as, true } ->
      elixir_aliases:last(Old);
    { as, false } ->
      Old;
    { as, Atom } when is_atom(Atom) ->
      Atom;
    false ->
      if
        IncludeByDefault -> elixir_aliases:last(Old);
        true -> Old
      end;
    _ ->
      compile_error(Meta, S#elixir_scope.file,
        "invalid :as for alias, expected a compile time atom or alias")
  end,

  case (New == Old) orelse (length(string:tokens(atom_to_list(New), ".")) == 2) of
    true  -> ok;
    false -> compile_error(Meta, S#elixir_scope.file,
               "invalid :as for alias, nested alias ~s not allowed", [elixir_errors:inspect(New)])
  end,

  { { atom, ?line(Meta), nil }, elixir_aliases:store(Meta, New, Old, S) }.

no_alias_opts(KV) when is_list(KV) ->
  case lists:keyfind(as, 1, KV) of
    { as, As } -> lists:keystore(as, 1, KV, { as, no_alias_expansion(As) });
    false -> KV
  end;

no_alias_opts(KV) -> KV.

no_alias_expansion({ '__aliases__', Meta, [H|T] }) when (H /= 'Elixir') and is_atom(H) ->
  { '__aliases__', Meta, ['Elixir',H|T] };

no_alias_expansion(Other) ->
  Other.

is_atom_tuple({ atom, _, _ }) -> true;
is_atom_tuple(_) -> false.

%% Locals

translate_local(Meta, Name, Args, #elixir_scope{local=nil,function=nil} = S) ->
  compile_error(Meta, S#elixir_scope.file, "function ~ts/~B undefined", [Name, length(Args)]);

translate_local(Meta, Name, Args, #elixir_scope{local=nil,module=Module,function=Function} = S) ->
  elixir_tracker:record_local({ Name, length(Args) }, Module, Function),
  Line = ?line(Meta),
  { TArgs, NS } = translate_args(Args, S),
  { { call, Line, { atom, Line, Name }, TArgs }, NS };

translate_local(Meta, Name, Args, S) ->
  translate_each({ { '.', Meta, [S#elixir_scope.local, Name] }, Meta, Args }, S).

%% Translate args

%% Variables in arguments are not propagated from one
%% argument to the other. For instance:
%%
%%   x = 1
%%   foo(x = x + 2, x)
%%   x
%%
%% Should be the same as:
%%
%%   foo(3, 1)
%%   3
%%
%% However, notice that if we are doing an assignment,
%% it behaves the same as translate.

translate_arg(Arg, { Acc, S }) ->
  { TArg, TAcc } = translate_each(Arg, Acc),
  { TArg, { umergea(Acc, TAcc), umergev(S, TAcc) } }.

translate_args(Args, #elixir_scope{context=match} = S) ->
  translate(Args, S);

translate_args(Args, S) ->
  { TArgs, { SC, SV } } = lists:mapfoldl(fun translate_arg/2, {S, S}, Args),
  { TArgs, umergea(SV, SC) }.

%% Translate apply
%% Used by both apply and external function invocation macros.

translate_apply(Meta, TLeft, TRight, Args, S, SL, SR) ->
  Line = ?line(Meta),

  Optimize = case (Args == []) orelse lists:last(Args) of
    { '|', _, _ } -> false;
    _ ->
      case TRight of
        { atom, _, _ } -> true;
        _ -> false
      end
  end,

  case Optimize of
    true ->
      %% Register the remote
      case TLeft of
        { atom, _, Receiver } ->
          Tuple = { element(3, TRight), length(Args) },
          elixir_tracker:record_remote(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function);
        _ ->
          ok
      end,

      { TArgs, SA } = translate_args(Args, umergec(S, SR)),
      FS = umergev(SL, umergev(SR,SA)),
      { { call, Line, { remote, Line, TLeft, TRight }, TArgs }, FS };
    false ->
      { TArgs, SA } = translate_each(Args, umergec(S, SR)),
      FS = umergev(SL, umergev(SR,SA)),
      { ?wrap_call(Line, erlang, apply, [TLeft, TRight, TArgs]), FS }
  end.

%% __op__ helpers

convert_op('and')  -> 'andalso';
convert_op('or')   -> 'orelse';
convert_op('and!') -> 'and';
convert_op('or!')  -> 'or';
convert_op('!==')  -> '=/=';
convert_op('===')  -> '=:=';
convert_op('!=')   ->  '/=';
convert_op('<=')   ->  '=<';
convert_op('<-')   ->  '!';
convert_op(Else)   ->  Else.

assert_no_ambiguous_op(Name, Meta, [Arg], S) ->
  case lists:keyfind(ambiguous_op, 1, Meta) of
    { ambiguous_op, Kind } ->
      case orddict:find({ Name, Kind }, S#elixir_scope.vars) of
        error -> ok;
        _ ->
          syntax_error(Meta, S#elixir_scope.file, "\"~ts ~ts\" looks like a function call but "
                       "there is a variable named \"~ts\", please use explicit parenthesis or even spaces",
                       [Name, 'Elixir.Macro':to_string(Arg), Name])
      end;
    _ -> ok
  end;

assert_no_ambiguous_op(_Atom, _Meta, _Args, _S) -> ok.

%% Comprehensions

translate_comprehension(Meta, Kind, Args, S) ->
  case elixir_tree_helpers:split_last(Args) of
    { Cases, [{do,Expr}] } ->
      { TCases, SC } = lists:mapfoldl(fun(C, Acc) -> translate_comprehension_clause(Meta, C, Acc) end, S, Cases),
      { TExpr, SE }  = translate_comprehension_do(Meta, Kind, Expr, SC),
      { { Kind, ?line(Meta), TExpr, TCases }, umergec(S, SE) };
    _ ->
      syntax_error(Meta, S#elixir_scope.file, "missing do keyword in comprehension ~s", [Kind])
  end.

translate_comprehension_do(_Meta, bc, { '<<>>', _, _ } = Expr, S) ->
  translate_each(Expr, S);

translate_comprehension_do(Meta, bc, _Expr, S) ->
  syntax_error(Meta, S#elixir_scope.file, "a bit comprehension expects a bit string << >> to be returned");

translate_comprehension_do(_Meta, _Kind, Expr, S) ->
  translate_each(Expr, S).

translate_comprehension_clause(_Meta, {inbits, Meta, [{ '<<>>', _, _} = Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { b_generate, ?line(Meta), TLeft, TRight }, SL };

translate_comprehension_clause(_Meta, {inbits, Meta, [_Left, _Right]}, S) ->
  syntax_error(Meta, S#elixir_scope.file, "a bit comprehension expects a bit string << >> to be used in inbits generators");

translate_comprehension_clause(_Meta, {inlist, Meta, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { generate, ?line(Meta), TLeft, TRight }, SL };

translate_comprehension_clause(Meta, X, S) ->
  Line = ?line(Meta),
  { TX, TS } = translate_each(X, S),
  { BX, BS } = elixir_tree_helpers:convert_to_boolean(Line, TX, true, false, TS),
  { { match, Line, { var, Line, '_' }, BX }, BS }.
