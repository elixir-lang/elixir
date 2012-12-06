%% Main entry point for translations. Are macros that cannot be
%% overriden are defined in this file.
-module(elixir_translator).
-export([forms/4, 'forms!'/4]).
-export([translate/2, translate_each/2, translate_arg/2,
         translate_args/2, translate_apply/7, translate_fn/3]).
-import(elixir_scope, [umergev/2, umergec/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4, parse_error/4,
  assert_function_scope/3, assert_module_scope/3, assert_no_guard_scope/3,
  assert_no_assign_or_guard_scope/3]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

forms(String, StartLine, File, Opts) ->
  try elixir_tokenizer:tokenize(String, StartLine, [{ file, File }|Opts]) of
    { ok, Tokens } ->
      case elixir_parser:parse(Tokens) of
        { ok, Forms } -> { ok, Forms };
        { error, { Line, _, [Error, Token] } } -> { error, { Line, Error, Token } }
      end;
    { error, { _, _, _ } } = Else -> Else
  catch
    { interpolation_error, { _, _, _ } = Tuple} -> { error, Tuple }
  end.

'forms!'(String, StartLine, File, Opts) ->
  case forms(String, StartLine, File, Opts) of
    { ok, Forms } -> Forms;
    { error, { Line, Error, Token } } -> parse_error(Line, File, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Those macros are "low-level". They are the basic mechanism
%% that makes the language work and cannot be partially applied
%% nor overwritten.

%% Assignment operator

translate_each({'=', Line, [Left, Right]}, S) ->
  assert_no_guard_scope(Line, '=', S),
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL } = elixir_clauses:assigns(fun translate_each/2, Left, SR),
  { { match, Line, TLeft, TRight }, SL };

%% Containers

translate_each({ C, _, _ } = Original, S) when C == '[]'; C == '{}'; C == '<<>>' ->
  elixir_literal:translate(Original, S);

%% Blocks and scope rewriters

translate_each({ '__block__', Line, [] }, S) ->
  { { atom, Line, nil }, S };

translate_each({ '__block__', _Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ '__block__', Line, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, Line, TArgs }, NS };

translate_each({ '__scope__', _Line, [[{file,File}],[{do,Expr}]] }, S) ->
  Old = S#elixir_scope.file,
  { TExpr, TS } = translate_each(Expr, S#elixir_scope{file=File}),
  { TExpr, TS#elixir_scope{file=Old} };

%% Erlang op

translate_each({ '__op__', Line, [Op, Expr] }, S) when is_atom(Op) ->
  { TExpr, NS } = translate_each(Expr, S),
  { { op, Line, convert_op(Op), TExpr }, NS };

translate_each({ '__op__', Line, [Op, Left, Right] }, S) when is_atom(Op) ->
  { [TLeft, TRight], NS }  = translate_args([Left, Right], S),
  { { op, Line, convert_op(Op), TLeft, TRight }, NS };

translate_each({ '__ambiguousop__', Line, [Var, H|T] }, S) ->
  { Name, _, Kind } = Var,

  case orddict:find({ Name, Kind }, S#elixir_scope.vars) of
    error -> translate_each({ Name, Line, [H|T] }, S);
    _ ->
      case T of
        [] -> translate_each(rellocate_ambiguous_op(H, Var), S);
        _  -> syntax_error(Line, S#elixir_scope.file, "Many arguments given to ~s, but ~s is a variable. Use even spaces to solve ambiguity.", [Name, Name])
      end
  end;

%% Lexical

translate_each({ alias, Line, [Ref] }, S) ->
  translate_each({ alias, Line, [Ref,[]] }, S);

translate_each({ alias, Line, [Ref, KV] }, S) ->
  assert_no_assign_or_guard_scope(Line, alias, S),
  validate_opts(Line, alias, [as], KV, S),

  { TRef, SR } = translate_each(Ref, S),

  case TRef of
    { atom, _, Old } ->
      { New, SF } = case lists:keyfind(as, 1, KV) of
        Opt when Opt == { as, true }; Opt == false ->
          { elixir_aliases:last(Old), SR };
        { as, false } ->
          { Old, SR };
        { as, Other } ->
          { TOther, SA } = translate_each(Other, SR#elixir_scope{aliases=[]}),
          case TOther of
            { atom, _, Atom } -> { Atom, SA };
            _ -> syntax_error(Line, S#elixir_scope.file,
                   "invalid args for alias, expected an atom or alias as argument")
          end
      end,

      %% Avoid creating aliases if first == last
      %% unecessarily polluting the aliases dict
      case New == Old of
        true  -> { { nil, Line }, SF };
        false ->
          case string:tokens(atom_to_list(New), "-") of
            [_,_] -> [];
            _ -> syntax_error(Line, S#elixir_scope.file,
                   "invalid args for alias, cannot create nested alias ~s", [elixir_errors:inspect(New)])
          end,

          { { nil, Line }, SF#elixir_scope{
            aliases=orddict:store(New, Old, S#elixir_scope.aliases)
          } }
      end;
    _ ->
      syntax_error(Line, S#elixir_scope.file, "invalid args for alias, expected an atom or alias as argument")
  end;

translate_each({ require, Line, [Ref] }, S) ->
  translate_each({ require, Line, [Ref, []] }, S);

translate_each({ require, Line, [Ref, KV] }, S) ->
  assert_no_assign_or_guard_scope(Line, 'require', S),
  validate_opts(Line, require, [as], KV, S),

  { TRef, SR } = translate_each(Ref, S),

  As = case lists:keyfind(as, 1, KV) of
    false -> false;
    { as, Value } -> Value
  end,

  case TRef of
    { atom, _, Old } ->
      elixir_aliases:ensure_loaded(Line, Old, SR),

      SF = SR#elixir_scope{
        requires=ordsets:add_element(Old, S#elixir_scope.requires)
      },

      translate_each({ alias, Line, [Ref, [{ as, As }]] }, SF);
    _ ->
      syntax_error(Line, S#elixir_scope.file, "invalid args for require, expected an atom or alias as argument")
  end;

translate_each({ import, Line, [Left] }, S) ->
  translate_each({ import, Line, [Left, []]}, S);

translate_each({ import, Line, [Left,Opts] }, S) when is_list(Opts) ->
  translate_each({ import, Line, [default, Left, Opts]}, S);

translate_each({ import, Line, [Selector, Left] }, S) ->
  translate_each({ import, Line, [Selector, Left, []]}, S);

translate_each({ import, Line, [Left, Right, Opts] }, S) ->
  assert_no_assign_or_guard_scope(Line, 'import', S),
  { TSelector, SL } = translate_each(Left, S),
  { TRef, SR } = translate_each(Right, SL),

  Selector = case TSelector of
    { atom, _,  SelectorAtom } -> SelectorAtom;
    _ -> syntax_error(Line, S#elixir_scope.file, "invalid selector for import, expected an atom")
  end,

  Ref = case TRef of
    { atom, _, RefAtom } -> RefAtom;
    _ -> syntax_error(Line, S#elixir_scope.file, "invalid name for import, expected an atom or alias")
  end,

  validate_opts(Line, import, [as, only, except], Opts, S),

  As = case lists:keyfind(as, 1, Opts) of
    false -> false;
    { as, Value } -> Value
  end,

  elixir_aliases:ensure_loaded(Line, Ref, SR),
  SF = elixir_import:import(Line, Ref, Opts, Selector, SR),
  translate_each({ require, Line, [Ref, [{ as, As }]] }, SF);

%% Pseudo variables

translate_each({ '__MODULE__', Line, Atom }, S) when is_atom(Atom) ->
  { { atom, Line, S#elixir_scope.module }, S };

translate_each({ '__FILE__', _Line, Atom }, S) when is_atom(Atom) ->
  translate_each(S#elixir_scope.file, S);

translate_each({ '__ENV__', Line, Atom }, S) when is_atom(Atom) ->
  { elixir_scope:to_erl_env({ Line, S }), S };

translate_each({ '__CALLER__', Line, Atom }, S) when is_atom(Atom) ->
  { { var, Line, '__CALLER__' }, S#elixir_scope{caller=true} };

%% Aliases

translate_each({ '__aliases__', Line, [H] }, S) when H /= 'Elixir'  ->
  Atom = list_to_atom("Elixir-" ++ atom_to_list(H)),
  { { atom, Line, elixir_aliases:lookup(Atom, S#elixir_scope.aliases) }, S };

translate_each({ '__aliases__', Line, [H|T] }, S) ->
  Aliases = if
    is_atom(H) andalso (H /= 'Elixir') ->
      Atom = list_to_atom("Elixir-" ++ atom_to_list(H)),
      [elixir_aliases:lookup(Atom, S#elixir_scope.aliases)|T];
    true ->
      [H|T]
  end,

  { TAliases, SA } = translate_args(Aliases, S),

  case lists:all(fun is_atom_tuple/1, TAliases) of
    true ->
      Atoms = [Atom || { atom, _, Atom } <- TAliases],
      { { atom, Line, elixir_aliases:concat(Atoms) }, SA };
    false ->
      Args = [elixir_tree_helpers:build_simple_list(Line, TAliases)],
      { ?ELIXIR_WRAP_CALL(Line, elixir_aliases, concat, Args), SA }
  end;

%% Quoting

translate_each({ Unquote, Line, _Args }, S) when Unquote == unquote; Unquote == unquote_splicing ->
  syntax_error(Line, S#elixir_scope.file, "~p called outside quote", [Unquote]);

translate_each({ quote, Line, [Left, Right] }, S) ->
  translate_each({ quote, Line, [orddict:from_list(Left ++ Right)] }, S);

translate_each({ quote, GivenLine, [T] }, S) when is_list(T) ->
  Exprs =
    case lists:keyfind(do, 1, T) of
      { do, E } -> E;
      false ->
        syntax_error(GivenLine, S#elixir_scope.file, "invalid args for quote")
    end,

  Marker = case lists:keyfind(hygiene, 1, T) of
    { hygiene, false } -> nil;
    _ -> quoted
  end,

  { DefaultLine, DefaultFile } = case lists:keyfind(location, 1, T) of
    { location, keep }  -> { keep, keep };
    _ -> { 0, nil }
  end,

  Line = case lists:keyfind(line, 1, T) of
    { line, LineValue } -> LineValue;
    _ -> DefaultLine
  end,

  { TLine, SL } = case Line of
    keep -> { keep, S };
    _    -> translate_each(Line, S)
  end,

  File = case lists:keyfind(file, 1, T) of
    { file, FileValue } -> FileValue;
    _ -> DefaultFile
  end,

  TFile = case File of
    keep -> S#elixir_scope.file;
    _    -> File
  end,

  TExprs = if
    is_binary(TFile) ->
      { '__scope__', GivenLine, [[{file,TFile}],[{do,Exprs}]] };
    File == nil ->
      Exprs;
    true ->
      syntax_error(GivenLine, S#elixir_scope.file, "invalid args for quote, expected :file to be a binary")
  end,

  Unquote = case lists:keyfind(unquote, 1, T) of
    { unquote, false } -> false;
    _ -> true
  end,

  elixir_quote:quote(TExprs, #elixir_quote{marker=Marker, line=TLine, unquote=Unquote}, SL);

translate_each({ quote, GivenLine, [_] }, S) ->
  syntax_error(GivenLine, S#elixir_scope.file, "invalid args for quote");

%% Functions

translate_each({ fn, Line, [[{do, { '->', _, Pairs }}]] }, S) ->
  assert_no_assign_or_guard_scope(Line, 'fn', S),
  translate_fn(Line, Pairs, S);

%% Comprehensions

translate_each({ Kind, Line, Args }, S) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  translate_comprehension(Line, Kind, Args, S);

%% Super (exceptionally supports partial application)

translate_each({ super, Line, Args } = Original, S) ->
  case elixir_partials:handle(Original, S) of
    error ->
      assert_no_assign_or_guard_scope(Line, super, S),
      Module = assert_module_scope(Line, super, S),
      Function = assert_function_scope(Line, super, S),
      elixir_def_overridable:ensure_defined(Line, Module, Function, S),

      { _, Arity } = Function,

      { TArgs, TS } = if
        is_atom(Args) ->
          elixir_def_overridable:retrieve_args(Line, Arity, S);
        length(Args) == Arity ->
          translate_args(Args, S);
        true ->
          syntax_error(Line, S#elixir_scope.file, "super must be called with the same number of arguments as the current function")
      end,

      Super = elixir_def_overridable:name(Module, Function),
      { { call, Line, { atom, Line, Super }, TArgs }, TS#elixir_scope{super=true} };
    Else -> Else
  end;

translate_each({ 'super?', Line, [] }, S) ->
  Module = assert_module_scope(Line, 'super?', S),
  Function = assert_function_scope(Line, 'super?', S),
  Bool = elixir_def_overridable:is_defined(Module, Function),
  { { atom, Line, Bool }, S };

%% Variables

translate_each({ '^', Line, [ { Name, _, Kind } ] }, S) when is_list(Kind) ->
  syntax_error(Line, S#elixir_scope.file, "cannot use ^ with expression at ^~s, ^ must be used only with variables", [Name]);

translate_each({ '^', Line, [ { Name, _, Kind } ] }, #elixir_scope{context=assign} = S) when is_atom(Kind) ->
  case orddict:find({ Name, Kind }, S#elixir_scope.vars) of
    { ok, Value } ->
      { { var, Line, Value }, S };
    error ->
      syntax_error(Line, S#elixir_scope.file, "unbound variable ^~s", [Name])
  end;

translate_each({ '^', Line, [ { Name, _, Kind } ] }, S) when is_atom(Kind) ->
  syntax_error(Line, S#elixir_scope.file,
    "cannot access variable ^~s outside of assignment", [Name]);

translate_each({ Name, Line, Kind }, S) when is_atom(Name), (Kind == nil orelse Kind == quoted) ->
  elixir_scope:translate_var(Line, Name, Kind, S);

%% Local calls

translate_each({ Atom, Line, Args } = Original, S) when is_atom(Atom) ->
  case elixir_partials:is_sequential(Args) andalso
       elixir_dispatch:import_function(Line, Atom, length(Args), S) of
    false ->
      case elixir_partials:handle(Original, S) of
        error ->
          Callback = fun() ->
            case S#elixir_scope.context of
              guard ->
                Arity = length(Args),
                File  = S#elixir_scope.file,
                case Arity of
                  0 -> syntax_error(Line, File, "unknown variable ~s or cannot invoke local ~s/~B inside guard", [Atom, Atom, Arity]);
                  _ -> syntax_error(Line, File, "cannot invoke local ~s/~B inside guard", [Atom, Arity])
                end;
              _ -> translate_local(Line, Atom, Args, S)
            end
          end,
          elixir_dispatch:dispatch_import(Line, Atom, Args, S, Callback);
        Else  -> Else
      end;
    Else -> Else
  end;

%% Remote calls

translate_each({ { '.', _, [Left, Right] }, Line, Args } = Original, S) when is_atom(Right) ->
  { TLeft,  SL } = translate_each(Left, S),

  Fun = (element(1, TLeft) == atom) andalso elixir_partials:is_sequential(Args) andalso
    elixir_dispatch:require_function(Line, element(3, TLeft), Right, length(Args), SL),

  case Fun of
    false ->
      case elixir_partials:handle(Original, S) of
        error ->
          { TRight, SR } = translate_each(Right, umergec(S, SL)),
          Callback = fun() -> translate_apply(Line, TLeft, TRight, Args, S, SL, SR) end,

          case TLeft of
            { atom, _, Receiver } ->
              elixir_dispatch:dispatch_require(Line, Receiver, Right, Args, umergev(SL, SR), Callback);
            _ ->
              Callback()
          end;
        Else -> Else
      end;
    Else -> Else
  end;

%% Anonymous function calls

translate_each({ { '.', _, [Expr] }, Line, Args } = Original, S) ->
  { TExpr, SE } = translate_each(Expr, S),
  case TExpr of
    { atom, _, Atom } ->
      translate_each({ Atom, Line, Args }, S);
    _ ->
      case elixir_partials:handle(Original, S) of
        error ->
          { TArgs, SA } = translate_args(Args, umergec(S, SE)),
          { {call, Line, TExpr, TArgs}, umergev(SE, SA) };
        Else -> Else
      end
  end;

%% Invalid calls

translate_each({ Invalid, Line, _Args }, S) ->
  syntax_error(Line, S#elixir_scope.file, "unexpected parenthesis after ~ts",
    ['Elixir.Macro':to_binary(Invalid)]);

%% Literals

translate_each(Literal, S) ->
  elixir_literal:translate(Literal, S).

%% Helpers

validate_opts(Line, Kind, Allowed, Opts, S) when is_list(Opts) ->
  [begin
    syntax_error(Line, S#elixir_scope.file, "unsupported option ~s given to ~s", [Key, Kind])
  end || { Key, _ } <- Opts, not lists:member(Key, Allowed)];

validate_opts(Line, Kind, _Allowed, _Opts, S) ->
  syntax_error(Line, S#elixir_scope.file, "invalid options for ~s, expected a keyword list", [Kind]).

translate_fn(Line, Clauses, S) ->
  Transformer = fun({ ArgsWithGuards, Expr }, Acc) ->
    { Args, Guards } = elixir_clauses:extract_last_guards(ArgsWithGuards),
    elixir_clauses:assigns_block(Line, fun elixir_translator:translate/2, Args, [Expr], Guards, umergec(S, Acc))
  end,

  { TClauses, NS } = lists:mapfoldl(Transformer, S, Clauses),
  Arities = [length(Args) || { clause, _Line, Args, _Guards, _Exprs } <- TClauses],

  case length(lists:usort(Arities)) of
    1 ->
      { { 'fun', Line, { clauses, TClauses } }, umergec(S, NS) };
    _ ->
      syntax_error(Line, S#elixir_scope.file, "cannot mix clauses with different arities in function definition")
  end.

translate_local(Line, Name, Args, #elixir_scope{local=nil} = S) ->
  { TArgs, NS } = translate_args(Args, S),
  { { call, Line, { atom, Line, Name }, TArgs }, NS };

translate_local(Line, Name, Args, S) ->
  { TArgs, NS } = translate_args(Args, S),
  Remote = { remote, Line,
    { atom, Line, S#elixir_scope.local },
    { atom, Line, Name }
  },
  { { call, Line, Remote, TArgs }, NS }.

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
  { TArg, { umergec(S, TAcc), umergev(S, TAcc) } }.

translate_args(Args, #elixir_scope{context=assign} = S) ->
  translate(Args, S);

translate_args(Args, S) ->
  { TArgs, { SC, SV } } = lists:mapfoldl(fun translate_arg/2, {S, S}, Args),
  { TArgs, umergec(SV, SC) }.

%% Translate apply
%% Used by both apply and external function invocation macros.

translate_apply(Line, TLeft, TRight, Args, S, SL, SR) ->
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
      { TArgs, SA } = translate_args(Args, umergec(S, SR)),
      FS = umergev(SL, umergev(SR,SA)),
      { { call, Line, { remote, Line, TLeft, TRight }, TArgs }, FS };
    false ->
      { TArgs, SA } = translate_each(Args, umergec(S, SR)),
      FS = umergev(SL, umergev(SR,SA)),
      { ?ELIXIR_WRAP_CALL(Line, erlang, apply, [TLeft, TRight, TArgs]), FS }
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

is_atom_tuple({ atom, _, _ }) -> true;
is_atom_tuple(_) -> false.

rellocate_ambiguous_op({ Op, Line, [Expr] }, Var) when Op == '+'; Op == '-' ->
  { Op, Line, [Var, Expr] };

rellocate_ambiguous_op({ Call, Line, [H|T] }, Var) ->
  { Call, Line, [rellocate_ambiguous_op(H, Var)|T] }.

%% Comprehensions

translate_comprehension(Line, Kind, Args, S) ->
  case elixir_tree_helpers:split_last(Args) of
    { Cases, [{do,Expr}] } ->
      { TCases, SC } = lists:mapfoldl(fun(C, Acc) -> translate_comprehension_clause(Line, C, Acc) end, S, Cases),
      { TExpr, SE }  = translate_comprehension_do(Line, Kind, Expr, SC),
      { { Kind, Line, TExpr, TCases }, umergec(S, SE) };
    _ ->
      syntax_error(Line, S#elixir_scope.file, "keyword argument :do missing for comprehension ~s", [Kind])
  end.

translate_comprehension_do(_Line, bc, { '<<>>', _, _ } = Expr, S) ->
  translate_each(Expr, S);

translate_comprehension_do(Line, bc, _Expr, S) ->
  syntax_error(Line, S#elixir_scope.file, "a bit comprehension expects a bit string << >> to be returned");

translate_comprehension_do(_Line, _Kind, Expr, S) ->
  translate_each(Expr, S).

translate_comprehension_clause(_Line, {inbits, Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { b_generate, Line, TLeft, TRight }, SL };

translate_comprehension_clause(_Line, {inlist, Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { generate, Line, TLeft, TRight }, SL };

translate_comprehension_clause(Line, X, S) ->
  { TX, TS } = translate_each(X, S),
  elixir_tree_helpers:convert_to_boolean(Line, TX, true, false, TS).
