%% Main entry point for translations. Are macros that cannot be
%% overriden are defined in this file.
-module(elixir_translator).
-export([forms/4, 'forms!'/4]).
-export([translate/2, translate_each/2, translate_arg/2,
         translate_args/2, translate_apply/7, translate_fn/3]).
-import(elixir_scope, [umergev/2, umergec/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4, parse_error/4,
  assert_function_scope/3, assert_module_scope/3, assert_no_guard_scope/3,
  assert_no_match_or_guard_scope/3]).
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

translate_each({ '__ambiguousop__', Meta, [Var, H|T] }, S) ->
  { Name, _, Kind } = Var,

  case orddict:find({ Name, Kind }, S#elixir_scope.vars) of
    error -> translate_each({ Name, Meta, [H|T] }, S);
    _ ->
      case T of
        [] -> translate_each(rellocate_ambiguous_op(H, Var), S);
        _  -> syntax_error(Meta, S#elixir_scope.file, "Many arguments given to ~s, but ~s is a variable. Use even spaces to solve ambiguity.", [Name, Name])
      end
  end;

%% Lexical

translate_each({ alias, Meta, [Ref] }, S) ->
  translate_each({ alias, Meta, [Ref,[]] }, S);

translate_each({ alias, Meta, [Ref, KV] }, S) ->
  assert_no_match_or_guard_scope(Meta, alias, S),
  validate_opts(Meta, alias, [as], KV, S),

  { TRef, SR } = translate_each(Ref, S),

  case TRef of
    { atom, _, Old } ->
      { New, SF } = case lists:keyfind(as, 1, KV) of
        Opt when Opt == { as, true }; Opt == false ->
          { elixir_aliases:last(Old), SR };
        { as, false } ->
          { Old, SR };
        { as, Other } ->
          { TOther, SA } = translate_each(no_alias_expansion(Other),
            SR#elixir_scope{aliases=[],macro_aliases=[]}),
          case TOther of
            { atom, _, Atom } -> { Atom, SA };
            _ -> syntax_error(Meta, S#elixir_scope.file,
                   "invalid args for alias, expected an atom or alias as argument")
          end
      end,

      %% Avoid creating aliases if first == last
      %% unecessarily polluting the aliases dict
      case New == Old of
        true  -> { { atom, ?line(Meta), nil }, SF };
        false ->
          case string:tokens(atom_to_list(New), "-") of
            [_,_] -> [];
            _ -> syntax_error(Meta, S#elixir_scope.file,
                   "invalid args for alias, cannot create nested alias ~s", [elixir_errors:inspect(New)])
          end,

          { { atom, ?line(Meta), nil }, SF#elixir_scope{
            aliases=orddict:store(New, Old, S#elixir_scope.aliases),
            macro_aliases=orddict:store(New, Old, S#elixir_scope.macro_aliases)
          } }
      end;
    _ ->
      syntax_error(Meta, S#elixir_scope.file, "invalid args for alias, expected an atom or alias as argument")
  end;

translate_each({ require, Meta, [Ref] }, S) ->
  translate_each({ require, Meta, [Ref, []] }, S);

translate_each({ require, Meta, [Ref, KV] }, S) ->
  assert_no_match_or_guard_scope(Meta, 'require', S),
  validate_opts(Meta, require, [as], KV, S),

  { TRef, SR } = translate_each(Ref, S),

  As = case lists:keyfind(as, 1, KV) of
    false -> false;
    { as, Value } -> Value
  end,

  case TRef of
    { atom, _, Old } ->
      elixir_aliases:ensure_loaded(Meta, Old, SR),

      SF = SR#elixir_scope{
        requires=ordsets:add_element(Old, S#elixir_scope.requires)
      },

      translate_each({ alias, Meta, [Ref, [{ as, As }]] }, SF);
    _ ->
      syntax_error(Meta, S#elixir_scope.file, "invalid args for require, expected an atom or alias as argument")
  end;

translate_each({ import, Meta, [Left] }, S) ->
  translate_each({ import, Meta, [Left, []]}, S);

translate_each({ import, Meta, [Left,Opts] }, S) when is_list(Opts) ->
  translate_each({ import, Meta, [default, Left, Opts]}, S);

translate_each({ import, Meta, [Selector, Left] }, S) ->
  translate_each({ import, Meta, [Selector, Left, []]}, S);

translate_each({ import, Meta, [Left, Right, Opts] }, S) ->
  assert_no_match_or_guard_scope(Meta, 'import', S),
  { TSelector, SL } = translate_each(Left, S),
  { TRef, SR } = translate_each(Right, SL),

  Selector = case TSelector of
    { atom, _,  SelectorAtom } -> SelectorAtom;
    _ -> syntax_error(Meta, S#elixir_scope.file, "invalid selector for import, expected an atom")
  end,

  Ref = case TRef of
    { atom, _, RefAtom } -> RefAtom;
    _ -> syntax_error(Meta, S#elixir_scope.file, "invalid name for import, expected an atom or alias")
  end,

  validate_opts(Meta, import, [as, only, except, warn], Opts, S),

  As = case lists:keyfind(as, 1, Opts) of
    false -> false;
    { as, Value } -> Value
  end,

  elixir_aliases:ensure_loaded(Meta, Ref, SR),
  SF = elixir_import:import(Meta, Ref, Opts, Selector, SR),
  translate_each({ require, Meta, [Ref, [{ as, As }]] }, SF);

%% Pseudo variables

translate_each({ '__MODULE__', Meta, Atom }, S) when is_atom(Atom) ->
  { { atom, ?line(Meta), S#elixir_scope.module }, S };

translate_each({ '__FILE__', _Meta, Atom }, S) when is_atom(Atom) ->
  translate_each(S#elixir_scope.file, S);

translate_each({ '__DIR__', _Meta, Atom }, S) when is_atom(Atom) ->
  translate_each(filename:dirname(S#elixir_scope.file), S);

translate_each({ '__ENV__', Meta, Atom }, S) when is_atom(Atom) ->
  Env = elixir_scope:to_ex_env({ ?line(Meta), S }),
  { elixir_tree_helpers:abstract_syntax(Env), S };

translate_each({ '__CALLER__', Meta, Atom }, S) when is_atom(Atom) ->
  { { var, ?line(Meta), '__CALLER__' }, S#elixir_scope{caller=true} };

%% Aliases

translate_each({ '__aliases__', Meta, _ } = Alias, S) ->
  case elixir_aliases:expand(Alias, S#elixir_scope.aliases, S#elixir_scope.macro_aliases) of
    Atom when is_atom(Atom) -> { { atom, ?line(Meta), Atom }, S };
    Aliases ->
      { TAliases, SA } = translate_args(Aliases, S),

      case lists:all(fun is_atom_tuple/1, TAliases) of
        true ->
          Atoms = [Atom || { atom, _, Atom } <- TAliases],
          { { atom, ?line(Meta), elixir_aliases:concat(Atoms) }, SA };
        false ->
          Args = [elixir_tree_helpers:list_to_cons(?line(Meta), TAliases)],
          { ?wrap_call(?line(Meta), elixir_aliases, concat, Args), SA }
      end
  end;

%% Quoting

translate_each({ Unquote, Meta, [_|_] }, S) when Unquote == unquote; Unquote == unquote_splicing ->
  syntax_error(Meta, S#elixir_scope.file, "~p called outside quote", [Unquote]);

translate_each({ quote, Meta, [Left, Right] }, S) ->
  translate_each({ quote, Meta, [orddict:from_list(Left ++ Right)] }, S);

translate_each({ quote, Meta, [T] }, S) when is_list(T) ->
  Exprs =
    case lists:keyfind(do, 1, T) of
      { do, E } -> E;
      false -> syntax_error(Meta, S#elixir_scope.file, "invalid args for quote")
    end,

  Hygiene = case lists:keyfind(hygiene, 1, T) of
    { hygiene, List } when is_list(List) ->
      List;
    false ->
      []
  end,

  Vars = case lists:keyfind(var_context, 1, T) of
    { var_context, VarContext } ->
      expand_var_context(Meta, VarContext, "invalid argument given for var_context in quote", S);
    false ->
      case lists:keyfind(vars, 1, Hygiene) of
        { vars, false } -> nil;
        _ ->
          case S#elixir_scope.module of
            nil -> 'Elixir';
            Mod -> Mod
          end
      end
  end,

  Aliases = lists:keyfind(aliases, 1, Hygiene) /= { aliases, false },
  Imports = lists:keyfind(imports, 1, Hygiene) /= { imports, false },

  { DefaultLine, DefaultFile } = case lists:keyfind(location, 1, T) of
    { location, keep } -> { keep, keep };
    false -> { nil, nil }
  end,

  Line = case lists:keyfind(line, 1, T) of
    { line, LineValue } -> LineValue;
    false -> DefaultLine
  end,

  File = case lists:keyfind(file, 1, T) of
    { file, FileValue } -> FileValue;
    false -> DefaultFile
  end,

  TFile = case File of
    keep -> S#elixir_scope.file;
    _    -> File
  end,

  WExprs = if
    is_binary(TFile) ->
      { '__scope__', Meta, [[{file,TFile}],[{do,Exprs}]] };
    File == nil ->
      Exprs;
    true ->
      syntax_error(Meta, S#elixir_scope.file, "invalid args for quote, expected :file to be a binary")
  end,

  Unquote = case lists:keyfind(unquote, 1, T) of
    { unquote, false } -> false;
    _ -> true
  end,

  Q = #elixir_quote{vars_hygiene=Vars, line=Line, unquote=Unquote,
        aliases_hygiene=Aliases, imports_hygiene=Imports},

  { TExprs, _TQ, TS } = elixir_quote:erl_quote(WExprs, Q, S),
  { TExprs, TS };

translate_each({ quote, Meta, [_] }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid args for quote");

translate_each({ 'alias!', _Meta, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ 'var!', Meta, [Arg] }, S) ->
  translate_each({ 'var!', Meta, [Arg, nil] }, S);

translate_each({ 'var!', Meta, [{Name, _, Atom}, Kind] }, S) when is_atom(Name), is_atom(Atom) ->
  translate_each({ 'var!', Meta, [Name, Kind] }, S);

translate_each({ 'var!', Meta, [Name, Kind] }, S) when is_atom(Name) ->
  Expanded = expand_var_context(Meta, Kind, "invalid second argument for var!", S),
  elixir_scope:translate_var(Meta, Name, Expanded, S, fun() ->
    syntax_error(Meta, S#elixir_scope.file, "expected var!(~ts) to expand to an existing variable or be a part of a match", [Name])
  end);

translate_each({ 'var!', Meta, [_, _] }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid first argument for var!, expected a var or an atom");

%% Functions

translate_each({ fn, Meta, [[{do, { '->', _, Pairs }}]] }, S) ->
  assert_no_match_or_guard_scope(Meta, 'fn', S),
  translate_fn(Meta, Pairs, S);

%% Comprehensions

translate_each({ Kind, Meta, Args }, S) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  translate_comprehension(Meta, Kind, Args, S);

%% Super (exceptionally supports partial application)

translate_each({ super, Meta, Args } = Original, S) ->
  case elixir_partials:handle(Original, S) of
    error ->
      assert_no_match_or_guard_scope(Meta, super, S),
      Module = assert_module_scope(Meta, super, S),
      Function = assert_function_scope(Meta, super, S),
      elixir_def_overridable:ensure_defined(Meta, Module, Function, S),

      { _, Arity } = Function,

      { TArgs, TS } = if
        is_atom(Args) ->
          elixir_def_overridable:retrieve_args(Meta, Arity, S);
        length(Args) == Arity ->
          translate_args(Args, S);
        true ->
          syntax_error(Meta, S#elixir_scope.file, "super must be called with the same number of arguments as the current function")
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

translate_each({ '^', Meta, [ { Name, _, Kind } ] }, #elixir_scope{context=match} = S) when is_atom(Name), is_atom(Kind) ->
  case orddict:find({ Name, Kind }, S#elixir_scope.vars) of
    { ok, Value } ->
      { { var, Meta, Value }, S };
    error ->
      syntax_error(Meta, S#elixir_scope.file, "unbound variable ^~ts", [Name])
  end;

translate_each({ '^', Meta, [ { Name, _, Kind } ] }, S) when is_atom(Name), is_atom(Kind) ->
  syntax_error(Meta, S#elixir_scope.file,
    "cannot use ^~ts outside of match clauses", [Name]);

translate_each({ '^', Meta, [ Expr ] }, S) ->
  syntax_error(Meta, S#elixir_scope.file,
    "the unary operator ^ can only be used with variables, invalid expression ^~ts", ['Elixir.Macro':to_binary(Expr)]);

translate_each({ Name, Meta, Kind }, S) when is_atom(Name), is_atom(Kind) ->
  elixir_scope:translate_var(Meta, Name, Kind, S, fun() ->
    translate_each({ Name, Meta, [] }, S)
  end);

%% Local calls

translate_each({ Atom, Meta, Args } = Original, S) when is_atom(Atom) ->
  case elixir_partials:is_sequential(Args) andalso
       elixir_dispatch:import_function(Meta, Atom, length(Args), S) of
    false ->
      case elixir_partials:handle(Original, S) of
        error ->
          Callback = fun() ->
            case S#elixir_scope.context of
              match ->
                Arity = length(Args),
                File  = S#elixir_scope.file,
                case Arity of
                  0 -> syntax_error(Meta, File, "unknown variable ~ts or cannot invoke local ~ts/~B inside guard", [Atom, Atom, Arity]);
                  _ -> syntax_error(Meta, File, "cannot invoke local ~ts/~B inside guard", [Atom, Arity])
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
              elixir_dispatch:dispatch_require(Meta, Receiver, Right, Args, umergev(SL, SR), Callback);
            _ ->
              Callback()
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
    ['Elixir.Macro':to_binary(Invalid)]);

%% Literals

translate_each(Literal, S) ->
  elixir_literal:translate(Literal, S).

%% Helpers

validate_opts(Meta, Kind, Allowed, Opts, S) when is_list(Opts) ->
  [begin
    syntax_error(Meta, S#elixir_scope.file, "unsupported option ~s given to ~s", [Key, Kind])
  end || { Key, _ } <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, _Opts, S) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid options for ~s, expected a keyword list", [Kind]).

translate_fn(Meta, Clauses, S) ->
  Line = ?line(Meta),

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
      syntax_error(Meta, S#elixir_scope.file, "cannot mix clauses with different arities in function definition")
  end.

translate_local(Meta, Name, Args, #elixir_scope{local=nil,module=Module} = S) ->
  elixir_import:record({ Name, length(Args) }, Module, Module),
  Line = ?line(Meta),
  { TArgs, NS } = translate_args(Args, S),
  { { call, Line, { atom, Line, Name }, TArgs }, NS };

translate_local(Meta, Name, Args, S) ->
  translate_each({ { '.', Meta, [S#elixir_scope.local, Name] }, Meta, Args }, S).

expand_var_context(_Meta, Atom, _Msg, _S) when is_atom(Atom) -> Atom;
expand_var_context(Meta, Alias, Msg, S) ->
  case translate_each(Alias, S) of
    { { atom, _, Atom }, _ } ->
      Atom;
    _ ->
      syntax_error(Meta, S#elixir_scope.file, "~ts, expected a compile time available alias or an atom", [Msg])
  end.

no_alias_expansion({ '__aliases__', Meta, [H|T] }) when (H /= 'Elixir') and is_atom(H) ->
  { '__aliases__', Meta, ['Elixir',H|T] };

no_alias_expansion(Other) ->
  Other.

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
  { TArg, { umergec(Acc, TAcc), umergev(S, TAcc) } }.

translate_args(Args, #elixir_scope{context=match} = S) ->
  translate(Args, S);

translate_args(Args, S) ->
  { TArgs, { SC, SV } } = lists:mapfoldl(fun translate_arg/2, {S, S}, Args),
  { TArgs, umergec(SV, SC) }.

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

is_atom_tuple({ atom, _, _ }) -> true;
is_atom_tuple(_) -> false.

rellocate_ambiguous_op({ Op, Meta, [Expr] }, Var) when Op == '+'; Op == '-' ->
  { Op, Meta, [Var, Expr] };

rellocate_ambiguous_op({ Call, Meta, [H|T] }, Var) ->
  { Call, Meta, [rellocate_ambiguous_op(H, Var)|T] }.

%% Comprehensions

translate_comprehension(Meta, Kind, Args, S) ->
  case elixir_tree_helpers:split_last(Args) of
    { Cases, [{do,Expr}] } ->
      { TCases, SC } = lists:mapfoldl(fun(C, Acc) -> translate_comprehension_clause(Meta, C, Acc) end, S, Cases),
      { TExpr, SE }  = translate_comprehension_do(Meta, Kind, Expr, SC),
      { { Kind, ?line(Meta), TExpr, TCases }, umergec(S, SE) };
    _ ->
      syntax_error(Meta, S#elixir_scope.file, "keyword argument :do missing for comprehension ~s", [Kind])
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
  { TX, TS } = translate_each(X, S),
  elixir_tree_helpers:convert_to_boolean(?line(Meta), TX, true, false, TS).
