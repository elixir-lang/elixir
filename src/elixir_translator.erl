%% Main entry point for translations. Are macros that cannot be
%% overriden are defined in this file.
-module(elixir_translator).
-export([translate/2, translate_each/2, translate_args/2, translate_apply/7, forms/3]).
-import(elixir_variables, [umergev/2, umergec/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4, parse_error/4, assert_function_scope/3, assert_module_scope/3]).
-include("elixir.hrl").

forms(String, StartLine, Filename) ->
  try elixir_tokenizer:tokenize(String, StartLine) of
    {ok, Tokens} ->
      case elixir_parser:parse(Tokens) of
        {ok, Forms} -> Forms;
        {error, {Line, _, [Error, Token]}} -> parse_error(Line, Filename, Error, Token)
      end;
    {error, {Line, Error, Token}} -> parse_error(Line, Filename, Error, Token)
  catch
    {interpolation_error, {Line, Error, Token}} -> parse_error(Line, Filename, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Those macros are "low-level". They are the basic mechanism
%% that makes the language work and cannot be partially applied
%% nor overwritten.

%% Assignment operator

translate_each({'=', Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL } = elixir_clauses:assigns(fun translate_each/2, Left, SR),
  { { match, Line, TLeft, TRight }, SL };

%% Blocks

translate_each({ '__block__', Line, [] }, S) ->
  { { atom, Line, nil }, S };

translate_each({ '__block__', _Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ '__block__', Line, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, Line, TArgs }, NS };

translate_each({ '__kvblock__', _, [{[Expr],nil}] }, S) ->
  translate_each(Expr, S);

translate_each({ '__kvblock__', Line, Args }, S) when is_list(Args) ->
  case S#elixir_scope.macro of
    { Receiver, Name, Arity } ->
      Desc = [elixir_errors:inspect(Receiver), Name, Arity],
      syntax_error(Line, S#elixir_scope.filename, "key value blocks not supported by ~s.~s/~B", Desc);
    _ ->
      % TODO: This shuold be raised at runtime
      syntax_error(Line, S#elixir_scope.filename, "unhandled key value blocks", "")
  end;

%% Erlang op

translate_each({ '__op__', Line, [Op, Expr] }, S) when is_atom(Op) ->
  { TExpr, NS } = translate_each(Expr, S),
  { { op, Line, convert_op(Op), TExpr }, NS };

translate_each({ '__op__', Line, [Op|Args] }, S) when is_atom(Op) ->
  { [TLeft, TRight], NS }  = translate_args(Args, S),
  { { op, Line, convert_op(Op), TLeft, TRight }, NS };

%% Containers

translate_each({ '<<>>', Line, Args }, S) when is_list(Args) ->
  case S#elixir_scope.assign of
    true ->
      elixir_tree_helpers:build_bitstr(fun translate_each/2, Args, Line, S);
    false ->
      { TArgs, { SC, SV } } = elixir_tree_helpers:build_bitstr(fun translate_arg/2, Args, Line, { S, S }),
      { TArgs, umergec(SV, SC) }
  end;

translate_each({'{}', Line, Args}, S) when is_list(Args) ->
  { TArgs, SE } = translate_args(Args, S),
  { {tuple, Line, TArgs}, SE };

translate_each({'[]', _Line, Args}, S) when is_list(Args) ->
  translate_each(Args, S);

%% Lexical

translate_each({refer, Line, [Ref|T]}, S) ->
  KV = case T of
    [NotEmpty] -> NotEmpty;
    [] -> []
  end,

  { TRef, SR } = translate_each(Ref, S),

  case TRef of
    { atom, _, Old } ->
      { New, SF } = case orddict:find(as, KV) of
        { ok, false } ->
          { Old, SR };
        { ok, true } ->
          { elixir_ref:last(Old), SR };
        { ok, Other } ->
          { TOther, SA } = translate_each(Other, SR),
          case TOther of
            { atom, _, Atom } -> { Atom, SA };
            _ -> syntax_error(Line, S#elixir_scope.filename, "invalid args for refer, expected a reference as argument")
          end;
        error ->
          { elixir_ref:last(Old), SR }
      end,

      { { nil, Line }, SF#elixir_scope{
        refer=orddict:store(New, Old, S#elixir_scope.refer)
      } };
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "invalid args for refer, expected a reference as argument")
  end;

translate_each({require, Line, [Ref|T]}, S) ->
  KV = case T of
    [NotEmpty] -> NotEmpty;
    [] -> []
  end,

  { TRef, SR } = translate_each(Ref, S),

  As = case orddict:find(as, KV) of
    { ok, Value } -> Value;
    error -> false
  end,

  case TRef of
    { atom, _, Old } ->
      elixir_ref:ensure_loaded(Line, Old, SR, true),

      SF = SR#elixir_scope{
        requires=ordsets:add_element(Old, S#elixir_scope.requires)
      },

      translate_each({ refer, Line, [Ref, [{ as, As }]] }, SF);
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "invalid args for require, expected a reference as argument")
  end;

translate_each({import, Line, [Left]}, S) ->
  translate_each({ import, Line, [Left, []]}, S);

translate_each({import, Line, [Left,Opts]}, S) when is_list(Opts) ->
  translate_each({ import, Line, [all, Left, Opts]}, S);

translate_each({import, Line, [Selector, Left]}, S) ->
  translate_each({ import, Line, [Selector, Left, []]}, S);

translate_each({import, Line, [Left, Right, Opts]}, S) ->
  { TSelector, SL } = translate_each(Left, S),
  { TRef, SR } = translate_each(Right, SL),

  Selector = case TSelector of
    { atom, _,  SelectorAtom } -> SelectorAtom;
    _ -> syntax_error(Line, S#elixir_scope.filename, "invalid selector for import")
  end,

  Ref = case TRef of
    { atom, _, RefAtom } -> RefAtom;
    _ -> syntax_error(Line, S#elixir_scope.filename, "invalid name for import")
  end,

  case is_list(Opts) of
    true -> [];
    _ -> syntax_error(Line, S#elixir_scope.filename, "invalid options for import")
  end,

  As = case orddict:find(as, Opts) of
    { ok, Value } -> Value;
    error -> false
  end,

  elixir_ref:ensure_loaded(Line, Ref, SR, true),

  SF = case (Selector == all) or (Selector == functions) of
    false -> SR;
    true  ->
      Functions = elixir_import:calculate(
        Line, Ref, Opts, SR#elixir_scope.functions,
        elixir_dispatch:get_functions(Ref), SR),
      SR#elixir_scope{functions=Functions}
  end,

  SM = case (Selector == all) or (Selector == macros) of
    false -> SF;
    true  ->
      Available = case Selector of
        all -> elixir_dispatch:get_optional_macros(Ref);
        _ -> elixir_dispatch:get_macros(Line, Ref, SF)
      end,
      Macros = elixir_import:calculate(
        Line, Ref, Opts, SF#elixir_scope.macros, Available, SF),
      SF#elixir_scope{macros=Macros}
  end,

  translate_each({ require, Line, [Ref, [{ as, As }]] }, SM);

%% Arg-less macros

translate_each({'__MODULE__', Line, Atom}, S) when is_atom(Atom) ->
  Module = case S#elixir_scope.module of
    [] -> nil;
    Other -> Other
  end,
  { { atom, Line, Module }, S };

translate_each({'__FUNCTION__', Line, Atom}, S) when is_atom(Atom) ->
  case S#elixir_scope.function of
    [] ->
      { { atom, Line, nil }, S };
    { Name, Arity } ->
      { { tuple, Line, [ { atom, Line, Name }, { integer, Line, Arity } ] }, S }
  end;

translate_each({'__LINE__', Line, Atom}, S) when is_atom(Atom) ->
  { { integer, Line, Line }, S };

translate_each({'__FILE__', _Line, Atom}, S) when is_atom(Atom) ->
  translate_each(list_to_binary(S#elixir_scope.filename), S);

translate_each({'__MAIN__', Line, Atom}, S) when is_atom(Atom) ->
  { {atom, Line, '__MAIN__' }, S };

%% References

translate_each({'__ref__', Line, [Ref]}, S) when is_atom(Ref) ->
  Atom = list_to_atom("__MAIN__." ++ atom_to_list(Ref)),

  Final = case S#elixir_scope.noref of
    true  -> Atom;
    false -> elixir_ref:lookup(Atom, S#elixir_scope.refer)
  end,

  { {atom, Line, Final }, S };

%% Quoting

translate_each({quote, Line, [Left, Right]}, S) ->
  translate_each({ quote, Line, [orddict:from_list(Left ++ Right)] }, S);

translate_each({quote, _Line, [[{do,Exprs}|T]]}, S) ->
  Marker = case orddict:find(hygiene, T) of
    { ok, false } -> nil;
    _ -> quoted
  end,

  Line = case orddict:find(line, T) of
    { ok, Value } -> Value;
    _ -> 0
  end,

  Unquote = case orddict:find(unquote, T) of
    { ok, false } -> false;
    _ -> true
  end,

  elixir_quote:translate_each(Exprs, #elixir_quote{marker=Marker, line=Line, unquote=Unquote}, S);

translate_each({quote, Line, [_]}, S) ->
  syntax_error(Line, S#elixir_scope.filename, "invalid args for quote");

translate_each({in_guard, _, [[{do,Guard},{else,Else}]]}, S) ->
  case S#elixir_scope.guard of
    true  -> translate_each(Guard, S);
    false -> translate_each(Else, S)
  end;

%% Functions

translate_each({fn, Line, RawArgs}, S) when is_list(RawArgs) ->
  Clauses = case lists:split(length(RawArgs) - 1, RawArgs) of
    { Args, [[{do,Expr}]] } ->
      [{match,Args,Expr}];
    { [], [KV] } when is_list(KV) ->
      elixir_kv_block:decouple(orddict:erase(do, KV));
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "no block given to fn")
  end,

  Transformer = fun({ match, ArgsWithGuards, Expr }, Acc) ->
    { FinalArgs, Guards } = elixir_clauses:extract_last_guards(ArgsWithGuards),
    elixir_clauses:assigns_block(Line, fun elixir_translator:translate/2, FinalArgs, [Expr], Guards, umergec(S, Acc))
  end,

  { TClauses, NS } = lists:mapfoldl(Transformer, S, Clauses),
  { { 'fun', Line, {clauses, TClauses} }, umergec(S, NS) };

%% Loop and recur

translate_each({loop, Line, RawArgs}, S) when is_list(RawArgs) ->
  case lists:split(length(RawArgs) - 1, RawArgs) of
    { Args, [KV] } when is_list(KV) ->
      %% Generate a variable that will store the function
      { FunVar, VS }  = elixir_variables:build_ex(Line, S),

      %% Add this new variable to all match clauses
      [{match, KVBlock}] = elixir_kv_block:normalize(orddict:erase(do, KV)),
      Values = [{ [FunVar|Conds], Expr } || { Conds, Expr } <- element(3, KVBlock)],
      NewKVBlock = setelement(3, KVBlock, Values),

      %% Generate a function with the match blocks
      Function = { fn, Line, [[{match,NewKVBlock}]] },

      %% Finally, assign the function to a variable and
      %% invoke it passing the function itself as first arg
      Block = { '__block__', Line, [
        { '=', Line, [FunVar, Function] },
        { { '.', Line, [FunVar] }, Line, [FunVar|Args] }
      ] },

      { TBlock, TS } = translate_each(Block, VS#elixir_scope{recur=element(1,FunVar)}),
      { TBlock, TS#elixir_scope{recur=S#elixir_scope.recur} };
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "invalid args for loop")
  end;

translate_each({recur, Line, Args}, S) when is_list(Args) ->
  case S#elixir_scope.recur of
    [] ->
      syntax_error(Line, S#elixir_scope.filename, "cannot invoke recur outside of a loop");
    Recur ->
      ExVar = { Recur, Line, nil },
      Call = { { '.', Line, [ExVar] }, Line, [ExVar|Args] },
      translate_each(Call, S)
  end;

%% Super

translate_each({ super, Line, Args }, #elixir_scope{filename=Filename} = S) ->
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
      syntax_error(Line, Filename, "super must be called with the same number of arguments as the current function")
  end,

  Super = elixir_def_overridable:name(Module, Function),
  { { call, Line, { atom, Line, Super }, TArgs }, TS#elixir_scope{super=true} };

translate_each({ 'super?', Line, [] }, S) ->
  Module = assert_module_scope(Line, 'super?', S),
  Function = assert_function_scope(Line, 'super?', S),
  Bool = elixir_def_overridable:is_defined(Module, Function),
  { { atom, Line, Bool }, S };

%% Comprehensions

translate_each({ Kind, Line, Args }, S) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  translate_comprehension(Line, Kind, Args, S);

%% Variables

translate_each({'^', Line, [ { Name, _, Args } ] }, S) ->
  Result = case is_atom(Args) of
    true ->
      case S#elixir_scope.assign of
        false -> "cannot access variable ^~s outside of assignment";
        true  ->
          case dict:find(Name, S#elixir_scope.vars) of
            error -> "unbound variable ^~s";
            { ok, Value } -> { {var, Line, Value}, S }
          end
      end;
    false -> "cannot use ^ with expression at ^~s, ^ must be used only with variables"
  end,

  case is_list(Result) of
    true ->
      syntax_error(Line, S#elixir_scope.filename, Result, [Name]);
    false ->
      Result
  end;

translate_each({Name, Line, quoted}, S) when is_atom(Name) ->
  NewS = S#elixir_scope{vars=S#elixir_scope.quote_vars,noname=true},
  { TVar, VS } = elixir_variables:translate_each(Line, Name, NewS),
  { TVar, VS#elixir_scope{
    quote_vars=VS#elixir_scope.vars,
    noname=S#elixir_scope.noname,
    vars=S#elixir_scope.vars
  } };

translate_each({Name, Line, nil}, S) when is_atom(Name) ->
  elixir_variables:translate_each(Line, Name, S);

%% Local calls

translate_each({Atom, Line, Args} = Original, S) when is_atom(Atom) ->
  case handle_partials(Line, Original, S) of
    error ->
      Callback = fun() -> translate_local(Line, Atom, Args, S) end,
      elixir_dispatch:dispatch_imports(Line, Atom, Args, S, Callback);
    Else  -> Else
  end;

%% __LOCAL__ proxy calls

translate_each({{'.', _, [{'__LOCAL__', _, Atom}, Name]}, Line, Args} = Original, S) when is_atom(Atom), is_atom(Name) ->
  case handle_partials(Line, Original, S) of
    error -> translate_local(Line, Name, Args, S);
    Else  -> Else
  end;

%% Dot calls

translate_each({{'.', _, [Left, Right]}, Line, Args} = Original, S) when is_atom(Right) ->
  case handle_partials(Line, Original, S) of
    error ->
      { TLeft,  SL } = translate_each(Left, S),
      { TRight, SR } = translate_each(Right, umergec(S, SL)),

      Callback = fun() -> translate_apply(Line, TLeft, TRight, Args, S, SL, SR) end,

      case { TLeft, TRight } of
        { { atom, _, '__MAIN__.Erlang' }, { atom, _, Atom } } ->
          case Args of
            [] -> { { atom, Line, Atom }, S };
            _ ->
              Message = "invalid args for Erlang.~s expression",
              syntax_error(Line, S#elixir_scope.filename, Message, [Atom])
          end;
        { { atom, _, Receiver }, { atom, _, Atom } }  ->
          elixir_dispatch:dispatch_require(Line, Receiver, Atom, Args, umergev(SL, SR), Callback);
        _ ->
          Callback()
      end;
    Else -> Else
  end;

translate_each({{'.', _, [Left, Right]}, Line, _Args}, S) ->
  { TLeft, LS } = translate_each(Left, S),
  { TRight, RS } = translate_each(Right, (umergec(S, LS))#elixir_scope{noref=true}),
  TArgs = [TLeft, TRight],
  Atoms = [Atom || { atom, _, Atom } <- TArgs],
  Final = case length(Atoms) == length(TArgs) of
    true  -> { atom, Line, elixir_ref:concat(Atoms) };
    false ->
      FArgs = [elixir_tree_helpers:build_simple_list(Line, TArgs)],
      ?ELIXIR_WRAP_CALL(Line, elixir_ref, concat, FArgs)
  end,
  { Final, (umergev(LS, RS))#elixir_scope{noref=S#elixir_scope.noref} };

%% Anonymous function calls

translate_each({{'.', _, [Expr]}, Line, Args} = Original, S) ->
  { TExpr, SE } = translate_each(Expr, S),
  case TExpr of
    { atom, _, Atom } ->
      translate_each({ Atom, Line, Args }, S);
    _ ->
      case handle_partials(Line, Original, S) of
        error ->
          { TArgs, SA } = translate_args(Args, umergec(S, SE)),
          { {call, Line, TExpr, TArgs}, umergev(SE, SA) };
        Else -> Else
      end
  end;

%% Literals

translate_each({ Left, Right }, S) ->
  translate_each({ '{}', 0, [Left, Right]}, S);

translate_each([], S) ->
  { { nil, 0 }, S };

translate_each(Args, S) when is_list(Args) ->
  [RTail|RArgs] = lists:reverse(Args),

  case RTail of
    {'|',_,[Left,Right]} ->
      Exprs = [Left|RArgs],
      { Tail, ST } = translate_each(Right, S),
      ListS = umergec(S, ST);
    _ ->
      Exprs = [RTail|RArgs],
      Tail = { nil, 0 },
      ST = S,
      ListS = S
  end,

  { FExprs, FS } = case S#elixir_scope.assign of
    true ->
      elixir_tree_helpers:build_reverse_list(fun translate_each/2, Exprs, 0, ListS, Tail);
    false ->
      { TArgs, { SC, SV } } =
        elixir_tree_helpers:build_reverse_list(fun translate_arg/2, Exprs, 0, { ListS, ListS }, Tail),
      { TArgs, umergec(SV, SC) }
  end,

  { FExprs, umergev(ST, FS) };

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate_each(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

%% Helpers

translate_local(Line, Name, Args, #elixir_scope{local=[]} = S) ->
  { TArgs, NS } = translate_args(Args, S),
  { { call, Line, { atom, Line, Name }, TArgs }, NS };

translate_local(Line, Name, Args, S) ->
  { TArgs, NS } = translate_args(Args, S),
  Remote = { remote, Line,
    { atom, Line, S#elixir_scope.local },
    { atom, Line, Name }
  },
  { { call, Line, Remote, TArgs }, NS }.

% Variables in arguments are not propagated from one
% argument to the other. For instance:
%
%   x = 1
%   foo(x = x + 2, x)
%   x
%
% Should be the same as:
%
%   foo(3, 1)
%   3
%
% However, notice that if we are doing an assignment,
% it behaves the same as translate.
translate_arg(Arg, { Acc, S }) ->
  { TArg, TAcc } = translate_each(Arg, Acc),
  { TArg, { umergec(S, TAcc), umergev(S, TAcc) } }.

translate_args(Args, #elixir_scope{assign=true} = S) ->
  translate(Args, S);

translate_args(Args, S) ->
  { TArgs, { SC, SV } } = lists:mapfoldl(fun translate_arg/2, {S, S}, Args),
  { TArgs, umergec(SV, SC) }.

% Translate apply. Used by both apply and
% external function invocation macros.
translate_apply(Line, TLeft, TRight, Args, S, SL, SR) ->
  Optimize = case (Args == []) orelse lists:last(Args) of
    { '|', _, _ } -> false;
    _ ->
      case { TLeft, TRight } of
        { { Kind, _, _ }, { atom, _, _ } } when Kind == var; Kind == tuple; Kind == atom ->
          true;
        _ ->
          false
      end
  end,

  case Optimize of
    true ->
      { TArgs, SA } = translate_args(Args, umergec(S, SR)),
      FS = umergev(SL, umergev(SR,SA)),
      Remote = case TLeft of
        { atom, _, Atom } when Atom /= erlang ->
          { record_field, 1, { atom, 1, '' }, TLeft };
        _ -> TLeft
      end,
      { { call, Line, { remote, Line, Remote, TRight }, TArgs }, FS };
    false ->
      { TArgs, SA } = translate_each(Args, umergec(S, SR)),
      FS = umergev(SL, umergev(SR,SA)),
      { ?ELIXIR_WRAP_CALL(Line, erlang, apply, [TLeft, TRight, TArgs]), FS }
  end.

%% Handle partials by automatically wrapping them in a function.
%% It also checks if we are in an assignment scope and does not
%% apply the function if this is the case.
handle_partials(_Line, _Original, #elixir_scope{assign=true}) ->
  error;

handle_partials(Line, Original, S) ->
  case convert_partials(Line, element(3, Original), S) of
    { Call, Def, SC } when Def /= [] ->
      Final = validate_partials(Line, Def, SC),
      Block = [{do, setelement(3, Original, Call)}],
      translate_each({ fn, Line, Final ++ [Block] }, SC);
    _ -> error
  end.

validate_partials(Line, Def, S) ->
  validate_partials(Line, lists:sort(Def), 1, S).

validate_partials(Line, [{ Pos, Item }|T], Pos, S) ->
  [Item|validate_partials(Line, T, Pos + 1, S)];

validate_partials(Line, [{ Pos, _ }|_], Expected, S) ->
  syntax_error(Line, S#elixir_scope.filename, "partial variable &~w cannot be defined without &~w", [Pos, Expected]);

validate_partials(_Line, [], _Pos, _S) ->
  [].

%% This function receives arguments and then checks
%% the args for partial application. It returns a tuple
%% with three elements where the first element is the list
%% of call args, the second the list of def args for the
%% function definition and the third one is the new scope.
convert_partials(Line, List, S) -> convert_partials(Line, List, S, [], []).

convert_partials(Line, [{'&', _, [Pos]}|T], S, CallAcc, DefAcc) ->
  case lists:keyfind(Pos, 1, DefAcc) of
    false ->
      { Var, SC } = elixir_variables:build_ex(Line, S),
      convert_partials(Line, T, SC, [Var|CallAcc], [{Pos,Var}|DefAcc]);
    {Pos,Var} ->
      convert_partials(Line, T, S, [Var|CallAcc], DefAcc)
  end;

convert_partials(Line, [H|T], S, CallAcc, DefAcc) ->
  convert_partials(Line, T, S, [H|CallAcc], DefAcc);

convert_partials(_Line, [], S, CallAcc, DefAcc) ->
  { lists:reverse(CallAcc), lists:reverse(DefAcc), S }.

%% Convert operators

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

%% Comprehensions

translate_comprehension(Line, Kind, Args, S) ->
  case lists:split(length(Args) - 1, Args) of
    { Cases, [[{do,Expr}]] } ->
      { TCases, SC } = lists:mapfoldl(fun translate_each_comprehension/2, S, Cases),
      { TExpr, SE } = translate_each(Expr, SC),
      { { Kind, Line, TExpr, TCases }, umergec(S, SE) };
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "no block given to comprehension ~s", [Kind])
  end.

translate_each_comprehension({ in, Line, [{'<<>>', _, _} = Left, Right] }, S) ->
  translate_each_comprehension({ inbin, Line, [Left, Right]}, S);

translate_each_comprehension({inbin, Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { b_generate, Line, TLeft, TRight }, SL };

translate_each_comprehension({Kind, Line, [Left, Right]}, S) when Kind == in; Kind == inlist ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { generate, Line, TLeft, TRight }, SL };

translate_each_comprehension(X, S) ->
  { TX, TS } = translate_each(X, S),
  Line = case X of
    { _, L, _ } -> L;
    _ -> 0
  end,
  { elixir_tree_helpers:convert_to_boolean(Line, TX, true), TS }.