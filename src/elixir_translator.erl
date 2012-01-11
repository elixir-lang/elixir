-module(elixir_translator).
-export([translate/2, translate_each/2, translate_args/2, translate_apply/7, forms/3]).
-import(elixir_variables, [umergev/2, umergec/2]).
-import(elixir_errors, [syntax_error/4]).
-include("elixir.hrl").

forms(String, StartLine, Filename) ->
  try elixir_tokenizer:tokenize(String, StartLine) of
    {ok, Tokens} ->
      case elixir_parser:parse(Tokens) of
        {ok, Forms} -> Forms;
        {error, {Line, _, [Error, Token]}} -> syntax_error(Line, Filename, Error, Token)
      end;
    {error, {Line, Error, Token}} -> syntax_error(Line, Filename, Error, Token)
  catch
    { interpolation_error, { Line, Error, Token } } -> syntax_error(Line, Filename, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Those macros are "low-level". They are the basic mechanism
%% that makes the language work and cannot be partially applied
%% nor overwritten.

%% Assignment operator

translate_each({'=', Line, [Left, Right]}, S) ->
  record('=', S),
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL } = elixir_clauses:assigns(fun translate_each/2, Left, SR),
  { { match, Line, TLeft, TRight }, SL };

%% Blocks

translate_each({ block, Line, [] }, S) ->
  { { atom, Line, nil }, S };

translate_each({ block, _Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ block, Line, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, Line, TArgs }, NS };

translate_each({ kv_block, _, [{[Expr],nil}] }, S) ->
  translate_each(Expr, S);

translate_each({ kv_block, Line, Args }, S) when is_list(Args) ->
  case S#elixir_scope.macro of
    { Receiver, Name, Arity } ->
      Desc = io_lib:format("~s.~s/~B", [Receiver, Name, Arity]),
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "key value blocks not supported by: ", Desc);
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "unhandled key value blocks", "")
  end;

%% Containers

translate_each({ '<<>>', Line, Args }, S) when is_list(Args) ->
  { TArgs, { SC, SV } } = elixir_tree_helpers:build_bitstr(fun translate_arg/2, Args, Line, { S, S }),
  { TArgs, umergec(SV, SC) };

translate_each({'{}', Line, Args}, S) when is_list(Args) ->
  { TArgs, SE } = translate_args(Args, S),
  { {tuple, Line, TArgs}, SE };

%% Lexical

translate_each({refer, Line, [Ref|T]}, S) ->
  record(refer, S),

  KV = case T of
    [NotEmpty] -> NotEmpty;
    [] -> []
  end,

  Extractor = fun
    ({ atom, _, Atom }) -> Atom;
    (_) -> syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "refer")
  end,

  { TRef, SR } = translate_each(Ref, S),
  Old = Extractor(TRef),

  { New, SF } = case orddict:find(as, KV) of
    { ok, false } ->
      { Old, SR };
    { ok, Other } ->
      { TOther, SA } = translate_each(Other, SR#elixir_scope{noref=true}),
      { Extractor(TOther), SA };
    error ->
      { elixir_ref:last(Old), SR }
  end,

  Tuple = { tuple, Line, [{atom, Line, Old}, {atom, Line, New}] },

  { Tuple, SF#elixir_scope{
    refer=orddict:store(New, Old, S#elixir_scope.refer),
    noref=S#elixir_scope.noref
  } };

translate_each({require, Line, [Left]}, S) ->
  translate_each({ require, Line, [Left, []]}, S);

translate_each({require, Line, [Left,Opts]}, S) ->
  record(require, S),

  As = proplists:get_value(as, Opts, false),
  { TRef, SR }  = translate_each(Left, S),

  Ref = case TRef of
    { atom, _,  Atom } -> Atom;
    _ -> syntax_error(Line, S#elixir_scope.filename, "invalid name for: ", "require")
  end,

  Truthy = fun(X) -> proplists:get_value(X, Opts, false ) /= false end,
  Import = lists:any(Truthy, [import, only, except]),

  elixir_module:ensure_loaded(Line, Ref, SR, Import),

  IS = case Import of
    true ->
      OldImports = lists:keydelete(Ref, 1, SR#elixir_scope.imports),
      NewImports = elixir_import:calculate(Line, SR#elixir_scope.filename, Ref,
        Opts, OldImports, fun() -> elixir_dispatch:get_macros(Line, Ref, SR) end, macro),
      SR#elixir_scope{imports=[NewImports|OldImports]};
    false -> SR
  end,

  translate_each({ refer, Line, [Ref, [{as,As}]] }, IS);

%% Arg-less macros

translate_each({'__MODULE__', Line, false}, S) ->
  { _, Module } = S#elixir_scope.module,
  { { atom, Line, Module }, S };

translate_each({'__LINE__', Line, false}, S) ->
  { { integer, Line, Line }, S };

translate_each({'__FILE__', _Line, false}, S) ->
  translate_each(list_to_binary(S#elixir_scope.filename), S);

translate_each({'__STOP_ITERATOR__', Line, false}, S) ->
  { { atom, Line, '__STOP_ITERATOR__' }, S };

%% References

translate_each({module_ref, Line, [Ref]}, S) when is_atom(Ref) ->
  Atom = list_to_atom("::" ++ atom_to_list(Ref)),
  { _, Module } = S#elixir_scope.module,

  Final = case S#elixir_scope.noref or (Module == nil) of
    true  -> Atom;
    false -> elixir_ref:lookup(Atom, S#elixir_scope.refer)
  end,

  { {atom, Line, Final }, S };

%% ::

translate_each({'::', Line, [Left]}, S) ->
  translate_each({'::', Line, [nil,Left]}, S);

translate_each({'::', Line, [Left|Right]}, S) ->
  { TLeft, LS } = translate_each(Left, S),
  { TRight, RS } = translate_args(Right, (umergec(S, LS))#elixir_scope{noref=true}),
  TArgs = [TLeft|TRight],
  Atoms = [Atom || { atom, _, Atom } <- TArgs],
  Final = case length(Atoms) == length(TArgs) of
    true  -> { atom, Line, elixir_ref:concat(Atoms) };
    false ->
      FArgs = [elixir_tree_helpers:build_simple_list(Line, TArgs)],
      ?ELIXIR_WRAP_CALL(Line, elixir_ref, concat, FArgs)
  end,
  { Final, (umergev(LS, RS))#elixir_scope{noref=S#elixir_scope.noref} };

%% Quoting

translate_each({quote, _Line, [[{do,Exprs}]]}, S) ->
  record(quote, S),
  elixir_quote:translate_each(Exprs, S);

translate_each({quote, Line, [_]}, S) ->
  record(quote, S),
  syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "quote");

%% Variables & Function calls

translate_each({'^', Line, [ { Name, _, Args } ] }, S) ->
  Result = case Args of
    false ->
      case S#elixir_scope.assign of
        false -> "non-assignment scope for: ";
        true  ->
          case dict:find(Name, S#elixir_scope.vars) of
            error -> "unbound variable: ";
            { ok, Value } -> { {var, Line, Value}, S }
          end
      end;
    _ -> "cannot bind expression at token: "
  end,

  case is_list(Result) of
    true ->
      Desc = io_lib:format("^~s", [Name]),
      syntax_error(Line, S#elixir_scope.filename, Result, Desc);
    false ->
      Result
  end;

translate_each({Name, Line, false}, S) when is_atom(Name) ->
  Match = S#elixir_scope.assign,
  Vars = S#elixir_scope.vars,
  TempVars = S#elixir_scope.temp_vars,
  ClauseVars = S#elixir_scope.clause_vars,

  case Name of
    '_' -> { {var, Line, Name}, S };
    _ ->
      case { Match, dict:is_key(Name, Vars), dict:is_key(Name, TempVars) } of
        { true, true, true } -> { {var, Line, dict:fetch(Name, Vars) }, S };
        { true, Else, _ } ->
          { NewVar, NS } = case Else or S#elixir_scope.noname of
            true -> elixir_variables:build_erl(Line, S);
            false -> { {var, Line, Name}, S }
          end,
          RealName = element(3, NewVar),
          { NewVar, NS#elixir_scope{
            vars=dict:store(Name, RealName, Vars),
            temp_vars=dict:store(Name, RealName, TempVars),
            clause_vars=dict:store(Name, RealName, ClauseVars)
          } };
        { false, false, _ } -> translate_each({Name, Line, []}, S);
        { false, true, _ }  -> { {var, Line, dict:fetch(Name, Vars) }, S }
      end
  end;

translate_each({Atom, Line, _} = Original, S) when is_atom(Atom) ->
  case handle_partials(Line, Original, S) of
    error -> elixir_macros:translate_macro(Original, S);
    Else  -> Else
  end;

%% Dot calls

translate_each({{'.', _, [Left, Right]}, Line, Args} = Original, S) ->
  case handle_partials(Line, Original, S) of
    error ->
      { TLeft,  SL } = translate_each(Left, S),
      { TRight, SR } = translate_each(Right, umergec(S, SL)),

      Callback = fun() -> translate_apply(Line, TLeft, TRight, Args, S, SL, SR) end,

      case { TLeft, TRight } of
        { { atom, _, '::Erlang' }, { atom, _, Atom } } ->
          case Args of
            [] -> { { atom, Line, Atom }, S };
            _ ->
              Message = "invalid args for Erlang.MODULE expression: ",
              syntax_error(Line, S#elixir_scope.filename, Message, atom_to_list(Atom))
          end;
        { { atom, _, Receiver }, { atom, _, Atom } }  ->
          elixir_dispatch:dispatch_refer(Line, Receiver, Atom, Args, umergev(SL, SR), Callback);
        _ ->
          Callback()
      end;
    Else -> Else
  end;

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
  { TLeft, SL }  = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, SL),
  { { tuple, 0, [TLeft, TRight] }, SR };

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

  { TExprs, { SC, SV } } = elixir_tree_helpers:build_reverse_list(fun translate_arg/2, Exprs, 0, { ListS, ListS }, Tail),
  { TExprs, umergev(ST, umergec(SV, SC)) };

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate_each(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

%% Helpers

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
  { TArgs, SA } = translate_args(Args, umergec(S, SR)),
  FS = umergev(SL, umergev(SR,SA)),

  case { TLeft, TRight } of
    { { Kind, _, _ }, { atom, _, _ } } when Kind == var; Kind == tuple; Kind == atom ->
      { { call, Line, { remote, Line, TLeft, TRight }, TArgs }, FS };
    _ ->
      Apply = [TLeft, TRight, elixir_tree_helpers:build_simple_list(Line, TArgs)],
      { { call, Line, { atom, Line, apply }, Apply }, FS }
  end.

%% Handle partials by automatically wrapping them in a function.
%% It also checks if we are in an assignment scope and does not
%% apply the function if this is the case.
handle_partials(_Line, _Original, #elixir_scope{assign=true}) ->
  error;

handle_partials(Line, Original, S) ->
  case convert_partials(Line, element(3, Original), S) of
    { Call, Def, SC } when Def /= [] ->
      Block = [{do, setelement(3, Original, Call)}],
      elixir_macros:translate_macro({ fn, Line, Def ++ [Block] }, SC);
    _ -> error
  end.

%% This function receives arguments and then checks
%% the args for partial application. It returns a tuple
%% with three elements where the first element is the list
%% of call args, the second the list of def args for the
%% function definition and the third one is the new scope.
convert_partials(Line, List, S) -> convert_partials(Line, List, S, [], []).

convert_partials(Line, [{'_', _, false}|T], S, CallAcc, DefAcc) ->
  { Var, SC } = elixir_variables:build_ex(Line, S),
  convert_partials(Line, T, SC, [Var|CallAcc], [Var|DefAcc]);

convert_partials(Line, [H|T], S, CallAcc, DefAcc) ->
  convert_partials(Line, T, S, [H|CallAcc], DefAcc);

convert_partials(_Line, [], S, CallAcc, DefAcc) ->
  { lists:reverse(CallAcc), lists:reverse(DefAcc), S }.

% We need to record macros invoked so we raise users
% a nice error in case they define a local that overrides
% an invoked macro instead of silently failing.
%
% Some macros are not recorded because they will always
% raise an error to users if they define something similar
% regardless if they invoked it or not.
record(Atom, S) ->
  elixir_import:record(internal, { Atom, nil }, in_erlang_macros, S).
