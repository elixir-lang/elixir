%% Those macros behave like they belong to Elixir.Builtin,
%% but do not since they need to be implemented in Erlang.
-module(elixir_macros).
-export([translate_macro/2]).
-import(elixir_translator, [translate_each/2, translate/2, translate_args/2, translate_apply/7]).
-import(elixir_variables, [umergec/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4, assert_no_function_scope/3, assert_module_scope/3]).
-include("elixir.hrl").
-define(FUNS(), Kind == def; Kind == defp; Kind == defmacro; Kind == defmacrop).

%% Operators

translate_macro({ '+', _Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(Expr, S);

translate_macro({ '-', _Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(-1 * Expr, S);

translate_macro({ Op, Line, Exprs }, S) when is_list(Exprs),
  Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == '<-';
  Op == '++'; Op == '--'; Op == 'not'; Op == 'and';
  Op == 'or'; Op == 'xor'; Op == '<'; Op == '>';
  Op == '<='; Op == '>='; Op == '=='; Op == '!=';
  Op == '==='; Op == '!==' ->
  translate_each({ '__op__', Line, [Op|Exprs] }, S);

%% @

translate_macro({'@', Line, [{ Name, _, Args }]}, S) ->
  assert_module_scope(Line, '@', S),
  assert_no_function_scope(Line, '@', S),
  case is_reserved_data(Name) andalso elixir_compiler:get_opt(internal) of
    true ->
      { { nil, Line }, S };
    _ ->
      case Args of
        [Arg] ->
          translate_each({
            { '.', Line, ['__MAIN__.Module', merge_data] },
              Line,
              [ { '__MODULE__', Line, false }, [{ Name, Arg }] ]
          }, S);
        _ when is_atom(Args) or (Args == []) ->
            translate_each({
              { '.', Line, ['__MAIN__.Module', read_data] },
              Line,
              [ { '__MODULE__', Line, false }, Name ]
            }, S);
        _ ->
          syntax_error(Line, S#elixir_scope.filename, "expected 0 or 1 argument for @~s, got: ~p", [Name, length(Args)])
      end
  end;

%% Case

translate_macro({'case', Line, [Expr, KV]}, S) ->
  Clauses = elixir_clauses:get_pairs(Line, do, KV, S),
  { TExpr, NS } = translate_each(Expr, S),
  { TClauses, TS } = elixir_clauses:match(Line, Clauses, NS),
  { { 'case', Line, TExpr, TClauses }, TS };

%% Try

translate_macro({'try', Line, [Clauses]}, RawS) ->
  S  = RawS#elixir_scope{noname=true},

  Do = proplists:get_value('do', Clauses, []),
  { TDo, SB } = translate([Do], S),

  Catch = [Tuple || { X, _ } = Tuple <- Clauses, X == 'rescue' orelse X == 'catch'],
  { TCatch, SC } = elixir_try:clauses(Line, Catch, umergec(S, SB)),

  { TAfter, SA } = case orddict:find('after', Clauses) of
    { ok, After } -> translate([After], umergec(S, SC));
    error -> { [], SC }
  end,

  { { 'try', Line, unpack(TDo), [], TCatch, unpack(TAfter) }, umergec(RawS, SA) };

%% Receive

translate_macro({'receive', Line, [KV] }, S) ->
  Do = elixir_clauses:get_pairs(Line, do, KV, S, true),

  case orddict:is_key('after', KV) of
    true ->
      After = elixir_clauses:get_pairs(Line, 'after', KV, S),
      { TClauses, SC } = elixir_clauses:match(Line, Do ++ After, S),
      { FClauses, TAfter } = elixir_tree_helpers:split_last(TClauses),
      { _, _, [FExpr], _, FAfter } = TAfter,
      { { 'receive', Line, FClauses, FExpr, FAfter }, SC };
    false ->
      { TClauses, SC } = elixir_clauses:match(Line, Do, S),
      { { 'receive', Line, TClauses }, SC }
  end;

%% Definitions

translate_macro({defmodule, Line, [Ref, KV]}, S) ->
  { TRef, _ } = translate_each(Ref, S),

  Block = case orddict:find(do, KV) of
    { ok, DoValue } -> DoValue;
    error -> syntax_error(Line, S#elixir_scope.filename, "expected do: argument in defmodule")
  end,

  { FRef, FS } = case TRef of
    { atom, _, Module } ->
      NewModule = module_ref(Ref, Module, S#elixir_scope.module),

      RS = case Module == NewModule of
        true  -> S;
        false ->
          element(2, translate_each({ refer, Line, [NewModule, [{as,Module}]] }, S))
      end,

      {
        { atom, Line, NewModule },
        RS#elixir_scope{scheduled=[NewModule|S#elixir_scope.scheduled]}
      };
    _ ->
      { TRef, S }
  end,

  { elixir_module:translate(Line, FRef, Block, S), FS };

translate_macro({Kind, Line, [Call]}, S) when ?FUNS() ->
  translate_macro({Kind, Line, [Call, skip_definition]}, S);

translate_macro({Kind, Line, [Call, Expr]}, S) when ?FUNS() ->
  assert_module_scope(Line, Kind, S),
  assert_no_function_scope(Line, Kind, S),
  { TCall, Guards } = elixir_clauses:extract_guards(Call),
  { Name, Args }    = elixir_clauses:extract_args(TCall),
  TName             = elixir_tree_helpers:abstract_syntax(Name),
  TArgs             = elixir_tree_helpers:abstract_syntax(Args),
  TGuards           = elixir_tree_helpers:abstract_syntax(Guards),
  TExpr             = elixir_tree_helpers:abstract_syntax(Expr),
  { elixir_def:wrap_definition(Kind, Line, TName, TArgs, TGuards, TExpr, S), S };

translate_macro({Kind, Line, [Name, Args, Guards, Expr]}, S) when ?FUNS() ->
  assert_module_scope(Line, Kind, S),
  assert_no_function_scope(Line, Kind, S),
  { TName, NS }   = translate_each(Name, S),
  { TArgs, AS }   = translate_each(Args, NS),
  { TGuards, TS } = translate_each(Guards, AS),
  TExpr           = elixir_tree_helpers:abstract_syntax(Expr),
  { elixir_def:wrap_definition(Kind, Line, TName, TArgs, TGuards, TExpr, TS), TS };

%% Modules directives

translate_macro({use, Line, [Raw]}, S) ->
  translate_macro({use, Line, [Raw, []]}, S);

translate_macro({use, Line, [Raw, Args]}, S) ->
  assert_module_scope(Line, use, S),
  Module = S#elixir_scope.module,
  { TRef, SR } = translate_each(Raw, S),

  Ref = case TRef of
    { atom, _, RefAtom } -> RefAtom;
    _ -> syntax_error(Line, S#elixir_scope.filename, "invalid args for use, expected a reference as argument")
  end,

  elixir_ref:ensure_loaded(Line, Ref, SR),

  Call = { '__block__', Line, [
    { require, Line, [Ref] },
    { { '.', Line, [Ref, '__using__'] }, Line, [Module, Args] }
  ] },

  translate_each(Call, S);

%% Access

translate_macro({ access, Line, [Element, Keyword] }, S) ->
  case translate_each(Element, S) of
    { { atom, _, Atom }, _ } -> Atom;
    _ -> Atom = false
  end,

  case { S#elixir_scope.assign, Atom } of
    { false, false } ->
      Fallback = { { '.', Line, ['__MAIN__.Access', access] }, Line, [Element, Keyword] },
      translate_each(Fallback, S);
    { true, false } ->
      syntax_error(Line, S#elixir_scope.filename, "invalid usage of access protocol in signature");
    { Assign, _ } ->
      case is_orddict(Keyword) of
        true -> [];
        false ->
          Message0 = "expected contents inside brackets to be a Keyword",
          syntax_error(Line, S#elixir_scope.filename, Message0)
      end,

      elixir_ref:ensure_loaded(Line, Atom, S),

      try Atom:'__record__'(fields) of
        Fields ->
          { Match, Remaining } = lists:mapfoldl(fun({Field, Default}, KeywordEach) ->
            { case orddict:find(Field, KeywordEach) of
              { ok, Value } -> Value;
              error ->
                case Assign of
                  true  -> { '_', Line, nil };
                  false -> '__MAIN__.Macro':escape(Default)
                end
            end, orddict:erase(Field, KeywordEach) }
          end, Keyword, Fields),

          case Remaining of
            [] -> translate_each({ '{}', Line, [Atom|Match] }, S);
            _ ->
              Keys = [Key || {Key,_} <- Remaining],
              Message1 = "record ~s does not have some of the given keys: ~p",
              syntax_error(Line, S#elixir_scope.filename, Message1, [elixir_errors:inspect(Atom), Keys])
          end
      catch
        error:undef ->
          Message2 = "cannot use module ~s in access protocol because it doesn't represent a record",
          syntax_error(Line, S#elixir_scope.filename, Message2, [elixir_errors:inspect(Atom)])
      end
  end;

%% Apply - Optimize apply by checking what doesn't need to be dispatched dynamically

translate_macro({ apply, Line, [Left, Right, Args] }, S) when is_list(Args) ->
  { TLeft,  SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  translate_apply(Line, TLeft, TRight, Args, S, SL, SR);

translate_macro({ apply, Line, Args }, S) ->
  { TArgs, NS } = translate_args(Args, S),
  { ?ELIXIR_WRAP_CALL(Line, erlang, apply, TArgs), NS };

%% Handle forced variables

translate_macro({ 'var!', _, [{Name, Line, Atom}] }, S) when is_atom(Name), is_atom(Atom) ->
  elixir_variables:translate_each(Line, Name, S);

translate_macro({ 'var!', Line, [_] }, S) ->
  syntax_error(Line, S#elixir_scope.filename, "invalid args for var!").

%% HELPERS

module_ref(_Raw, Module, nil) ->
  Module;

module_ref({ '__aliases__', _, [{ '__MAIN__', _, Atom }|_]}, Module, _Nesting) when is_atom(Atom) ->
  Module;

module_ref(_F, Module, Nesting) ->
  elixir_ref:concat([Nesting, Module]).

is_orddict(Keyword) -> is_list(Keyword) andalso lists:all(fun is_orddict_tuple/1, Keyword).

is_orddict_tuple({X,_}) when is_atom(X) -> true;
is_orddict_tuple(_) -> false.

is_reserved_data(moduledoc) -> true;
is_reserved_data(doc)       -> true;
is_reserved_data(_)         -> false.

% Unpack a list of expressions from a block.
unpack([{ '__block__', _, Exprs }]) -> Exprs;
unpack(Exprs)                       -> Exprs.