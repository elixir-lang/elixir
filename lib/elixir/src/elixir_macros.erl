%% Those macros behave like they belong to Kernel,
%% but do not since they need to be implemented in Erlang.
-module(elixir_macros).
-export([translate/2]).
-import(elixir_translator, [translate_each/2]).
-import(elixir_errors, [compile_error/3, syntax_error/3, syntax_error/4]).

-include("elixir.hrl").
-define(opt_in_types(Kind), Kind == atom orelse Kind == integer orelse Kind == float).

%% Operators

translate({ in, Meta, [Left, Right] }, #elixir_scope{extra_guards=nil} = S) ->
  translate_in(Meta, Left, Right, S);

%% Definitions

translate({defmodule, Meta, [Ref, KV]}, S) when is_list(KV) ->
  { TRef, _ } = translate_each(Ref, S),

  Block = case lists:keyfind(do, 1, KV) of
    { do, DoValue } -> DoValue;
    false -> syntax_error(Meta, S#elixir_scope.file, "missing do keyword in defmodule")
  end,

  { FRef, FS } = case TRef of
    { atom, _, Module } ->
      FullModule = expand_module(Ref, Module, S),

      RS = case elixir_aliases:nesting_alias(S#elixir_scope.module, FullModule) of
        { New, Old } -> elixir_aliases:store(Meta, New, Old, [{warn,false}], S);
        false -> S
      end,

      {
        FullModule,
        RS#elixir_scope{context_modules=[FullModule|S#elixir_scope.context_modules]}
      };
    _ ->
      { Ref, S }
  end,

  Env = elixir_env:scope_to_ex({ ?line(Meta), FS }),
  translate_each(elixir_module:translate(FRef, Block, Env), FS);

translate({ Name, Meta, Args }, S) ->
  syntax_error(Meta, S#elixir_scope.file,
               "invalid arguments for macro ~ts/~B", [Name, length(Args)]).

%% Helpers

translate_in(Meta, Left, Right, S) ->
  Line = ?line(Meta),

  { TLeft, SL }  = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, SL),

  Cache = (S#elixir_scope.context == nil),

  { Var, SV } = case Cache of
    true  -> elixir_scope:build_erl_var(Line, SR);
    false -> { TLeft, SR }
  end,

  { TCache, TExpr } = case TRight of
    { nil, _ } ->
      Expr = { atom, Line, false },
      { Cache, Expr };
    { cons, _, _, _ } ->
      [H|T] = elixir_utils:cons_to_list(TRight),
      Expr = lists:foldr(fun(X, Acc) ->
        { op, Line, 'orelse', { op, Line, '=:=', Var, X }, Acc }
      end, { op, Line, '=:=', Var, H }, T),
      { Cache, Expr };
    { string, _, [H|T] } ->
      Expr = lists:foldl(fun(X, Acc) ->
        { op, Line, 'orelse', { op, Line, '=:=', Var, { integer, Line, X } }, Acc }
      end, { op, Line, '=:=', Var, { integer, Line, H } }, T),
      { Cache, Expr };
    { tuple, _, [{ atom, _, 'Elixir.Range' }, Start, End] } ->
      Expr = case { Start, End } of
        { { K1, _, StartInt }, { K2, _, EndInt } } when ?opt_in_types(K1), ?opt_in_types(K2), StartInt =< EndInt ->
          increasing_compare(Line, Var, Start, End);
        { { K1, _, _ }, { K2, _, _ } } when ?opt_in_types(K1), ?opt_in_types(K2) ->
          decreasing_compare(Line, Var, Start, End);
        _ ->
          { op, Line, 'orelse',
            { op, Line, 'andalso',
              { op, Line, '=<', Start, End},
              increasing_compare(Line, Var, Start, End) },
            { op, Line, 'andalso',
              { op, Line, '<', End, Start},
              decreasing_compare(Line, Var, Start, End) } }
      end,
      { Cache, Expr };
    _ ->
      case Cache of
        true ->
          { false, ?wrap_call(Line, 'Elixir.Enum', 'member?', [TRight, TLeft]) };
        false ->
          compile_error(Meta, S#elixir_scope.file, "invalid args for operator in, it expects a "
                        "compile time list or range on the right side when used in guard expressions")
      end
  end,

  case TCache of
    true  -> { { block, Line, [ { match, Line, Var, TLeft }, TExpr ] }, SV };
    false -> { TExpr, SV }
  end.

increasing_compare(Line, Var, Start, End) ->
  { op, Line, 'andalso',
    { op, Line, '>=', Var, Start },
    { op, Line, '=<', Var, End } }.

decreasing_compare(Line, Var, Start, End) ->
  { op, Line, 'andalso',
    { op, Line, '=<', Var, Start },
    { op, Line, '>=', Var, End } }.

%% defmodule :foo
expand_module(Raw, _Module, _S) when is_atom(Raw) ->
  Raw;

%% defmodule Hello
expand_module({ '__aliases__', _, [H] }, _Module, S) ->
  elixir_aliases:concat([S#elixir_scope.module, H]);

%% defmodule Hello.World
expand_module({ '__aliases__', _, _ } = Alias, Module, S) ->
  case elixir_aliases:expand(Alias, S#elixir_scope.aliases, S#elixir_scope.macro_aliases,
                             S#elixir_scope.lexical_tracker) of
    Atom when is_atom(Atom) ->
      Module;
    Aliases when is_list(Aliases) ->
      elixir_aliases:concat([S#elixir_scope.module, Module])
  end;

%% defmodule Elixir.Hello.World
expand_module(_Raw, Module, S) ->
  elixir_aliases:concat([S#elixir_scope.module, Module]).


