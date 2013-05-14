%% Convenience functions used throughout elixir source code
%% to generate erlang abstract format for basic structures
%% as lists, condition clauses, etc.

-module(elixir_tree_helpers).
-compile({parse_transform, elixir_transform}).
-export([elixir_to_erl/1, erl_to_elixir/1, split_last/1,
  cons_to_list/1, list_to_cons/2, list_to_cons/3,
  convert_to_boolean/5, returns_boolean/1, get_line/1]).
-include("elixir.hrl").

get_line(Opts) ->
  case lists:keyfind(line, 1, Opts) of
    { line, Line } when is_integer(Line) -> Line;
    false -> 0
  end.

split_last([])         -> { [], [] };
split_last(List)       -> split_last(List, []).
split_last([H], Acc)   -> { lists:reverse(Acc), H };
split_last([H|T], Acc) -> split_last(T, [H|Acc]).

cons_to_list({ nil, _ }) -> [];
cons_to_list({ cons, _, Left, Right }) -> [Left|cons_to_list(Right)].

list_to_cons(Line, List) ->
  list_to_cons(Line, List, { nil, Line }).

list_to_cons(Line, List, Tail) ->
  lists:foldr(fun(X, Acc) ->
    { cons, Line, X, Acc }
  end, Tail, List).

%% erl <-> elixir

elixir_to_erl(Tree) when is_tuple(Tree) ->
  { tuple, 0, [elixir_to_erl(X) || X <- tuple_to_list(Tree)] };

elixir_to_erl(Tree) when is_list(Tree) ->
  elixir_to_erl_cons(lists:reverse(Tree), { nil, 0 });

elixir_to_erl(Tree) when is_atom(Tree) ->
  { atom, 0, Tree };

elixir_to_erl(Tree) when is_integer(Tree) ->
  { integer, 0, Tree };

elixir_to_erl(Tree) when is_float(Tree) ->
  { float, 0, Tree };

elixir_to_erl(Tree) when is_binary(Tree) ->
  { bin, 0, [{ bin_element, 0, { string, 0, binary_to_list(Tree) }, default, default }] }.

elixir_to_erl_cons([H|T], Acc) ->
  elixir_to_erl_cons(T, { cons, 0, elixir_to_erl(H), Acc });

elixir_to_erl_cons([], Acc) -> Acc.

erl_to_elixir({ tuple, _, Args }) ->
  list_to_tuple([erl_to_elixir(X) || X <- Args]);

erl_to_elixir({ Kind, _, Value }) when Kind == atom; Kind == integer; Kind == float ->
  Value;

erl_to_elixir({ bin, _, [{ bin_element, 0, { string, 0, String }, default, default }] }) ->
  list_to_binary(String);

erl_to_elixir({ nil, _ }) -> [];

erl_to_elixir({ cons, _, Left, Right }) -> [erl_to_elixir(Left)|erl_to_elixir(Right)];

erl_to_elixir(Other) ->
  { '__scope__', [], [[{erl,true}],[{do,Other}]] }.

%% Others

returns_boolean({ op, _, Op, _ }) when Op == 'not' -> true;

returns_boolean({ op, _, Op, _, _ }) when
  Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '==';  Op == '/='; Op == '=<';  Op == '>=';
  Op == '<';   Op == '>';  Op == '=:='; Op == '=/=' -> true;

returns_boolean({ op, _, Op, _, Right }) when Op == 'andalso'; Op == 'orelse' ->
  returns_boolean(Right);

returns_boolean({ call, _, { remote, _, { atom, _, erlang }, { atom, _, Fun } }, [_] }) when
  Fun == is_atom;   Fun == is_binary;   Fun == is_bitstring; Fun == is_boolean;
  Fun == is_float;  Fun == is_function; Fun == is_integer;   Fun == is_list;
  Fun == is_number; Fun == is_pid;      Fun == is_port;      Fun == is_reference;
  Fun == is_tuple -> true;

returns_boolean({ call, _, { remote, _, { atom, _, erlang }, { atom, _, Fun } }, [_,_] }) when
  Fun == is_function -> true;

returns_boolean({ call, _, { remote, _, { atom, _, erlang }, { atom, _, Fun } }, [_,_,_] }) when
  Fun == function_exported -> true;

returns_boolean({ atom, _, Bool }) when Bool == true; Bool == false -> true;

returns_boolean({ 'case', _, _, Clauses }) ->
  lists:all(fun
    ({clause,_,_,_,[Expr]}) -> returns_boolean(Expr);
    (_) -> false
  end, Clauses);

returns_boolean(_) -> false.

convert_to_boolean(Line, Expr, Bool, InGuard, S) when is_integer(Line) ->
  case { returns_boolean(Expr), Bool } of
    { true, true }  -> { Expr, S };
    { true, false } -> { { op, Line, 'not', Expr }, S };
    _               -> do_convert_to_boolean(Line, Expr, Bool, InGuard, S)
  end.

%% Notice we use a temporary var and bundle nil
%% and false checks in the same clause since
%% it makes dialyzer happy.
do_convert_to_boolean(Line, Expr, Bool, false, S) ->
  Any         = { var, Line, '_' },
  { Var, TS } = elixir_scope:build_erl_var(Line, S),
  OrElse      = do_guarded_convert_to_boolean(Line, Var, 'orelse', '=='),

  FalseResult = { atom,Line,not Bool },
  TrueResult  = { atom,Line,Bool },

  { { 'case', Line, Expr, [
    { clause, Line, [Var], [[OrElse]], [FalseResult] },
    { clause, Line, [Any], [], [TrueResult] }
  ] }, TS };

do_convert_to_boolean(Line, Expr, true, true, S) ->
  { do_guarded_convert_to_boolean(Line, Expr, 'andalso', '/='), S };

do_convert_to_boolean(Line, Expr, false, true, S) ->
  { do_guarded_convert_to_boolean(Line, Expr, 'orelse', '=='), S }.

do_guarded_convert_to_boolean(Line, Expr, Op, Comp) ->
  Left  = { op, Line, Comp, Expr, { atom, Line, false } },
  Right = { op, Line, Comp, Expr, { atom, Line, nil } },
  { op, Line, Op, Left, Right }.