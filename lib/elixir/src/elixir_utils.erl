%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([elixir_to_erl/1, elixir_to_erl/3,
  get_line/1, split_last/1,
  characters_to_list/1, characters_to_binary/1,
  cons_to_list/1, list_to_cons/2, list_to_cons/3,
  convert_to_boolean/5, returns_boolean/1,
  file_type/1, file_type/2, relative_to_cwd/1]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

get_line(Opts) ->
  case lists:keyfind(line, 1, Opts) of
    { line, Line } when is_integer(Line) -> Line;
    false -> 0
  end.

split_last([])         -> { [], [] };
split_last(List)       -> split_last(List, []).
split_last([H], Acc)   -> { lists:reverse(Acc), H };
split_last([H|T], Acc) -> split_last(T, [H|Acc]).

file_type(File) ->
  file_type(File, read_link_info).

file_type(File, Op) ->
  case file:Op(File) of
    { ok, #file_info{type=Type} } -> { ok, Type };
    { error, _ } = Error -> Error
  end.

relative_to_cwd(Path) ->
  case elixir_compiler:get_opt(internal) of
    true  -> Path;
    false -> 'Elixir.Path':relative_to_cwd(Path)
  end.

characters_to_list(Data) when is_list(Data) ->
  Data;
characters_to_list(Data) ->
  case elixir_compiler:get_opt(internal) of
    true  -> unicode:characters_to_list(Data);
    false -> 'Elixir.String':'to_char_list!'(Data)
  end.

characters_to_binary(Data) when is_binary(Data) ->
  Data;
characters_to_binary(Data) ->
  case elixir_compiler:get_opt(internal) of
    true  -> unicode:characters_to_binary(Data);
    false -> 'Elixir.String':'from_char_list!'(Data)
  end.

%% List conversion

cons_to_list({ nil, _ }) -> [];
cons_to_list({ cons, _, Left, Right }) -> [Left|cons_to_list(Right)].

list_to_cons(Line, List) ->
  list_to_cons(Line, List, { nil, Line }).

list_to_cons(Line, List, Tail) ->
  lists:foldr(fun(X, Acc) ->
    { cons, Line, X, Acc }
  end, Tail, List).

%% erl <-> elixir

elixir_to_erl(Tree) ->
  elixir_to_erl(Tree, fun(X) -> error({ badarg, X }) end).

elixir_to_erl(Line, Tree, S) ->
  elixir_to_erl(Tree, fun
    (X) when is_function(X) ->
      elixir_errors:compile_error(Line, S#elixir_scope.file,
        "anonymous functions cannot be translated into a quoted expression, got: ~ts",
        ['Elixir.Kernel':inspect(X)]);
    (X) ->
      elixir_errors:compile_error(Line, S#elixir_scope.file,
        "cannot translate ~ts into a quoted expression",
        ['Elixir.Kernel':inspect(X)])
  end).

elixir_to_erl(Tree, Fun) when is_tuple(Tree) ->
  { tuple, 0, [elixir_to_erl(X, Fun) || X <- tuple_to_list(Tree)] };

elixir_to_erl([], _Fun) ->
  { nil, 0 };

elixir_to_erl(Tree, Fun) when is_list(Tree) ->
  elixir_to_erl_cons_1(Tree, [], Fun);

elixir_to_erl(Tree, _Fun) when is_atom(Tree) ->
  { atom, 0, Tree };

elixir_to_erl(Tree, _Fun) when is_integer(Tree) ->
  { integer, 0, Tree };

elixir_to_erl(Tree, _Fun) when is_float(Tree) ->
  { float, 0, Tree };

elixir_to_erl(Tree, _Fun) when is_binary(Tree) ->
  { bin, 0, [{ bin_element, 0, { string, 0, binary_to_list(Tree) }, default, default }] };

elixir_to_erl(Function, Fun) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == { type, external }) andalso
       (erlang:fun_info(Function, env) == { env, [] }) of
    true ->
      { module, Module } = erlang:fun_info(Function, module),
      { name, Name }     = erlang:fun_info(Function, name),
      { arity, Arity }   = erlang:fun_info(Function, arity),

      { 'fun', 0, { function,
        { atom, 0, Module },
        { atom, 0, Name },
        { integer, 0, Arity } } };
    false ->
      Fun(Function)
  end;

elixir_to_erl(Other, Fun) ->
  Fun(Other).

elixir_to_erl_cons_1([H|T], Acc, Fun) -> elixir_to_erl_cons_1(T, [H|Acc], Fun);
elixir_to_erl_cons_1(Other, Acc, Fun) -> elixir_to_erl_cons_2(Acc, elixir_to_erl(Other, Fun), Fun).

elixir_to_erl_cons_2([H|T], Acc, Fun) ->
  elixir_to_erl_cons_2(T, { cons, 0, elixir_to_erl(H, Fun), Acc }, Fun);
elixir_to_erl_cons_2([], Acc, _Fun) ->
  Acc.

%% Boolean checks

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

returns_boolean({ atom, _, Bool }) when is_boolean(Bool) -> true;

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