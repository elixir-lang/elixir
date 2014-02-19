%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([elixir_to_erl/1, get_line/1, split_last/1,
  characters_to_list/1, characters_to_binary/1,
  convert_to_boolean/4, returns_boolean/1,
  file_type/1, file_type/2, relative_to_cwd/1]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

get_line(Opts) when is_list(Opts) ->
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

%% elixir to erl. Handles only valid quoted expressions,
%% that's why things like maps and references are not in the list.

elixir_to_erl(Tree) when is_tuple(Tree) ->
  { tuple, 0, [elixir_to_erl(X) || X <- tuple_to_list(Tree)] };

elixir_to_erl([]) ->
  { nil, 0 };

elixir_to_erl(<<>>) ->
  { bin, 0, [] };

elixir_to_erl(Tree) when is_list(Tree) ->
  elixir_to_erl_cons_1(Tree, []);

elixir_to_erl(Tree) when is_atom(Tree) ->
  { atom, 0, Tree };

elixir_to_erl(Tree) when is_integer(Tree) ->
  { integer, 0, Tree };

elixir_to_erl(Tree) when is_float(Tree) ->
  { float, 0, Tree };

elixir_to_erl(Tree) when is_binary(Tree) ->
  %% Note that our binaries are utf-8 encoded and we are converting
  %% to a list using binary_to_list. The reason for this is that Erlang
  %% considers a string in a binary to be encoded in latin1, so the bytes
  %% are not changed in any fashion.
  { bin, 0, [{ bin_element, 0, { string, 0, binary_to_list(Tree) }, default, default }] };

elixir_to_erl(Function) when is_function(Function) ->
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
      error(badarg)
  end;

elixir_to_erl(Pid) when is_pid(Pid) ->
  ?wrap_call(0, erlang, binary_to_term, [elixir_utils:elixir_to_erl(term_to_binary(Pid))]);

elixir_to_erl(_Other) ->
  error(badarg).

elixir_to_erl_cons_1([H|T], Acc) -> elixir_to_erl_cons_1(T, [H|Acc]);
elixir_to_erl_cons_1(Other, Acc) -> elixir_to_erl_cons_2(Acc, elixir_to_erl(Other)).

elixir_to_erl_cons_2([H|T], Acc) ->
  elixir_to_erl_cons_2(T, { cons, 0, elixir_to_erl(H), Acc });
elixir_to_erl_cons_2([], Acc) ->
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

convert_to_boolean(Line, Expr, Bool, S) when is_integer(Line) ->
  case { returns_boolean(Expr), Bool } of
    { true, true }  -> { Expr, S };
    { true, false } -> { { op, Line, 'not', Expr }, S };
    _               -> do_convert_to_boolean(Line, Expr, Bool, S)
  end.

%% Notice we use a temporary var and bundle nil
%% and false checks in the same clause since
%% it makes dialyzer happy.
do_convert_to_boolean(Line, Expr, Bool, S) ->
  { Name, _, TS } = elixir_scope:build_var('_', S),
  Var = { var, Line, Name },
  Any = { var, Line, '_' },
  OrElse = do_guarded_convert_to_boolean(Line, Var, 'orelse', '=='),

  FalseResult = { atom,Line,not Bool },
  TrueResult  = { atom,Line,Bool },

  { { 'case', Line, Expr, [
    { clause, Line, [Var], [[OrElse]], [FalseResult] },
    { clause, Line, [Any], [], [TrueResult] }
  ] }, TS }.

do_guarded_convert_to_boolean(Line, Expr, Op, Comp) ->
  Left  = { op, Line, Comp, Expr, { atom, Line, false } },
  Right = { op, Line, Comp, Expr, { atom, Line, nil } },
  { op, Line, Op, Left, Right }.