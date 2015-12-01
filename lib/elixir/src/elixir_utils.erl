%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([elixir_to_erl/1, get_ann/1, get_line/1, split_last/1,
  characters_to_list/1, characters_to_binary/1, macro_name/1,
  convert_to_boolean/4, returns_boolean/1, atom_concat/1,
  read_file_type/1, read_link_type/1, relative_to_cwd/1,
  change_universal_time/2, erl_call/4, meta_location/1]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

macro_name(Macro) ->
  list_to_atom(lists:concat(['MACRO-', Macro])).

atom_concat(Atoms) ->
  list_to_atom(lists:concat(Atoms)).

erl_call(Ann, Module, Function, Args) ->
  {call, Ann,
    {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}},
    Args
  }.

get_line(Opts) when is_list(Opts) ->
  case lists:keyfind(line, 1, Opts) of
    {line, Line} when is_integer(Line) -> Line;
    false -> 0
  end.

get_ann(Opts) when is_list(Opts) ->
  get_ann(Opts, [], 0).

get_ann([{generated,Gen}|T], Acc, Line) -> get_ann(T, [{generated,Gen}|Acc], Line);
get_ann([{line,Line}|T], Acc, _) -> get_ann(T, Acc, Line);
get_ann([_|T], Acc, Line) -> get_ann(T, Acc, Line);
get_ann([], [], Line) -> Line;
get_ann([], Acc, Line) -> [{location,Line}|Acc].

split_last([])         -> {[], []};
split_last(List)       -> split_last(List, []).
split_last([H], Acc)   -> {lists:reverse(Acc), H};
split_last([H|T], Acc) -> split_last(T, [H|Acc]).

read_file_type(File) ->
  case file:read_file_info(File) of
    {ok, #file_info{type=Type}} -> {ok, Type};
    {error, _} = Error -> Error
  end.

read_link_type(File) ->
  case file:read_link_info(File) of
    {ok, #file_info{type=Type}} -> {ok, Type};
    {error, _} = Error -> Error
  end.

change_universal_time(Name, {{Y, M, D}, {H, Min, Sec}}=Time)
  when is_integer(Y), is_integer(M), is_integer(D),
       is_integer(H), is_integer(Min), is_integer(Sec)->
    file:write_file_info(Name, #file_info{mtime=Time}, [{time, universal}]).

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
    false -> 'Elixir.String':to_char_list(Data)
  end.

characters_to_binary(Data) when is_binary(Data) ->
  Data;
characters_to_binary(Data) ->
  case elixir_compiler:get_opt(internal) of
    true  -> unicode:characters_to_binary(Data);
    false -> 'Elixir.List':to_string(Data)
  end.

%% Meta location.
%%
%% Macros add a file+keep pair on location keep
%% which we should take into account for error
%% reporting.
%%
%% Returns {binary, integer} on location keep or
%% nil.

meta_location(Meta) ->
  case lists:keyfind(file, 1, Meta) of
    {file, MetaFile} when is_binary(MetaFile) ->
      MetaLine =
        case lists:keyfind(keep, 1, Meta) of
          {keep, Keep} when is_integer(Keep) -> Keep;
          _ -> 0
        end,
      {MetaFile, MetaLine};
    _ ->
      nil
  end.

%% elixir to erl. Handles only valid quoted expressions,
%% that's why things like maps and references are not in the list.

elixir_to_erl(Tree) when is_tuple(Tree) ->
  {tuple, 0, [elixir_to_erl(X) || X <- tuple_to_list(Tree)]};

elixir_to_erl([]) ->
  {nil, 0};

elixir_to_erl(<<>>) ->
  {bin, 0, []};

elixir_to_erl(Tree) when is_list(Tree) ->
  elixir_to_erl_cons_1(Tree, []);

elixir_to_erl(Tree) when is_atom(Tree) ->
  {atom, 0, Tree};

elixir_to_erl(Tree) when is_integer(Tree) ->
  {integer, 0, Tree};

elixir_to_erl(Tree) when is_float(Tree) ->
  {float, 0, Tree};

elixir_to_erl(Tree) when is_binary(Tree) ->
  %% Note that our binaries are utf-8 encoded and we are converting
  %% to a list using binary_to_list. The reason for this is that Erlang
  %% considers a string in a binary to be encoded in latin1, so the bytes
  %% are not changed in any fashion.
  {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Tree)}, default, default}]};

elixir_to_erl(Function) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == {type, external}) andalso
       (erlang:fun_info(Function, env) == {env, []}) of
    true ->
      {module, Module} = erlang:fun_info(Function, module),
      {name, Name}     = erlang:fun_info(Function, name),
      {arity, Arity}   = erlang:fun_info(Function, arity),

      {'fun', 0, {function,
        {atom, 0, Module},
        {atom, 0, Name},
        {integer, 0, Arity}}};
    false ->
      error(badarg)
  end;

elixir_to_erl(Pid) when is_pid(Pid) ->
  elixir_utils:erl_call(0, erlang, binary_to_term,
    [elixir_utils:elixir_to_erl(term_to_binary(Pid))]);

elixir_to_erl(_Other) ->
  error(badarg).

elixir_to_erl_cons_1([H|T], Acc) -> elixir_to_erl_cons_1(T, [H|Acc]);
elixir_to_erl_cons_1(Other, Acc) -> elixir_to_erl_cons_2(Acc, elixir_to_erl(Other)).

elixir_to_erl_cons_2([H|T], Acc) ->
  elixir_to_erl_cons_2(T, {cons, 0, elixir_to_erl(H), Acc});
elixir_to_erl_cons_2([], Acc) ->
  Acc.

%% Boolean checks

returns_boolean(Bool) when is_boolean(Bool) -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_]}) when Op == 'not' -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_, _]}) when
  Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '==';  Op == '/='; Op == '=<';  Op == '>=';
  Op == '<';   Op == '>';  Op == '=:='; Op == '=/=' -> true;

returns_boolean({'__op__', _, [Op, _, Right]}) when Op == 'andalso'; Op == 'orelse' ->
  returns_boolean(Right);

returns_boolean({{'.', _, [erlang, Fun]}, _, [_]}) when
  Fun == is_atom;   Fun == is_binary;   Fun == is_bitstring; Fun == is_boolean;
  Fun == is_float;  Fun == is_function; Fun == is_integer;   Fun == is_list;
  Fun == is_number; Fun == is_pid;      Fun == is_port;      Fun == is_reference;
  Fun == is_tuple -> true;

returns_boolean({{'.', _, [erlang, Fun]}, _, [_, _]}) when
  Fun == is_function -> true;

returns_boolean({{'.', _, [erlang, Fun]}, _, [_, _, _]}) when
  Fun == function_exported -> true;

returns_boolean({'case', _, [_, [{do, Clauses}]]}) ->
  lists:all(fun
    ({'->', _, [_, Expr]}) -> returns_boolean(Expr)
  end, Clauses);

returns_boolean({'cond', _, [[{do, Clauses}]]}) ->
  lists:all(fun
    ({'->', _, [_, Expr]}) -> returns_boolean(Expr)
  end, Clauses);

returns_boolean({'__block__', [], Exprs}) ->
  returns_boolean(lists:last(Exprs));

returns_boolean(_) -> false.

convert_to_boolean(Line, Expr, Bool, S) when is_integer(Line) ->
  case {returns_boolean(Expr), Bool} of
    {true, true}  -> {Expr, S};
    {true, false} -> {{op, Line, 'not', Expr}, S};
    _               -> do_convert_to_boolean(Line, Expr, Bool, S)
  end.

%% Notice we use a temporary var and bundle nil
%% and false checks in the same clause since
%% it makes dialyzer happy.
do_convert_to_boolean(Line, Expr, Bool, S) ->
  {Name, _, TS} = elixir_scope:build_var('_', S),
  Var = {var, Line, Name},
  Any = {var, Line, '_'},
  OrElse = do_guarded_convert_to_boolean(Line, Var, 'orelse', '=='),

  FalseResult = {atom, Line, not Bool},
  TrueResult  = {atom, Line, Bool},

  {{'case', Line, Expr, [
    {clause, Line, [Var], [[OrElse]], [FalseResult]},
    {clause, Line, [Any], [], [TrueResult]}
  ]}, TS}.

do_guarded_convert_to_boolean(Line, Expr, Op, Comp) ->
  Left  = {op, Line, Comp, Expr, {atom, Line, false}},
  Right = {op, Line, Comp, Expr, {atom, Line, nil}},
  {op, Line, Op, Left, Right}.
