%% TODO: Split into elixir and elixir_erl
%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([elixir_to_erl/1, get_ann/1, get_line/1, split_last/1,
  characters_to_list/1, characters_to_binary/1, macro_name/1,
  convert_to_boolean/4, returns_boolean/1,
  read_file_type/1, read_link_type/1, relative_to_cwd/1, caller/4,
  read_mtime/1, change_universal_time/2, erl_call/4, meta_location/1,
  noop/0, guard_op/2, match_op/2, extract_splat_guards/1, extract_guards/1]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

macro_name(Macro) ->
  list_to_atom("MACRO-" ++ atom_to_list(Macro)).

match_op('++', 2) -> true;
match_op('+', 1) -> true;
match_op('-', 1) -> true;
match_op(_, _) -> false.

guard_op('andalso', 2) ->
  true;
guard_op('orelse', 2) ->
  true;
guard_op(Op, Arity) ->
  try erl_internal:op_type(Op, Arity) of
    arith -> true;
    list  -> true;
    comp  -> true;
    bool  -> true;
    send  -> false
  catch
    _:_ -> false
  end.

% Extract guards

extract_guards({'when', _, [Left, Right]}) -> {Left, extract_or_guards(Right)};
extract_guards(Else) -> {Else, []}.

extract_or_guards({'when', _, [Left, Right]}) -> [Left | extract_or_guards(Right)];
extract_or_guards(Term) -> [Term].

% Extract guards when multiple left side args are allowed.

extract_splat_guards([{'when', _, [_, _ | _] = Args}]) ->
  {Left, Right} = elixir_utils:split_last(Args),
  {Left, extract_or_guards(Right)};
extract_splat_guards(Else) ->
  {Else, []}.

%% No-op function that can be used for stuff like preventing tail-call
%% optimization to kick in.
noop() ->
  ok.

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

get_ann([{generated, Gen} | T], Acc, Line) -> get_ann(T, [{generated, Gen} | Acc], Line);
get_ann([{line, Line} | T], Acc, _) -> get_ann(T, Acc, Line);
get_ann([_ | T], Acc, Line) -> get_ann(T, Acc, Line);
get_ann([], [], Line) -> Line;
get_ann([], Acc, Line) -> [{location, Line} | Acc].

split_last([])           -> {[], []};
split_last(List)         -> split_last(List, []).
split_last([H], Acc)     -> {lists:reverse(Acc), H};
split_last([H | T], Acc) -> split_last(T, [H | Acc]).

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

read_mtime(File) ->
  case file:read_file_info(File, [{time, universal}]) of
    {ok, #file_info{mtime=Mtime}} -> {ok, Mtime};
    {error, _} = Error -> Error
  end.

change_universal_time(Name, {{Y, M, D}, {H, Min, Sec}}=Time)
  when is_integer(Y), is_integer(M), is_integer(D),
       is_integer(H), is_integer(Min), is_integer(Sec)->
    file:write_file_info(Name, #file_info{mtime=Time}, [{time, universal}]).

relative_to_cwd(Path) ->
  case elixir_compiler:get_opt(relative_paths) of
    true  -> 'Elixir.Path':relative_to_cwd(Path);
    false -> Path
  end.

characters_to_list(Data) when is_list(Data) ->
  Data;
characters_to_list(Data) ->
  case elixir_compiler:get_opt(internal) of
    true  -> unicode:characters_to_list(Data);
    false -> 'Elixir.String':to_charlist(Data)
  end.

characters_to_binary(Data) when is_binary(Data) ->
  Data;
characters_to_binary(Data) ->
  case elixir_compiler:get_opt(internal) of
    true  -> unicode:characters_to_binary(Data);
    false -> 'Elixir.List':to_string(Data)
  end.

%% Returns the caller as a stacktrace entry.
caller(Line, File, nil, _) ->
  {elixir_compiler_0, '__FILE__', 1, stack_location(Line, File)};
caller(Line, File, Module, nil) ->
  {Module, '__MODULE__', 0, stack_location(Line, File)};
caller(Line, File, Module, {Name, Arity}) ->
  {Module, Name, Arity, stack_location(Line, File)}.

stack_location(Line, File) ->
  [{file, elixir_utils:characters_to_list(elixir_utils:relative_to_cwd(File))},
   {line, Line}].

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
  elixir_to_erl_cons1(Tree, []);

elixir_to_erl(Tree) when is_atom(Tree) ->
  {atom, 0, Tree};

elixir_to_erl(Tree) when is_integer(Tree) ->
  {integer, 0, Tree};

elixir_to_erl(Tree) when is_float(Tree) ->
  {float, 0, Tree};

elixir_to_erl(Tree) when is_binary(Tree) ->
  %% Note that our binaries are UTF-8 encoded and we are converting
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

elixir_to_erl_cons1([H | T], Acc) -> elixir_to_erl_cons1(T, [H | Acc]);
elixir_to_erl_cons1(Other, Acc) -> elixir_to_erl_cons2(Acc, elixir_to_erl(Other)).

elixir_to_erl_cons2([H | T], Acc) ->
  elixir_to_erl_cons2(T, {cons, 0, elixir_to_erl(H), Acc});
elixir_to_erl_cons2([], Acc) ->
  Acc.

%% Boolean checks

returns_boolean(Bool) when is_boolean(Bool) -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_]}) when Op == 'not' -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_, _]}) when
  Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '==';  Op == '/='; Op == '=<';  Op == '>=';
  Op == '<';   Op == '>';  Op == '=:='; Op == '=/=' -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_, Right]}) when
  Op == 'andalso'; Op == 'orelse' ->
  returns_boolean(Right);

returns_boolean({{'.', _, [erlang, Fun]}, _, [_]}) when
  Fun == is_atom;   Fun == is_binary;   Fun == is_bitstring; Fun == is_boolean;
  Fun == is_float;  Fun == is_function; Fun == is_integer;   Fun == is_list;
  Fun == is_number; Fun == is_pid;      Fun == is_port;      Fun == is_reference;
  Fun == is_tuple;  Fun == is_map;      Fun == is_process_alive -> true;

returns_boolean({{'.', _, [erlang, Fun]}, _, [_, _]}) when
  Fun == is_function; Fun == is_record -> true;

returns_boolean({{'.', _, [erlang, Fun]}, _, [_, _, _]}) when
  Fun == function_exported; Fun == is_record -> true;

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

%% Notice we use a temporary var and include nil
%% and false checks in the same clause since
%% it makes Dialyzer happy.
do_convert_to_boolean(Line, Expr, Bool, S) ->
  {Name, _, TS} = elixir_erl_var:build('_', S),
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
