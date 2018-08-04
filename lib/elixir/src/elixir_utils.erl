%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([get_line/1, split_last/1, noop/0,
  characters_to_list/1, characters_to_binary/1, relative_to_cwd/1,
  macro_name/1, returns_boolean/1, caller/4, meta_keep/1,
  read_file_type/1, read_file_type/2, read_link_type/1, read_posix_mtime_and_size/1,
  change_posix_time/2, change_universal_time/2,
  guard_op/2, extract_splat_guards/1, extract_guards/1,
  erlang_comparison_op_to_elixir/1]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

% Builds the macro name

macro_name(Macro) ->
  list_to_atom("MACRO-" ++ atom_to_list(Macro)).

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

erlang_comparison_op_to_elixir('/=') -> '!=';
erlang_comparison_op_to_elixir('=<') -> '<=';
erlang_comparison_op_to_elixir('=:=') -> '===';
erlang_comparison_op_to_elixir('=/=') -> '!==';
erlang_comparison_op_to_elixir(Other) -> Other.

% Extract guards

extract_guards({'when', _, [Left, Right]}) -> {Left, extract_or_guards(Right)};
extract_guards(Else) -> {Else, []}.

extract_or_guards({'when', _, [Left, Right]}) -> [Left | extract_or_guards(Right)];
extract_or_guards(Term) -> [Term].

% Extract guards when multiple left side args are allowed.

extract_splat_guards([{'when', _, [_ | _] = Args}]) ->
  {Left, Right} = split_last(Args),
  {Left, extract_or_guards(Right)};
extract_splat_guards(Else) ->
  {Else, []}.

%% No-op function that can be used for stuff like preventing tail-call
%% optimization to kick in.
noop() ->
  ok.

split_last([])           -> {[], []};
split_last(List)         -> split_last(List, []).
split_last([H], Acc)     -> {lists:reverse(Acc), H};
split_last([H | T], Acc) -> split_last(T, [H | Acc]).

read_file_type(File) ->
  read_file_type(File, []).

read_file_type(File, Opts) ->
  case file:read_file_info(File, [{time, posix} | Opts]) of
    {ok, #file_info{type=Type}} -> {ok, Type};
    {error, _} = Error -> Error
  end.

read_link_type(File) ->
  case file:read_link_info(File) of
    {ok, #file_info{type=Type}} -> {ok, Type};
    {error, _} = Error -> Error
  end.

read_posix_mtime_and_size(File) ->
  case file:read_file_info(File, [raw, {time, posix}]) of
    {ok, #file_info{mtime=Mtime, size=Size}} -> {ok, Mtime, Size};
    {error, _} = Error -> Error
  end.

change_posix_time(Name, Time) when is_integer(Time) ->
  file:write_file_info(Name, #file_info{mtime=Time}, [raw, {time, posix}]).

change_universal_time(Name, {{Y, M, D}, {H, Min, Sec}}=Time)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(Min), is_integer(Sec) ->
  file:write_file_info(Name, #file_info{mtime=Time}, [{time, universal}]).

relative_to_cwd(Path) ->
  try elixir_compiler:get_opt(relative_paths) of
    true  -> 'Elixir.Path':relative_to_cwd(Path);
    false -> Path
  catch
    _:_ -> Path
  end.

characters_to_list(Data) when is_list(Data) ->
  Data;
characters_to_list(Data) ->
  case elixir_config:get(bootstrap, true) of
    true  -> unicode:characters_to_list(Data);
    false -> 'Elixir.String':to_charlist(Data)
  end.

characters_to_binary(Data) when is_binary(Data) ->
  Data;
characters_to_binary(Data) ->
  case elixir_config:get(bootstrap, true) of
    true -> unicode:characters_to_binary(Data);
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

get_line(Opts) when is_list(Opts) ->
  case lists:keyfind(line, 1, Opts) of
    {line, Line} when is_integer(Line) -> Line;
    _ -> 0
  end.

%% Meta location.
%%
%% Macros add a file pair on location keep which we
%% should take into account for error reporting.
%%
%% Returns {binary, integer} on location keep or nil.

meta_keep(Meta) ->
  case lists:keyfind(keep, 1, Meta) of
    {keep, {File, Line} = Pair} when is_binary(File), is_integer(Line) ->
      Pair;
    _ ->
      nil
  end.

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

returns_boolean({'__block__', _, Exprs}) ->
  returns_boolean(lists:last(Exprs));

returns_boolean(_) -> false.
