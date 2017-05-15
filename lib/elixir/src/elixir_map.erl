-module(elixir_map).
-export([expand_map/3, translate_map/3, expand_struct/4, translate_struct/4]).
-import(elixir_errors, [compile_error/4]).
-include("elixir.hrl").

expand_map(Meta, [{'|', UpdateMeta, [Left, Right]}], E) ->
  {[ELeft | ERight], EA} = elixir_exp:expand_args([Left | Right], E),
  validate_kv(Meta, ERight, Right, E),
  {{'%{}', Meta, [{'|', UpdateMeta, [ELeft, ERight]}]}, EA};
expand_map(Meta, Args, E) ->
  {EArgs, EA} = elixir_exp:expand_args(Args, E),
  validate_kv(Meta, EArgs, Args, E),
  {{'%{}', Meta, EArgs}, EA}.

expand_struct(Meta, Left, {'%{}', MapMeta, MapArgs}, #{context := Context} = E) ->
  CleanArgs = clean_struct_key_from_map_args(Meta, MapArgs, E),
  {[ELeft, ERight], EE} = elixir_exp:expand_args([Left, {'%{}', MapMeta, CleanArgs}], E),

  case validate_struct(ELeft, Context) of
    true when is_atom(ELeft) ->
      %% We always record structs when they are expanded
      %% as they expect the reference at compile time.
      elixir_lexical:record_remote(ELeft, '__struct__', 1, nil, ?line(Meta), ?m(E, lexical_tracker));
    true ->
      ok;
    false when Context == match ->
      compile_error(Meta, ?m(E, file), "expected struct name in a match to be a compile "
        "time atom, alias or a variable, got: ~ts", ['Elixir.Macro':to_string(ELeft)]);
    false ->
      compile_error(Meta, ?m(E, file), "expected struct name to be a compile "
        "time atom or alias, got: ~ts", ['Elixir.Macro':to_string(ELeft)])
  end,

  EMeta =
    %% We also include the current module because it won't be present
    %% in context module in case the module name is defined dynamically.
    case lists:member(ELeft, [?m(E, module) | ?m(E, context_modules)]) of
      true  -> [{struct, context} | Meta];
      false -> Meta
    end,

  {{'%', EMeta, [ELeft, ERight]}, EE};
expand_struct(Meta, _Left, Right, E) ->
  compile_error(Meta, ?m(E, file), "expected struct to be followed by a map, got: ~ts",
                ['Elixir.Macro':to_string(Right)]).

clean_struct_key_from_map_args(Meta, [{'|', PipeMeta, [Left, MapAssocs]}], E) ->
  [{'|', PipeMeta, [Left, clean_struct_key_from_map_assocs(Meta, MapAssocs, E)]}];
clean_struct_key_from_map_args(Meta, MapAssocs, E) ->
  clean_struct_key_from_map_assocs(Meta, MapAssocs, E).

clean_struct_key_from_map_assocs(Meta, Assocs, E) ->
  case lists:keytake('__struct__', 1, Assocs) of
    {value, _, CleanAssocs} ->
      elixir_errors:warn(?line(Meta), ?m(E, file), "key :__struct__ is ignored when using structs"),
      CleanAssocs;
    false ->
      Assocs
  end.

validate_struct({'^', _, [{Var, _, Ctx}]}, match) when is_atom(Var), is_atom(Ctx) -> true;
validate_struct({Var, _Meta, Ctx}, match) when is_atom(Var), is_atom(Ctx) -> true;
validate_struct(Atom, _) when is_atom(Atom) -> true;
validate_struct(_, _) -> false.

validate_kv(Meta, KV, Original, E) ->
  lists:foldl(fun
    ({_K, _V}, Acc) -> Acc + 1;
    (_, Acc) ->
      compile_error(Meta, ?m(E, file),
        "expected key-value pairs in a map, got: ~ts",
        ['Elixir.Macro':to_string(lists:nth(Acc, Original))])
  end, 1, KV).

translate_map(Meta, Args, S) ->
  {Assocs, TUpdate, US} = extract_assoc_update(Args, S),
  translate_map(Meta, Assocs, TUpdate, US).

translate_struct(_Meta, Name, {'%{}', MapMeta, Assocs}, S) when is_tuple(Name) ->
  translate_map(MapMeta, [{'__struct__', Name} | Assocs], nil, S);

translate_struct(Meta, Name, {'%{}', MapMeta, Args}, S) ->
  {Assocs, TUpdate, US} = extract_assoc_update(Args, S),
  Operation = operation(TUpdate, S),

  Struct = case Operation of
    expand -> load_struct(Meta, Name, [Args], S);
    _ -> load_struct(Meta, Name, [], S)
  end,

  assert_struct_keys(Meta, Name, Struct, Assocs, S),

  case Operation of
    update ->
      Ann = ?ann(Meta),
      Generated = ?generated(Meta),
      {VarName, _, VS} = elixir_scope:build_var('_', US),

      Var = {var, Ann, VarName},
      Map = {map, Ann, [{map_field_exact, Ann, {atom, Ann, '__struct__'}, {atom, Ann, Name}}]},

      Match = {match, Ann, Var, Map},
      Error = {tuple, Ann, [{atom, Ann, badstruct}, {atom, Ann, Name}, Var]},

      {TMap, TS} = translate_map(MapMeta, Assocs, Var, VS),

      {{'case', Generated, TUpdate, [
        {clause, Ann, [Match], [], [TMap]},
        {clause, Generated, [Var], [], [elixir_utils:erl_call(Ann, erlang, error, [Error])]}
      ]}, TS};
    match ->
      translate_map(MapMeta, [{'__struct__', Name} | Assocs], nil, US);
    expand ->
      Keys = ['__struct__'] ++ [K || {K, _} <- Assocs],
      {StructAssocs, _} = elixir_quote:escape(maps:to_list(maps:without(Keys, Struct)), false),
      translate_map(MapMeta, [{'__struct__', Name}] ++ StructAssocs ++ Assocs, nil, US)
  end.

%% Helpers

operation(nil, #elixir_scope{context=match}) -> match;
operation(nil, _) -> expand;
operation(_, _) -> update.

load_struct(Meta, Name, Args, S) ->
  Context = lists:keyfind(struct, 1, Meta) == {struct, context},
  Arity = length(Args),

  Local =
    not(ensure_loaded(Name)) andalso
      (Context orelse wait_for_struct(Name)),

  try
    case Local of
      true ->
        try
          apply(elixir_locals:local_for(Name, '__struct__', Arity, def), Args)
        catch
          error:undef  -> apply(Name, '__struct__', Args);
          error:badarg -> apply(Name, '__struct__', Args)
        end;
      false ->
        apply(Name, '__struct__', Args)
    end
  of
    #{} = Struct ->
      Struct;
    Other ->
      compile_error(Meta, S#elixir_scope.file, "expected ~ts.__struct__/~p to "
        "return a map, got: ~ts", [elixir_aliases:inspect(Name), Arity, 'Elixir.Kernel':inspect(Other)])
  catch
    error:undef ->
      Inspected = elixir_aliases:inspect(Name),

      case Context andalso (S#elixir_scope.function == nil) of
        true ->
          compile_error(Meta, S#elixir_scope.file,
            "cannot access struct ~ts, the struct was not yet defined or the struct is being "
            "accessed in the same context that defines it", [Inspected]);
        false ->
          compile_error(Meta, S#elixir_scope.file, "~ts.__struct__/~p is undefined, "
            "cannot expand struct ~ts", [Inspected, Arity, Inspected])
      end;

    Kind:Reason ->
      Info = [{Name, '__struct__', Arity, [{file, "expanding struct"}]},
              elixir_utils:caller(?line(Meta), S#elixir_scope.file,
                                  S#elixir_scope.module, S#elixir_scope.function)],
      erlang:raise(Kind, Reason, prune_stacktrace(erlang:get_stacktrace(), Name, Arity) ++ Info)
  end.

prune_stacktrace([{Module, '__struct__', Arity, _} | _], Module, Arity) ->
  [];
prune_stacktrace([H | T], Module, Arity) ->
  [H | prune_stacktrace(T, Module, Arity)];
prune_stacktrace([], _Module, _Arity) ->
  [].

ensure_loaded(Module) ->
  code:ensure_loaded(Module) == {module, Module}.

wait_for_struct(Module) ->
  is_pid(erlang:get(elixir_compiler_pid)) andalso
    'Elixir.Kernel.ErrorHandler':ensure_compiled(Module, struct).

translate_map(Meta, Assocs, TUpdate, #elixir_scope{extra=Extra} = S) ->
  {Op, KeyFun, ValFun} = extract_key_val_op(TUpdate, S),

  Ann = ?ann(Meta),

  {TArgs, SA} = lists:mapfoldl(fun({Key, Value}, Acc) ->
    {TKey, Acc1}   = KeyFun(Key, Acc),
    {TValue, Acc2} = ValFun(Value, Acc1#elixir_scope{extra=Extra}),
    {{Op, ?ann(Meta), TKey, TValue}, Acc2}
  end, S, Assocs),

  build_map(Ann, TUpdate, TArgs, SA).

extract_assoc_update([{'|', _Meta, [Update, Args]}], S) ->
  {TArg, SA} = elixir_translator:translate_arg(Update, S, S),
  {Args, TArg, SA};
extract_assoc_update(Args, SA) -> {Args, nil, SA}.

extract_key_val_op(_TUpdate, #elixir_scope{context=match}) ->
  {map_field_exact,
    fun(X, Acc) -> elixir_translator:translate(X, Acc#elixir_scope{extra=map_key}) end,
    fun elixir_translator:translate/2};
extract_key_val_op(TUpdate, S) ->
  KS = S#elixir_scope{extra=map_key},
  Op = if TUpdate == nil -> map_field_assoc; true -> map_field_exact end,
  {Op,
    fun(X, Acc) -> elixir_translator:translate_arg(X, Acc, KS) end,
    fun(X, Acc) -> elixir_translator:translate_arg(X, Acc, S) end}.

build_map(Ann, nil, TArgs, SA) -> {{map, Ann, TArgs}, SA};
build_map(Ann, TUpdate, TArgs, SA) -> {{map, Ann, TUpdate, TArgs}, SA}.

assert_struct_keys(Meta, Name, Struct, Assocs, S) ->
  [begin
     compile_error(Meta, S#elixir_scope.file, "unknown key ~ts for struct ~ts",
                   ['Elixir.Macro':to_string(Key), elixir_aliases:inspect(Name)])
   end || {Key, _} <- Assocs, not maps:is_key(Key, Struct)].
