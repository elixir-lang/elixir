-module(elixir_map).
-export([expand_map/3, expand_struct/4]).
-import(elixir_errors, [compile_error/4]).
-include("elixir.hrl").

expand_map(Meta, [{'|', UpdateMeta, [Left, Right]}], #{context := nil} = E) ->
  {[ELeft | ERight], EE} = elixir_exp:expand_args([Left | Right], E),
  validate_kv(Meta, ERight, Right, E),
  {{'%{}', Meta, [{'|', UpdateMeta, [ELeft, ERight]}]}, EE};
expand_map(Meta, [{'|', _, [_, _]}] = Args, #{context := Context, file := File}) ->
  compile_error(Meta, File, "cannot use map/struct update syntax in ~ts, got: ~ts",
                [Context, 'Elixir.Macro':to_string({'%{}', Meta, Args})]);
expand_map(Meta, Args, #{context := match} = E) ->
  {EArgs, EE} =
    lists:mapfoldl(fun
      ({Key, Value}, EA) ->
        {EKey, EK} = elixir_exp:expand(Key, EA),
        validate_match_key(Meta, EKey, EK),
        {EValue, EV} = elixir_exp:expand(Value, EK),
        {{EKey, EValue}, EV};
      (Other, EA) ->
        elixir_exp:expand(Other, EA)
      end, E, Args),
  validate_kv(Meta, EArgs, Args, E),
  {{'%{}', Meta, EArgs}, EE};
expand_map(Meta, Args, E) ->
  {EArgs, EE} = elixir_exp:expand_args(Args, E),
  validate_kv(Meta, EArgs, Args, E),
  {{'%{}', Meta, EArgs}, EE}.

expand_struct(Meta, Left, Right, #{context := Context} = E) ->
  {[ELeft, ERight], EE} = elixir_exp:expand_args([Left, Right], E),

  case validate_struct(ELeft, Context) of
    true when is_atom(ELeft) ->
      %% We always record structs when they are expanded
      %% as they expect the reference at compile time.
      elixir_lexical:record_remote(ELeft, '__struct__', 1, nil, ?line(Meta), ?m(E, lexical_tracker)),

      %% We also include the current module because it won't be present
      %% in context module in case the module name is defined dynamically.
      InContext = lists:member(ELeft, [?m(E, module) | ?m(E, context_modules)]),

      case extract_assocs(Meta, ERight, E) of
        {expand, MapMeta, Assocs} when Context /= match -> %% Expand
          Struct = load_struct(Meta, ELeft, [Assocs], InContext, EE),
          assert_struct_keys(Meta, ELeft, Struct, Assocs, EE),
          Keys = ['__struct__'] ++ [K || {K, _} <- Assocs],
          {StructAssocs, _} = elixir_quote:escape(maps:to_list(maps:without(Keys, Struct)), false),
          {{'%', Meta, [ELeft, {'%{}', MapMeta, StructAssocs ++ Assocs}]}, EE};

        {_, _, Assocs} -> %% Update or match
          Struct = load_struct(Meta, ELeft, [], InContext, EE),
          assert_struct_keys(Meta, ELeft, Struct, Assocs, EE),
          {{'%', Meta, [ELeft, ERight]}, EE}
      end;

    true ->
      %% A match without a compile time struct is treated as a regular map.
      {expand, _, Assocs} = extract_assocs(Meta, ERight, E),
      {{'%{}', Meta, Assocs ++ [{'__struct__', ELeft}]}, EE};

    false when Context == match ->
      compile_error(Meta, ?m(E, file), "expected struct name in a match to be a compile "
        "time atom, alias or a variable, got: ~ts", ['Elixir.Macro':to_string(ELeft)]);

    false ->
      compile_error(Meta, ?m(E, file), "expected struct name to be a compile "
        "time atom or alias, got: ~ts", ['Elixir.Macro':to_string(ELeft)])
  end.

validate_match_key(_Meta, {'^', _, [_]}, _E) ->
  ok;
validate_match_key(Meta, {Name, _, Context}, E) when is_atom(Name), is_atom(Context) ->
  Message = "illegal use of variable ~ts inside map key match, "
            "maps can only match on existing variable by using ^~ts",
  compile_error(Meta, ?m(E, file), Message, [Name, Name]);
validate_match_key(Meta, {Left, _, Right}, E) ->
  validate_match_key(Meta, Left, E),
  validate_match_key(Meta, Right, E);
validate_match_key(Meta, {Left, Right}, E) ->
  validate_match_key(Meta, Left, E),
  validate_match_key(Meta, Right, E);
validate_match_key(Meta, List, E) when is_list(List) ->
  [validate_match_key(Meta, Each, E) || Each <- List];
validate_match_key(_, _, _) ->
  ok.

validate_kv(Meta, KV, Original, E) ->
  lists:foldl(fun
    ({_K, _V}, Acc) ->
      Acc + 1;
    (_, Acc) ->
      compile_error(Meta, ?m(E, file),
        "expected key-value pairs in a map, got: ~ts",
        ['Elixir.Macro':to_string(lists:nth(Acc, Original))])
  end, 1, KV).

extract_assocs(_, {'%{}', Meta, [{'|', _, [_, Assocs]}]}, _) ->
  {update, Meta, Assocs};
extract_assocs(_, {'%{}', Meta, Assocs}, _) ->
  {expand, Meta, Assocs};
extract_assocs(Meta, Other, E) ->
  compile_error(Meta, ?m(E, file),
                "expected struct to be followed by a map, got: ~ts",
                ['Elixir.Macro':to_string(Other)]).

validate_struct({'^', _, [{Var, _, Ctx}]}, match) when is_atom(Var), is_atom(Ctx) -> true;
validate_struct({Var, _Meta, Ctx}, match) when is_atom(Var), is_atom(Ctx) -> true;
validate_struct(Atom, _) when is_atom(Atom) -> true;
validate_struct(_, _) -> false.

load_struct(Meta, Name, Args, InContext, E) ->
  Arity = length(Args),

  Local =
    not(ensure_loaded(Name)) andalso
      (InContext orelse wait_for_struct(Name)),

  try
    case Local andalso elixir_def:local_for(Name, '__struct__', Arity, [def]) of
      false -> apply(Name, '__struct__', Args);
      LocalFun -> apply(LocalFun, Args)
    end
  of
    #{} = Struct ->
      Struct;
    Other ->
      compile_error(Meta, ?m(E, file), "expected ~ts.__struct__/~p to "
        "return a map, got: ~ts", [elixir_aliases:inspect(Name), Arity, 'Elixir.Kernel':inspect(Other)])
  catch
    error:undef ->
      Inspected = elixir_aliases:inspect(Name),

      case InContext andalso (?m(E, function) == nil) of
        true ->
          compile_error(Meta, ?m(E, file),
            "cannot access struct ~ts, the struct was not yet defined or the struct is being "
            "accessed in the same context that defines it", [Inspected]);
        false ->
          compile_error(Meta, ?m(E, file), "~ts.__struct__/~p is undefined, "
            "cannot expand struct ~ts", [Inspected, Arity, Inspected])
      end;

    Kind:Reason ->
      Info = [{Name, '__struct__', Arity, [{file, "expanding struct"}]},
              elixir_utils:caller(?line(Meta), ?m(E, file), ?m(E, module), ?m(E, function))],
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

assert_struct_keys(Meta, Name, Struct, Assocs, E) ->
  [begin
     compile_error(Meta, ?m(E, file), "unknown key ~ts for struct ~ts",
                   ['Elixir.Macro':to_string(Key), elixir_aliases:inspect(Name)])
   end || {Key, _} <- Assocs, not maps:is_key(Key, Struct)].
