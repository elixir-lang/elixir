-module(elixir_map).
-export([expand_map/4, expand_struct/5, format_error/1, maybe_load_struct/5]).
-import(elixir_errors, [function_error/4, file_error/4, file_warn/4]).
-include("elixir.hrl").

expand_map(Meta, [{'|', UpdateMeta, [Left, Right]}], S, #{context := nil} = E) ->
  {[ELeft | ERight], SE, EE} = elixir_expand:expand_args([Left | Right], S, E),
  validate_kv(Meta, ERight, Right, E),
  {{'%{}', Meta, [{'|', UpdateMeta, [ELeft, ERight]}]}, SE, EE};
expand_map(Meta, [{'|', _, [_, _]}] = Args, _S, #{context := Context, file := File}) ->
  file_error(Meta, File, ?MODULE, {update_syntax_in_wrong_context, Context, {'%{}', Meta, Args}});
expand_map(Meta, Args, S, E) ->
  {EArgs, SE, EE} = elixir_expand:expand_args(Args, S, E),
  validate_kv(Meta, EArgs, Args, E),
  {{'%{}', Meta, EArgs}, SE, EE}.

expand_struct(Meta, Left, {'%{}', MapMeta, MapArgs}, S, #{context := Context} = E) ->
  CleanMapArgs = clean_struct_key_from_map_args(Meta, MapArgs, E),
  {[ELeft, ERight], SE, EE} = elixir_expand:expand_args([Left, {'%{}', MapMeta, CleanMapArgs}], S, E),

  case validate_struct(ELeft, Context) of
    true when is_atom(ELeft) ->
      case extract_struct_assocs(Meta, ERight, E) of
        {expand, MapMeta, Assocs} when Context /= match -> %% Expand
          AssocKeys = [K || {K, _} <- Assocs],
          Struct = load_struct(Meta, ELeft, [Assocs], Assocs, EE),
          Keys = ['__struct__'] ++ AssocKeys,
          WithoutKeys = lists:sort(maps:to_list(maps:without(Keys, Struct))),
          StructAssocs = elixir_quote:escape(WithoutKeys, none, false),
          {{'%', Meta, [ELeft, {'%{}', MapMeta, StructAssocs ++ Assocs}]}, SE, EE};

        {_, _, Assocs} -> %% Update or match
          _ = load_struct(Meta, ELeft, [], Assocs, EE),
          {{'%', Meta, [ELeft, ERight]}, SE, EE}
      end;

    true ->
      {{'%', Meta, [ELeft, ERight]}, SE, EE};

    false when Context == match ->
      file_error(Meta, E, ?MODULE, {invalid_struct_name_in_match, ELeft});

    false ->
      file_error(Meta, E, ?MODULE, {invalid_struct_name, ELeft})
  end;
expand_struct(Meta, _Left, Right, _S, E) ->
  file_error(Meta, E, ?MODULE, {non_map_after_struct, Right}).

clean_struct_key_from_map_args(Meta, [{'|', PipeMeta, [Left, MapAssocs]}], E) ->
  [{'|', PipeMeta, [Left, clean_struct_key_from_map_assocs(Meta, MapAssocs, E)]}];
clean_struct_key_from_map_args(Meta, MapAssocs, E) ->
  clean_struct_key_from_map_assocs(Meta, MapAssocs, E).

clean_struct_key_from_map_assocs(Meta, Assocs, E) ->
  case lists:keytake('__struct__', 1, Assocs) of
    {value, _, CleanAssocs} ->
      file_warn(Meta, ?key(E, file), ?MODULE, ignored_struct_key_in_struct),
      CleanAssocs;
    false ->
      Assocs
  end.

validate_match_key(Meta, {Name, _, Context}, E) when is_atom(Name), is_atom(Context) ->
  file_error(Meta, E, ?MODULE, {invalid_variable_in_map_key_match, Name});
validate_match_key(Meta, {'::', _, [Left, _]}, E) ->
  validate_match_key(Meta, Left, E);
validate_match_key(_, {'^', _, [{Name, _, Context}]}, _) when is_atom(Name), is_atom(Context) ->
  ok;
validate_match_key(_, {'%{}', _, [_ | _]}, _) ->
  ok;
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

validate_not_repeated(Meta, Key, Used, E) ->
  case is_literal(Key) andalso Used of
    #{Key := true} ->
      case E of
        #{context := match} -> function_error(Meta, ?key(E, file), ?MODULE, {repeated_key, Key});
        _ -> file_warn(Meta, ?key(E, file), ?MODULE, {repeated_key, Key})
      end,
      Used;

    #{} ->
      Used#{Key => true};

    false ->
      Used
  end.

is_literal({_, _, _}) -> false;
is_literal({Left, Right}) -> is_literal(Left) andalso is_literal(Right);
is_literal([_ | _] = List) -> lists:all(fun is_literal/1, List);
is_literal(_) -> true.

validate_kv(Meta, KV, Original, #{context := Context} = E) ->
  lists:foldl(fun
    ({K, _V}, {Index, Used}) ->
      (Context == match) andalso validate_match_key(Meta, K, E),
      NewUsed = validate_not_repeated(Meta, K, Used, E),
      {Index + 1, NewUsed};
    (_, {Index, _Used}) ->
      file_error(Meta, E, ?MODULE, {not_kv_pair, lists:nth(Index, Original)})
  end, {1, #{}}, KV).

extract_struct_assocs(_, {'%{}', Meta, [{'|', _, [_, Assocs]}]}, _) ->
  {update, Meta, delete_struct_key(Assocs)};
extract_struct_assocs(_, {'%{}', Meta, Assocs}, _) ->
  {expand, Meta, delete_struct_key(Assocs)};
extract_struct_assocs(Meta, Other, E) ->
  file_error(Meta, E, ?MODULE, {non_map_after_struct, Other}).

delete_struct_key(Assocs) ->
  lists:keydelete('__struct__', 1, Assocs).

validate_struct({'^', _, [{Var, _, Ctx}]}, match) when is_atom(Var), is_atom(Ctx) -> true;
validate_struct({Var, _Meta, Ctx}, match) when is_atom(Var), is_atom(Ctx) -> true;
validate_struct(Atom, _) when is_atom(Atom) -> true;
validate_struct(_, _) -> false.

load_struct(Meta, Name, Args, Assocs, E) ->
  Keys = [begin
    is_atom(K) orelse function_error(Meta, E, ?MODULE, {invalid_key_for_struct, K}),
    K
  end || {K, _} <- Assocs],

  case maybe_load_struct(Meta, Name, Args, Keys, E) of
    {ok, Struct} -> Struct;
    {error, Desc} -> file_error(Meta, E, ?MODULE, Desc)
  end.

maybe_load_struct(Meta, Name, Args, Keys, E) ->
  try
    wrapped_maybe_load_struct(Meta, Name, Args, Keys, E)
  catch
    Kind:Reason ->
      Info = [{Name, '__struct__', length(Args), [{file, "expanding struct"}]},
              elixir_utils:caller(?line(Meta), ?key(E, file), ?key(E, module), ?key(E, function))],
      erlang:raise(Kind, Reason, Info)
  end.

wrapped_maybe_load_struct(Meta, Name, Args, Keys, E) ->
  %% We also include the current module because it won't be present
  %% in context module in case the module name is defined dynamically.
  Module = ?key(E, module),
  InContext = lists:member(Name, [Module | ?key(E, context_modules)]),

  Arity = length(Args),
  External = InContext orelse (not(ensure_loaded(Name)) andalso wait_for_struct(Name)),

  try
    case External andalso elixir_def:external_for(Meta, Name, '__struct__', Arity, [def]) of
      %% If I am accessing myself and there is no __struct__ function,
      %% don't invoke the fallback to avoid calling loaded code.
      false when Module == Name ->
        error(undef);

      false ->
        apply(Name, '__struct__', Args);

      ExternalFun ->
        %% There is an inherent race condition when using local_for.
        %% By the time we got to execute the function, the ETS table
        %% with temporary definitions for the given module may no longer
        %% be available, so any function invocation happening inside the
        %% local function will fail. In this case, we need to fall back to
        %% the regular dispatching since the module will be available if
        %% the table has not been deleted (unless compilation of that
        %% module failed which should then cause this call to fail too).
        try
          apply(ExternalFun, Args)
        catch
          error:undef -> apply(Name, '__struct__', Args)
        end
    end
  of
    #{'__struct__' := Name} = Struct ->
      assert_struct_keys(Meta, Name, Struct, Keys, E),
      elixir_env:trace({struct_expansion, Meta, Name, Keys}, E),
      {ok, Struct};

    #{'__struct__' := StructName} when is_atom(StructName) ->
      {error, {struct_name_mismatch, Name, Arity, StructName}};

    Other ->
      {error, {invalid_struct_return_value, Name, Arity, Other}}
  catch
    error:undef ->
      case InContext andalso (?key(E, function) == nil) of
        true ->
          {error, {inaccessible_struct, Name}};
        false ->
          {error, {undefined_struct, Name, Arity}}
      end
  end.

ensure_loaded(Module) ->
  code:ensure_loaded(Module) == {module, Module}.

wait_for_struct(Module) ->
  (erlang:get(elixir_compiler_info) /= undefined) andalso
    ('Elixir.Kernel.ErrorHandler':ensure_compiled(Module, struct, hard) =:= found).

assert_struct_keys(Meta, Name, Struct, Keys, E) ->
  [begin
     function_error(Meta, E, ?MODULE, {unknown_key_for_struct, Name, Key})
   end || Key <- Keys, not maps:is_key(Key, Struct)],
  ok.

format_error({update_syntax_in_wrong_context, Context, Expr}) ->
  io_lib:format("cannot use map/struct update syntax in ~ts, got: ~ts",
                [Context, 'Elixir.Macro':to_string(Expr)]);
format_error({invalid_struct_name_in_match, Expr}) ->
  Message =
    "expected struct name in a match to be a compile time atom, alias or a "
    "variable, got: ~ts",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]);
format_error({invalid_struct_name, Expr}) ->
  Message = "expected struct name to be a compile time atom or alias, got: ~ts",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]);
format_error({invalid_variable_in_map_key_match, Name}) ->
  Message =
    "cannot use variable ~ts as map key inside a pattern. Map keys in patterns can only be literals "
    "(such as atoms, strings, tuples, and the like) or an existing variable matched with the pin operator "
    "(such as ^some_var)",
  io_lib:format(Message, [Name]);
format_error({repeated_key, Key}) ->
    io_lib:format("key ~ts will be overridden in map", ['Elixir.Macro':to_string(Key)]);
format_error({not_kv_pair, Expr}) ->
  io_lib:format("expected key-value pairs in a map, got: ~ts",
                ['Elixir.Macro':to_string(Expr)]);
format_error({non_map_after_struct, Expr}) ->
  io_lib:format("expected struct to be followed by a map, got: ~ts",
                ['Elixir.Macro':to_string(Expr)]);
format_error({struct_name_mismatch, Module, Arity, StructName}) ->
  Name = elixir_aliases:inspect(Module),
  Message = "expected struct name returned by ~ts.__struct__/~p to be ~ts, got: ~ts",
  io_lib:format(Message, [Name, Arity, Name, elixir_aliases:inspect(StructName)]);
format_error({invalid_struct_return_value, Module, Arity, Value}) ->
  Message =
    "expected ~ts.__struct__/~p to return a map with a :__struct__ key that holds the "
    "name of the struct (atom), got: ~ts",
  io_lib:format(Message, [elixir_aliases:inspect(Module), Arity, 'Elixir.Kernel':inspect(Value)]);
format_error({inaccessible_struct, Module}) ->
  Message =
    "cannot access struct ~ts, the struct was not yet defined or the struct is "
    "being accessed in the same context that defines it",
  io_lib:format(Message, [elixir_aliases:inspect(Module)]);
format_error({undefined_struct, Module, Arity}) ->
  Name = elixir_aliases:inspect(Module),
  io_lib:format(
    "~ts.__struct__/~p is undefined, cannot expand struct ~ts. "
    "Make sure the struct name is correct. If the struct name exists and is correct "
    "but it still cannot be found, you likely have cyclic module usage in your code",
    [Name, Arity, Name]);
format_error({unknown_key_for_struct, Module, Key}) ->
  io_lib:format("unknown key ~ts for struct ~ts",
                ['Elixir.Macro':to_string(Key), elixir_aliases:inspect(Module)]);
format_error({invalid_key_for_struct, Key}) ->
  io_lib:format("invalid key ~ts for struct, struct keys must be atoms, got: ",
                ['Elixir.Macro':to_string(Key)]);
format_error(ignored_struct_key_in_struct) ->
  "key :__struct__ is ignored when using structs".
