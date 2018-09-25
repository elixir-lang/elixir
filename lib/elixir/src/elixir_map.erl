-module(elixir_map).
-export([expand_map/3, expand_struct/4, format_error/1]).
-import(elixir_errors, [form_error/4, form_warn/4]).
-include("elixir.hrl").

expand_map(Meta, [{'|', UpdateMeta, [Left, Right]}], #{context := nil} = E) ->
  {[ELeft | ERight], EE} = elixir_expand:expand_args([Left | Right], E),
  validate_kv(Meta, ERight, Right, E),
  {{'%{}', Meta, [{'|', UpdateMeta, [ELeft, ERight]}]}, EE};
expand_map(Meta, [{'|', _, [_, _]}] = Args, #{context := Context, file := File}) ->
  form_error(Meta, File, ?MODULE, {update_syntax_in_wrong_context, Context, {'%{}', Meta, Args}});
expand_map(Meta, Args, E) ->
  {EArgs, EE} = elixir_expand:expand_args(Args, E),
  validate_kv(Meta, EArgs, Args, E),
  {{'%{}', Meta, EArgs}, EE}.

expand_struct(Meta, Left, {'%{}', MapMeta, MapArgs}, #{context := Context} = E) ->
  CleanMapArgs = clean_struct_key_from_map_args(Meta, MapArgs, E),
  {[ELeft, ERight], EE} = elixir_expand:expand_args([Left, {'%{}', MapMeta, CleanMapArgs}], E),

  case validate_struct(ELeft, Context) of
    true when is_atom(ELeft) ->
      %% We always record structs when they are expanded
      %% as they expect the reference at compile time.
      elixir_lexical:record_struct(ELeft, ?line(Meta), ?key(E, lexical_tracker)),

      %% We also include the current module because it won't be present
      %% in context module in case the module name is defined dynamically.
      InContext = lists:member(ELeft, [?key(E, module) | ?key(E, context_modules)]),

      case extract_struct_assocs(Meta, ERight, E) of
        {expand, MapMeta, Assocs} when Context /= match -> %% Expand
          Struct = load_struct(Meta, ELeft, [Assocs], InContext, EE),
          assert_struct_keys(Meta, ELeft, Struct, Assocs, EE),
          Keys = ['__struct__'] ++ [K || {K, _} <- Assocs],
          WithoutKeys = maps:to_list(maps:without(Keys, Struct)),
          StructAssocs = elixir_quote:escape(WithoutKeys, default, false),
          {{'%', Meta, [ELeft, {'%{}', MapMeta, StructAssocs ++ Assocs}]}, EE};

        {_, _, Assocs} -> %% Update or match
          Struct = load_struct(Meta, ELeft, [], InContext, EE),
          assert_struct_keys(Meta, ELeft, Struct, Assocs, EE),
          {{'%', Meta, [ELeft, ERight]}, EE}
      end;

    true ->
      {{'%', Meta, [ELeft, ERight]}, EE};

    false when Context == match ->
      form_error(Meta, ?key(E, file), ?MODULE, {invalid_struct_name_in_match, ELeft});

    false ->
      form_error(Meta, ?key(E, file), ?MODULE, {invalid_struct_name, ELeft})
  end;
expand_struct(Meta, _Left, Right, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {non_map_after_struct, Right}).

clean_struct_key_from_map_args(Meta, [{'|', PipeMeta, [Left, MapAssocs]}], E) ->
  [{'|', PipeMeta, [Left, clean_struct_key_from_map_assocs(Meta, MapAssocs, E)]}];
clean_struct_key_from_map_args(Meta, MapAssocs, E) ->
  clean_struct_key_from_map_assocs(Meta, MapAssocs, E).

clean_struct_key_from_map_assocs(Meta, Assocs, E) ->
  case lists:keytake('__struct__', 1, Assocs) of
    {value, _, CleanAssocs} ->
      form_warn(Meta, ?key(E, file), ?MODULE, ignored_struct_key_in_struct),
      CleanAssocs;
    false ->
      Assocs
  end.

validate_match_key(Meta, {'^', _, [{Name, _, Context}]}, E) when is_atom(Name), is_atom(Context) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_pin_in_map_key_match, Name});
validate_match_key(Meta, {Name, _, Context}, E) when is_atom(Name), is_atom(Context) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_variable_in_map_key_match, Name});
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
  try
    Literal = literal_key(Key),
    case Used of
      #{Literal := true} ->
        form_warn(Meta, ?key(E, file), ?MODULE, {repeated_key, Key});

      #{} ->
        Used#{Literal => true}
    end
  catch
    non_literal_key -> Used
  end.

literal_key(Literal) when is_atom(Literal); is_number(Literal) ->
  Literal;
literal_key({'%{}', _, Args}) ->
  maps:from_list(literal_key(Args));
literal_key({'{}', _, Args}) ->
  erlang:list_to_tuple(literal_key(Args));
literal_key({_, _} = Tuple) ->
  Tuple;
literal_key([Head | Tail]) ->
  [literal_key(Head) | literal_key(Tail)];
literal_key([]) ->
  [];
literal_key(_) ->
  throw(non_literal_key).

validate_kv(Meta, KV, Original, #{context := Context} = E) ->
  lists:foldl(fun
    ({{'^', _, [_]}, _}, {Index, Used}) ->
      {Index + 1, Used};
    ({K, _V}, {Index, Used}) ->
      (Context == match) andalso validate_match_key(Meta, K, E),
      NewUsed = validate_not_repeated(Meta, K, Used, E),
      {Index + 1, NewUsed};
    (_, {Index, _Used}) ->
      form_error(Meta, ?key(E, file), ?MODULE, {not_kv_pair, lists:nth(Index, Original)})
  end, {1, #{}}, KV).

extract_struct_assocs(_, {'%{}', Meta, [{'|', _, [_, Assocs]}]}, _) ->
  {update, Meta, delete_struct_key(Assocs)};
extract_struct_assocs(_, {'%{}', Meta, Assocs}, _) ->
  {expand, Meta, delete_struct_key(Assocs)};
extract_struct_assocs(Meta, Other, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {non_map_after_struct, Other}).

delete_struct_key(Assocs) ->
  lists:keydelete('__struct__', 1, Assocs).

validate_struct({'^', _, [{Var, _, Ctx}]}, match) when is_atom(Var), is_atom(Ctx) -> true;
validate_struct({Var, _Meta, Ctx}, match) when is_atom(Var), is_atom(Ctx) -> true;
validate_struct(Atom, _) when is_atom(Atom) -> true;
validate_struct(_, _) -> false.

load_struct(Meta, Name, Args, InContext, E) ->
  Arity = length(Args),
  Local = InContext orelse (not(ensure_loaded(Name)) andalso wait_for_struct(Name)),

  try
    case Local andalso elixir_def:local_for(Name, '__struct__', Arity, [def]) of
      false ->
        apply(Name, '__struct__', Args);
      LocalFun ->
        %% There is an inherent race condition when using local_for.
        %% By the time we got to execute the function, the ETS table
        %% with temporary definitions for the given module may no longer
        %% be available, so any function invocation happening inside the
        %% local function will fail. In this case, we need to fall back to
        %% the regular dispatching since the module will be available if
        %% the table has not been deleted (unless compilation of that
        %% module failed which should then cause this call to fail too).
        try
          apply(LocalFun, Args)
        catch
          error:undef -> apply(Name, '__struct__', Args)
        end
    end
  of
    #{} = Struct ->
      Struct;
    Other ->
      form_error(Meta, ?key(E, file), ?MODULE, {invalid_struct_return_value, Name, Arity, Other})
  catch
    error:undef ->
      case InContext andalso (?key(E, function) == nil) of
        true ->
          form_error(Meta, ?key(E, file), ?MODULE, {inaccessible_struct, Name});
        false ->
          form_error(Meta, ?key(E, file), ?MODULE, {undefined_struct, Name, Arity})
      end;

    Kind:Reason ->
      Info = [{Name, '__struct__', Arity, [{file, "expanding struct"}]},
              elixir_utils:caller(?line(Meta), ?key(E, file), ?key(E, module), ?key(E, function))],
      erlang:raise(Kind, Reason, Info)
  end.

ensure_loaded(Module) ->
  code:ensure_loaded(Module) == {module, Module}.

wait_for_struct(Module) ->
  is_pid(erlang:get(elixir_compiler_pid)) andalso
    'Elixir.Kernel.ErrorHandler':ensure_compiled(Module, struct).

assert_struct_keys(Meta, Name, Struct, Assocs, E) ->
  [begin
     form_error(Meta, ?key(E, file), ?MODULE, {unknown_key_for_struct, Name, Key})
   end || {Key, _} <- Assocs, not maps:is_key(Key, Struct)].

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
format_error({invalid_pin_in_map_key_match, Name}) ->
  Message =
    "cannot use pin operator ^~ts inside a data structure as a map key in a pattern. "
    "The pin operator can only be used as the whole key",
  io_lib:format(Message, [Name]);
format_error({invalid_variable_in_map_key_match, Name}) ->
  Message =
    "cannot use variable ~ts as map key inside a pattern. Map keys in patterns can only be literals "
    "(such as atoms, strings, tuples, etc) or an existing variable matched with the pin operator "
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
format_error({invalid_struct_return_value, Module, Arity, Expr}) ->
  io_lib:format("expected ~ts.__struct__/~p to return a map, got: ~ts",
                ['Elixir.Macro':to_string(Module), Arity, 'Elixir.Macro':to_string(Expr)]);
format_error({inaccessible_struct, Module}) ->
  Message =
    "cannot access struct ~ts, the struct was not yet defined or the struct is "
    "being accessed in the same context that defines it",
  io_lib:format(Message, ['Elixir.Macro':to_string(Module)]);
format_error({undefined_struct, Module, Arity}) ->
  StringName = 'Elixir.Macro':to_string(Module),
  io_lib:format("~ts.__struct__/~p is undefined, cannot expand struct ~ts",
                [StringName, Arity, StringName]);
format_error({unknown_key_for_struct, Module, Key}) ->
  io_lib:format("unknown key ~ts for struct ~ts",
                ['Elixir.Macro':to_string(Key), 'Elixir.Macro':to_string(Module)]);
format_error(ignored_struct_key_in_struct) ->
  "key :__struct__ is ignored when using structs".
