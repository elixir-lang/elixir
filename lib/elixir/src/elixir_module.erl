-module(elixir_module).
-export([file/1, data_tables/1, is_open/1, mode/1, delete_definition_attributes/6,
         compile/4, expand_callback/6, format_error/1, compiler_modules/0,
         write_cache/3, read_cache/2, next_counter/1]).
-include("elixir.hrl").
-define(counter_attr, {elixir, counter}).

%% Stores modules currently being defined by the compiler

compiler_modules() ->
  case erlang:get(elixir_compiler_modules) of
    undefined -> [];
    M when is_list(M) -> M
  end.

put_compiler_modules([]) ->
  erlang:erase(elixir_compiler_modules);
put_compiler_modules(M) when is_list(M) ->
  erlang:put(elixir_compiler_modules, M).

%% Table functions

file(Module) ->
  ets:lookup_element(elixir_modules, Module, 4).

data_tables(Module) ->
  ets:lookup_element(elixir_modules, Module, 2).

is_open(Module) ->
  ets:member(elixir_modules, Module).

mode(Module) ->
  try ets:lookup_element(elixir_modules, Module, 5) of
    Mode -> Mode
  catch
    _:badarg -> closed
  end.

make_readonly(Module) ->
  ets:update_element(elixir_modules, Module, {5, readonly}).

delete_definition_attributes(#{module := Module}, _, _, _, _, _) ->
  {DataSet, _} = data_tables(Module),
  ets:delete(DataSet, doc),
  ets:delete(DataSet, deprecated),
  ets:delete(DataSet, impl).

write_cache(Module, Key, Value) ->
  {DataSet, _} = data_tables(Module),
  ets:insert(DataSet, {{cache, Key}, Value}).

read_cache(Module, Key) ->
  {DataSet, _} = data_tables(Module),
  ets:lookup_element(DataSet, {cache, Key}, 2).

next_counter(nil) -> erlang:unique_integer();
next_counter(Module) ->
  try
    {DataSet, _} = data_tables(Module),
    {Module, ets:update_counter(DataSet, ?counter_attr, 1)}
  catch
    _:_ -> erlang:unique_integer()
  end.

%% Compilation hook

compile(Module, Block, Vars, Env) when is_atom(Module) ->
  #{line := Line, function := Function, versioned_vars := OldVerVars} = Env,

  {VerVars, _} =
    lists:mapfoldl(fun({Var, _}, I) -> {{Var, I}, I + 1} end, 0, maps:to_list(OldVerVars)),

  BaseEnv = Env#{module := Module, versioned_vars := maps:from_list(VerVars)},

  MaybeLexEnv =
    case Function of
      nil -> BaseEnv;
      _   -> BaseEnv#{lexical_tracker := nil, tracers := [], function := nil}
    end,

  case MaybeLexEnv of
    #{lexical_tracker := nil} ->
      elixir_lexical:run(
        MaybeLexEnv,
        fun(LexEnv) -> compile(Line, Module, Block, Vars, LexEnv) end,
        fun(_LexEnv) -> ok end
      );
    _ ->
      compile(Line, Module, Block, Vars, MaybeLexEnv)
  end;
compile(Module, _Block, _Vars, #{line := Line, file := File}) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {invalid_module, Module}).

compile(Line, Module, Block, Vars, E) ->
  File = ?key(E, file),
  check_module_availability(Line, File, Module),
  ModuleAsCharlist = validate_module_name(Line, File, Module),

  CompilerModules = compiler_modules(),
  {Tables, Ref} = build(Line, File, Module),
  {DataSet, DataBag} = Tables,

  try
    put_compiler_modules([Module | CompilerModules]),
    {Result, NE} = eval_form(Line, Module, DataBag, Block, Vars, E),
    CheckerInfo = checker_info(),

    {Binary, PersistedAttributes, Autoload} =
      elixir_erl_compiler:spawn(fun() ->
        PersistedAttributes = ets:lookup_element(DataBag, persisted_attributes, 2),
        Attributes = attributes(DataSet, DataBag, PersistedAttributes),
        {AllDefinitions, Private} = elixir_def:fetch_definitions(File, Module),

        OnLoadAttribute = lists:keyfind(on_load, 1, Attributes),
        NewPrivate = validate_on_load_attribute(OnLoadAttribute, AllDefinitions, Private, File, Line),

        DialyzerAttribute = lists:keyfind(dialyzer, 1, Attributes),
        validate_dialyzer_attribute(DialyzerAttribute, AllDefinitions, File, Line),

        Unreachable = elixir_locals:warn_unused_local(File, Module, AllDefinitions, NewPrivate),
        elixir_locals:ensure_no_undefined_local(File, Module, AllDefinitions),
        elixir_locals:ensure_no_import_conflict(File, Module, AllDefinitions),

        %% We stop tracking locals here to avoid race conditions in case after_load
        %% evaluates code in a separate process that may write to locals table.
        elixir_locals:stop({DataSet, DataBag}),
        make_readonly(Module),

        (not elixir_config:is_bootstrap()) andalso
         'Elixir.Module':'__check_attributes__'(E, DataSet, DataBag),

        RawCompileOpts = bag_lookup_element(DataBag, {accumulate, compile}, 2),
        CompileOpts = validate_compile_opts(RawCompileOpts, AllDefinitions, Unreachable, File, Line),
        UsesBehaviours = bag_lookup_element(DataBag, {accumulate, behaviour}, 2),
        Impls = bag_lookup_element(DataBag, impls, 2),

        AfterVerify = bag_lookup_element(DataBag, {accumulate, after_verify}, 2),
        [elixir_env:trace({remote_function, [], VerifyMod, VerifyFun, 1}, E) ||
         {VerifyMod, VerifyFun} <- AfterVerify],

        ModuleMap = #{
          struct => get_struct(DataSet),
          module => Module,
          line => Line,
          file => File,
          relative_file => elixir_utils:relative_to_cwd(File),
          attributes => Attributes,
          definitions => AllDefinitions,
          unreachable => Unreachable,
          after_verify => AfterVerify,
          compile_opts => CompileOpts,
          deprecated => get_deprecated(DataBag),
          defines_behaviour => defines_behaviour(DataBag),
          uses_behaviours => UsesBehaviours,
          impls => Impls
        },

        Binary = elixir_erl:compile(ModuleMap),
        Autoload = proplists:get_value(autoload, CompileOpts, true),
        spawn_parallel_checker(CheckerInfo, Module, ModuleMap),
        {Binary, PersistedAttributes, Autoload}
      end),

    Autoload andalso code:load_binary(Module, beam_location(ModuleAsCharlist), Binary),
    eval_callbacks(Line, DataBag, after_compile, [NE, Binary], NE),
    elixir_env:trace({on_module, Binary, none}, E),
    warn_unused_attributes(File, DataSet, DataBag, PersistedAttributes),
    make_module_available(Module, Binary),
    (CheckerInfo == undefined) andalso
      [VerifyMod:VerifyFun(Module) ||
       {VerifyMod, VerifyFun} <- bag_lookup_element(DataBag, {accumulate, after_verify}, 2)],
    {module, Module, Binary, Result}
  catch
    error:undef:Stacktrace ->
      case Stacktrace of
        [{Module, Fun, Args, _Info} | _] = Stack when is_list(Args) ->
          compile_undef(Module, Fun, length(Args), Stack);
        [{Module, Fun, Arity, _Info} | _] = Stack ->
          compile_undef(Module, Fun, Arity, Stack);
        Stack ->
          erlang:raise(error, undef, Stack)
      end
  after
    put_compiler_modules(CompilerModules),
    ets:delete(DataSet),
    ets:delete(DataBag),
    elixir_code_server:call({undefmodule, Ref})
  end.

validate_compile_opts(Opts, Defs, Unreachable, File, Line) ->
  lists:flatmap(fun (Opt) -> validate_compile_opt(Opt, Defs, Unreachable, File, Line) end, Opts).

%% TODO: Make this an error on v2.0
validate_compile_opt({parse_transform, Module} = Opt, _Defs, _Unreachable, File, Line) ->
  elixir_errors:form_warn([{line, Line}], File, ?MODULE, {parse_transform, Module}),
  [Opt];
validate_compile_opt({inline, Inlines}, Defs, Unreachable, File, Line) ->
  case validate_inlines(Inlines, Defs, Unreachable, []) of
    {ok, []} -> [];
    {ok, FilteredInlines} -> [{inline, FilteredInlines}];
    {error, Def} -> elixir_errors:form_error([{line, Line}], File, ?MODULE, {bad_inline, Def})
  end;
validate_compile_opt(Opt, Defs, Unreachable, File, Line) when is_list(Opt) ->
  validate_compile_opts(Opt, Defs, Unreachable, File, Line);
validate_compile_opt(Opt, _Defs, _Unreachable, _File, _Line) ->
  [Opt].

validate_inlines([Inline | Inlines], Defs, Unreachable, Acc) ->
  case lists:keyfind(Inline, 1, Defs) of
    false -> {error, Inline};
    _ ->
      case lists:member(Inline, Unreachable) of
        true -> validate_inlines(Inlines, Defs, Unreachable, Acc);
        false -> validate_inlines(Inlines, Defs, Unreachable, [Inline | Acc])
      end
  end;
validate_inlines([], _Defs, _Unreachable, Acc) -> {ok, Acc}.

validate_on_load_attribute({on_load, Def}, Defs, Private, File, Line) ->
  case lists:keyfind(Def, 1, Defs) of
    false ->
      elixir_errors:form_error([{line, Line}], File, ?MODULE, {undefined_on_load, Def});
    {_, Kind, _, _} when Kind == def; Kind == defp ->
      lists:keydelete(Def, 1, Private);
    {_, WrongKind, _, _} ->
      elixir_errors:form_error([{line, Line}], File, ?MODULE, {wrong_kind_on_load, Def, WrongKind})
  end;
validate_on_load_attribute(false, _Module, Private, _File, _Line) -> Private.

validate_dialyzer_attribute({dialyzer, Dialyzer}, Defs, File, Line) ->
  [case lists:keyfind(Fun, 1, Defs) of
    false ->
      elixir_errors:form_error([{line, Line}], File, ?MODULE, {bad_dialyzer, Key, Fun});
    _ ->
      ok
   end || {Key, Funs} <- lists:flatten([Dialyzer]), Fun <- lists:flatten([Funs])];
validate_dialyzer_attribute(false, _Defs, _File, _Line) ->
  ok.

defines_behaviour(DataBag) ->
  ets:member(DataBag, {accumulate, callback}) orelse ets:member(DataBag, {accumulate, macrocallback}).

%% An undef error for a function in the module being compiled might result in an
%% exception message suggesting the current module is not loaded. This is
%% misleading so use a custom reason.
compile_undef(Module, Fun, Arity, Stack) ->
  case elixir_config:is_bootstrap() of
    false ->
      Opts = [{module, Module}, {function, Fun}, {arity, Arity},
              {reason, 'function not available'}],
      Exception = 'Elixir.UndefinedFunctionError':exception(Opts),
      erlang:raise(error, Exception, Stack);
    true ->
      erlang:raise(error, undef, Stack)
  end.

%% Handle reserved modules and duplicates.

check_module_availability(Line, File, Module) ->
  Reserved = ['Elixir.True', 'Elixir.False', 'Elixir.Nil',
              'Elixir.Any', 'Elixir.BitString', 'Elixir.PID',
              'Elixir.Reference', 'Elixir.Elixir', 'Elixir'],

  case lists:member(Module, Reserved) of
    true  -> elixir_errors:form_error([{line, Line}], File, ?MODULE, {module_reserved, Module});
    false -> ok
  end,

  case elixir_config:get(ignore_module_conflict) of
    false ->
      case code:ensure_loaded(Module) of
        {module, _} ->
          elixir_errors:form_warn([{line, Line}], File, ?MODULE, {module_defined, Module});
        {error, _}  ->
          ok
      end;
    true ->
      ok
  end.

validate_module_name(Line, File, Module) when Module == nil; is_boolean(Module) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {invalid_module, Module});
validate_module_name(Line, File, Module) ->
  Charlist = atom_to_list(Module),
  case lists:any(fun(Char) -> (Char =:= $/) or (Char =:= $\\) end, Charlist) of
    true ->
      elixir_errors:form_error([{line, Line}], File, ?MODULE, {invalid_module, Module});
    false ->
      Charlist
  end.

%% Hook that builds both attribute and functions and set up common hooks.

build(Line, File, Module) ->
  %% In the set table we store:
  %%
  %% * {Attribute, Value, AccumulateOrUnsetOrReadOrUnreadline, TraceLineOrNil}
  %% * {{elixir, ...}, ...}
  %% * {{cache, ...}, ...}
  %% * {{function, Tuple}, ...}, {{macro, Tuple}, ...}
  %% * {{type, Tuple}, ...}, {{opaque, Tuple}, ...}
  %% * {{callback, Tuple}, ...}, {{macrocallback, Tuple}, ...}
  %% * {{def, Tuple}, ...} (from elixir_def)
  %% * {{import, Tuple}, ...} (from elixir_locals)
  %% * {{overridable, Tuple}, ...} (from elixir_overridable)
  %%
  DataSet = ets:new(Module, [set, public]),

  %% In the bag table we store:
  %%
  %% * {{accumulate, Attribute}, ...} (includes typespecs)
  %% * {warn_attributes, ...}
  %% * {impls, ...}
  %% * {deprecated, ...}
  %% * {persisted_attributes, ...}
  %% * {defs, ...} (from elixir_def)
  %% * {overridables, ...} (from elixir_overridable)
  %% * {{default, Name}, ...} (from elixir_def)
  %% * {{clauses, Tuple}, ...} (from elixir_def)
  %% * {reattach, ...} (from elixir_locals)
  %% * {{local, Tuple}, ...} (from elixir_locals)
  %%
  DataBag = ets:new(Module, [duplicate_bag, public]),

  ets:insert(DataSet, [
    % {Key, Value, ReadOrUnreadLine, TraceLine}
    {moduledoc, nil, nil, []},

    % {Key, Value, accumulate, TraceLine}
    {after_compile, [], accumulate, []},
    {after_verify, [], accumulate, []},
    {before_compile, [], accumulate, []},
    {behaviour, [], accumulate, []},
    {compile, [], accumulate, []},
    {derive, [], accumulate, []},
    {dialyzer, [], accumulate, []},
    {external_resource, [], accumulate, []},
    {on_definition, [], accumulate, []},
    {type, [], accumulate, []},
    {opaque, [], accumulate, []},
    {typep, [], accumulate, []},
    {spec, [], accumulate, []},
    {callback, [], accumulate, []},
    {macrocallback, [], accumulate, []},
    {optional_callbacks, [], accumulate, []},

    % Others
    {?counter_attr, 0}
  ]),

  Persisted = [behaviour, on_load, external_resource, dialyzer, vsn],
  ets:insert(DataBag, [{persisted_attributes, Attr} || Attr <- Persisted]),

  OnDefinition =
    case elixir_config:is_bootstrap() of
      false -> {'Elixir.Module', compile_definition_attributes};
      _ -> {elixir_module, delete_definition_attributes}
    end,
  ets:insert(DataBag, {{accumulate, on_definition}, OnDefinition}),

  %% Setup definition related modules
  Tables = {DataSet, DataBag},
  elixir_def:setup(Tables),
  elixir_locals:setup(Tables),
  Tuple = {Module, Tables, Line, File, all},

  Ref =
    case elixir_code_server:call({defmodule, Module, self(), Tuple}) of
      {ok, ModuleRef} ->
        ModuleRef;
      {error, {Module, _, OldLine, OldFile, _}} ->
        ets:delete(DataSet),
        ets:delete(DataBag),
        Error = {module_in_definition, Module, OldFile, OldLine},
        elixir_errors:form_error([{line, Line}], File, ?MODULE, Error)
    end,

  {Tables, Ref}.

%% Handles module and callback evaluations.

eval_form(Line, Module, DataBag, Block, Vars, E) ->
  {Value, EE} = elixir_compiler:compile(Block, Vars, E),
  elixir_overridable:store_not_overridden(Module),
  EV = (elixir_env:reset_vars(EE))#{line := Line},
  EC = eval_callbacks(Line, DataBag, before_compile, [EV], EV),
  elixir_overridable:store_not_overridden(Module),
  {Value, EC}.

eval_callbacks(Line, DataBag, Name, Args, E) ->
  Callbacks = bag_lookup_element(DataBag, {accumulate, Name}, 2),
  lists:foldl(fun({M, F}, Acc) ->
    expand_callback(Line, M, F, Args, Acc, fun(AM, AF, AA) -> apply(AM, AF, AA) end)
  end, E, Callbacks).

expand_callback(Line, M, F, Args, Acc, Fun) ->
  E = elixir_env:reset_vars(Acc),
  S = elixir_env:env_to_ex(E),
  Meta = [{line, Line}, {required, true}],

  {EE, _S, ET} =
    elixir_dispatch:dispatch_require(Meta, M, F, Args, S, E, fun(AM, AF, AA) ->
      Fun(AM, AF, AA),
      {ok, S, E}
    end),

  if
    is_atom(EE) ->
      ET;
    true ->
      try
        {_Value, _Binding, EF} = elixir:eval_forms(EE, [], ET),
        EF
      catch
        Kind:Reason:Stacktrace ->
          Info = {M, F, length(Args), location(Line, E)},
          erlang:raise(Kind, Reason, prune_stacktrace(Info, Stacktrace))
      end
  end.

%% Add attributes handling to the form

attributes(DataSet, DataBag, PersistedAttributes) ->
  [{Key, Value} || Key <- PersistedAttributes, Value <- lookup_attribute(DataSet, DataBag, Key)].

lookup_attribute(DataSet, DataBag, Key) when is_atom(Key) ->
  case ets:lookup(DataSet, Key) of
    [{_, _, accumulate, _}] -> bag_lookup_element(DataBag, {accumulate, Key}, 2);
    [{_, _, unset, _}] -> [];
    [{_, Value, _, _}] -> [Value];
    [] -> []
  end.

warn_unused_attributes(File, DataSet, DataBag, PersistedAttrs) ->
  StoredAttrs = bag_lookup_element(DataBag, warn_attributes, 2),
  %% This is the same list as in Module.put_attribute
  %% without moduledoc which are never warned on.
  Attrs = [doc, typedoc, impl, deprecated | StoredAttrs -- PersistedAttrs],
  Query = [{{Attr, '_', '$1', '_'}, [{is_integer, '$1'}], [[Attr, '$1']]} || Attr <- Attrs],
  [elixir_errors:form_warn([{line, Line}], File, ?MODULE, {unused_attribute, Key})
   || [Key, Line] <- ets:select(DataSet, Query)].

get_struct(Set) ->
  case ets:lookup(Set, {elixir, struct}) of
    [] -> nil;
    [{_, Struct}] -> Struct
  end.

get_deprecated(Bag) ->
  lists:usort(bag_lookup_element(Bag, deprecated, 2)).

bag_lookup_element(Table, Name, Pos) ->
  try
    ets:lookup_element(Table, Name, Pos)
  catch
    error:badarg -> []
  end.

beam_location(ModuleAsCharlist) ->
  case get(elixir_compiler_dest) of
    Dest when is_binary(Dest) ->
      filename:join(elixir_utils:characters_to_list(Dest), ModuleAsCharlist ++ ".beam");
    _ ->
      ""
  end.

%% Integration with elixir_compiler that makes the module available

checker_info() ->
  case get(elixir_checker_info) of
    undefined -> undefined;
    _ -> 'Elixir.Module.ParallelChecker':get()
  end.

spawn_parallel_checker(undefined, _Module, _ModuleMap) ->
  nil;
spawn_parallel_checker(CheckerInfo, Module, ModuleMap) ->
  'Elixir.Module.ParallelChecker':spawn(CheckerInfo, Module, ModuleMap).

make_module_available(Module, Binary) ->
  case get(elixir_module_binaries) of
    Current when is_list(Current) ->
      put(elixir_module_binaries, [{Module, Binary} | Current]);
    _ ->
      ok
  end,

  case get(elixir_compiler_info) of
    undefined ->
      ok;
    {PID, _} ->
      Ref = make_ref(),
      PID ! {module_available, self(), Ref, get(elixir_compiler_file), Module, Binary},
      receive {Ref, ack} -> ok end
  end.

%% Error handling and helpers.

%% We've reached the elixir_module or eval internals, skip it with the rest
prune_stacktrace(Info, [{elixir, eval_forms, _, _} | _]) ->
  [Info];
prune_stacktrace(Info, [{elixir_module, _, _, _} | _]) ->
  [Info];
prune_stacktrace(Info, [H | T]) ->
  [H | prune_stacktrace(Info, T)];
prune_stacktrace(Info, []) ->
  [Info].

location(Line, E) ->
  [{file, elixir_utils:characters_to_list(?key(E, file))}, {line, Line}].

format_error({unused_attribute, typedoc}) ->
  "module attribute @typedoc was set but no type follows it";
format_error({unused_attribute, doc}) ->
  "module attribute @doc was set but no definition follows it";
format_error({unused_attribute, impl}) ->
  "module attribute @impl was set but no definition follows it";
format_error({unused_attribute, deprecated}) ->
  "module attribute @deprecated was set but no definition follows it";
format_error({unused_attribute, Attr}) ->
  io_lib:format("module attribute @~ts was set but never used", [Attr]);
format_error({invalid_module, Module}) ->
  io_lib:format("invalid module name: ~ts", ['Elixir.Kernel':inspect(Module)]);
format_error({module_defined, Module}) ->
  Extra =
    case code:which(Module) of
      "" ->
        " (current version defined in memory)";
      Path when is_list(Path) ->
        io_lib:format(" (current version loaded from ~ts)", [elixir_utils:relative_to_cwd(Path)]);
      _ ->
        ""
    end,
  io_lib:format("redefining module ~ts~ts", [elixir_aliases:inspect(Module), Extra]);
format_error({module_reserved, Module}) ->
  io_lib:format("module ~ts is reserved and cannot be defined", [elixir_aliases:inspect(Module)]);
format_error({module_in_definition, Module, File, Line}) ->
  io_lib:format("cannot define module ~ts because it is currently being defined in ~ts:~B",
    [elixir_aliases:inspect(Module), elixir_utils:relative_to_cwd(File), Line]);
format_error({bad_inline, {Name, Arity}}) ->
  io_lib:format("inlined function ~ts/~B undefined", [Name, Arity]);
format_error({bad_dialyzer, Key, {Name, Arity}}) ->
  io_lib:format("undefined function ~ts/~B given to @dialyzer :~ts", [Name, Arity, Key]);
format_error({undefined_on_load, {Name, Arity}}) ->
  io_lib:format("undefined function ~ts/~B given to @on_load", [Name, Arity]);
format_error({wrong_kind_on_load, {Name, Arity}, WrongKind}) ->
  io_lib:format("expected @on_load function ~ts/~B to be a function, got \"~ts\"",
                [Name, Arity, WrongKind]);
format_error({parse_transform, Module}) ->
  io_lib:format("@compile {:parse_transform, ~ts} is deprecated. Elixir will no longer support "
                "Erlang-based transforms in future versions", [elixir_aliases:inspect(Module)]).
