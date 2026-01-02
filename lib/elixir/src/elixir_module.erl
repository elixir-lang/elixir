%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

-module(elixir_module).
-export([file/1, data_tables/1, is_open/1, mode/1, delete_definition_attributes/6,
         compile/6, expand_callback/6, format_error/1, compiler_modules/0, exports_md5/3,
         write_cache/3, read_cache/2, next_counter/1, taint/1, cache_env/1, get_cached_env/1]).
-include("elixir.hrl").
-define(counter_attr, {elixir, counter}).
-define(cache_key, {elixir, cache_env}).

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

exports_md5(Def, Defmacro, Struct) ->
  erlang:md5(term_to_binary({lists:sort(Def), lists:sort(Defmacro), Struct}, [deterministic])).

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

taint(Module) ->
  try
    {DataSet, _} = data_tables(Module),
    ets:insert(DataSet, [{{elixir, taint}}]),
    true
  catch
    _:_ -> false
  end.

cache_env(#{line := Line, module := Module} = E) ->
  {Set, _} = data_tables(Module),
  Cache = elixir_env:reset_vars(E#{line := nil}),
  PrevKey = ets:lookup_element(Set, ?cache_key, 2),

  Pos =
    case ets:lookup(Set, {cache_env, PrevKey}) of
      [{_, Cache}] ->
        PrevKey;
      _ ->
        NewKey = PrevKey + 1,
        ets:insert(Set, [{{cache_env, NewKey}, Cache}, {?cache_key, NewKey}]),
        NewKey
    end,

  {Module, {Line, Pos}}.

get_cached_env({Module, {Line, Pos}}) ->
  {Set, _} = data_tables(Module),
  (ets:lookup_element(Set, {cache_env, Pos}, 2))#{line := Line};
get_cached_env(Env) ->
  Env.

%% Compilation hook

compile(Meta, Module, Block, Vars, Prune, Env) ->
  ModuleAsCharlist = validate_module_name(Module),
  #{function := Function, versioned_vars := OldVerVars} = Env,

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
        fun(LexEnv) -> compile(Meta, Module, ModuleAsCharlist, Block, Vars, Prune, LexEnv) end,
        fun(_LexEnv) -> ok end
      );
    _ ->
      compile(Meta, Module, ModuleAsCharlist, Block, Vars, Prune, MaybeLexEnv)
  end.

validate_module_name(Module) when Module == nil; is_boolean(Module); not is_atom(Module) ->
  invalid_module_name(Module);
validate_module_name(Module) ->
  Charlist = atom_to_list(Module),
  case lists:any(fun(Char) -> (Char =:= $/) or (Char =:= $\\) end, Charlist) of
    true -> invalid_module_name(Module);
    false -> Charlist
  end.

invalid_module_name(Module) ->
  %% We raise an argument error to keep it close to Elixir errors before it starts.
  erlang:error('Elixir.ArgumentError':exception(
    <<"invalid module name: ",
      ('Elixir.Kernel':inspect(Module))/binary>>
  )).

compile(Meta, Module, ModuleAsCharlist, Block, Vars, Prune, E) ->
  Anno = ?ann(Meta),
  Line = erl_anno:line(Anno),

  File = ?key(E, file),
  check_module_availability(Module, Line, E),
  elixir_env:trace(defmodule, E),

  CompilerModules = compiler_modules(),
  {Tables, Ref} = build(Module, Line, File, E),
  {DataSet, DataBag} = Tables,

  try
    put_compiler_modules([Module | CompilerModules]),
    {Result, ModuleE, CallbackE} = eval_form(Line, Module, DataBag, Block, Vars, Prune, E),
    CheckerInfo = checker_info(),
    {BeamLocation, Forceload} = beam_location(ModuleAsCharlist),

    {Binary, PersistedAttributes, Autoload} =
      elixir_erl_compiler:spawn(fun() ->
        PersistedAttributes = ets:lookup_element(DataBag, persisted_attributes, 2),
        Attributes = attributes(DataSet, DataBag, PersistedAttributes),
        AllDefinitions = elixir_def:fetch_definitions(Module, E),

        OnLoadAttribute = lists:keyfind(on_load, 1, Attributes),
        validate_on_load_attribute(OnLoadAttribute, AllDefinitions, DataBag, Line, E),

        DialyzerAttribute = lists:keyfind(dialyzer, 1, Attributes),
        validate_dialyzer_attribute(DialyzerAttribute, AllDefinitions, Line, E),

        NifsAttribute = lists:keyfind(nifs, 1, Attributes),
        validate_nifs_attribute(NifsAttribute, AllDefinitions, Line, E),
        elixir_import:ensure_no_local_conflict(Module, AllDefinitions, E),
        make_readonly(Module),

        (not elixir_config:is_bootstrap()) andalso
         'Elixir.Module':'__check_attributes__'(E, DataSet, DataBag),

        AfterVerify = bag_lookup_element(DataBag, {accumulate, after_verify}, 2),
        [elixir_env:trace({remote_function, [{line, Line}], VerifyMod, VerifyFun, 1}, CallbackE) ||
         {VerifyMod, VerifyFun} <- AfterVerify],

        %% Ensure there are no errors before we infer types
        compile_error_if_tainted(DataSet, E),

        {Signatures, Unreachable} =
          case elixir_config:is_bootstrap() of
            true -> {#{}, []};
            false ->
              UsedPrivate = bag_lookup_element(DataBag, used_private, 2),
              'Elixir.Module.Types':infer(Module, File, Attributes, AllDefinitions, UsedPrivate, E, CheckerInfo)
          end,

        RawCompileOpts = bag_lookup_element(DataBag, {accumulate, compile}, 2),
        CompileOpts = validate_compile_opts(RawCompileOpts, AllDefinitions, Unreachable, Line, E),
        Impls = bag_lookup_element(DataBag, impls, 2),

        Struct = get_struct(DataSet),
        set_exports_md5(DataSet, AllDefinitions, Struct),

        ModuleMap = #{
          struct => Struct,
          module => Module,
          anno => Anno,
          file => File,
          relative_file => elixir_utils:relative_to_cwd(File),
          attributes => Attributes,
          definitions => AllDefinitions,
          after_verify => AfterVerify,
          compile_opts => CompileOpts,
          deprecated => get_deprecated(DataBag),
          defines_behaviour => defines_behaviour(DataBag),
          impls => Impls,
          unreachable => Unreachable
        },

        compile_error_if_tainted(DataSet, E),
        Binary = elixir_erl:compile(ModuleMap, Signatures),
        Autoload = Forceload or proplists:get_value(autoload, CompileOpts, false),
        spawn_parallel_checker(CheckerInfo, Module, ModuleMap, Signatures, BeamLocation),
        {Binary, PersistedAttributes, Autoload}
      end),

    Autoload andalso code:load_binary(Module, BeamLocation, Binary),
    make_module_available(Module, Binary, Autoload),
    put_compiler_modules(CompilerModules),
    eval_callbacks(Line, DataBag, after_compile, [CallbackE, Binary], CallbackE),
    elixir_env:trace({on_module, Binary, none}, ModuleE),
    warn_unused_attributes(DataSet, DataBag, PersistedAttributes, E),
    (element(2, CheckerInfo) == nil) andalso
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

compile_error_if_tainted(DataSet, E) ->
  case ets:member(DataSet, {elixir, taint}) of
    true -> elixir_errors:compile_error(E);
    false -> ok
  end.

set_exports_md5(DataSet, AllDefinitions, Struct) ->
  {Funs, Macros} =
    lists:foldl(fun
      ({Tuple, def, _Meta, _Clauses}, {Funs, Macros}) -> {[Tuple | Funs], Macros};
      ({Tuple, defmacro, _Meta, _Clauses}, {Funs, Macros}) -> {Funs, [Tuple | Macros]};
      ({_Tuple, _Kind, _Meta, _Clauses}, {Funs, Macros}) -> {Funs, Macros}
    end, {[], []}, AllDefinitions),
  MD5 = exports_md5(Funs, Macros, Struct),
  ets:insert(DataSet, {exports_md5, MD5, nil, []}).

validate_compile_opts(Opts, Defs, Unreachable, Line, E) ->
  lists:flatmap(fun (Opt) -> validate_compile_opt(Opt, Defs, Unreachable, Line, E) end, Opts).

%% TODO: Make this an error on v2.0
validate_compile_opt({parse_transform, Module} = Opt, _Defs, _Unreachable, Line, E) ->
  elixir_errors:file_warn([{line, Line}], E, ?MODULE, {parse_transform, Module}),
  [Opt];
validate_compile_opt({inline, Inlines}, Defs, Unreachable, Line, E) ->
  case validate_inlines(Inlines, Defs, Unreachable, []) of
    {ok, []} ->
      [];
    {ok, FilteredInlines} ->
      [{inline, FilteredInlines}];
    {error, Reason} ->
      elixir_errors:module_error([{line, Line}], E, ?MODULE, Reason),
      []
  end;
validate_compile_opt(Opt, Defs, Unreachable, Line, E) when is_list(Opt) ->
  validate_compile_opts(Opt, Defs, Unreachable, Line, E);
validate_compile_opt(Opt, _Defs, _Unreachable, _Line, _E) ->
  [Opt].

validate_inlines([Inline | Inlines], Defs, Unreachable, Acc) ->
  case lists:keyfind(Inline, 1, Defs) of
    false ->
      {error, {undefined_function, {compile, inline}, Inline}};
    {_Def, Kind, _Meta, _Clauses} when Kind == defmacro; Kind == defmacrop ->
      {error, {bad_macro, {compile, inline}, Inline}};
    _ ->
      case lists:member(Inline, Unreachable) of
        true -> validate_inlines(Inlines, Defs, Unreachable, Acc);
        false -> validate_inlines(Inlines, Defs, Unreachable, [Inline | Acc])
      end
  end;
validate_inlines([], _Defs, _Unreachable, Acc) -> {ok, Acc}.

validate_on_load_attribute({on_load, Def}, Defs, Bag, Line, E) ->
  case lists:keyfind(Def, 1, Defs) of
    false ->
      elixir_errors:module_error([{line, Line}], E, ?MODULE, {undefined_function, on_load, Def});
    {_Def, Kind, _Meta, _Clauses} when Kind == defmacro; Kind == defmacrop ->
      elixir_errors:module_error([{line, Line}], E, ?MODULE, {bad_macro, on_load, Def});
    {{Name, Arity}, Kind, _Meta, _Clauses} ->
      elixir_env:trace({local_function, [{line, Line}], Name, Arity}, E),
      (Kind == defp) andalso ets:insert(Bag, {used_private, Def})
  end;
validate_on_load_attribute(false, _Defs, _Bag, _Line, _E) -> ok.

validate_dialyzer_attribute({dialyzer, Dialyzer}, Defs, Line, E) ->
  [validate_definition({dialyzer, Key}, Fun, Defs, Line, E)
   || {Key, Funs} <- lists:flatten([Dialyzer]), Fun <- lists:flatten([Funs])];
validate_dialyzer_attribute(false, _Defs, _Line, _E) ->
  ok.

validate_nifs_attribute({nifs, Funs}, Defs, Line, E) ->
  [validate_definition(nifs, Fun, Defs, Line, E) || Fun <- lists:flatten([Funs])];
validate_nifs_attribute(false, _Defs, _Line, _E) ->
  ok.

validate_definition(Key, Fun, Defs, Line, E) ->
  case lists:keyfind(Fun, 1, Defs) of
    false ->
      elixir_errors:module_error([{line, Line}], E, ?MODULE, {undefined_function, Key, Fun});
    {Fun, Type, _Meta, _Clauses} when Type == defmacro; Type == defmacrop ->
      elixir_errors:module_error([{line, Line}], E, ?MODULE, {bad_macro, Key, Fun});
    _ ->
      ok
   end.

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

check_module_availability(Module, Line, E) ->
  Reserved = ['Elixir.True', 'Elixir.False', 'Elixir.Nil',
              'Elixir.Any', 'Elixir.BitString', 'Elixir.PID',
              'Elixir.Reference', 'Elixir.Elixir', 'Elixir'],

  case lists:member(Module, Reserved) of
    true  -> elixir_errors:file_error([{line, Line}], E, ?MODULE, {module_reserved, Module});
    false -> ok
  end,

  case elixir_config:get(ignore_module_conflict) of
    false ->
      case code:ensure_loaded(Module) of
        {module, _} ->
          elixir_errors:file_warn([{line, Line}], E, ?MODULE, {module_defined, Module});
        {error, _}  ->
          ok
      end;
    true ->
      ok
  end.

%% Hook that builds both attribute and functions and set up common hooks.

build(Module, Line, File, E) ->
  %% In the set table we store:
  %%
  %% * {Attribute, Value, AccumulateOrUnsetOrReadOrUnreadline, TraceLineOrNil}
  %% * {{elixir, ...}, ...}
  %% * {{cache, ...}, ...}
  %% * {{function, Tuple}, ...}, {{macro, Tuple}, ...}
  %% * {{type, Tuple}, ...}, {{opaque, Tuple}, ...}
  %% * {{callback, Tuple}, ...}, {{macrocallback, Tuple}, ...}
  %% * {{def, Tuple}, ...} (from elixir_def)
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
    {nifs, [], accumulate, []},
    {on_definition, [], accumulate, []},
    {opaque, [], accumulate, []},
    {type, [], accumulate, []},
    {typep, [], accumulate, []},
    {spec, [], accumulate, []},
    {callback, [], accumulate, []},
    {macrocallback, [], accumulate, []},
    {optional_callbacks, [], accumulate, []},

    % Others
    {?cache_key, 0},
    {?counter_attr, 0}
  ]),

  Persisted = [behaviour, dialyzer, external_resource, nifs, on_load, vsn],
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
  Tuple = {Module, Tables, Line, File, all},

  Ref =
    case elixir_code_server:call({defmodule, Module, self(), Tuple}) of
      {ok, ModuleRef} ->
        ModuleRef;
      {error, {Module, _, OldLine, OldFile, _}} ->
        ets:delete(DataSet),
        ets:delete(DataBag),
        Error = {module_in_definition, Module, OldFile, OldLine},
        elixir_errors:file_error([{line, Line}], E, ?MODULE, Error)
    end,

  {Tables, Ref}.

%% Handles module and callback evaluations.

eval_form(Line, Module, DataBag, Block, Vars, Prune, E) ->
  %% Given Elixir modules can get very long to compile due to metaprogramming,
  %% we disable expansions that take linear time to code size.
  {Value, ExS, EE} = elixir_compiler:compile(Block, Vars, [no_bool_opt, no_ssa_opt], E),
  elixir_overridable:store_not_overridden(Module),
  EV = (elixir_env:reset_vars(EE))#{line := Line},
  EC = eval_callbacks(Line, DataBag, before_compile, [EV], EV),
  elixir_overridable:store_not_overridden(Module),
  {Value, maybe_prune_versioned_vars(Prune, Vars, ExS, E), EC}.

maybe_prune_versioned_vars(false, _Vars, _Exs, E) ->
  E;
maybe_prune_versioned_vars(true, Vars, ExS, E) ->
  PruneBefore = length(Vars),
  #elixir_ex{vars={ExVars, _}, unused={Unused, _}} = ExS,

  VersionedVars =
    maps:filter(fun
      (Pair, Version) when Version < PruneBefore, not is_map_key({Pair, Version}, Unused) -> false;
      (_, _) -> true
    end, ExVars),

  E#{versioned_vars := VersionedVars}.

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
    elixir_dispatch:dispatch_require(Meta, M, F, Args, S, E, fun(AM, AF) ->
      Fun(AM, AF, Args),
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

warn_unused_attributes(DataSet, DataBag, PersistedAttrs, E) ->
  StoredAttrs = bag_lookup_element(DataBag, warn_attributes, 2),
  %% This is the same list as in Module.put_attribute
  %% without moduledoc which are never warned on.
  Attrs = [doc, typedoc, impl, deprecated | StoredAttrs -- PersistedAttrs],
  Query = [{{Attr, '_', '$1', '_'}, [{is_integer, '$1'}], [[Attr, '$1']]} || Attr <- Attrs],
  [elixir_errors:file_warn([{line, Line}], E, ?MODULE, {unused_attribute, Key})
   || [Key, Line] <- ets:select(DataSet, Query)].

get_struct(Set) ->
  case ets:lookup(Set, {elixir, struct}) of
    [] -> nil;
    [{_, Fields}] -> Fields
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
    {Dest, Forceload} when is_binary(Dest) ->
      {filename:join(elixir_utils:characters_to_list(Dest), ModuleAsCharlist ++ ".beam"),
       Forceload};
    _ ->
      {"", true}
  end.

%% Integration with elixir_compiler that makes the module available

checker_info() ->
  case get(elixir_checker_info) of
    undefined -> {self(), nil};
    _ -> 'Elixir.Module.ParallelChecker':get()
  end.

spawn_parallel_checker({_, nil}, _Module, _ModuleMap, _Signatures, _BeamLocation) ->
  ok;
spawn_parallel_checker(CheckerInfo, Module, ModuleMap, Signatures, BeamLocation) ->
  Log =
    case erlang:get(elixir_code_diagnostics) of
      {_, false} -> false;
      _ -> true
    end,
  'Elixir.Module.ParallelChecker':spawn(CheckerInfo, Module, ModuleMap, Signatures, BeamLocation, Log).

make_module_available(Module, Binary, Loaded) ->
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
      PID ! {module_available, self(), Ref, get(elixir_compiler_file), Module, Binary, Loaded},
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
format_error({undefined_function, {Attr, Key}, {Name, Arity}}) ->
  io_lib:format("undefined function ~ts/~B given to @~ts :~ts", [Name, Arity, Attr, Key]);
format_error({undefined_function, Attr, {Name, Arity}}) ->
  io_lib:format("undefined function ~ts/~B given to @~ts", [Name, Arity, Attr]);
format_error({bad_macro, {Attr, Key}, {Name, Arity}}) ->
  io_lib:format("macro ~ts/~B given to @~ts :~ts (only functions are supported)", [Name, Arity, Attr, Key]);
format_error({bad_macro, Attr, {Name, Arity}}) ->
  io_lib:format("macro ~ts/~B given to @~ts (only functions are supported)", [Name, Arity, Attr]);
format_error({parse_transform, Module}) ->
  io_lib:format("@compile {:parse_transform, ~ts} is deprecated. Elixir will no longer support "
                "Erlang-based transforms in future versions", [elixir_aliases:inspect(Module)]).
