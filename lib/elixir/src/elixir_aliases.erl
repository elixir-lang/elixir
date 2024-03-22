-module(elixir_aliases).
-export([inspect/1, last/1, concat/1, safe_concat/1, format_error/1,
         ensure_loaded/3, expand/4, expand_or_concat/4, store/6, require/5]).
-include("elixir.hrl").

inspect(Atom) when is_atom(Atom) ->
  case elixir_config:is_bootstrap() of
    true  -> atom_to_binary(Atom);
    false -> 'Elixir.Macro':inspect_atom(literal, Atom)
  end.

require(Meta, Ref, Opts, E, Trace) ->
  Trace andalso elixir_env:trace({require, Meta, Ref, Opts}, E),
  E#{requires := ordsets:add_element(Ref, ?key(E, requires))}.

%% Store an alias in the given scope
store(Meta, New, New, _Opts, #{aliases := Aliases, macro_aliases := MacroAliases} = E, _Trace) ->
  E#{aliases := remove_alias(New, Aliases),
     macro_aliases := remove_macro_alias(Meta, New, MacroAliases)};
store(Meta, New, Old, Opts, #{aliases := Aliases, macro_aliases := MacroAliases} = E, Trace) ->
  Trace andalso elixir_env:trace({alias, Meta, Old, New, Opts}, E),
  E#{aliases := store_alias(New, Old, Aliases),
     macro_aliases := store_macro_alias(Meta, New, Old, MacroAliases)}.

store_alias(New, Old, Aliases) ->
  lists:keystore(New, 1, Aliases, {New, Old}).

store_macro_alias(Meta, New, Old, Aliases) ->
  case lists:keyfind(counter, 1, Meta) of
    {counter, Counter} ->
      lists:keystore(New, 1, Aliases, {New, {Counter, Old}});
    false ->
      Aliases
  end.

remove_alias(Atom, Aliases) ->
  lists:keydelete(Atom, 1, Aliases).

remove_macro_alias(Meta, Atom, Aliases) ->
  case lists:keyfind(counter, 1, Meta) of
    {counter, _Counter} ->
      lists:keydelete(Atom, 1, Aliases);
    false ->
      Aliases
  end.

%% Expand an alias. It returns an atom (meaning that there
%% was an expansion) or a list of atoms.

expand(_Meta, ['Elixir' | _] = List, _E, _Trace) ->
  List;

expand(_Meta, [H | _] = List, _E, _Trace) when not is_atom(H) ->
  List;

expand(Meta, List, #{aliases := Aliases, macro_aliases := MacroAliases} = E, Trace) ->
  case lists:keyfind(alias, 1, Meta) of
    {alias, false} ->
      expand(Meta, List, MacroAliases, E, Trace);
    {alias, Atom} when is_atom(Atom) ->
      Atom;
    false ->
      expand(Meta, List, Aliases, E, Trace)
  end.

expand(Meta, [H | T], Aliases, E, Trace) ->
  Lookup  = list_to_atom("Elixir." ++ atom_to_list(H)),

  Counter = case lists:keyfind(counter, 1, Meta) of
    {counter, C} -> C;
    _ -> nil
  end,

  case lookup(Lookup, Aliases, Counter) of
    Lookup -> [H | T];
    Atom ->
      Trace andalso elixir_env:trace({alias_expansion, Meta, Lookup, Atom}, E),
      case T of
        [] -> Atom;
        _  -> concat([Atom | T])
      end
  end.

%% Expands or concat if possible.

expand_or_concat(Meta, List, E, Trace) ->
  case expand(Meta, List, E, Trace) of
    [H | T] when is_atom(H) -> concat([H | T]);
    AtomOrList -> AtomOrList
  end.

%% Ensure a module is loaded before its usage.

%% Skip Kernel verification for bootstrap purposes.
ensure_loaded(_Meta, 'Elixir.Kernel', _E) ->
  ok;
ensure_loaded(Meta, Module, #{module := Module} = E) ->
  elixir_errors:file_error(Meta, E, ?MODULE, {circular_module, Module});
ensure_loaded(Meta, Module, E) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      ok;

    _ ->
      case wait_for_module(Module) of
        found ->
          ok;

        Wait ->
          Kind = case lists:member(Module, ?key(E, context_modules)) of
            true -> scheduled_module;
            false when Wait == deadlock -> deadlock_module;
            false -> unloaded_module
          end,

          elixir_errors:file_error(Meta, E, ?MODULE, {Kind, Module})
      end
  end.

wait_for_module(Module) ->
  case erlang:get(elixir_compiler_info) of
    undefined -> not_found;
    _ -> 'Elixir.Kernel.ErrorHandler':ensure_compiled(Module, module, hard)
  end.

%% Receives an atom and returns the last bit as an alias.

last(Atom) ->
  case atom_to_list(Atom) of
    ("Elixir." ++ [FirstLetter | _]) = List when FirstLetter >= $A, FirstLetter =< $Z ->
      Last = last(lists:reverse(List), []),
      {ok, list_to_atom("Elixir." ++ Last)};
    _ ->
      error
  end.

last([$. | _], Acc) -> Acc;
last([H | T], Acc)  -> last(T, [H | Acc]);
last([], Acc)       -> Acc.

%% Receives a list of atoms, binaries or lists
%% representing modules and concatenates them.

concat(Args)      -> binary_to_atom(do_concat(Args), utf8).
safe_concat(Args) -> binary_to_existing_atom(do_concat(Args), utf8).

do_concat([H | T]) when is_atom(H), H /= nil ->
  do_concat([atom_to_binary(H) | T]);
do_concat([<<"Elixir.", _/binary>>=H | T]) ->
  do_concat(T, H);
do_concat([<<"Elixir">>=H | T]) ->
  do_concat(T, H);
do_concat(T) ->
  do_concat(T, <<"Elixir">>).

do_concat([nil | T], Acc) ->
  do_concat(T, Acc);
do_concat([H | T], Acc) when is_atom(H) ->
  do_concat(T, <<Acc/binary, $., (to_partial(atom_to_binary(H)))/binary>>);
do_concat([H | T], Acc) when is_binary(H) ->
  do_concat(T, <<Acc/binary, $., (to_partial(H))/binary>>);
do_concat([], Acc) ->
  Acc.

to_partial(<<"Elixir.", Arg/binary>>) -> Arg;
to_partial(<<".", Arg/binary>>)       -> Arg;
to_partial(Arg) when is_binary(Arg)   -> Arg.

%% Lookup an alias in the current scope.

lookup(Else, List, Counter) ->
  case lists:keyfind(Else, 1, List) of
    {Else, {Counter, Value}} -> Value;
    {Else, Value} when is_atom(Value) -> Value;
    _ -> Else
  end.

%% Errors

format_error({unloaded_module, Module}) ->
  io_lib:format("module ~ts is not loaded and could not be found", [inspect(Module)]);

format_error({deadlock_module, Module}) ->
  io_lib:format("module ~ts is not loaded and could not be found. "
                "This may be happening because the module you are trying to load "
                "directly or indirectly depends on the current module",
                [inspect(Module)]);

format_error({scheduled_module, Module}) ->
  io_lib:format(
    "module ~ts is not loaded but was defined. This happens when you depend on "
    "a module in the same context in which it is defined. For example:\n"
    "\n"
    "    defmodule MyApp do\n"
    "      defmodule Mod do\n"
    "      end\n"
    "\n"
    "      use Mod\n"
    "    end\n"
    "\n"
    "Try defining the module outside the context that uses it:\n"
    "\n"
    "    defmodule MyApp.Mod do\n"
    "    end\n"
    "\n"
    "    defmodule MyApp do\n"
    "      use MyApp.Mod\n"
    "    end\n"
    "\n"
    "If the module is defined at the top-level and you are trying to "
    "use it at the top-level, this is not supported by Elixir",
    [inspect(Module)]);

format_error({circular_module, Module}) ->
  io_lib:format(
    "you are trying to use/import/require the module ~ts which is currently being defined.\n"
    "\n"
    "This may happen if you accidentally override the module you want to use. For example:\n"
    "\n"
    "    defmodule MyApp do\n"
    "      defmodule Supervisor do\n"
    "        use Supervisor\n"
    "      end\n"
    "    end\n"
    "\n"
    "In the example above, the new Supervisor conflicts with Elixir's Supervisor. "
    "This may be fixed by using the fully qualified name in the definition:\n"
    "\n"
    "    defmodule MyApp.Supervisor do\n"
    "      use Supervisor\n"
    "    end\n",
    [inspect(Module)]).
