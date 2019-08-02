-module(elixir_aliases).
-export([inspect/1, last/1, concat/1, safe_concat/1, format_error/1,
         ensure_loaded/3, expand/2, store/5]).
-include("elixir.hrl").

inspect(Atom) when is_atom(Atom) ->
  case elixir_config:get(bootstrap) of
    true  -> atom_to_binary(Atom, utf8);
    false -> 'Elixir.Code.Identifier':inspect_as_atom(Atom)
  end.

%% Store an alias in the given scope
store(Meta, New, New, _TOpts, #{aliases := Aliases, macro_aliases := MacroAliases}) ->
  {remove_alias(New, Aliases), remove_macro_alias(Meta, New, MacroAliases)};
store(Meta, New, Old, TOpts, #{aliases := Aliases, macro_aliases := MacroAliases} = E) ->
  elixir_env:trace({alias, Meta, Old, New, TOpts}, E),
  {store_alias(New, Old, Aliases), store_macro_alias(Meta, New, Old, MacroAliases)}.

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

expand({'__aliases__', _Meta, ['Elixir' | _] = List}, _E) ->
  concat(List);

expand({'__aliases__', Meta, _} = Alias, #{aliases := Aliases, macro_aliases := MacroAliases} = E) ->
  case lists:keyfind(alias, 1, Meta) of
    {alias, false} ->
      expand(Alias, MacroAliases, E);
    {alias, Atom} when is_atom(Atom) ->
      Atom;
    false ->
      expand(Alias, Aliases, E)
  end.

expand({'__aliases__', Meta, [H | T]}, Aliases, E) when is_atom(H) ->
  Lookup  = list_to_atom("Elixir." ++ atom_to_list(H)),
  Counter = case lists:keyfind(counter, 1, Meta) of
    {counter, C} -> C;
    _ -> nil
  end,
  case lookup(Lookup, Aliases, Counter) of
    Lookup -> [H | T];
    Atom ->
      elixir_env:trace({alias_expansion, Meta, Lookup, Atom}, E),
      case T of
        [] -> Atom;
        _  -> concat([Atom | T])
      end
  end;

expand({'__aliases__', _Meta, List}, _Aliases, _E) ->
  List.

%% Ensure a module is loaded before its usage.

ensure_loaded(_Meta, 'Elixir.Kernel', _E) -> ok;
ensure_loaded(Meta, Ref, E) ->
  try
    Ref:module_info(module)
  catch
    error:undef ->
      Kind = case lists:member(Ref, ?key(E, context_modules)) of
        true  ->
          case ?key(E, module) of
            Ref -> circular_module;
            _ -> scheduled_module
          end;
        false -> unloaded_module
      end,
      elixir_errors:form_error(Meta, E, ?MODULE, {Kind, Ref})
  end.

%% Receives an atom and returns the last bit as an alias.

last(Atom) ->
  Last = last(lists:reverse(atom_to_list(Atom)), []),
  list_to_atom("Elixir." ++ Last).

last([$. | _], Acc) -> Acc;
last([H | T], Acc)  -> last(T, [H | Acc]);
last([], Acc)       -> Acc.

%% Receives a list of atoms, binaries or lists
%% representing modules and concatenates them.

concat(Args)      -> binary_to_atom(do_concat(Args), utf8).
safe_concat(Args) -> binary_to_existing_atom(do_concat(Args), utf8).

do_concat([H | T]) when is_atom(H), H /= nil ->
  do_concat([atom_to_binary(H, utf8) | T]);
do_concat([<<"Elixir.", _/binary>>=H | T]) ->
  do_concat(T, H);
do_concat([<<"Elixir">>=H | T]) ->
  do_concat(T, H);
do_concat(T) ->
  do_concat(T, <<"Elixir">>).

do_concat([nil | T], Acc) ->
  do_concat(T, Acc);
do_concat([H | T], Acc) when is_atom(H) ->
  do_concat(T, <<Acc/binary, $., (to_partial(atom_to_binary(H, utf8)))/binary>>);
do_concat([H | T], Acc) when is_binary(H) ->
  do_concat(T, <<Acc/binary, $., (to_partial(H))/binary>>);
do_concat([], Acc) ->
  Acc.

to_partial(<<"Elixir.", Arg/binary>>) -> Arg;
to_partial(<<".", Arg/binary>>)       -> Arg;
to_partial(Arg) when is_binary(Arg)   -> Arg.

%% Lookup an alias in the current scope.

lookup(Else, Dict, Counter) ->
  case lists:keyfind(Else, 1, Dict) of
    {Else, {Counter, Value}} -> lookup(Value, Dict, Counter);
    {Else, Value} when is_atom(Value) -> lookup(Value, Dict, Counter);
    _ -> Else
  end.

%% Errors

format_error({unloaded_module, Module}) ->
  io_lib:format("module ~ts is not loaded and could not be found", [inspect(Module)]);

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
    "you are trying to use the module ~ts which is currently being defined.\n"
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
