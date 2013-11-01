-module(elixir_aliases).
-export([nesting_alias/2, last/1, concat/1, safe_concat/1,
  format_error/1, ensure_loaded/3, ensure_loaded/4, expand/4, store/5]).
-include("elixir.hrl").

%% Store an alias in the given scope
store(_Meta, New, New, _TKV, S) -> S;
store(Meta, New, Old, TKV, S) ->
  record_warn(Meta, New, TKV, S),

  SA = S#elixir_scope{
    aliases=orddict:store(New, Old, S#elixir_scope.aliases)
  },

  case lists:keymember(context, 1, Meta) of
    true ->
      SA#elixir_scope{
        macro_aliases=orddict:store(New, Old, S#elixir_scope.macro_aliases)
      };
    false ->
      SA
  end.

record_warn(Meta, Ref, Opts, S) ->
  Warn =
    case lists:keyfind(warn, 1, Opts) of
      { warn, false } -> false;
      { warn, true } -> true;
      false -> not lists:keymember(context, 1, Meta)
    end,
  elixir_lexical:record_alias(Ref, ?line(Meta), Warn, S#elixir_scope.lexical_tracker).

%% Expand an alias. It returns an atom (meaning that there
%% was an expansion) or a list of atoms.

expand({ '__aliases__', Meta, _ } = Alias, Aliases, MacroAliases, LexicalTracker) ->
  case lists:keyfind(alias, 1, Meta) of
    { alias, false } ->
      expand(Alias, MacroAliases, LexicalTracker);
    { alias, Atom } when is_atom(Atom) ->
      case expand(Alias, MacroAliases, LexicalTracker) of
        OtherAtom when is_atom(OtherAtom) -> OtherAtom;
        OtherAliases when is_list(OtherAliases) -> Atom
      end;
    false ->
      expand(Alias, Aliases, LexicalTracker)
  end.

expand({ '__aliases__', _Meta, [H] }, Aliases, LexicalTracker) when H /= 'Elixir' ->
  case expand_one(H, Aliases, LexicalTracker) of
    false -> [H];
    Atom  -> Atom
  end;

expand({ '__aliases__', _Meta, [H|T] }, Aliases, LexicalTracker) when is_atom(H) ->
  case H of
    'Elixir' ->
      concat(T);
    _ ->
      case expand_one(H, Aliases, LexicalTracker) of
        false -> [H|T];
        Atom  -> concat([Atom|T])
      end
  end;

expand({ '__aliases__', _Meta, List }, _Aliases, _LexicalTracker) ->
  List.

expand_one(H, Aliases, LexicalTracker) ->
  Lookup = list_to_atom("Elixir." ++ atom_to_list(H)),
  case lookup(Lookup, Aliases) of
    Lookup -> false;
    Else ->
      elixir_lexical:record_alias(Lookup, LexicalTracker),
      Else
  end.

%% Ensure a module is loaded before its usage.

ensure_loaded(Line, Ref, S) ->
  ensure_loaded(Line, S#elixir_scope.file, Ref, S#elixir_scope.context_modules).

ensure_loaded(_Line, _File, 'Elixir.Kernel', _FileModules) ->
  ok;

ensure_loaded(Line, File, Ref, FileModules) ->
  try
    Ref:module_info(compile)
  catch
    error:undef ->
      Kind = case lists:member(Ref, FileModules) of
        true  -> scheduled_module;
        false -> unloaded_module
      end,
      elixir_errors:form_error(Line, File, ?MODULE, { Kind, Ref })
  end.

%% Receives an atom and returns the last bit as an alias.

last(Atom) ->
  Last = last(lists:reverse(atom_to_list(Atom)), []),
  list_to_atom("Elixir." ++ Last).

last([$.|_], Acc) -> Acc;
last([H|T], Acc) -> last(T, [H|Acc]);
last([], Acc) -> Acc.

%% Gets two modules names and return an alias
%% which can be passed down to the alias directive
%% and it will create a proper shortcut representing
%% the given nesting.
%%
%% Examples:
%%
%%     nesting_alias('Elixir.Foo.Bar', 'Elixir.Foo.Bar.Baz.Bat')
%%     { 'Elixir.Baz', 'Elixir.Foo.Bar.Baz' }
%%
%% When passed to alias, the example above will generate an
%% alias like:
%%
%%     'Elixir.Baz' => 'Elixir.Foo.Bar.Baz'
%%
nesting_alias(nil, _Full) -> false;

nesting_alias(Prefix, Full) ->
  PrefixList = list_nesting(Prefix),
  FullList   = list_nesting(Full),
  (PrefixList /= []) andalso do_nesting(PrefixList, FullList, []).

do_nesting([X|PreTail], [X|Tail], Acc) ->
  do_nesting(PreTail, Tail, [X|Acc]);
do_nesting([], [H|_], Acc) ->
  { list_to_atom("Elixir." ++ H), concat(lists:reverse([H|Acc])) };
do_nesting(_, _, _Acc) ->
  false.

list_nesting(Atom) ->
  case string:tokens(atom_to_list(Atom), ".") of
    ["Elixir"|T] -> T;
    _ -> []
  end.

%% Receives a list of atoms, binaries or lists
%% representing modules and concatenates them.

concat(Args) -> list_to_atom(raw_concat(Args)).
safe_concat(Args) -> list_to_existing_atom(raw_concat(Args)).

raw_concat(['Elixir'|Args]) -> do_concat(Args);
raw_concat([nil|Args])      -> do_concat(Args);
raw_concat(Args)            -> do_concat(Args).

do_concat(Args) ->
  Aliases = [to_partial(Arg) || Arg <- Args],
  "Elixir" ++ lists:concat(Aliases).

to_partial(Arg) when is_binary(Arg) -> to_partial(binary_to_list(Arg));
to_partial(Arg) when is_atom(Arg)   -> to_partial(atom_to_list(Arg));
to_partial("Elixir." ++ Arg)        -> [$.|Arg];
to_partial([$.|_] = Arg)            -> Arg;
to_partial(Arg) when is_list(Arg)   -> [$.|Arg].

%% Lookup an alias in the current scope.

lookup(Else, Dict) ->
  case orddict:find(Else, Dict) of
    { ok, Value } when Value /= Else -> lookup(Value, Dict);
    _ -> Else
  end.

%% Errors

format_error({unloaded_module, Module}) ->
  io_lib:format("module ~ts is not loaded and could not be found", [elixir_errors:inspect(Module)]);

format_error({scheduled_module, Module}) ->
  io_lib:format("module ~ts is not loaded but was defined. This happens because you are trying to use a module in the same context it is defined. Try defining the module outside the context that requires it.",
    [elixir_errors:inspect(Module)]).