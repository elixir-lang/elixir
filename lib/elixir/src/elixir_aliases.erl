-module(elixir_aliases).
-export([nesting_alias/2, last/1, concat/1, safe_concat/1,
  format_error/1, ensure_loaded/3, expand/3]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

%% Expand an alias. It returns an atom (meaning that there
%% was an expansion) or a list of atoms.

expand({ '__aliases__', Meta, _ } = Alias, Aliases, MacroAliases) ->
  case lists:keyfind(alias, 1, Meta) of
    { alias, false } ->
      expand(Alias, MacroAliases);
    { alias, Atom } when is_atom(Atom) ->
      case expand(Alias, MacroAliases) of
        OtherAtom when is_atom(OtherAtom) -> OtherAtom;
        OtherAliases when is_list(OtherAliases) -> Atom
      end;
    false ->
      expand(Alias, Aliases)
  end.

expand({ '__aliases__', _Meta, [H] }, Aliases) when H /= 'Elixir' ->
  case expand_one(H, Aliases) of
    false -> [H];
    Atom  -> Atom
  end;

expand({ '__aliases__', _Meta, [H|T] }, Aliases) when is_atom(H) ->
  case H of
    'Elixir' ->
      concat(T);
    _ ->
      case expand_one(H, Aliases) of
        false -> [H|T];
        Atom  -> concat([Atom|T])
      end
  end;

expand({ '__aliases__', _Meta, List }, _Aliases) ->
  List.

expand_one(H, Aliases) ->
  Lookup = list_to_atom("Elixir-" ++ atom_to_list(H)),
  case lookup(Lookup, Aliases) of
    Lookup -> false;
    Else   -> Else
  end.

%% Ensure a module is loaded before its usage.

ensure_loaded(_Line, 'Elixir.Kernel', _S) ->
  ok;

ensure_loaded(Line, Ref, S) ->
  try
    Ref:module_info(compile)
  catch
    error:undef ->
      Kind = case lists:member(Ref, S#elixir_scope.scheduled) of
        true  -> scheduled_module;
        false -> unloaded_module
      end,
      elixir_errors:form_error(Line, S#elixir_scope.file, ?MODULE, { Kind, Ref })
  end.

%% Receives an atom and returns the last bit as an alias.

last(Atom) ->
  Last = last(lists:reverse(atom_to_list(Atom)), []),
  list_to_atom("Elixir-" ++ Last).

last([$-|_], Acc) -> Acc;
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
  { list_to_atom("Elixir-" ++ H), concat(lists:reverse([H|Acc])) };
do_nesting(_, _, _Acc) ->
  false.

list_nesting(Atom) ->
  case string:tokens(atom_to_list(Atom), "-") of
    ["Elixir"|T] -> T;
    _ -> []
  end.

%% Receives a list of atoms, binaries or lists
%% representing modules and concatenates them.

concat(Args) -> list_to_atom(raw_concat(Args)).
safe_concat(Args) -> list_to_existing_atom(raw_concat(Args)).

raw_concat(['Elixir'|Args]) -> do_concat(Args);
raw_concat(Args)            -> do_concat(Args).

do_concat(Args) ->
  Aliases = [to_partial(Arg) || Arg <- Args, Arg /= nil],
  "Elixir" ++ lists:concat(Aliases).

to_partial(Arg) when is_binary(Arg) -> to_partial(binary_to_list(Arg));
to_partial(Arg) when is_atom(Arg)   -> to_partial(atom_to_list(Arg));
to_partial("Elixir-" ++ Arg)        -> dot_to_dash([$-|Arg]);
to_partial([$-|_] = Arg)            -> dot_to_dash(Arg);
to_partial(Arg) when is_list(Arg)   -> [$-|dot_to_dash(Arg)].

dot_to_dash(List) ->
  [case X of
    $. -> $-;
    _  -> X
   end || X <- List].

%% Lookup an alias in the current scope.

lookup(Else, Dict) ->
  case orddict:find(Else, Dict) of
    { ok, Value } when Value /= Else -> lookup(Value, Dict);
    _ -> Else
  end.

%% Errors

format_error({unloaded_module, Module}) ->
  io_lib:format("module ~s is not loaded and could not be found", [elixir_errors:inspect(Module)]);

format_error({scheduled_module, Module}) ->
  io_lib:format("module ~s is not loaded but was defined. This happens because you are trying to use a module in the same context it is defined. Try defining the module outside the context that requires it.",
    [elixir_errors:inspect(Module)]).