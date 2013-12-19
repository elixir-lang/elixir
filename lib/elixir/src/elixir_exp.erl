-module(elixir_exp).
-export([expand/2]).
-import(elixir_errors, [compile_error/3, compile_error/4]).
-include("elixir.hrl").

%% =

expand({ '=', Meta, [Left, Right] }, E) ->
  % assert_no_guard_scope(Meta, '=', S),
  { ERight, ER } = expand(Right, E),
  { ELeft, EL }  = match(fun expand/2, Left, ER),
  { { '=', Meta, [ELeft, ERight] }, EL };

%% {}, <<>>, __op__, ->

%% __block__

expand({ '__block__', _Meta, [] }, E) ->
  { nil, E };
expand({ '__block__', _Meta, [Arg] }, E) ->
  expand(Arg, E);
expand({ '__block__', Meta, Args }, E) when is_list(Args) ->
  { EArgs, EA } = expand_many(Args, E),
  { { '__block__', Meta, EArgs }, EA };

%% __aliases__

expand({ '__aliases__', Meta, _ } = Alias, E) ->
  case elixir_aliases:expand(Alias, E#elixir_env.aliases,
                             E#elixir_env.macro_aliases, E#elixir_env.lexical_tracker) of
    Receiver when is_atom(Receiver) ->
      elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker),
      { Receiver, E };
    Aliases ->
      { EAliases, EA } = expand_args(Aliases, E),

      case lists:all(fun is_atom/1, EAliases) of
        true ->
          Receiver = elixir_aliases:concat(EAliases),
          elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker),
          { Receiver, EA };
        false ->
          { { '__aliases__', Meta, EAliases }, EA }
      end
  end;

%% alias

expand({ alias, Meta, [Ref] }, E) ->
  expand({ alias, Meta, [Ref,[]] }, E);
expand({ alias, Meta, [Ref, KV] }, E) ->
  % assert_no_match_or_guard_scope(Meta, alias, S),
  { ERef, ER } = expand(Ref, E),
  { EKV, EO }  = expand_opts(Meta, alias, [as, warn], no_alias_opts(KV), ER),

  if
    is_atom(ERef) ->
      expand_alias(Meta, true, ERef, EKV, EO);
    true ->
      compile_error(Meta, E#elixir_env.file,
        "invalid args for alias, expected a compile time atom or alias as argument")
  end;

%% Pseudo vars

expand({ '__MODULE__', _, Atom }, E) when is_atom(Atom) ->
  { E#elixir_env.module, E };
expand({ '__FILE__', _, Atom }, E) when is_atom(Atom) ->
  { E#elixir_env.file, E };
expand({ '__DIR__', _, Atom }, E) when is_atom(Atom) ->
  { filename:dirname(E#elixir_env.file), E };
expand({ '__CALLER__', _, Atom } = Caller, E) when is_atom(Atom) ->
  { Caller, E };
expand({ '__ENV__', _, Atom }, E) when is_atom(Atom) ->
  { elixir_env:env_to_ex(E), E };
expand({ { '.', _, [{ '__ENV__', _, Atom }, Field] }, _, [] }, E) when is_atom(Atom), is_atom(Field) ->
  { (elixir_env:env_to_ex(E)):Field(), E };

%% Vars

expand({ Name, Meta, Kind } = Var, #elixir_env{context=match,vars=Vars} = E) when is_atom(Name), is_atom(Kind) ->
  { Var, E#elixir_env{vars=ordsets:add_element({ Name, var_kind(Meta, Kind) }, Vars)} };
expand({ Name, Meta, Kind } = Var, #elixir_env{vars=Vars} = E) when is_atom(Name), is_atom(Kind) ->
  case lists:member({ Name, var_kind(Meta, Kind) }, Vars) of
    true ->
      { Var, E };
    false ->
      VarMeta = lists:keyfind(var, 1, Meta),
      if
        VarMeta == { var, true } ->
          compile_error(Meta, E#elixir_env.file, "expected var ~ts to expand to an existing "
                        "variable or be a part of a match", [Name]);
        E#elixir_env.context == guard ->
          compile_error(Meta, E#elixir_env.file, "unknown variable ~ts or cannot invoke "
                        "function ~ts/0 inside guard", [Name, Name]);
        true ->
          expand({ Name, Meta, [] }, E)
      end
  end;

%% Local calls

expand({ '->', Meta, _Args }, E) ->
  compile_error(Meta, E#elixir_env.file, "unhandled operator ->");

expand({ Atom, Meta, Args }, E) when is_atom(Atom), is_list(Meta), is_list(Args) ->
  % assert_no_ambiguous_op(Atom, Meta, Args, S),

  elixir_exp_dispatch:dispatch_import(Meta, Atom, Args, E, fun() ->
    expand_local(Meta, Atom, Args, E)
  end);

%% Literals

expand({ Left, Right }, E) ->
  { [ELeft, ERight], EE } = expand_args([Left, Right], E),
  { { ELeft, ERight }, EE };

expand(List, E) when is_list(List) ->
  expand_args(List, E);

expand(Other, E) ->
  { Other, E }.

%% Helpers

expand_many(Args, E) ->
  lists:mapfoldl(fun expand/2, E, Args).

expand_args(Args, E) ->
  lists:mapfoldl(fun expand/2, E, Args).

%% Match/var helpers

match(Fun, Expr, #elixir_env{context=Context} = E) ->
  { EExpr, EE } = Fun(Expr, E#elixir_env{context=match}),
  { EExpr, EE#elixir_env{context=Context} }.

var_kind(Meta, Kind) ->
  case lists:keyfind(counter, 1, Meta) of
    { counter, Counter } -> Counter;
    false -> Kind
  end.

%% Locals

expand_local(Meta, Name, Args, #elixir_env{context=Context, file=File}) when
    Context == guard; Context == match ->
  compile_error(Meta, File, "cannot invoke local ~ts/~B inside ~ts",
                [Name, length(Args), Context]);

expand_local(Meta, Name, Args, #elixir_env{local=nil, function=nil, file=File}) ->
  compile_error(Meta, File, "function ~ts/~B undefined", [Name, length(Args)]);

expand_local(Meta, Name, Args, #elixir_env{local=nil, module=Module, function=Function} = E) ->
  elixir_tracker:record_local({ Name, length(Args) }, Module, Function),
  { { Name, Meta, Args }, E };

expand_local(Meta, Name, Args, E) ->
  expand({ { '.', Meta, [E#elixir_env.local, Name] }, Meta, Args }, E).

%% Lexical helpers

expand_opts(Meta, Kind, Allowed, Opts, E) ->
  { EOpts, EE } = expand(Opts, E),
  validate_opts(Meta, Kind, Allowed, EOpts, EE),
  { EOpts, EE }.

validate_opts(Meta, Kind, Allowed, Opts, E) when is_list(Opts) ->
  [begin
    compile_error(Meta, E#elixir_env.file,
                  "unsupported option ~ts given to ~s", ['Elixir.Kernel':inspect(Key), Kind])
  end || { Key, _ } <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, _Opts, S) ->
  compile_error(Meta, S#elixir_scope.file, "invalid options for ~s, expected a keyword list", [Kind]).

no_alias_opts(KV) when is_list(KV) ->
  case lists:keyfind(as, 1, KV) of
    { as, As } -> lists:keystore(as, 1, KV, { as, no_alias_expansion(As) });
    false -> KV
  end;
no_alias_opts(KV) -> KV.

no_alias_expansion({ '__aliases__', Meta, [H|T] }) when (H /= 'Elixir') and is_atom(H) ->
  { '__aliases__', Meta, ['Elixir',H|T] };
no_alias_expansion(Other) ->
  Other.

expand_alias(Meta, IncludeByDefault, Ref, KV, #elixir_env{context_modules=Context} = E) ->
  New = expand_as(lists:keyfind(as, 1, KV), Meta, IncludeByDefault, Ref, E),

  %% Add the alias to context_modules if defined is true.
  %% This is used by defmodule in order to store the defined
  %% module in context modules.
  NewContext =
    case lists:keyfind(defined, 1, Meta) of
      { defined, Mod } when is_atom(Mod) -> [Mod|Context];
      false -> Context
    end,

  { Aliases, MacroAliases } = elixir_aliases:store(Meta, New, Ref, KV, E#elixir_env.aliases,
                                E#elixir_env.macro_aliases, E#elixir_env.lexical_tracker),

  { { 'alias', Meta, [Ref, KV] },
    E#elixir_env{aliases=Aliases, macro_aliases=MacroAliases, context_modules=NewContext} }.

%% TODO: Move to elixir_aliases
expand_as({ as, true }, _Meta, _IncludeByDefault, Ref, _E) ->
  elixir_aliases:last(Ref);
expand_as({ as, false }, _Meta, _IncludeByDefault, Ref, _E) ->
  Ref;
expand_as({ as, Atom }, Meta, _IncludeByDefault, _Ref, E) when is_atom(Atom) ->
  case length(string:tokens(atom_to_list(Atom), ".")) of
    1 -> compile_error(Meta, E#elixir_env.file,
           "invalid :as, expected an alias, got atom: ~ts", [elixir_aliases:inspect(Atom)]);
    2 -> Atom;
    _ -> compile_error(Meta, E#elixir_env.file,
           "invalid :as, expected an alias, got nested alias: ~ts", [elixir_aliases:inspect(Atom)])
  end;
expand_as(false, _Meta, IncludeByDefault, Ref, _E) ->
  if IncludeByDefault -> elixir_aliases:last(Ref);
     true -> Ref
  end;
expand_as({ as, Other }, Meta, _IncludeByDefault, _Ref, E) ->
  compile_error(Meta, E#elixir_env.file,
    "invalid :as, expected an alias, got: ~ts", ['Elixir.Macro':to_string(Other)]).
