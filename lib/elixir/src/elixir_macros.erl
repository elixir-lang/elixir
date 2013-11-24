%% Those macros behave like they belong to Kernel,
%% but do not since they need to be implemented in Erlang.
-module(elixir_macros).
-export([translate/2]).
-import(elixir_translator, [translate_each/2]).
-import(elixir_errors, [compile_error/3, syntax_error/3, syntax_error/4]).
-include("elixir.hrl").

%% Definitions

translate({defmodule, Meta, [Ref, KV]}, S) when is_list(KV) ->
  { TRef, _ } = translate_each(Ref, S),

  Block = case lists:keyfind(do, 1, KV) of
    { do, DoValue } -> DoValue;
    false -> syntax_error(Meta, S#elixir_scope.file, "missing do keyword in defmodule")
  end,

  { FRef, FS } = case TRef of
    { atom, _, Module } ->
      FullModule = expand_module(Ref, Module, S),

      RS = case elixir_aliases:nesting_alias(S#elixir_scope.module, FullModule) of
        { New, Old } ->
          { Aliases, MacroAliases } = elixir_aliases:store(ref_meta(Ref), New, Old, [{warn,false}],
                                        S#elixir_scope.aliases, S#elixir_scope.macro_aliases, S#elixir_scope.lexical_tracker),
           S#elixir_scope{aliases=Aliases, macro_aliases=MacroAliases};
        false -> S
      end,

      { FullModule, RS#elixir_scope{context_modules=[FullModule|S#elixir_scope.context_modules]} };
    _ ->
      { Ref, S }
  end,

  Env = elixir_env:scope_to_ex({ ?line(Meta), FS }),
  translate_each(elixir_module:translate(FRef, Block, Env), FS);

translate({ Name, Meta, Args }, S) ->
  syntax_error(Meta, S#elixir_scope.file,
               "invalid arguments for macro ~ts/~B", [Name, length(Args)]).

%% Helpers

ref_meta({ '__aliases__', Meta, _ }) -> Meta;
ref_meta(_) -> [].

%% defmodule :foo
expand_module(Raw, _Module, _S) when is_atom(Raw) ->
  Raw;

%% defmodule Hello
expand_module({ '__aliases__', _, [H] }, _Module, S) ->
  elixir_aliases:concat([S#elixir_scope.module, H]);

%% defmodule Hello.World
expand_module({ '__aliases__', _, _ } = Alias, Module, S) ->
  case elixir_aliases:expand(Alias, S#elixir_scope.aliases, S#elixir_scope.macro_aliases,
                             S#elixir_scope.lexical_tracker) of
    Atom when is_atom(Atom) ->
      Module;
    Aliases when is_list(Aliases) ->
      elixir_aliases:concat([S#elixir_scope.module, Module])
  end;

%% defmodule Elixir.Hello.World
expand_module(_Raw, Module, S) ->
  elixir_aliases:concat([S#elixir_scope.module, Module]).


