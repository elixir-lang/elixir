-module(elixir_env).
-include("elixir.hrl").
-export([ex_to_env/1, env_to_scope/1, env_to_scope_with_vars/2,
         ex_to_scope/1, scope_to_ex/1, serialize/1, serialize_with_vars/1]).

%% Conversion in between #elixir_env, #elixir_scope and Macro.Env

ex_to_env(Env) when element(1, Env) == 'Elixir.Macro.Env' ->
  erlang:setelement(1, Env, elixir_env).

env_to_scope(#elixir_env{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros,functions=Functions,
    macro_functions=MacroFunctions,macro_macros=MacroMacros,
    context_modules=ContextModules,macro_aliases=MacroAliases,
    lexical_tracker=Lexical}) ->
  #elixir_scope{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros,functions=Functions,
    macro_functions=MacroFunctions,macro_macros=MacroMacros,
    context_modules=ContextModules,macro_aliases=MacroAliases,
    lexical_tracker=Lexical}.

env_to_scope_with_vars(#elixir_env{} = Env, Vars) ->
  (env_to_scope(Env))#elixir_scope{
    vars=orddict:from_list(Vars),
    counter=[{'',length(Vars)}]
  }.

scope_to_ex({ Line, #elixir_scope{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros,functions=Functions,
    context_modules=ContextModules,macro_aliases=MacroAliases,
    macro_functions=MacroFunctions, macro_macros=MacroMacros,
    vars=Vars,lexical_tracker=Lexical} }) when is_integer(Line) ->
  { 'Elixir.Macro.Env', Module, File, Line, Function, Context, Requires, Aliases,
    Functions, Macros, MacroAliases, MacroFunctions, MacroMacros, ContextModules,
    [Pair || { Pair, _ } <- Vars], Lexical }.

ex_to_scope(Env) ->
  env_to_scope(ex_to_env(Env)).

%% Serialization

serialize(#elixir_env{lexical_tracker=nil} = Env) ->
  { '{}', [], tuple_to_list(Env) };
serialize(#elixir_env{lexical_tracker=Lexical} = Env) ->
  Expr = { { '.', [], [erlang, binary_to_term] }, [], [term_to_binary(Lexical)] },
  { '{}', [], tuple_to_list(Env#elixir_env{lexical_tracker=Expr}) }.

serialize_with_vars(#elixir_env{} = Env) ->
  { Vars, _ } = lists:mapfoldl(fun var_to_tuple/2, 0, Env#elixir_env.vars),
  { serialize(Env), Vars }.

var_to_tuple({ Key, Kind }, Counter) ->
  Var = if is_atom(Kind) -> { Key, [], Kind };
           is_integer(Kind) -> { Key, [{ counter, Kind }], nil }
        end,
  Args = [Key, Kind, ?atom_concat(["_@", Counter]), Var],
  { { '{}', [], Args }, Counter + 1 }.
