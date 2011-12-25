-module(elixir_tree_helpers).
-export([abstract_syntax/1, build_var_name/2, build_bitstr/4,
  build_list/4, build_list/5, build_simple_list/2, build_reverse_list/5]).
-include("elixir.hrl").

abstract_syntax(Tree) ->
  erl_syntax:revert(erl_syntax:abstract(Tree)).

build_var_name(Line, #elixir_scope{counter=Counter} = S) ->
  NS = S#elixir_scope{counter=Counter+1},
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["X", Counter]) },
  { Var, NS }.

% Build a list transforming each expression and accumulating
% vars in one pass. It uses tail-recursive form.
%
% It receives a function to transform each expression given
% in Exprs, a Line used to build the List and the variables
% scope V is passed down item by item.
%
% The function needs to return a tuple where the first element
% is an erlang abstract form and the second is the new variables
% list.
build_list(Fun, Exprs, Line, S) ->
  build_list(Fun, Exprs, Line, S, {nil, Line}).

build_list(Fun, Exprs, Line, S, Tail) ->
  build_list_each(Fun, lists:reverse(Exprs), Line, S, Tail).

% Same as build_list, but the list given is in reverse other.
build_reverse_list(Fun, Exprs, Line, S, Tail) ->
  build_list_each(Fun, Exprs, Line, S, Tail).

% Builds a simple list, without transformation, just by generating the cons-cell.
build_simple_list(Line, Args) ->
  { List, [] } = build_list(fun(X,Y) -> {X,Y} end, Args, Line, []),
  List.

build_list_each(_Fun, [], _Line, S, Acc) ->
  { Acc, S };

build_list_each(Fun, [H|T], Line, S, Acc) ->
  { Expr, NS } = Fun(H, S),
  build_list_each(Fun, T, Line, NS, { cons, Line, Expr, Acc }).

% Build a bitstring taking into accounts the following types:
%
% * If a bitstring or a list is given, we just append its items
% * If a tuple is given, extract the bitstring information and transform the rest
%
build_bitstr(Fun, Exprs, Line, S) ->
  build_bitstr_each(Fun, Exprs, Line, S, []).

build_bitstr_each(_Fun, [], Line, S, Acc) ->
  { { bin, Line, lists:reverse(Acc) }, S };

build_bitstr_each(Fun, [H|T], Line, S, Acc) when is_list(H) ->
  NewAcc = lists:foldl(fun(Integer, FinalAcc) ->
    [{bin_element,Line,{integer,Line,Integer},default,default}|FinalAcc]
  end, Acc, H),
  build_bitstr_each(Fun, T, Line, S, NewAcc);

build_bitstr_each(Fun, [H|T], Line, S, Acc) when is_bitstring(H) ->
  { bin, _, Elements } = elixir_tree_helpers:abstract_syntax(H),
  NewAcc = lists:foldl(fun(Element, FinalAcc) -> [Element|FinalAcc] end, Acc, Elements),
  build_bitstr_each(Fun, T, Line, S, NewAcc);

build_bitstr_each(Fun, [H|T], Line, S, Acc) ->
  { Expr, NS } = Fun(H, S),
  build_bitstr_each(Fun, T, Line, NS, [{ bin_element, Line, Expr, default, default }|Acc]).