% Responsible for code inlining and optimizations in general.
-module(elixir_inliner).
-export([binary_op/8]).
-include("elixir.hrl").

binary_op(Line, Left, Right, TLeft, TRight, Op, S, SF) ->
  Optimize = S#elixir_scope.assign or S#elixir_scope.guard,

  % Check if left side is an integer or float, if so, dispatch straight to the operator
  case Optimize orelse is_number_form(TLeft) orelse is_var_form(TLeft, S, fun({_,X}) -> is_number_form(X) end) of
    true -> { {op, Line, Op, TLeft, TRight}, SF };
    false ->
      Args = [TRight],

      % Check if left side surely requires a method dispatch, if not, create a case expression
      case is_op_call_form(element(1, Left)) orelse is_var_form(TLeft, S, fun({X,_}) -> is_op_call_form(element(1, X)) end) of
        true -> { elixir_tree_helpers:build_method_call(Op, Line, Args, TLeft), SF };
        false ->
          { Var, NS } = elixir_tree_helpers:build_var_name(Line, SF),

          Match = [{match, Line, Var, TLeft}],
          True = [{atom,Line,true}],
          False = [{atom,Line,false}],
          IsNumber = {call,Line,{atom,Line,is_number},Match},

          { { 'case', Line, IsNumber, [
            { clause, Line, True, [], [{op, Line, Op, Var, TRight}] },
            { clause, Line, False, [], [elixir_tree_helpers:build_method_call(Op, Line, Args, Var)] }
          ] }, NS }
      end
  end.

% Helpers

is_number_form({integer, _, _}) -> true;
is_number_form({float, _, _}) -> true;
is_number_form({op, _, Op, _ }) when Op == '+'; Op == '-' -> true;
is_number_form({op, _, Op, Left, _ }) when Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == 'div'; Op == 'rem' -> is_number_form(Left);
is_number_form(_) -> false.

is_op_call_form(string) -> true;
is_op_call_form(regexp) -> true;
is_op_call_form(char_list) -> true;
is_op_call_form(list) -> true;
is_op_call_form(tuple) -> true;
is_op_call_form(atom) -> true;
is_op_call_form(function) -> true;
is_op_call_form(orddict) -> true;
is_op_call_form(bin) -> true;
is_op_call_form(interpolated_string) -> true;
is_op_call_form(interpolated_regexp) -> true;
is_op_call_form(interpolated_atom) -> true;
is_op_call_form(interpolated_char_list) -> true;
is_op_call_form(_) -> false.

is_var_form({ var, _, Name }, #elixir_scope{assigned_vars=Dict}, Function) ->
  case dict:find(Name, Dict) of
    { ok, Var } -> Function(Var);
    error -> false
  end;

is_var_form(_, _, _) -> false.