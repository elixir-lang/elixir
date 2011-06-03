% Responsible for code inlining and optimizations in general.
-module(elixir_inliner).
-export([binary_op/8,get_ivar/3,set_ivar/4,set_ivars/4,method_call/5]).
-include("elixir.hrl").

method_call(Line, Expr, Name, TArgs, Else) ->
  Snapshot = case Expr of
    {constant,Line,Constant} ->
      try
        elixir_constants:lookup(Constant)
      catch
        error:{noconstant,Constant} -> []
      end;
    _ -> []
  end,

  case Snapshot of
    [] -> Else;
    _  ->
      FArgs = elixir_tree_helpers:handle_new_call(Name, Line, TArgs),
      { Module, Method, MArgs } = elixir_dispatch:dispatch_candidate(Line, Snapshot, Name, length(FArgs) + 1, FArgs),
      Reverse = elixir_tree_helpers:abstract_syntax(Snapshot),
      { call, Line, { remote, Line, { atom, Line, Module }, { atom, Line, Method } }, [Reverse|MArgs] }
  end.

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

get_ivar(Line, Name, Else) ->
  {'try',Line,
    [{call,Line,
      {remote,Line,{atom,Line,elixir_helpers},{atom,Line,orddict_find}},
      [{atom,Line,Name},{call,Line,{atom,Line,element},[{integer,Line,3},{var,Line,self}]}]
    }],
    [],
    else_clause(Line, Else),
    []
  }.

set_ivar(Line, [{atom,_,_} = Left, Right], Else, S) ->
  Element = [{integer,Line,3},{var,Line,self}],
  { Var, FS } = elixir_tree_helpers:build_var_name(Line, S),

  Call = {block,Line,[
    {match, Line, Var, Right},
    {'try',Line,
      [{call,Line,{atom,Line,setelement},Element ++
        [{call,Line,
          {remote,Line,{atom,Line,orddict},{atom,Line,store}},
          [Left, Var, {call,Line,{atom,Line,element},Element}]
        }]
      }],
      [],
      else_clause(Line, Else),
      []
    }
  ]},
  
  { Call, FS };

set_ivar(_Line, _Expr, Else, S) ->
  { Else, S }.

% If it is a raw, already ordered dict, optimize it.
set_ivars(Line, [{tuple,_,[{atom,_,_elixir_orddict__},{cons,_,_,_} = Dict]}], Else, S) ->
  Element = [{integer,Line,3},{var,Line,self}],
  { Var, FS } = elixir_tree_helpers:build_var_name(Line, S),

  Call = {block,Line,[
    {match, Line, Var, Dict},
    {'try',Line,
      [{call,Line,{atom,Line,setelement},Element ++
        [{call,Line,
          {remote,Line,{atom,Line,elixir_helpers},{atom,Line,orddict_merge}},
          [{call,Line,{atom,Line,element},Element}, Var]
        }]
      }],
      [],
      else_clause(Line, Else),
      []
    }
  ]},

  { Call, FS };

set_ivars(_Line, _Expr, Else, S) ->
  { Else, S }.

% Helpers

else_clause(Line, Else) ->
  [{clause,Line,
    [{tuple,Line,[{atom,Line,error},{var,Line,'_'},{var,Line,'_'}]}],[],[Else]
  }].

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