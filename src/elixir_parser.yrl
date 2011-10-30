% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar expr_list expr call_expr max_expr base_expr
  break comma_separator
  add_op mult_op unary_op match_op
  open_paren close_paren
  call_args call_args_parens
  operator_call
  .

Terminals
  number atom
  '+' '-' '*' '/' '=' call_op
  '(' ')' eol ';' ','
  .

Rootsymbol grammar.

Right     20 match_op.
Left     110 add_op.
Left     120 mult_op.
Nonassoc 130 unary_op.
Nonassoc 140 call_op.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> expr_list : '$1'.
grammar -> '$empty' : [nil].

% List of expressions delimited by break
expr_list -> eol : [].
expr_list -> expr : ['$1'].
expr_list -> expr break : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr break expr_list : ['$1'|'$3'].

expr -> expr match_op expr : build_op('$2', '$1', '$3').
expr -> expr add_op expr : build_op('$2', '$1', '$3').
expr -> expr mult_op expr : build_op('$2', '$1', '$3').
expr -> unary_op expr : build_unary_op('$1', '$2').
expr -> call_expr : '$1'.

call_expr -> operator_call : '$1'.
call_expr -> max_expr : '$1'.

max_expr -> base_expr : '$1'.
max_expr -> '(' grammar ')' : '$2'.

base_expr -> number : ?exprs('$1').
base_expr -> atom : ?exprs('$1').

%% Helpers

break -> eol : '$1'.
break -> ';' : { eol, ?line('$1') }.

comma_separator -> ','     : '$1'.
comma_separator -> ',' eol : '$1'.

open_paren -> '('      : '$1'.
open_paren -> '(' eol  : '$1'.
close_paren -> ')'     : '$1'.
close_paren -> eol ')' : '$2'.

% Operators

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> add_op eol : '$1'.

mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
mult_op -> mult_op eol : '$1'.

unary_op -> '+' : '$1'.
unary_op -> '-' : '$1'.

match_op -> '=' : '$1'.
match_op -> '=' eol : '$1'.

% Function calls

operator_call -> call_op call_args_parens : build_call_op('$1', '$2').

call_args -> expr : ['$1'].
call_args -> expr comma_separator call_args : ['$1'|'$3'].

call_args_parens -> open_paren ')' : [].
call_args_parens -> open_paren call_args close_paren : '$2'.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(exprs(Node), element(3, Node)).

% The following directive is needed for (significantly) faster compilation
% of the generated .erl file by the HiPE compiler. Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

build_op(Op, Left, Right) ->
  { ?op(Op), ?line(Op), Left, Right }.

build_unary_op(Op, Expr) ->
  { ?op(Op), ?line(Op), Expr }.

build_call_op(Op, [Left, Right]) ->
  { ?exprs(Op), ?line(Op), Left, Right };

build_call_op(Op, [Expr]) ->
  { ?exprs(Op), ?line(Op), Expr }.