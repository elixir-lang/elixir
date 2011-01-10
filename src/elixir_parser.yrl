% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar
  expr_list
  expr
  assign_expr
  add_expr
  mult_expr
  unary_expr
  max_expr
  number
  unary_op
  add_op
  mult_op
  .

Terminals
  var float integer eol
  '=' '+' '-' '*' '/' '(' ')'
  .

Rootsymbol grammar.

grammar -> expr_list : '$1'.
grammar -> '$empty' : [].

expr_list -> eol : [].
expr_list -> expr : ['$1'].
expr_list -> expr eol : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr eol expr_list : ['$1'|'$3'].

expr -> assign_expr : '$1'.

%% Assignment
assign_expr -> add_expr '=' assign_expr :
  { match, ?line('$2'), '$1', '$3' }.

assign_expr -> add_expr : '$1'.

%% Arithmetic operations
add_expr -> add_expr add_op mult_expr :
  { binary_op, ?line('$1'), ?op('$2'), '$1', '$3' }.

add_expr -> mult_expr : '$1'.

mult_expr -> mult_expr mult_op unary_expr :
  { binary_op, ?line('$1'), ?op('$2'), '$1', '$3' }.

mult_expr -> unary_expr : '$1'.

unary_expr -> unary_op max_expr :
  { unary_op, ?line('$1'), ?op('$1'), '$2' }.

unary_expr -> max_expr : '$1'.

%% Minimum expressions
max_expr -> var : '$1'.
max_expr -> number : '$1'.
max_expr -> '(' expr ')' : '$2'.

%% Numbers
number -> float   : '$1'.
number -> integer : '$1'.

%% Unary operator
unary_op -> '+' : '$1'.
unary_op -> '-' : '$1'.

%% Addition operators
add_op -> '+' : '$1'.
add_op -> '-' : '$1'.

%% Multiplication operators
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(char(Node), element(3, Node)).