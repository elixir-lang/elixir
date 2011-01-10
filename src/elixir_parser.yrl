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
  fun_expr
  given_args
  given_args_tail
  body
  stabber
  max_expr
  number
  unary_op
  add_op
  mult_op
  .

Terminals
  var float integer eol
  'do' 'end'
  '=' '+' '-' '*' '/' '(' ')' '->' ','
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

unary_expr -> fun_expr : '$1'.

%% Function definitions
fun_expr -> stabber given_args expr :
  { 'fun', ?line('$1'),
    { clauses, [ { clause, ?line('$1'), '$2', [], ['$3'] } ] }
  }.

fun_expr -> stabber expr :
  { 'fun', ?line('$1'),
    { clauses, [ { clause, ?line('$1'), [], [], ['$2'] } ] }
  }.

fun_expr -> stabber given_args eol body 'end' :
  { 'fun', ?line('$1'),
    { clauses, [ { clause, ?line('$1'), '$2', [], '$4' } ] }
  }.

fun_expr -> stabber eol body 'end' :
  { 'fun', ?line('$1'),
    { clauses, [ { clause, ?line('$1'), [], [], '$3' } ] }
  }.

fun_expr -> max_expr : '$1'.

%% Args given to function declarations
given_args -> '(' ')'                     : [].
given_args -> '(' eol ')'                 : [].
given_args -> '(' var given_args_tail     : ['$2'|'$3'].
given_args -> '(' eol var given_args_tail : ['$3'|'$4'].

given_args_tail -> ',' var given_args_tail         : ['$2'|'$3'].
given_args_tail -> ',' eol var given_args_tail     : ['$3'|'$4'].
given_args_tail -> eol ',' var given_args_tail     : ['$3'|'$4'].
given_args_tail -> eol ',' eol var given_args_tail : ['$4'|'$5'].

given_args_tail -> ')'     : [].
given_args_tail -> eol ')' : [].

%% Function bodies
body -> '$empty'  : [].
body -> expr_list : '$1'.

%% Minimum expressions
max_expr -> var : '$1'.
max_expr -> number : '$1'.
max_expr -> '(' expr ')' : '$2'.

%% Stab syntax
stabber -> '->' : '$1'.
stabber -> 'do' : '$1'.

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