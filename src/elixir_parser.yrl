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
  comma_separator
  body
  stabber
  min_expr
  open_paren
  close_paren
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

Left 200 open_paren.
Left 100 close_paren.

% Handle a = -> b = 1 as a = (-> b = 1)
Left 300 fun_expr.
Left 400 '='.

grammar -> expr_list : '$1'.
grammar -> '$empty' : [].

expr_list -> eol : [].
expr_list -> expr : ['$1'].
expr_list -> expr eol : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr eol expr_list : ['$1'|'$3'].

expr -> assign_expr : '$1'.

%% Assignment
assign_expr -> assign_expr '=' fun_expr :
  { match, ?line('$2'), '$1', '$3' }.

assign_expr -> fun_expr : '$1'.

%% Function definitions
fun_expr -> stabber given_args assign_expr :
  build_fun('$1', [ { clause, ?line('$1'), '$2', [], ['$3'] } ]).

fun_expr -> stabber assign_expr :
  build_fun('$1', [ { clause, ?line('$1'), [], [], ['$2'] } ]).

fun_expr -> stabber given_args eol body 'end' :
  build_fun('$1', [ { clause, ?line('$1'), '$2', [], '$4' } ]).

fun_expr -> stabber eol body 'end' :
  build_fun('$1', [ { clause, ?line('$1'), [], [], '$3' } ]).

fun_expr -> add_expr : '$1'.

%% Args given to function declarations
given_args -> open_paren ')'                 : [].
given_args -> open_paren var given_args_tail : ['$2'|'$3'].

given_args_tail -> comma_separator var given_args_tail : ['$2'|'$3'].
given_args_tail -> close_paren                         : [].

%% Commas and eol
comma_separator -> ','         : ','.
comma_separator -> eol ','     : ','.
comma_separator -> ',' eol     : ','.
comma_separator -> eol ',' eol : ','.

%% Function bodies
body -> '$empty'  : [].
body -> expr_list : '$1'.

%% Arithmetic operations
add_expr -> add_expr add_op mult_expr :
  { binary_op, ?line('$1'), ?op('$2'), '$1', '$3' }.

add_expr -> mult_expr : '$1'.

mult_expr -> mult_expr mult_op unary_expr :
  { binary_op, ?line('$1'), ?op('$2'), '$1', '$3' }.

mult_expr -> unary_expr : '$1'.

unary_expr -> unary_op min_expr :
  { unary_op, ?line('$1'), ?op('$1'), '$2' }.

unary_expr -> min_expr : '$1'.

%% Minimum expressions
open_paren -> '('      : '('.
open_paren -> '(' eol  : '('.
close_paren -> ')'     : ')'.
close_paren -> eol ')' : ')'.

min_expr -> var : '$1'.
min_expr -> number : '$1'.
min_expr -> open_paren expr close_paren : '$2'.

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

build_fun(Stab, Clauses) ->
  { 'fun', ?line(Stab), { clauses, Clauses } }.