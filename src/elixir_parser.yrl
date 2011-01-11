% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar
  expr_list
  expr
  assign_expr _assign_expr
  fun_expr _fun_expr
  add_expr _add_expr
  mult_expr _mult_expr
  unary_expr _unary_expr
  min_expr

  fun_base
  given_arg
  given_args
  given_args_tail
  comma_separator
  body
  stabber
  base_expr
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

Left 300 '='. % Handle a = -> b = 1 as a = (-> b = 1)
Left 200 open_paren.
Left 100 close_paren.

%%%% MAIN FLOW OF EXPRESSIONS

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
fun_expr -> fun_base : '$1'.
fun_expr -> add_expr : '$1'.

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
min_expr -> base_expr : '$1'.
min_expr -> open_paren expr close_paren : '$2'.

%%%% COPY OF MAIN FLOW FOR STABBER BUT WITHOUT MIN_EXPR PARENS

%% Assignment
_assign_expr -> _assign_expr '=' _fun_expr :
  { match, ?line('$2'), '$1', '$3' }.

_assign_expr -> _fun_expr : '$1'.

%% Function definitions
_fun_expr -> fun_base : '$1'.
_fun_expr -> _add_expr : '$1'.

%% Arithmetic operations
_add_expr -> _add_expr add_op _mult_expr :
  { binary_op, ?line('$1'), ?op('$2'), '$1', '$3' }.

_add_expr -> _mult_expr : '$1'.

_mult_expr -> _mult_expr mult_op _unary_expr :
  { binary_op, ?line('$1'), ?op('$2'), '$1', '$3' }.

_mult_expr -> _unary_expr : '$1'.

_unary_expr -> unary_op base_expr :
  { unary_op, ?line('$1'), ?op('$1'), '$2' }.

_unary_expr -> base_expr : '$1'.

%%%% BUILDING BLOCKS

%% Base function declarations
fun_base -> stabber given_args assign_expr :
  build_fun('$1', [ { clause, ?line('$1'), '$2', [], ['$3'] } ]).

fun_base -> stabber _assign_expr :
  build_fun('$1', [ { clause, ?line('$1'), [], [], ['$2'] } ]).

fun_base -> stabber given_args eol body 'end' :
  build_fun('$1', [ { clause, ?line('$1'), '$2', [], '$4' } ]).

fun_base -> stabber eol body 'end' :
  build_fun('$1', [ { clause, ?line('$1'), [], [], '$3' } ]).

%% Args given to function declarations
given_arg -> var : '$1'.
given_args -> open_paren ')'                 : [].
given_args -> open_paren given_arg given_args_tail : ['$2'|'$3'].

given_args_tail -> comma_separator given_arg given_args_tail : ['$2'|'$3'].
given_args_tail -> close_paren                         : [].

%% Commas and eol
comma_separator -> ','         : ','.
comma_separator -> eol ','     : ','.
comma_separator -> ',' eol     : ','.
comma_separator -> eol ',' eol : ','.

%% Function bodies
body -> '$empty'  : [].
body -> expr_list : '$1'.

%% Parens handling
open_paren -> '('      : '('.
open_paren -> '(' eol  : '('.
close_paren -> ')'     : ')'.
close_paren -> eol ')' : ')'.

%% Base expressions
base_expr -> var : '$1'.
base_expr -> number : '$1'.

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