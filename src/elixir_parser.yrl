% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar
  expr_list
  decl_list
  decl
  expr _expr
  method_call_expr _method_call_expr
  match_expr _match_expr
  fun_expr _fun_expr
  add_expr _add_expr
  mult_expr _mult_expr
  unary_expr _unary_expr
  fun_call_expr _fun_call_expr
  min_expr

  fun_base
  match_arg
  match_args
  match_args_tail
  call_args
  call_args_parens
  call_args_optional
  fun_args
  fun_args_parens
  comma_separator
  body
  stabber
  base_expr
  open_paren
  close_paren
  open_bracket
  close_bracket
  number
  var
  unary_op
  add_op
  mult_op
  module_decl
  module_body
  method_list
  method_decl
  method_name
  prototype_decl
  prototype_body
  prototype_name
  .

Terminals
  punctuated_identifier identifier float integer constant
  module prototype 'do' 'end' def eol
  '=' '+' '-' '*' '/' '(' ')' '->' ',' '.' '[' ']'
  .

Rootsymbol grammar.

Left 500 call_args.
Left 400 '.'. % Handle a = -> b.to_s as a = (-> b.to_s)
Left 300 '='. % Handle a = -> b = 1 as a = (-> b = 1)
Left 200 open_paren.
Left 100 close_paren.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> decl_list : '$1'.
grammar -> '$empty' : [].

% List of declarations delimited by eol
decl_list -> eol : ['$1'].
decl_list -> decl : ['$1'].
decl_list -> decl eol : ['$1'].
decl_list -> eol decl_list : '$2'.
decl_list -> decl eol decl_list : ['$1'|'$3'].

% Basic declarations
decl -> prototype_decl : '$1'.
decl -> module_decl : '$1'.
decl -> expr : '$1'.

% List of expressions delimited by eol
expr_list -> eol : [].
expr_list -> expr : ['$1'].
expr_list -> expr eol : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr eol expr_list : ['$1'|'$3'].

% Basic expressions
expr -> method_call_expr : '$1'.

% Method call
method_call_expr -> match_expr '.' method_name call_args_optional : build_method_call('$1', '$3', '$4').
method_call_expr -> match_expr '.' method_name : build_method_call('$1', '$3', []).
method_call_expr -> match_expr : '$1'.

% Assignment
match_expr -> match_expr '=' fun_expr : build_match('$1', '$2', '$3').
match_expr -> fun_expr : '$1'.

% Function definitions
fun_expr -> fun_base : '$1'.
fun_expr -> add_expr : '$1'.

% Arithmetic operations
add_expr -> add_expr add_op mult_expr : build_binary_op('$1', '$2', '$3').
add_expr -> mult_expr : '$1'.

mult_expr -> mult_expr mult_op unary_expr : build_binary_op('$1', '$2', '$3').
mult_expr -> unary_expr : '$1'.

unary_expr -> unary_op fun_call_expr : build_unary_op('$1', '$2').
unary_expr -> fun_call_expr : '$1'.

fun_call_expr -> min_expr fun_args_parens : build_call('$1', '$2').
fun_call_expr -> min_expr : '$1'.

% Minimum expressions
min_expr -> base_expr : '$1'.
min_expr -> open_paren expr close_paren : '$2'.

%%% COPY OF MAIN FLOW FOR STABBER BUT WITHOUT MIN_EXPR PARENS

_expr -> _method_call_expr : '$1'.

% Method call
_method_call_expr -> _match_expr '.' method_name call_args_optional : build_method_call('$1', '$3', '$4').
_method_call_expr -> _match_expr '.' method_name : build_method_call('$1', '$3', []).
_method_call_expr -> _match_expr : '$1'.

% Assignment
_match_expr -> _match_expr '=' _fun_expr : build_match('$1', '$2', '$3').
_match_expr -> _fun_expr : '$1'.

% Function definitions
_fun_expr -> fun_base : '$1'.
_fun_expr -> _add_expr : '$1'.

% Arithmetic operations
_add_expr -> _add_expr add_op _mult_expr : build_binary_op('$1', '$2', '$3').
_add_expr -> _mult_expr : '$1'.

_mult_expr -> _mult_expr mult_op _unary_expr : build_binary_op('$1', '$2', '$3').
_mult_expr -> _unary_expr : '$1'.

_unary_expr -> unary_op _fun_call_expr : build_unary_op('$1', '$2').
_unary_expr -> _fun_call_expr : '$1'.

_fun_call_expr -> base_expr fun_args_parens : build_call('$1', '$2').
_fun_call_expr -> base_expr : '$1'.

%%% BUILDING BLOCKS

% Base function declarations
fun_base -> stabber match_args match_expr :
  build_fun('$1', build_clause('$1', '$2', ['$3'])).

fun_base -> stabber _expr :
  build_fun('$1', build_clause('$1', [], ['$2'])).

fun_base -> stabber match_args eol body 'end' :
  build_fun('$1', build_clause('$1', '$2', '$4')).

fun_base -> stabber eol body 'end' :
  build_fun('$1', build_clause('$1', [], '$3')).

% Args given as match criteria.
% Used on function declarations and pattern matching.
match_arg -> var : '$1'.
match_args -> open_paren ')' : [].
match_args -> open_paren match_arg match_args_tail : ['$2'|'$3'].

match_args_tail -> comma_separator match_arg match_args_tail : ['$2'|'$3'].
match_args_tail -> close_paren : [].

% Args given on method invocations.
call_args -> expr : ['$1'].
call_args -> expr comma_separator call_args : ['$1'|'$3'].

call_args_parens -> open_paren ')' : [].
call_args_parens -> open_paren call_args close_paren : '$2'.

call_args_optional -> call_args : '$1'.
call_args_optional -> call_args_parens : '$1'.

% Args given on function invocations.
fun_args -> expr : ['$1'].
fun_args -> expr comma_separator fun_args : ['$1'|'$3'].

fun_args_parens -> open_paren ')' : [].
fun_args_parens -> open_paren fun_args close_paren : '$2'.

% Variables
var -> identifier : { var, ?line('$1'), ?chars('$1') }.

% Commas and eol
comma_separator -> ','     : ','.
comma_separator -> ',' eol : ','.

% Function bodies
body -> '$empty'  : [].
body -> expr_list : '$1'.

% Parens handling
open_paren -> '('      : '('.
open_paren -> '(' eol  : '('.
close_paren -> ')'     : ')'.
close_paren -> eol ')' : ')'.

% Bracket handling
open_bracket -> '['      : '('.
open_bracket -> '[' eol  : '('.
close_bracket -> ']'     : ')'.
close_bracket -> eol ']' : ')'.

% Base expressions
base_expr -> var : '$1'.
base_expr -> number : '$1'.
base_expr -> constant : '$1'.

% Stab syntax
stabber -> '->' : '$1'.
stabber -> 'do' : '$1'.

% Numbers
number -> float   : '$1'.
number -> integer : '$1'.

% Unary operator
unary_op -> '+' : '$1'.
unary_op -> '-' : '$1'.

% Addition operators
add_op -> '+' : '$1'.
add_op -> '-' : '$1'.

% Multiplication operators
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.

% Module declaration
module_decl -> module constant eol module_body 'end' : build_module('$2', '$4').
module_body -> '$empty'  : [].
module_body -> decl_list : '$1'.
module_body -> method_list : '$1'.

% Method declarations
method_list -> method_decl : ['$1'].
method_list -> method_decl eol : ['$1'].
method_list -> eol method_list : '$2'.
method_list -> method_decl eol method_list : ['$1'|'$3'].

method_decl -> def method_name eol body 'end' :
  build_method('$2', [], build_clause('$2', [], '$4')).

method_decl -> def method_name match_args eol body 'end' :
  build_method('$2', '$3', build_clause('$2', '$3', '$5')).

method_name -> identifier : '$1'.
method_name -> punctuated_identifier : '$1'.

% Prototype declaration
prototype_decl -> prototype prototype_name eol prototype_body 'end' : build_prototype('$2', '$4').
prototype_name -> constant : '$1'.
prototype_body -> module_body : '$1'.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(chars(Node), element(3, Node)).

build_call(Target, Args) ->
  { call, ?line(Target), Target, Args }.

build_clause(Parent, Args, Body) ->
  { clause, ?line(Parent), Args, [], Body }.

build_module(Name, Body) ->
  { module, ?line(Name), ?chars(Name), Body }.

build_prototype(Name, Body) ->
  { prototype, ?line(Name), ?chars(Name), Body }.

build_fun(Stab, Clauses) ->
  { 'fun', ?line(Stab), { clauses, [Clauses] } }.

build_binary_op(Left, Op, Right) ->
  { binary_op, ?line(Op), ?op(Op), Left, Right }.

build_unary_op(Op, Value) ->
  { unary_op, ?line(Op), ?op(Op), Value }.

build_match(Left, Op, Right) ->
  { match, ?line(Op), Left, Right }.

build_method(Name, Args, Clauses) ->
  { method, ?line(Name), ?chars(Name), length(Args), [Clauses] }.

build_method_call(Expr, Name, Args) ->
  { method_call, ?line(Name), ?chars(Name), Args, Expr }.