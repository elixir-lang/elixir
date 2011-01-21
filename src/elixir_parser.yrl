% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar
  expr_list
  decl_list
  decl
  fun_expr
  expr _expr
  match_expr _match_expr
  add_expr _add_expr
  mult_expr _mult_expr
  unary_expr _unary_expr
  call_exprs _call_exprs
  fun_call_expr _fun_call_expr
  method_call_expr _method_call_expr
  erlang_call_expr
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
  tuple_args
  tuple
  comma_separator
  body
  stabber
  base_expr
  open_paren
  close_paren
  open_bracket
  close_bracket
  open_curly
  close_curly
  number
  var
  base_identifier
  break
  match_op
  unary_op
  add_op
  mult_op
  module_decl
  module_body
  module_body_list
  module_body_decl
  method_decl
  method_name
  method_ops_identifier
  object_decl
  object_body
  .

Terminals
  punctuated_identifier identifier float integer constant atom
  module object 'do' 'end' def eol erl
  '=' '+' '-' '*' '/' '(' ')' '->' ',' '.' '[' ']' ';' '@' '{' '}'
  .

Rootsymbol grammar.

Left     100 call_args.
Left     200 '.'. % Handle a = -> b.to_s as a = (-> b.to_s)
Left     300 match_op. % Handle a = -> b = 1 as a = (-> b = 1)
Left     400 add_op.
Left     500 mult_op.
Nonassoc 600 unary_op.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> decl_list : '$1'.
grammar -> '$empty' : [{nil, 0}].

% List of declarations delimited by break
decl_list -> break : ['$1'].
decl_list -> decl : ['$1'].
decl_list -> decl break : ['$1'].
decl_list -> break decl_list : '$2'.
decl_list -> decl break decl_list : ['$1'|'$3'].

% Basic declarations
decl -> object_decl : '$1'.
decl -> module_decl : '$1'.
decl -> expr : '$1'.

% List of expressions delimited by break
expr_list -> break : [].
expr_list -> expr : ['$1'].
expr_list -> expr break : ['$1'].
expr_list -> break expr_list : '$2'.
expr_list -> expr break expr_list : ['$1'|'$3'].

% Basic expressions
expr -> match_expr : '$1'.

% Assignment
match_expr -> match_expr match_op fun_expr : build_match('$1', '$2', '$3').
match_expr -> fun_expr : '$1'.

% Function definitions
fun_expr -> fun_base : '$1'.
fun_expr -> add_expr : '$1'.

% Arithmetic operations
add_expr -> add_expr add_op mult_expr : build_binary_op('$1', '$2', '$3').
add_expr -> mult_expr : '$1'.

mult_expr -> mult_expr mult_op unary_expr : build_binary_op('$1', '$2', '$3').
mult_expr -> unary_expr : '$1'.

unary_expr -> unary_op call_exprs : build_unary_op('$1', '$2').
unary_expr -> call_exprs : '$1'.

% Calls
call_exprs -> fun_call_expr : '$1'.
call_exprs -> method_call_expr : '$1'.
call_exprs -> erlang_call_expr : '$1'.
call_exprs -> min_expr : '$1'.

% Function call
% TODO This will be ambigous in the future with implicit self.
fun_call_expr -> min_expr fun_args_parens : build_fun_call('$1', '$2').

% Method call
method_call_expr -> call_exprs '.' method_name call_args_optional : build_method_call('$1', '$3', '$4').
method_call_expr -> call_exprs '.' method_name : build_method_call('$1', '$3', []).

% Minimum expressions
min_expr -> base_expr : '$1'.
min_expr -> open_paren expr close_paren : '$2'.

%%% COPY OF MAIN FLOW FOR STABBER BUT WITHOUT MIN_EXPR PARENS

_expr -> _match_expr : '$1'.

% Assignment
_match_expr -> _match_expr match_op _add_expr : build_match('$1', '$2', '$3').
_match_expr -> _add_expr : '$1'.

% Arithmetic operations
_add_expr -> _add_expr add_op _mult_expr : build_binary_op('$1', '$2', '$3').
_add_expr -> _mult_expr : '$1'.

_mult_expr -> _mult_expr mult_op _unary_expr : build_binary_op('$1', '$2', '$3').
_mult_expr -> _unary_expr : '$1'.

_unary_expr -> unary_op _call_exprs : build_unary_op('$1', '$2').
_unary_expr -> _call_exprs : '$1'.

% Calls
_call_exprs -> _fun_call_expr : '$1'.
_call_exprs -> _method_call_expr : '$1'.
_call_exprs -> erlang_call_expr : '$1'.
_call_exprs -> base_expr : '$1'.

% Function call
% TODO This will be ambigous in the future with implicit self.
_fun_call_expr -> base_expr fun_args_parens : build_fun_call('$1', '$2').

% Method call
% TODO Test method calls inside inline functions ->
_method_call_expr -> _call_exprs '.' method_name call_args_optional : build_method_call('$1', '$3', '$4').
_method_call_expr -> _call_exprs '.' method_name : build_method_call('$1', '$3', []).

%%% BUILDING BLOCKS

% Base function declarations
fun_base -> stabber match_args expr :
  build_fun('$1', build_clause('$1', '$2', ['$3'])).

fun_base -> stabber _expr :
  build_fun('$1', build_clause('$1', [], ['$2'])).

fun_base -> stabber match_args break body 'end' :
  build_fun('$1', build_clause('$1', '$2', '$4')).

fun_base -> stabber break body 'end' :
  build_fun('$1', build_clause('$1', [], '$3')).

% Args given as match criteria.
% Used on function declarations and pattern matching.
match_args -> open_paren ')' : [].
match_args -> open_paren expr match_args_tail : ['$2'|'$3'].

match_args_tail -> comma_separator expr match_args_tail : ['$2'|'$3'].
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

% Tuples declaration.
tuple_args -> expr : ['$1'].
tuple_args -> expr comma_separator tuple_args : ['$1'|'$3'].

tuple -> open_curly '}' : { tuple, ?line('$1'), [] }.
tuple -> open_curly tuple_args close_curly : { tuple, ?line('$1'), '$2' }.

% Variables
var -> base_identifier : { var, ?line('$1'), ?chars('$1') }.

% Base identifiers. Convert keywords to basic words.
base_identifier -> identifier : '$1'.
base_identifier -> module : { identifier, ?line('$1'), module }.
base_identifier -> object : { identifier, ?line('$1'), object }.

% Commas and break
comma_separator -> ','       : '$1'.
comma_separator -> ',' break : '$1'.

% Function bodies
body -> '$empty'  : [{nil, 0}].
body -> expr_list : '$1'.

% Parens handling
open_paren -> '('        : '$1'.
open_paren -> '(' break  : '$1'.
close_paren -> ')'       : '$1'.
close_paren -> break ')' : '$2'.

% Bracket handling
open_bracket -> '['        : '$1'.
open_bracket -> '[' break  : '$1'.
close_bracket -> ']'       : '$1'.
close_bracket -> break ']' : '$2'.

% Curly brackets handling
open_curly  -> '{'       : '$1'.
open_curly  -> '{' break : '$1'.
close_curly -> '}'       : '$1'.
close_curly -> break '}' : '$2'.

% Base expressions
base_expr -> var : '$1'.
base_expr -> atom : '$1'.
base_expr -> number : '$1'.
base_expr -> constant : '$1'.
base_expr -> tuple : '$1'.

% Erlang calls
erlang_call_expr -> erl '.' base_identifier '.' base_identifier call_args_optional : build_erlang_call('$1', ?chars('$3'), ?chars('$5'), '$6').
erlang_call_expr -> erl '.' base_identifier call_args_optional : build_erlang_call('$1', erlang, ?chars('$3'), '$4').

% Stab syntax
stabber -> '->' : '$1'.
stabber -> 'do' : '$1'.

% Numbers
number -> float   : '$1'.
number -> integer : '$1'.

% Break
break -> eol : '$1'.
break -> ';' : { eol, ?line('$1') }.

% Match operator
match_op -> '=' : '$1'.
match_op -> '=' eol : '$1'.

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
module_decl -> module constant break module_body 'end' : build_module('$2', '$4').
module_body -> '$empty' : [{nil, 0}].
module_body -> module_body_list : '$1'.

module_body_list -> break : [].
module_body_list -> module_body_decl : ['$1'].
module_body_list -> module_body_decl break : ['$1'].
module_body_list -> break module_body_list : '$2'.
module_body_list -> module_body_decl break module_body_list : ['$1'|'$3'].

module_body_decl -> decl : '$1'.
module_body_decl -> method_decl : '$1'.

% Method declarations
method_decl -> def method_name break body 'end' :
  build_method('$2', [], build_clause('$2', [], '$4')).

method_decl -> def method_name match_args break body 'end' :
  build_method('$2', '$3', build_clause('$2', '$3', '$5')).

method_name -> base_identifier : '$1'.
method_name -> punctuated_identifier : '$1'.
method_name -> method_ops_identifier : { identifier, ?line('$1'), ?op('$1') }.

method_ops_identifier -> '+' : '$1'.
method_ops_identifier -> '-' : '$1'.
method_ops_identifier -> '*' : '$1'.
method_ops_identifier -> '/' : '$1'.
method_ops_identifier -> '@' '+' : { '@+', ?line('$1') }.
method_ops_identifier -> '@' '-' : { '@-', ?line('$1') }.

% Object declaration
object_decl -> object constant break object_body 'end' : build_object('$2', '$4').
object_body -> '$empty'  : [{nil, 0}].
object_body -> decl_list : '$1'.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(chars(Node), element(3, Node)).

build_fun_call(Target, Args) ->
  { fun_call, ?line(Target), Target, Args }.

build_clause(Parent, Args, Body) ->
  { clause, ?line(Parent), Args, [], Body }.

build_module(Name, Body) ->
  { module, ?line(Name), ?chars(Name), Body }.

build_object(Name, Body) ->
  { object, ?line(Name), ?chars(Name), Body }.

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

build_erlang_call(Op, Prefix, Suffix, Args) ->
  { erlang_call, ?line(Op), Prefix, Suffix, Args }.