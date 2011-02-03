% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar
  expr_list
  decl_list
  decl
  expr _expr
  call_exprs _call_exprs
  fun_call_expr _fun_call_expr
  method_call_expr _method_call_expr
  erlang_call_expr
  min_expr

  string_base
  string_list
  fun_base
  comma_expr
  call_args
  call_args_parens
  call_args_optional
  tuple
  list
  colon_comma_expr _colon_comma_expr
  base_dict _base_dict
  dict
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
  base_identifier
  ivar
  break
  match_op
  unary_op
  add_op
  mult_op
  dot_eol
  object_decl
  module_decl
  objmod_body
  objmod_body_list
  objmod_body_decl
  method_decl
  method_name
  implicit_method_name
  method_ops_identifier
  implicit_method_ops_identifier
  if_expr if_clause elsif_clauses elsif_clause if_elsif_clauses else_clause
  .

Terminals
  punctuated_identifier identifier float integer constant
  atom interpolated_atom string interpolated_string regexp interpolated_regexp
  div rem module object 'do' 'end' def eol Erlang true false
  if elsif else unless
  '=' '+' '-' '*' '/' '(' ')' '->' ',' '.' '[' ']'
  ':' ';' '@' '{' '}' '<' '|' '_'
  .

Rootsymbol grammar.

% Solve implicit self conflicts. Imagine the following:
%
%   x + 1
%
% It can either be treated as x(+1) or (x) + 1. That said,
% we give some precedence to base_expr so the second one
% is chosen.
Nonassoc 100 base_identifier.

% Solve nested call_args conflicts
Nonassoc 100 ')'.
Nonassoc 100 eol.

Left     300 match_op. % Handle a = -> b = 1 as a = (-> b = 1)
Left     400 ':'.
Left     400 '|'.
Left     500 add_op.
Left     600 mult_op.
Left     700 '.'. % Handle a = -> b.to_s as a = (-> b.to_s)
Left     800 ','.
Nonassoc 900 unary_op.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> decl_list : '$1'.
grammar -> '$empty' : [{nil, 0}].

% List of declarations delimited by break
decl_list -> eol : ['$1'].
decl_list -> decl : ['$1'].
decl_list -> decl break : ['$1'].
decl_list -> eol decl_list : '$2'.
decl_list -> decl break decl_list : ['$1'|'$3'].

% Basic declarations
decl -> object_decl : '$1'.
decl -> module_decl : '$1'.
decl -> expr : '$1'.

% List of expressions delimited by break
expr_list -> eol : [].
expr_list -> expr : ['$1'].
expr_list -> expr break : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr break expr_list : ['$1'|'$3'].

% Assignment
expr -> expr match_op expr : build_match('$1', '$2', '$3').

% Function definitions
expr -> fun_base : '$1'.

% Arithmetic operations
expr -> expr add_op expr : build_binary_op('$1', '$2', '$3').
expr -> expr mult_op expr : build_binary_op('$1', '$2', '$3').
expr -> unary_op expr : build_unary_op('$1', '$2').
expr -> call_exprs : '$1'.

% Calls
call_exprs -> fun_call_expr : '$1'.
call_exprs -> method_call_expr : '$1'.
call_exprs -> erlang_call_expr : '$1'.
call_exprs -> min_expr : '$1'.

% Function call
fun_call_expr -> min_expr call_args_parens : build_fun_call('$1', '$2').

% Method call
method_call_expr -> call_exprs dot_eol method_name call_args_parens : build_method_call(true, '$1', '$3', '$4').
method_call_expr -> call_exprs dot_eol method_name call_args_optional : build_method_call(false, '$1', '$3', '$4').
method_call_expr -> call_exprs dot_eol method_name : build_method_call(true, '$1', '$3', []).
method_call_expr -> implicit_method_name call_args_optional : build_local_call('$1', '$2').

% Minimum expressions
min_expr -> base_expr : '$1'.
min_expr -> open_paren expr close_paren : '$2'.

%%% COPY OF MAIN FLOW FOR STABBER BUT WITHOUT MIN_EXPR PARENS

% Assignment
_expr -> _expr match_op _expr : build_match('$1', '$2', '$3').

% Function definitions
_expr -> fun_base : '$1'.

% Arithmetic operations
_expr -> _expr add_op _expr : build_binary_op('$1', '$2', '$3').
_expr -> _expr mult_op _expr : build_binary_op('$1', '$2', '$3').
_expr -> unary_op _expr : build_unary_op('$1', '$2').
_expr -> _call_exprs : '$1'.

% Calls
_call_exprs -> _fun_call_expr : '$1'.
_call_exprs -> _method_call_expr : '$1'.
_call_exprs -> erlang_call_expr : '$1'.
_call_exprs -> base_expr : '$1'.

% Function call
_fun_call_expr -> base_expr call_args_parens : build_fun_call('$1', '$2').

% Method call
_method_call_expr -> _call_exprs dot_eol method_name call_args_parens : build_method_call(true, '$1', '$3', '$4').
_method_call_expr -> _call_exprs dot_eol method_name call_args_optional : build_method_call(false, '$1', '$3', '$4').
_method_call_expr -> _call_exprs dot_eol method_name : build_method_call(true, '$1', '$3', []).
_method_call_expr -> implicit_method_name call_args_optional : build_local_call('$1', '$2').

%%% BUILDING BLOCKS

% Base function declarations
fun_base -> stabber call_args_parens expr :
  build_fun('$1', build_clause('$1', '$2', ['$3'])).

fun_base -> stabber _expr :
  build_fun('$1', build_clause('$1', [], ['$2'])).

fun_base -> stabber call_args_parens break body 'end' :
  build_fun('$1', build_clause('$1', '$2', '$4')).

fun_base -> stabber break body 'end' :
  build_fun('$1', build_clause('$1', [], '$3')).

% Args given on method invocations.
base_dict  -> colon_comma_expr : { dict, ?line(lists:nth(1, '$1')), '$1' }.
_base_dict -> _colon_comma_expr : { dict, ?line(lists:nth(1, '$1')), '$1' }.

comma_expr -> expr : ['$1'].
comma_expr -> expr comma_separator comma_expr : ['$1'|'$3'].

call_args -> expr : ['$1'].
call_args -> base_dict : ['$1'].
call_args -> expr comma_separator call_args : ['$1'|'$3'].

call_args_parens -> open_paren ')' : [].
call_args_parens -> open_paren call_args close_paren : '$2'.

call_args_optional -> _expr : ['$1'].
call_args_optional -> _expr comma_separator call_args : ['$1'|'$3'].
call_args_optional -> _base_dict : ['$1'].

% Tuples declaration.
tuple -> open_curly '}' : { tuple, ?line('$1'), [] }.
tuple -> open_curly comma_expr close_curly : { tuple, ?line('$1'), '$2' }.

% Lists declaration.
list -> open_bracket ']' : build_list(?line('$1'), []).
list -> open_bracket comma_expr close_bracket : build_list(?line('$1'), '$2').
list -> open_bracket comma_expr '|' expr close_bracket : build_list(?line('$1'), '$2', '$4').

% Dicts declarations
colon_comma_expr -> expr ':' expr : [build_dict_tuple('$1', '$3')].
colon_comma_expr -> expr ':' expr comma_separator colon_comma_expr : [build_dict_tuple('$1', '$3')|'$5'].

_colon_comma_expr -> _expr ':' expr : [build_dict_tuple('$1', '$3')].
_colon_comma_expr -> _expr ':' expr comma_separator colon_comma_expr : [build_dict_tuple('$1', '$3')|'$5'].

dict -> open_curly ':' '}' : { dict, ?line('$1'), [] }.
dict -> open_curly colon_comma_expr close_curly : { dict, ?line('$1'), '$2' }.

% Base identifiers. Some keywords are converted to base identifier and
% are used as variable names. Notice extra_identifiers are not allowed as
% implicit method call.
base_identifier -> identifier : '$1'.
base_identifier -> module : { identifier, ?line('$1'), module }.
base_identifier -> object : { identifier, ?line('$1'), object }.
base_identifier -> div : { identifier, ?line('$1'), 'div' }.
base_identifier -> rem : { identifier, ?line('$1'), 'rem' }.
base_identifier -> '_' : { identifier, ?line('$1'), '_' }.

% ivar
ivar -> '@' base_identifier : { ivar, ?line('$1'), ?chars('$2') }.

% Commas and eol
comma_separator -> ','     : '$1'.
comma_separator -> ',' eol : '$1'.

% Function bodies
body -> '$empty'  : [{nil, 0}].
body -> expr_list : '$1'.

% Parens handling
open_paren -> '('      : '$1'.
open_paren -> '(' eol  : '$1'.
close_paren -> ')'     : '$1'.
close_paren -> eol ')' : '$2'.

% Bracket handling
open_bracket -> '['      : '$1'.
open_bracket -> '[' eol  : '$1'.
close_bracket -> ']'     : '$1'.
close_bracket -> eol ']' : '$2'.

% Curly brackets handling
open_curly  -> '{'     : '$1'.
open_curly  -> '{' eol : '$1'.
close_curly -> '}'     : '$1'.
close_curly -> eol '}' : '$2'.

% Base expressions
base_expr -> base_identifier : '$1'.
base_expr -> string_list : build_string_list('$1').
base_expr -> ivar : '$1'.
base_expr -> atom : '$1'.
base_expr -> interpolated_atom : '$1'.
base_expr -> regexp : '$1'.
base_expr -> interpolated_regexp : '$1'.
base_expr -> number : '$1'.
base_expr -> constant : '$1'.
base_expr -> tuple : '$1'.
base_expr -> list : '$1'.
base_expr -> dict : '$1'.
base_expr -> true : { atom, ?line('$1'), true }.
base_expr -> false : { atom, ?line('$1'), false }.
base_expr -> if_expr : '$1'.

% Conditionals
if_expr -> if_elsif_clauses 'end' : build_if_expr('$1').
if_expr -> if_elsif_clauses else_clause 'end' : build_if_expr('$1', '$2').

if_clause -> 'if' expr break expr_list : { 'if_clause', ?line('$1'), [], '$2', '$4' }.
if_clause -> 'unless' expr break expr_list : { 'if_clause', ?line('$1'), 'not', '$2', '$4' }.

elsif_clauses -> elsif_clause elsif_clauses : ['$1'|'$2'].
elsif_clauses -> elsif_clause : ['$1'].
elsif_clause  -> elsif expr break expr_list : { 'if_clause', ?line('$1'), [], '$2', '$4' }.

if_elsif_clauses -> if_clause : ['$1'].
if_elsif_clauses -> if_clause elsif_clauses : ['$1'|'$2'].

else_clause -> else expr_list : '$2'.
else_clause -> else ';' expr_list : '$3'.

% String expressions
string_base -> string : '$1'.
string_base -> interpolated_string : '$1'.

string_list -> string_base : ['$1'].
string_list -> string_base eol : ['$1'].
string_list -> string_base string_list : ['$1'|'$2'].
string_list -> string_base eol string_list : ['$1'|'$3'].

% Erlang calls
erlang_call_expr -> Erlang '.' base_identifier '.' base_identifier call_args_parens : build_erlang_call(true, '$1', ?chars('$3'), ?chars('$5'), '$6').
erlang_call_expr -> Erlang '.' base_identifier '.' base_identifier call_args_optional : build_erlang_call(false, '$1', ?chars('$3'), ?chars('$5'), '$6').
erlang_call_expr -> Erlang '.' base_identifier '.' base_identifier : build_erlang_call(true, '$1', ?chars('$3'), ?chars('$5'), []).
erlang_call_expr -> Erlang '.' base_identifier call_args_parens : build_erlang_call(true, '$1', [], ?chars('$3'), '$4').
erlang_call_expr -> Erlang '.' base_identifier call_args_optional : build_erlang_call(false, '$1', [], ?chars('$3'), '$4').
erlang_call_expr -> Erlang '.' base_identifier : build_erlang_call(true, '$1', [], ?chars('$3'), []).

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

mult_op -> div : '$1'.
mult_op -> rem : '$1'.

% Dot break
dot_eol -> '.'     : '$1'.
dot_eol -> '.' eol : '$1'.

% Object/Module declaration
object_decl -> object constant '<' constant break objmod_body 'end' : build_object('$2', '$6', ?chars('$4')).
object_decl -> object constant break objmod_body 'end' : build_object('$2', '$4', 'Object').
module_decl -> module constant break objmod_body 'end' : build_object('$2', '$4', 'Module').

objmod_body -> '$empty' : [{nil, 0}].
objmod_body -> objmod_body_list : '$1'.

objmod_body_list -> eol : [].
objmod_body_list -> objmod_body_decl : ['$1'].
objmod_body_list -> objmod_body_decl break : ['$1'].
objmod_body_list -> eol objmod_body_list : '$2'.
objmod_body_list -> objmod_body_decl break objmod_body_list : ['$1'|'$3'].

objmod_body_decl -> decl : '$1'.
objmod_body_decl -> method_decl : '$1'.

% Method declarations
method_decl -> def method_name break body 'end' :
  build_def_method('$2', [], build_clause('$2', [], '$4')).

method_decl -> def method_name call_args_parens break body 'end' :
  build_def_method('$2', '$3', build_clause('$2', '$3', '$5')).

% Method names do not inherit from base_identifier, which include object,
% module and _ as keywords, to avoid conflicts.
implicit_method_name -> identifier : '$1'.
implicit_method_name -> punctuated_identifier : '$1'.
implicit_method_name -> implicit_method_ops_identifier : { identifier, ?line('$1'), ?op('$1') }.

method_name -> implicit_method_name : '$1'.
method_name -> method_ops_identifier : { identifier, ?line('$1'), ?op('$1') }.

implicit_method_ops_identifier -> div : '$1'.
implicit_method_ops_identifier -> rem : '$1'.

method_ops_identifier -> '+' : '$1'.
method_ops_identifier -> '-' : '$1'.
method_ops_identifier -> '*' : '$1'.
method_ops_identifier -> '/' : '$1'.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(chars(Node), element(3, Node)).

% The following directive is needed for (significantly) faster compilation
% of the generated .erl file by the HiPE compiler.  Please do not remove.
% -compile([{hipe,[{regalloc,linear_scan}]}]).

build_fun_call(Target, Args) ->
  { fun_call, ?line(Target), Target, Args }.

build_clause(Parent, Args, Body) ->
  { clause, ?line(Parent), Args, [], Body }.

build_object(Name, Body, Parent) ->
  { object, ?line(Name), ?chars(Name), Parent, Body }.

build_fun(Stab, Clauses) ->
  { 'fun', ?line(Stab), { clauses, [Clauses] } }.

build_binary_op(Left, Op, Right) ->
  { binary_op, ?line(Op), ?op(Op), Left, Right }.

build_unary_op(Op, Value) ->
  { unary_op, ?line(Op), ?op(Op), Value }.

build_match(Left, Op, Right) ->
  { match, ?line(Op), Left, Right }.

build_def_method(Name, Args, Clauses) ->
  { def_method, ?line(Name), ?chars(Name), length(Args), [Clauses] }.

build_method_call(true, Expr, Name, Args) ->
  { method_call, ?line(Name), ?chars(Name), Args, Expr };

build_method_call(false, Expr, Name, Args) ->
  case Args of
    [{unary_op, Line, Op, Right}|T] ->
      Left = { method_call, ?line(Name), ?chars(Name), T, Expr },
      { binary_op, Line, Op, Left, Right };
    _ -> build_method_call(true, Expr, Name, Args)
  end.

build_local_call(Name, Args) ->
  case Args of
    [{unary_op, Line, Op, Right}|T] ->
      Left = { local_call, ?line(Name), ?chars(Name), T },
      { binary_op, Line, Op, Left, Right };
    _ -> { local_call, ?line(Name), ?chars(Name), Args }
  end.

build_erlang_call(true, Op, Prefix, Suffix, Args) ->
  { erlang_call, ?line(Op), Prefix, Suffix, Args };

build_erlang_call(false, Op, Prefix, Suffix, Args) ->
  case Args of
    [{unary_op, Line, Op, Expr}|T] ->
      Call = { erlang_call, ?line(Op), Prefix, Suffix, T },
      { binary_op, Line, Op, Call, Expr };
    _ -> build_erlang_call(true, Op, Prefix, Suffix, Args)
  end.

% Build a string list. If we are concatenating a non interpolated
% string with an interpolated string, we need to escape the interpolation
% characters of all non-interpolation allowed strings.

build_string_list(Collection) ->
  Line = ?line(hd(Collection)),
  Interpol = lists:any(fun(X) -> element(1, X) == interpolated_string end, Collection),
  Chars = build_string_list(Interpol, Collection, []),
  case Interpol of
    true  -> { interpolated_string, Line, Chars };
    false -> { string, Line, Chars }
  end.

build_string_list(Interpol, [], Acc) ->
  lists:reverse(Acc);

build_string_list(Interpol, [{Kind, _, Chars}|T], Acc) ->
  build_string_list(Interpol, T, build_string_list(Interpol, Kind, Chars, Acc)).

build_string_list(true, string, [$#|T], Acc) ->
  build_string_list(true, string, T, [$#,$\\|Acc]);

build_string_list(Interpol, Kind, [H|T], Acc) ->
  build_string_list(Interpol, Kind, T, [H|Acc]);

build_string_list(Interpol, Kind, [], Acc) ->
  Acc.

build_list(Line, Exprs) ->
  build_list(Line, Exprs, {nil, Line}).

build_list(Line, Exprs, Tail) ->
  { list, Line, Exprs, Tail }.

build_dict_tuple(Key, Value) ->
  { tuple, ?line(Key), [Key, Value] }.

build_if_expr(Exprs) ->
  build_if_expr(Exprs, [{nil, ?line(hd(Exprs))}]).

build_if_expr(Exprs, Else) ->
  { 'if', ?line(hd(Exprs)), Exprs, Else }.