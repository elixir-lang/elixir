% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar
  expr_list
  decl_list
  decl
  expr _expr
  np_call_exprs _np_call_exprs
  brackets_call _brackets_call
  np_method_call_expr _np_method_call_expr
  without_args_method_call_expr _without_args_method_call_expr
  np_erlang_call_expr
  brackets_expr _brackets_expr
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
  call_args_no_parens
  call_args_optional
  tuple
  list
  list_args
  generator
  comprehension_args
  list_comprehension
  bin_comprehension
  bin_base_expr
  bin_specifier
  bin_specifier_list
  bin_comma_expr
  binary
  colon_comma_expr _colon_comma_expr
  base_orddict _base_orddict
  orddict
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
  open_lt
  close_gt
  number
  base_identifier
  ivar
  break
  then_break
  match_op
  unary_op
  add_op
  mult_op
  comp_op
  or_op
  and_op
  andand_op
  oror_op
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
  case_expr case_clause case_clauses else_case_clauses
  if_expr if_clause elsif_clauses elsif_clause if_elsif_clauses else_clause
  exception_expr rescue_args rescue_clause rescue_clauses after_clause
  .

Terminals
  bracket_identifier punctuated_identifier identifier float integer constant
  atom interpolated_atom string interpolated_string regexp interpolated_regexp
  char_list interpolated_char_list
  div rem module object do end def eol Erlang true false
  if elsif else then unless case match begin rescue after filename
  and andalso or orelse not '||' '&&' for in inlist inbin
  '=' '+' '-' '*' '/' '(' ')' '->' ',' '.' '[' ']'
  ':' ';' '@' '{' '}' '|' '_' '<<' '>>'
  '!' '!!' '<' '>' '==' '!=' '<=' '>=' '=:=' '=!='
  .

Rootsymbol grammar.

% Solve implicit self conflicts. Imagine the following:
%
%   x + 1
%
% It can either be treated as x(+1) or (x) + 1. That said,
% we give some precedence to base_expr so the second one
% is chosen.
Nonassoc 10 base_identifier.

% Solve nested call_args conflicts
Nonassoc 10 ')'.
Nonassoc 10 eol.

Right    20 match_op. % Handle a = -> b = 1 as a = (-> b = 1) and a = b = c as a = (b = c)
Left     30 ':'.
Left     30 '|'.
Left     40 '.'. % Handle a = -> b.to_s as a = (-> b.to_s)
Left     40 ','.

Left     50 oror_op.
Left     60 andand_op.
Left     70 or_op.
Left     80 and_op.

Left     90 comp_op.
Left     100 add_op.
Left     110 mult_op.
Nonassoc 120 unary_op.
Nonassoc 130 without_args_method_call_expr.
Nonassoc 130 _without_args_method_call_expr.

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

% Operators
expr -> expr andand_op expr : build_comp_op('$1', '$2', '$3').
expr -> expr oror_op expr : build_comp_op('$1', '$2', '$3').
expr -> expr and_op expr : build_comp_op('$1', '$2', '$3').
expr -> expr or_op expr : build_comp_op('$1', '$2', '$3').
expr -> expr comp_op expr : build_comp_op('$1', '$2', '$3').
expr -> expr add_op expr : build_binary_op('$1', '$2', '$3').
expr -> expr mult_op expr : build_binary_op('$1', '$2', '$3').
expr -> unary_op expr : build_unary_op('$1', '$2').
expr -> np_call_exprs : '$1'.

% No Parens Calls
np_call_exprs -> np_method_call_expr : '$1'.
np_call_exprs -> np_erlang_call_expr : '$1'.
np_call_exprs -> base_identifier : '$1'.
np_call_exprs -> brackets_call : '$1'.
np_call_exprs -> brackets_expr : '$1'.

% Brackets call
brackets_call -> np_call_exprs dot_eol bracket_identifier list_args : build_bracket_call(build_method_call('$1', '$3', []), '$4').
brackets_call -> bracket_identifier list_args : build_bracket_call(build_identifier('$1'), '$2').

% Method call
np_method_call_expr -> np_call_exprs dot_eol method_name call_args_no_parens : build_method_call('$1', '$3', '$4').
np_method_call_expr -> implicit_method_name call_args_no_parens : build_local_call('$1', '$2').
np_method_call_expr -> without_args_method_call_expr : '$1'.
without_args_method_call_expr -> np_call_exprs dot_eol method_name : build_method_call('$1', '$3', []).

% Brackets expression
brackets_expr -> call_exprs list_args : build_bracket_call('$1', '$2').
brackets_expr -> call_exprs : '$1'.

% Calls with parens
call_exprs -> fun_call_expr : '$1'.
call_exprs -> method_call_expr : '$1'.
call_exprs -> erlang_call_expr : '$1'.
call_exprs -> min_expr : '$1'.

% Function call
fun_call_expr -> min_expr call_args_parens : build_fun_call('$1', '$2').
fun_call_expr -> base_identifier call_args_parens : build_fun_call('$1', '$2').

% Method call
method_call_expr -> np_call_exprs dot_eol method_name call_args_parens : build_method_call('$1', '$3', '$4').
method_call_expr -> punctuated_identifier call_args_parens : build_local_call('$1', '$2').

% Minimum expressions
min_expr -> base_expr : '$1'.
min_expr -> open_paren expr close_paren : '$2'.

%%% COPY OF MAIN FLOW FOR STABBER BUT WITHOUT MIN_EXPR PARENS

% Assignment
_expr -> _expr match_op _expr : build_match('$1', '$2', '$3').

% Function definitions
_expr -> fun_base : '$1'.

% Operators
_expr -> _expr andand_op _expr : build_comp_op('$1', '$2', '$3').
_expr -> _expr oror_op _expr : build_comp_op('$1', '$2', '$3').
_expr -> _expr and_op _expr : build_comp_op('$1', '$2', '$3'). 
_expr -> _expr or_op _expr : build_comp_op('$1', '$2', '$3').
_expr -> _expr comp_op _expr : build_comp_op('$1', '$2', '$3').
_expr -> _expr add_op _expr  : build_binary_op('$1', '$2', '$3').
_expr -> _expr mult_op _expr : build_binary_op('$1', '$2', '$3').
_expr -> unary_op _expr : build_unary_op('$1', '$2').
_expr -> _np_call_exprs : '$1'.

% No Parens Calls
_np_call_exprs -> _np_method_call_expr : '$1'.
_np_call_exprs -> np_erlang_call_expr : '$1'.
_np_call_exprs -> base_identifier : '$1'.
_np_call_exprs -> _brackets_call : '$1'.
_np_call_exprs -> _brackets_expr : '$1'.

% Brackets call
_brackets_call -> _np_call_exprs dot_eol bracket_identifier list_args : build_bracket_call(build_method_call('$1', '$3', []), '$4').
_brackets_call -> bracket_identifier list_args : build_bracket_call(build_identifier('$1'), '$2').

% Method call
_np_method_call_expr -> _np_call_exprs dot_eol method_name call_args_no_parens : build_method_call('$1', '$3', '$4').
_np_method_call_expr -> implicit_method_name call_args_no_parens : build_local_call('$1', '$2').
_np_method_call_expr -> _without_args_method_call_expr : '$1'.
_without_args_method_call_expr -> _np_call_exprs dot_eol method_name : build_method_call('$1', '$3', []).

% Brackets expression
_brackets_expr -> _call_exprs list_args : build_bracket_call('$1', '$2').
_brackets_expr -> _call_exprs : '$1'.

% Calls with parens
_call_exprs -> _fun_call_expr : '$1'.
_call_exprs -> _method_call_expr : '$1'.
_call_exprs -> erlang_call_expr : '$1'.
_call_exprs -> base_expr : '$1'.

% Function call
_fun_call_expr -> base_expr call_args_parens : build_fun_call('$1', '$2').
_fun_call_expr -> base_identifier call_args_parens : build_fun_call('$1', '$2').

% Method call
_method_call_expr -> _np_call_exprs dot_eol method_name call_args_parens : build_method_call('$1', '$3', '$4').
_method_call_expr -> punctuated_identifier call_args_parens : build_local_call('$1', '$2').

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
base_orddict  -> colon_comma_expr : { orddict, ?line(lists:nth(1, '$1')), '$1' }.
_base_orddict -> _colon_comma_expr : { orddict, ?line(lists:nth(1, '$1')), '$1' }.

comma_expr -> expr : ['$1'].
comma_expr -> expr comma_separator comma_expr : ['$1'|'$3'].

call_args -> expr : ['$1'].
call_args -> base_orddict : ['$1'].
call_args -> expr comma_separator call_args : ['$1'|'$3'].

call_args_parens -> open_paren ')' : [].
call_args_parens -> open_paren call_args close_paren : '$2'.

% The first item in call_args_no_parens cannot start with parens
call_args_no_parens -> _expr : ['$1'].
call_args_no_parens -> _expr comma_separator call_args : ['$1'|'$3'].
call_args_no_parens -> _base_orddict : ['$1'].

call_args_optional -> call_args_parens : '$1'.
call_args_optional -> call_args_no_parens : '$1'.

% Comprehension
generator -> expr in expr     : { undef_generate, ?line('$2'), '$1', '$3' }.
generator -> expr inbin expr  : { bin_generate, ?line('$2'), '$1', '$3' }.
generator -> expr inlist expr : { list_generate, ?line('$2'), '$1', '$3' }.

comprehension_args -> generator : ['$1'].
comprehension_args -> expr : ['$1'].
comprehension_args -> generator comma_separator comprehension_args : ['$1'|'$3'].

% Tuples declaration.
tuple -> open_curly '}' : { tuple, ?line('$1'), [] }.
tuple -> open_curly comma_expr close_curly : { tuple, ?line('$1'), '$2' }.

% Lists declaration.
list -> open_bracket ']' : build_list(?line('$1'), []).
list -> open_bracket comma_expr close_bracket : build_list(?line('$1'), '$2').
list -> open_bracket comma_expr '|' expr close_bracket : build_list(?line('$1'), '$2', '$4').

list_comprehension -> open_bracket expr for comprehension_args close_bracket : { lc, ?line('$3'), '$2', '$4' }.
list_args -> open_bracket comma_expr close_bracket : { ?line('$1'), '$2' }.

% Binaries declaration.
bin_base_expr -> expr : build_bin_element('$1', default, default).
bin_base_expr -> expr ':' integer : build_bin_element('$1', '$3', default).
bin_base_expr -> expr '|' bin_specifier_list : build_bin_element('$1', default, '$3').
bin_base_expr -> expr ':' integer '|' bin_specifier_list : build_bin_element('$1', '$3', '$5').

bin_specifier -> identifier : ?chars('$1').
bin_specifier_list -> bin_specifier : ['$1'].
bin_specifier_list -> bin_specifier '-' bin_specifier_list : ['$1'|'$3'].

bin_comma_expr -> bin_base_expr : ['$1'].
bin_comma_expr -> bin_base_expr comma_separator bin_comma_expr : ['$1'|'$3'].

binary -> open_lt '>>' : build_bin(?line('$1'), []).
binary -> open_lt bin_comma_expr close_gt : build_bin(?line('$1'), '$2').

bin_comprehension -> open_lt bin_comma_expr for comprehension_args close_gt : {bc, ?line('$3'), '$2', '$4' }.

% Dicts declarations
colon_comma_expr -> expr ':' expr : [build_orddict_tuple('$1', '$3')].
colon_comma_expr -> expr ':' expr comma_separator colon_comma_expr : [build_orddict_tuple('$1', '$3')|'$5'].

_colon_comma_expr -> _expr ':' expr : [build_orddict_tuple('$1', '$3')].
_colon_comma_expr -> _expr ':' expr comma_separator colon_comma_expr : [build_orddict_tuple('$1', '$3')|'$5'].

orddict -> open_curly ':' '}' : { orddict, ?line('$1'), [] }.
orddict -> open_curly colon_comma_expr close_curly : { orddict, ?line('$1'), '$2' }.

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

% lt and mt handling for binaries
open_lt  -> '<<'     : '$1'.
open_lt  -> '<<' eol : '$1'.
close_gt -> '>>'     : '$1'.
close_gt -> eol '>>' : '$2'.

% Base expressions
base_expr -> string_list : build_string_list('$1').
base_expr -> ivar : '$1'.
base_expr -> atom : '$1'.
base_expr -> interpolated_atom : '$1'.
base_expr -> char_list : '$1'.
base_expr -> interpolated_char_list : '$1'.
base_expr -> regexp : '$1'.
base_expr -> interpolated_regexp : '$1'.
base_expr -> number : '$1'.
base_expr -> constant : '$1'.
base_expr -> tuple : '$1'.
base_expr -> list : '$1'.
base_expr -> list_comprehension : '$1'.
base_expr -> bin_comprehension : '$1'.
base_expr -> orddict : '$1'.
base_expr -> binary : '$1'.
base_expr -> true : { atom, ?line('$1'), true }.
base_expr -> false : { atom, ?line('$1'), false }.
base_expr -> if_expr : '$1'.
base_expr -> exception_expr : '$1'.
base_expr -> case_expr : '$1'.
base_expr -> filename : '$1'.

% Conditionals
if_expr -> if_elsif_clauses 'end' : build_if_expr('$1').
if_expr -> if_elsif_clauses else_clause 'end' : build_if_expr('$1', '$2').

if_clause -> 'if' expr then_break expr_list : { 'if_clause', ?line('$1'), true, '$2', '$4' }.
if_clause -> 'unless' expr then_break expr_list : { 'if_clause', ?line('$1'), false, '$2', '$4' }.

elsif_clauses -> elsif_clause elsif_clauses : ['$1'|'$2'].
elsif_clauses -> elsif_clause : ['$1'].
elsif_clause  -> elsif expr then_break expr_list : { 'if_clause', ?line('$1'), true, '$2', '$4' }.

if_elsif_clauses -> if_clause : ['$1'].
if_elsif_clauses -> if_clause elsif_clauses : ['$1'|'$2'].

else_clause -> else expr_list : { else_clause, ?line('$1'), '$2' }.
else_clause -> else ';' expr_list : { else_clause, ?line('$1'), '$3' }.

% Case
case_expr -> case expr else_case_clauses 'end'       : { 'case', ?line('$1'), '$2', '$3' }.
case_expr -> case expr break else_case_clauses 'end' : { 'case', ?line('$1'), '$2', '$4' }.

case_clause -> match call_args_optional then_break expr_list : { clause, ?line('$1'), '$2', [], '$4' }.

case_clauses -> case_clause : ['$1'].
case_clauses -> case_clause case_clauses : ['$1'|'$2'].

else_case_clauses -> case_clauses : '$1'.
else_case_clauses -> case_clauses else_clause : '$1' ++ ['$2'].

% Begin/Rescue/After
exception_expr -> begin expr_list end : { 'try', ?line('$1'), '$2', [], [], [] }.
exception_expr -> begin expr_list rescue_clauses end : { 'try', ?line('$1'), '$2', [], '$3', [] }.
exception_expr -> begin expr_list after_clause end : { 'try', ?line('$1'), '$2', [], [], '$3' }.
exception_expr -> begin expr_list rescue_clauses after_clause end : { 'try', ?line('$1'), '$2', [], '$3', '$4' }.

rescue_args -> expr : ['$1'].
rescue_args -> expr ':' expr : [{'$1', '$3'}].
rescue_args -> expr comma_separator rescue_args : ['$1'|'$3'].
rescue_args -> expr ':' expr comma_separator rescue_args : [{'$1', '$3'}|'$5'].

rescue_clause -> rescue rescue_args then_break expr_list : build_rescue_clause('$1', '$2', '$4').
rescue_clauses -> rescue_clause : '$1'.
rescue_clauses -> rescue_clause rescue_clause : '$1' ++ '$2'.

after_clause -> after expr_list : '$2'.

% String expressions
string_base -> string : '$1'.
string_base -> interpolated_string : '$1'.

string_list -> string_base : ['$1'].
string_list -> string_base string_list : ['$1'|'$2'].

% Erlang calls
np_erlang_call_expr -> Erlang '.' base_identifier '.' base_identifier call_args_no_parens : build_erlang_call('$1', ?chars('$3'), ?chars('$5'), '$6').
np_erlang_call_expr -> Erlang '.' base_identifier '.' base_identifier : build_erlang_call('$1', ?chars('$3'), ?chars('$5'), []).
np_erlang_call_expr -> Erlang '.' base_identifier call_args_no_parens : build_erlang_call('$1', erlang, ?chars('$3'), '$4').
np_erlang_call_expr -> Erlang '.' base_identifier : build_erlang_call('$1', erlang, ?chars('$3'), []).

% Erlang bracket calls
np_erlang_call_expr -> Erlang '.' base_identifier '.' bracket_identifier list_args : build_bracket_call(build_erlang_call('$1', ?chars('$3'), ?chars('$5'), []), '$6').
np_erlang_call_expr -> Erlang '.' bracket_identifier list_args : build_bracket_call(build_erlang_call('$1', erlang, ?chars('$3'), []), '$4').

% Erlang calls with explicit parens
erlang_call_expr -> Erlang '.' base_identifier '.' base_identifier call_args_parens : build_erlang_call('$1', ?chars('$3'), ?chars('$5'), '$6').
erlang_call_expr -> Erlang '.' base_identifier call_args_parens : build_erlang_call('$1', erlang, ?chars('$3'), '$4').

% Stab syntax
stabber -> '->' : '$1'.
stabber -> 'do' : '$1'.

% Numbers
number -> float   : '$1'.
number -> integer : '$1'.

% Break
break -> eol : '$1'.
break -> ';' : { eol, ?line('$1') }.

% Then break
then_break -> break : '$1'.
then_break -> then : '$1'.

% Match operator
match_op -> '=' : '$1'.
match_op -> '=' eol : '$1'.

% Unary operator
unary_op -> '+'   : '$1'.
unary_op -> '-'   : '$1'.
unary_op -> '!!'  : '$1'.
unary_op -> '!'   : '$1'.
unary_op -> 'not' : '$1'.

% Addition operators
add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> add_op eol : '$1'.

% Multiplication operators
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
mult_op -> div : '$1'.
mult_op -> rem : '$1'.
mult_op -> mult_op eol : '$1'.

% Comparision operators
comp_op -> '>'   : '$1'.
comp_op -> '<'   : '$1'.
comp_op -> '=='  : '$1'.
comp_op -> '!='  : '$1'.
comp_op -> '<='  : '$1'.
comp_op -> '>='  : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=!=' : '$1'.
comp_op -> comp_op eol : '$1'.

% and/or operators
and_op -> and : '$1'.
and_op -> andalso : '$1'.
and_op -> and_op eol : '$1'.

andand_op -> '&&' : '$1'.
andand_op -> '&&' eol : '$1'.

or_op -> or : '$1'.
or_op -> orelse : '$1'.
or_op -> or_op eol : '$1'.

oror_op -> '||' : '$1'.
oror_op -> '||' eol : '$1'.

% Dot break
dot_eol -> '.'     : '$1'.
dot_eol -> '.' eol : '$1'.

% Object/Module declaration
object_decl -> object constant break objmod_body 'end' : build_object(object, '$2', '$4', []).
module_decl -> module constant break objmod_body 'end' : build_object(module, '$2', '$4', []).

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

method_decl -> def method_name call_args_optional break body 'end' :
  build_def_method('$2', '$3', build_clause('$2', '$3', '$5')).

% Method names do not inherit from base_identifier, which includes object,
% module and _ as keywords, to avoid conflicts.
implicit_method_name -> identifier : '$1'.
implicit_method_name -> punctuated_identifier : '$1'.
implicit_method_name -> implicit_method_ops_identifier : { identifier, ?line('$1'), ?op('$1') }.

method_name -> implicit_method_name : '$1'.
method_name -> method_ops_identifier : { identifier, ?line('$1'), ?op('$1') }.

implicit_method_ops_identifier -> div : '$1'.
implicit_method_ops_identifier -> rem : '$1'.

method_ops_identifier -> '+'     : '$1'.
method_ops_identifier -> '-'     : '$1'.
method_ops_identifier -> '*'     : '$1'.
method_ops_identifier -> '/'     : '$1'.
method_ops_identifier -> '[' ']' : { '[]', ?line('$1') }.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(chars(Node), element(3, Node)).

% The following directive is needed for (significantly) faster compilation
% of the generated .erl file by the HiPE compiler.  Please do not remove.
% -compile([{hipe,[{regalloc,linear_scan}]}]).
build_identifier(Thing) ->
  { identifier, ?line(Thing), ?chars(Thing) }.

build_bracket_call(Expr, Args) ->
  build_method_call(Expr, { identifier, element(1, Args), '[]' }, element(2, Args)).

build_fun_call(Target, Args) ->
  { fun_call, ?line(Target), Target, Args }.

build_rescue_clause(Parent, Args, Body) ->
  Transformer = fun(X) -> build_clause(Parent, X, Body) end,
  lists:map(Transformer, Args).

build_clause(Parent, Args, Body) ->
  { clause, ?line(Parent), Args, [], Body }.

build_object(Kind, Name, Body, Parent) ->
  { Kind, ?line(Name), ?chars(Name), Parent, Body }.

build_fun(Stab, Clauses) ->
  { 'fun', ?line(Stab), { clauses, [Clauses] } }.

build_comp_op(Left, Op, Right) ->
  { comp_op, ?line(Op), ?op(Op), Left, Right }.

build_binary_op(Left, Op, Right) ->
  { binary_op, ?line(Op), ?op(Op), Left, Right }.

build_unary_op(Op, Value) ->
  { unary_op, ?line(Op), ?op(Op), Value }.

build_match(Left, Op, Right) ->
  { match, ?line(Op), Left, Right }.

build_def_method(Name, Args, Clauses) ->
  { def_method, ?line(Name), ?chars(Name), length(Args), [Clauses] }.

build_method_call(Expr, Name, Args) ->
  { method_call, ?line(Name), ?chars(Name), Args, Expr }.

build_local_call(Name, Args) ->
  { local_call, ?line(Name), ?chars(Name), Args }.

build_erlang_call(Op, Prefix, Suffix, Args) ->
  { erlang_call, ?line(Op), Prefix, Suffix, Args }.

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
  lists:flatten(lists:reverse(Acc));

build_string_list(Interpol, [{Kind, _, Chars}|T], Acc) ->
  build_string_list(Interpol, T, [convert_string(Interpol, Kind, Chars)|Acc]).

% If we have interpolated strings in the mix and this string is not
% an interpolated one, we need to convert it to the interpolated format.
convert_string(true, string, Chars) ->
  {s, Chars};

convert_string(_, _, Chars) ->
  Chars.

build_list(Line, Exprs) ->
  build_list(Line, Exprs, {nil, Line}).

build_list(Line, Exprs, Tail) ->
  { list, Line, Exprs, Tail }.

build_orddict_tuple(Key, Value) ->
  { tuple, ?line(Key), [Key, Value] }.

build_if_expr(Exprs) ->
  Line = ?line(hd(Exprs)),
  { 'if', Line, Exprs, [{nil, Line}] }.

build_if_expr(Exprs, Else) ->
  { 'if', ?line(hd(Exprs)), Exprs, element(3, Else) }.

build_bin(Line, Elements) ->
  { bin, Line, Elements }.

build_bin_element(Expr, Type, Specifier) ->
  { bin_element, ?line(Expr), Expr, Type, Specifier }.