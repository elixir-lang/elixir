% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar expr_list
  expr call_expr max_expr base_expr block_expr
  break comma_separator
  add_op mult_op unary_op match_op
  open_paren close_paren
  open_bracket close_bracket
  open_curly close_curly
  call_args call_args_parens call_args_no_parens
  operator_call
  base_orddict kv_comma kv_eol do_block
  list list_args
  var
  .

Terminals
  'do' 'end'
  identifier do_identifier kv_identifier punctuated_identifier paren_identifier
  number signed_number atom
  '+' '-' '*' '/' '=' call_op
  '(' ')' eol ';' ',' '[' ']' '|' '{' '}'
  .

Rootsymbol grammar.

% Solve nested call_args conflicts

Right     20 match_op.
Left      40 ','.
Left     110 add_op.
Left     120 mult_op.
Nonassoc 140 unary_op.
Nonassoc 150 call_op.
Nonassoc 160 var.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> expr_list : '$1'.
grammar -> '$empty' : [nil].

% List of expressions delimited by break
expr_list -> eol : [].
expr_list -> block_expr : ['$1'].
expr_list -> block_expr break : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> block_expr break expr_list : ['$1'|'$3'].

block_expr -> paren_identifier call_args_parens do_block : build_identifier('$1', '$2' ++ '$3').
block_expr -> punctuated_identifier call_args_no_parens do_block : build_identifier('$1', '$2' ++ '$3').
block_expr -> punctuated_identifier do_block : build_identifier('$1', '$2').
block_expr -> identifier call_args_no_parens do_block : build_identifier('$1', '$2' ++ '$3').
block_expr -> do_identifier do_block : build_identifier('$1', '$2').
block_expr -> expr : '$1'.

expr -> expr match_op expr : build_op('$2', '$1', '$3').
expr -> expr add_op expr : build_op('$2', '$1', '$3').
expr -> expr mult_op expr : build_op('$2', '$1', '$3').
expr -> unary_op expr : build_unary_op('$1', '$2').
expr -> call_expr : '$1'.

call_expr -> operator_call : '$1'.
call_expr -> paren_identifier call_args_parens : build_identifier('$1', '$2').
call_expr -> punctuated_identifier call_args_no_parens : build_identifier('$1', '$2').
call_expr -> identifier call_args_no_parens : build_identifier('$1', '$2').
call_expr -> punctuated_identifier : build_identifier('$1', []).
call_expr -> max_expr : '$1'.

max_expr -> base_expr : '$1'.
max_expr -> '(' grammar ')' : '$2'.

base_expr -> number : ?exprs('$1').
base_expr -> signed_number : { element(4, '$1'), ?line('$1'), ?exprs('$1') }.
base_expr -> atom : ?exprs('$1').
base_expr -> var : build_identifier('$1', false).
base_expr -> do_identifier : build_identifier('$1', false).
base_expr -> list : '$1'.

%% Helpers

var -> identifier : '$1'.

break -> eol : '$1'.
break -> ';' : { eol, ?line('$1') }.

comma_separator -> ','     : '$1'.
comma_separator -> ',' eol : '$1'.

open_paren -> '('      : '$1'.
open_paren -> '(' eol  : '$1'.
close_paren -> ')'     : '$1'.
close_paren -> eol ')' : '$2'.

open_bracket  -> '['     : '$1'.
open_bracket  -> '[' eol : '$1'.
close_bracket -> ']'     : '$1'.
close_bracket -> eol ']' : '$2'.

open_curly  -> '{'     : '$1'.
open_curly  -> '{' eol : '$1'.
close_curly -> '}'     : '$1'.
close_curly -> eol '}' : '$2'.

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

call_args_no_parens -> expr : ['$1'].
call_args_no_parens -> base_orddict : ['$1'].
call_args_no_parens -> expr comma_separator call_args : ['$1'|'$3'].

call_args -> block_expr : ['$1'].
call_args -> base_orddict : ['$1'].
call_args -> block_expr comma_separator call_args : ['$1'|'$3'].

call_args_parens -> open_paren ')' : [].
call_args_parens -> open_paren call_args close_paren : '$2'.

% KV and orddict

base_orddict -> kv_comma : { '[]', ?line(hd('$1')), '$1' }.

kv_comma -> kv_eol expr : [{'{}',?line('$1'),[?exprs('$1'),'$2']}].
kv_comma -> kv_eol expr comma_separator kv_comma : [{'{}',?line('$1'),[?exprs('$1'),'$2']}|'$4'].

kv_eol -> kv_identifier : '$1'.
kv_eol -> kv_identifier eol : '$1'.

do_block -> 'do' expr_list 'end' : [{'[]', ?line('$1'), [{'{}', ?line('$1'), ['do','$2']}] }].

% Lists

% list_args is an special case of call_args because kv_comma
% does not generate a new array, it actually becomes tuples
% in the existing one. Except list, everything should depend
% on call_args.

list_args -> kv_comma : '$1'.
list_args -> block_expr : ['$1'].
list_args -> block_expr comma_separator call_args : ['$1'|'$3'].

list -> open_bracket ']' : build_list(?line('$1'), []).
list -> open_bracket list_args close_bracket : build_list(?line('$1'), '$2').
list -> open_bracket list_args '|' expr close_bracket : build_list(?line('$1'), '$2', ?line('$3'), '$4').

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(exprs(Node), element(3, Node)).

% The following directive is needed for (significantly) faster compilation
% of the generated .erl file by the HiPE compiler. Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

build_op(Op, Left, Right) ->
  { ?op(Op), ?line(Op), [Left, Right] }.

build_unary_op(Op, Expr) ->
  { ?op(Op), ?line(Op), [Expr] }.

build_call_op(Op, Args) ->
  { ?exprs(Op), ?line(Op), Args };

build_call_op(Op, [Expr]) ->
  { ?exprs(Op), ?line(Op), Expr }.

build_identifier({ _, Line, Identifier }, Args) ->
  { Identifier, Line, Args }.

build_list(Line, Args) ->
  { '[]', Line, Args }.

build_list(Line, Args, Pipe, Tail) ->
  [Last|Rest] = lists:reverse(Args),
  Final = [{'|',Pipe,[Last,Tail]}|Rest],
  { '[]', Line, lists:reverse(Final) }.