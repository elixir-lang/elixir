% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar expr_list
  expr call_expr max_expr base_expr block_expr curly_expr
  break comma_separator
  add_op mult_op unary_op match_op
  open_paren close_paren
  open_bracket close_bracket
  open_curly close_curly
  raw_call_args call_args call_args_parens call_args_no_parens operator_call
  base_orddict kv_comma kv_eol do_block curly_block
  list list_args
  dot_op dot_identifier dot_do_identifier dot_paren_identifier dot_punctuated_identifier
  ref_op ref_identifier
  var tuple
  .

Terminals
  'do' 'end'
  ref identifier do_identifier kv_identifier punctuated_identifier paren_identifier
  number signed_number atom
  '+' '-' '*' '/' '=' call_op special_op dot_call_op
  '(' ')' eol ';' ',' '[' ']' '|' '{' '}' '.' '::'
  .

Rootsymbol grammar.

% Solve nested call_args conflicts

% TODO: Test p {} and p { foo } becomes a block
Nonassoc  10 '{'.
Right     20 match_op.
Left      40 ','.
Left     110 add_op.
Left     120 mult_op.
Nonassoc 140 unary_op.
Nonassoc 150 call_op.
Nonassoc 150 dot_call_op.
Nonassoc 160 var.
Left     170 dot_op.
Right    180 ref_op.
Nonassoc 190 special_op.

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
expr -> special_op expr : build_special_op('$1', '$2').
expr -> block_expr : '$1'.

block_expr -> dot_paren_identifier call_args_parens do_block : build_identifier('$1', '$2', '$3').
block_expr -> dot_punctuated_identifier call_args_no_parens do_block : build_identifier('$1', '$2', '$3').
block_expr -> dot_punctuated_identifier do_block : build_identifier('$1', '$2').
block_expr -> dot_identifier call_args_no_parens do_block : build_identifier('$1', '$2', '$3').
block_expr -> dot_do_identifier do_block : build_identifier('$1', '$2').
block_expr -> curly_expr : '$1'.

curly_expr -> dot_paren_identifier call_args_parens curly_block : build_identifier('$1', '$2', '$3').
curly_expr -> dot_punctuated_identifier curly_block : build_identifier('$1', '$2').
curly_expr -> dot_identifier curly_block : build_identifier('$1', '$2').
curly_expr -> call_expr : '$1'.

call_expr -> operator_call : '$1'.
call_expr -> dot_paren_identifier call_args_parens : build_identifier('$1', '$2').
call_expr -> dot_punctuated_identifier call_args_no_parens : build_identifier('$1', '$2').
call_expr -> dot_identifier call_args_no_parens : build_identifier('$1', '$2').
call_expr -> dot_punctuated_identifier : build_identifier('$1', []).
call_expr -> expr dot_call_op call_args_parens : build_identifier({ '.', ?line('$2'), ['$1']}, '$3').
call_expr -> max_expr : '$1'.

max_expr -> base_expr : '$1'.
max_expr -> '(' expr_list ')' : build_block('$2').

base_expr -> number : ?exprs('$1').
base_expr -> signed_number : { element(4, '$1'), ?line('$1'), ?exprs('$1') }.
base_expr -> atom : ?exprs('$1').
base_expr -> var : build_identifier('$1', false).
base_expr -> list : '$1'.
base_expr -> tuple : '$1'.
base_expr -> ref_identifier : '$1'.

%% Helpers

var -> dot_identifier : '$1'.
var -> dot_do_identifier : '$1'.

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

% Ref operator

ref_op -> '::' : '$1'.
ref_op -> '::' eol : '$1'.

ref_identifier -> ref : '$1'.
ref_identifier -> ref ref_op ref_identifier : { '::', ?line('$2'), ['$1', '$3'] }.

% Dot operator

dot_op -> '.' : '$1'.
dot_op -> '.' eol : '$1'.

dot_identifier -> identifier : '$1'.
dot_identifier -> expr dot_op identifier : { '.', ?line('$2'), ['$1', '$3'] }.

dot_identifier -> dot_call_op call_args_parens : build_identifier('$1', '$2').
dot_identifier -> dot_call_op call_args_parens curly_block : build_identifier('$1', '$2', '$3').
dot_identifier -> dot_call_op call_args_parens do_block : build_identifier('$1', '$2', '$3').

dot_do_identifier -> do_identifier : '$1'.
dot_do_identifier -> expr dot_op do_identifier : { '.', ?line('$2'), ['$1', '$3'] }.

dot_paren_identifier -> paren_identifier : '$1'.
dot_paren_identifier -> expr dot_op paren_identifier : { '.', ?line('$2'), ['$1', '$3'] }.

dot_punctuated_identifier -> punctuated_identifier : '$1'.
dot_punctuated_identifier -> expr dot_op punctuated_identifier : { '.', ?line('$2'), ['$1', '$3'] }.

% Function calls

operator_call -> call_op call_args_parens : build_identifier('$1', '$2').
operator_call -> call_op call_args_parens curly_block : build_identifier('$1', '$2', '$3').
operator_call -> call_op call_args_parens do_block : build_identifier('$1', '$2', '$3').

raw_call_args -> expr : ['$1'].
raw_call_args -> base_orddict : ['$1'].
raw_call_args -> expr comma_separator raw_call_args : ['$1'|'$3'].

call_args -> raw_call_args : build_args('$1').

call_args_no_parens -> expr : ['$1'].
call_args_no_parens -> base_orddict : ['$1'].
call_args_no_parens -> expr comma_separator raw_call_args : ['$1'|'$3'].

call_args_parens -> open_paren ')' : [].
call_args_parens -> open_paren raw_call_args close_paren : '$2'.

% KV and orddict

base_orddict -> kv_comma : { '[:]', ?line(hd('$1')), sort_kv('$1') }.

kv_comma -> kv_eol expr : [{'{}',?line('$1'),[?exprs('$1'),'$2']}].
kv_comma -> kv_eol expr comma_separator kv_comma : [{'{}',?line('$1'),[?exprs('$1'),'$2']}|'$4'].

kv_eol -> kv_identifier : '$1'.
kv_eol -> kv_identifier eol : '$1'.

do_block -> 'do' 'end'           : build_kv_block('$1', []).
do_block -> 'do' expr_list 'end' : build_kv_block('$1', '$2').

curly_block -> '{' '}'           : build_kv_block('$1', []).
curly_block -> '{' expr_list '}' : build_kv_block('$1', '$2').

% Lists

% list_args is an special case of call_args because kv_comma
% does not generate a new array, it actually becomes tuples
% in the existing one. Except list, everything should depend
% on call_args.

list_args -> kv_comma : sort_kv('$1').
list_args -> expr : ['$1'].
list_args -> expr comma_separator call_args : ['$1'|'$3'].

list -> open_bracket ']' : build_list(?line('$1'), []).
list -> open_bracket list_args close_bracket : build_list(?line('$1'), '$2').
list -> open_bracket list_args '|' expr close_bracket : build_list(?line('$1'), '$2', ?line('$3'), '$4').

% Tuple

tuple -> open_curly '}' : { '{}', ?line('$1'), [] }.
tuple -> open_curly call_args close_curly :  { '{}', ?line('$1'), build_args('$2') }.

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

build_special_op(Op, Expr) ->
  { ?exprs(Op), ?line(Op), [Expr] }.

build_kv_block(Delimiter, Contents) ->
  Line = ?line(Delimiter),
  {'[:]', Line, [{'{}', Line, ['do',build_block(Contents)]}] }.

build_block([])     -> nil;
build_block([Expr]) -> Expr;
build_block(Exprs)  -> { block, 0, Exprs }.

build_list(Line, Args) -> Args.

build_list(Line, Args, Pipe, Tail) ->
  [Last|Rest] = lists:reverse(Args),
  Final = [{'|',Pipe,[Last,Tail]}|Rest],
  lists:reverse(Final).

% Build args by transforming [:] into the final form []

build_args(Args) -> lists:map(fun build_arg/1, Args).
build_arg({ '[:]', Line, Args }) -> Args;
build_arg(Else) -> Else.

% Build identifiers. Those helpers are responsible to:
% + Merge kv args and kv blocks arguments
% + Handle dot operators and transform them in the proper function call

build_identifier(Expr, Args, Block) ->
  build_identifier(Expr, merge_kv(Args, Block)).

build_identifier({ '.', DotLine, [Expr, { _, Line, Identifier }] }, Args) ->
  build_identifier({ '.', DotLine, [Expr, Identifier] }, Args);

build_identifier({ '.', Line, _ } = Dot, Args) ->
  FArgs = case Args of
    false -> [];
    _ -> Args
  end,
  { Dot, Line, build_args(FArgs) };

build_identifier({ _, Line, Identifier }, false) ->
  { Identifier, Line, false };

build_identifier({ _, Line, Identifier }, Args) ->
  { Identifier, Line, build_args(Args) }.

% Sort key-values

sort_kv(List) -> lists:sort(fun sort_kv/2, List).
sort_kv({ '{}', _, [A,_] }, { '{}', _, [B,_] }) -> A =< B.

% Merge key-value pairs from args and blocks

merge_kv([], Block)   -> [Block];
merge_kv(Args, Block) ->
  { Reverse, Last } = last(Args, []),
  case is_kv(Last) of
    true  ->
      { '[:]', Line, Left } = Last,
      { '[:]', _, Right }  = Block,
      KV = { '[:]', Line, sort_kv(Left ++ Right) },
      lists:reverse([KV|Reverse]);
    false ->
      lists:reverse([Block,Last|Reverse])
  end.

last([L], Acc)   -> { Acc, L };
last([H|T], Acc) -> last(T, [H|Acc]).

is_kv({'[:]', _, Args}) -> lists:all(fun is_kv_tuple/1, Args);
is_kv(_) -> false.

is_kv_tuple({ '{}', _, [Key, _ ] }) when is_atom(Key) -> true;
is_kv_tuple(_) -> false.