% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar expr_list
  expr block_expr curly_expr call_expr max_expr base_expr
  matched_expr matched_op_expr unmatched_expr unmatched_op_expr
  comma_separator kv_eol
  match_op add_op mult_op unary_op addadd_op multmult_op pipe_op arrow_op
  andand_op oror_op andalso_op orelse_op and_op or_op comp_expr_op
  open_paren close_paren
  open_bracket close_bracket
  open_curly close_curly
  comma_expr call_args_comma_expr call_args call_args_parens
  matched_comma_expr call_args_no_parens
  kv_comma base_orddict
  matched_kv_comma matched_base_orddict
  do_eol end_eol kv_list do_block curly_block
  dot_op dot_identifier dot_do_identifier dot_paren_identifier dot_punctuated_identifier dot_call_expr
  ref_op ref_identifier
  var tuple list
  .

Terminals
  'do' 'end'
  identifier do_identifier kv_identifier punctuated_identifier paren_identifier
  number signed_number atom ref string
  call_op special_op dot_call_op comp_op
  'not' 'and' 'or' 'xor' 'andalso' 'orelse'
  '=' '+' '-' '*' '/' '++' '--' '**' '//' '<-'
  '(' ')' eol ',' '[' ']' '|' '{' '}' '.' '::'
  '&&' '||' '!'
  .

Rootsymbol grammar.

Right     10 match_op.
Left      20 do.
Left      30 ','.  % Solve nested call_args conflicts
Left      40 pipe_op.
Right     50 arrow_op.
Left      60 oror_op.
Left      70 andand_op.
Left      80 orelse_op.
Left      90 andalso_op.
Left     100 or_op.
Left     110 and_op.
Left     150 comp_expr_op.
Left     160 add_op.
Left     170 mult_op.
Right    180 addadd_op.
Right    190 multmult_op.
Nonassoc 250 unary_op.
Nonassoc 260 call_op.
Nonassoc 260 dot_call_op.
Nonassoc 270 var.
Left     280 dot_op.
Right    290 ref_op.
Nonassoc 300 special_op.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> expr_list : lists:reverse('$1').
grammar -> eol expr_list : lists:reverse('$2').
grammar -> expr_list eol : lists:reverse('$1').
grammar -> eol expr_list eol : lists:reverse('$2').
grammar -> '$empty' : [nil].

% Note expressions are on reverse order
expr_list -> expr : ['$1'].
expr_list -> expr_list eol expr : ['$3'|'$1'].

expr -> matched_expr : '$1'.
expr -> unmatched_expr : '$1'.

matched_expr   -> matched_op_expr   : '$1'.
unmatched_expr -> unmatched_op_expr : '$1'.

unmatched_op_expr -> expr match_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr add_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr mult_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr addadd_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr multmult_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr andand_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr oror_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr andalso_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr orelse_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr and_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr or_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr pipe_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr arrow_op expr : build_op('$2', '$1', '$3').
unmatched_op_expr -> expr comp_expr_op expr : build_expr_op('$2', '$1', '$3').
unmatched_op_expr -> unary_op expr : build_unary_op('$1', '$2').
unmatched_op_expr -> special_op expr : build_special_op('$1', '$2').
unmatched_op_expr -> block_expr : '$1'.

matched_op_expr -> matched_expr match_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr add_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr mult_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr addadd_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr multmult_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr andand_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr oror_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr andalso_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr orelse_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr and_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr or_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr pipe_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr arrow_op matched_expr : build_op('$2', '$1', '$3').
matched_op_expr -> matched_expr comp_expr_op matched_expr : build_expr_op('$2', '$1', '$3').
matched_op_expr -> unary_op matched_expr : build_unary_op('$1', '$2').
matched_op_expr -> special_op matched_expr : build_special_op('$1', '$2').
matched_op_expr -> curly_expr : '$1'.

block_expr -> dot_paren_identifier call_args_parens do_block : build_identifier('$1', '$2', '$3').
block_expr -> dot_punctuated_identifier call_args_no_parens do_block : build_identifier('$1', '$2', '$3').
block_expr -> dot_identifier call_args_no_parens do_block : build_identifier('$1', '$2', '$3').
block_expr -> dot_do_identifier do_block : build_identifier('$1', [], '$2').
block_expr -> dot_call_expr call_args_parens do_block : build_identifier('$1', '$2', '$3').
block_expr -> call_op call_args_parens do_block : build_identifier('$1', '$2', '$3').

curly_expr -> dot_paren_identifier call_args_parens curly_block : build_identifier('$1', '$2', '$3').
curly_expr -> dot_call_expr call_args_parens curly_block : build_identifier('$1', '$2', '$3').
curly_expr -> call_op call_args_parens curly_block : build_identifier('$1', '$2', '$3').
curly_expr -> call_expr : '$1'.

call_expr -> call_op call_args_parens : build_identifier('$1', '$2').
call_expr -> dot_paren_identifier call_args_parens : build_identifier('$1', '$2').
call_expr -> dot_punctuated_identifier call_args_no_parens : build_maybe_curly_identifier('$1', '$2').
call_expr -> dot_identifier call_args_no_parens : build_maybe_curly_identifier('$1', '$2').
call_expr -> dot_punctuated_identifier : build_identifier('$1', []).
call_expr -> dot_do_identifier : build_identifier('$1', false).
call_expr -> dot_call_expr call_args_parens : build_identifier('$1', '$2').
call_expr -> max_expr : '$1'.

max_expr -> base_expr : '$1'.
max_expr -> open_paren ')' : build_block([]).
max_expr -> open_paren expr_list close_paren : build_block('$2').

base_expr -> number : ?exprs('$1').
base_expr -> signed_number : { element(4, '$1'), ?line('$1'), ?exprs('$1') }.
base_expr -> atom : build_atom('$1').
base_expr -> var : build_identifier('$1', false).
base_expr -> list : '$1'.
base_expr -> tuple : '$1'.
base_expr -> ref_identifier : '$1'.
base_expr -> string : build_string('$1').

%% Helpers

var -> dot_identifier : '$1'.

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
add_op -> '+' eol : '$1'.
add_op -> '-' eol : '$1'.

mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
mult_op -> '*' eol : '$1'.
mult_op -> '/' eol : '$1'.

addadd_op -> '++' : '$1'.
addadd_op -> '--' : '$1'.
addadd_op -> '++' eol : '$1'.
addadd_op -> '--' eol : '$1'.

multmult_op -> '**' : '$1'.
multmult_op -> '//' : '$1'.
multmult_op -> '**' eol : '$1'.
multmult_op -> '//' eol : '$1'.

unary_op -> '+' : '$1'.
unary_op -> '+' eol : '$1'.
unary_op -> '-' : '$1'.
unary_op -> '-' eol : '$1'.
unary_op -> '!' : '$1'.
unary_op -> '!' eol : '$1'.
unary_op -> 'not' : '$1'.
unary_op -> 'not' eol : '$1'.

match_op -> '=' : '$1'.
match_op -> '=' eol : '$1'.

andand_op -> '&&' : '$1'.
andand_op -> '&&' eol : '$1'.

oror_op -> '||' : '$1'.
oror_op -> '||' eol : '$1'.

andalso_op -> 'andalso' : '$1'.
andalso_op -> 'andalso' eol : '$1'.

orelse_op -> 'orelse' : '$1'.
orelse_op -> 'orelse' eol : '$1'.

and_op -> 'and' : '$1'.
and_op -> 'and' eol : '$1'.

or_op -> 'or' : '$1'.
or_op -> 'or' eol : '$1'.
or_op -> 'xor' : '$1'.
or_op -> 'xor' eol : '$1'.

pipe_op -> '|' : '$1'.
pipe_op -> '|' eol : '$1'.

arrow_op -> '<-' : '$1'.
arrow_op -> '<-' eol : '$1'.

comp_expr_op -> comp_op : '$1'.
comp_expr_op -> comp_op eol : '$1'.

% Ref operator

ref_op -> '::' : '$1'.
ref_op -> '::' eol : '$1'.

ref_identifier -> ref : '$1'.
ref_identifier -> ref ref_op ref_identifier : { '::', ?line('$2'), ['$1', '$3'] }.

% Dot operator

dot_op -> '.' : '$1'.
dot_op -> '.' eol : '$1'.

dot_identifier -> identifier : '$1'.
dot_identifier -> dot_call_op call_args_parens : build_identifier('$1', '$2'). % .(args)
dot_identifier -> matched_expr dot_op identifier : { '.', ?line('$2'), ['$1', '$3'] }.

dot_do_identifier -> do_identifier : '$1'.
dot_do_identifier -> matched_expr dot_op do_identifier : { '.', ?line('$2'), ['$1', '$3'] }.

dot_paren_identifier -> paren_identifier : '$1'.
dot_paren_identifier -> matched_expr dot_op paren_identifier : { '.', ?line('$2'), ['$1', '$3'] }.

dot_punctuated_identifier -> punctuated_identifier : '$1'.
dot_punctuated_identifier -> matched_expr dot_op punctuated_identifier : { '.', ?line('$2'), ['$1', '$3'] }.

dot_call_expr -> matched_expr dot_call_op : { '.', ?line('$2'), ['$1'] }.

% Function calls

matched_comma_expr -> matched_expr : ['$1'].
matched_comma_expr -> matched_comma_expr comma_separator matched_expr : ['$3'|'$1'].

call_args_no_parens -> matched_comma_expr : lists:reverse('$1').
call_args_no_parens -> matched_base_orddict : ['$1'].
call_args_no_parens -> matched_comma_expr comma_separator matched_base_orddict : lists:reverse(['$3'|'$1']).

comma_expr -> expr : ['$1'].
comma_expr -> comma_expr comma_separator expr : ['$3'|'$1'].

call_args_comma_expr -> comma_expr : lists:reverse('$1').
call_args_comma_expr -> base_orddict : ['$1'].
call_args_comma_expr -> comma_expr comma_separator base_orddict : lists:reverse(['$3'|'$1']).

call_args_parens -> open_paren ')' : [].
call_args_parens -> open_paren call_args_comma_expr close_paren : '$2'.

call_args -> call_args_comma_expr : build_args('$1').

% KV and orddict

kv_eol -> kv_identifier : '$1'.
kv_eol -> kv_identifier eol : '$1'.

kv_comma -> kv_eol expr : [{?exprs('$1'),'$2'}].
kv_comma -> kv_eol expr comma_separator kv_comma : [{?exprs('$1'), '$2'}|'$4'].

matched_kv_comma -> kv_eol matched_expr : [{?exprs('$1'),'$2'}].
matched_kv_comma -> kv_eol matched_expr comma_separator kv_comma : [{?exprs('$1'), '$2'}|'$4'].

base_orddict -> kv_comma : { '[:]', ?line(hd('$1')), '$1' }.
matched_base_orddict -> matched_kv_comma : { '[:]', ?line(hd('$1')), '$1' }.

% KV blocks

do_eol -> 'do' : '$1'.
do_eol -> 'do' eol : '$1'.

end_eol -> 'end' : '$1'.
end_eol -> eol 'end' : '$2'.

kv_list -> kv_eol expr_list eol : [{?exprs('$1'),'$2'}].
kv_list -> kv_eol expr_list eol kv_list : [{?exprs('$1'), '$2'}|'$4'].

do_block -> do_eol 'end'                         : build_kv_block('$1', [], []).
do_block -> do 'eol' kv_list 'end'               : build_kv_block('$1', [], '$3').
do_block -> do_eol expr_list end_eol             : build_kv_block('$1', '$2', []).
do_block -> do_eol expr_list 'eol' kv_list 'end' : build_kv_block('$1', '$2', '$4').

curly_block -> open_curly '}' : build_kv_block('$1', [], []).
curly_block -> open_curly expr_list close_curly : build_kv_block('$1', '$2', []).

% Lists

list -> open_bracket ']' : [].
list -> open_bracket kv_comma close_bracket : sort_kv('$2').
list -> open_bracket expr close_bracket : ['$2'].
list -> open_bracket expr comma_separator call_args close_bracket : ['$2'|'$4'].

% Tuple

tuple -> open_curly '}' : { '{}', ?line('$1'), [] }.
tuple -> open_curly call_args close_curly :  { '{}', ?line('$1'), '$2' }.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(exprs(Node), element(3, Node)).

% The following directive is needed for (significantly) faster compilation
% of the generated .erl file by the HiPE compiler. Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

%% Operators

build_op(Op, Left, Right) ->
  { ?op(Op), ?line(Op), [Left, Right] }.

build_expr_op(Op, Left, Right) ->
  { ?exprs(Op), ?line(Op), [Left, Right] }.

build_unary_op(Op, Expr) ->
  { ?op(Op), ?line(Op), [Expr] }.

build_special_op(Op, Expr) ->
  { ?exprs(Op), ?line(Op), [Expr] }.

%% Blocks

% Handle args that expects blocks of code
build_block([])                            -> nil;
build_block([Expr]) when not is_list(Expr) -> Expr;
build_block(Exprs)                         -> { block, 0, lists:reverse(Exprs) }.

% Handle key value blocks
build_kv_block(Delimiter, Contents, IncompleteList) ->
  Line = ?line(Delimiter),
  List = [{do,Contents}|IncompleteList],
  {'[:]', Line, build_kv_block(List, [])}.

% Build key value blocks by joining all clauses together
build_kv_block([{Key,Value}|T], Acc) ->
  New = build_block(Value),
  NewAcc = orddict:update(Key, fun(Old) -> [New|Old] end, [New], Acc),
  build_kv_block(T, NewAcc);

build_kv_block([], Acc) ->
  lists:reverse([build_kv_block(X) || X <- Acc]).

build_kv_block({Key,[Value]}) -> { Key, Value };
build_kv_block({Key,Else})    -> { Key, lists:reverse(Else) }.

%% Args
% Build args by transforming [:] into the final form []
% and properly sorting the items.

build_args(Args) -> lists:map(fun build_arg/1, Args).
build_arg({ '[:]', Line, Args }) -> sort_kv(Args);
build_arg(Else) -> Else.

%% Identifiers
% Those helpers are responsible to:
%
%   + Merge kv args and kv blocks arguments
%   + Handle dot operators and transform them in the proper function call
%   + Handle ambiguitity between p { }, p { 1 }, p { 1, 2 }

build_maybe_curly_identifier(Expr, [{ '{}', Line, []}]) ->
  build_identifier(Expr, [[{do,nil}]]);

build_maybe_curly_identifier(Expr, [{ '{}', Line, [Arg]}]) ->
  build_identifier(Expr, [[{do,Arg}]]);

build_maybe_curly_identifier(Expr, Args) ->
  build_identifier(Expr, Args).

build_identifier(Expr, Args, Block) ->
  build_identifier(Expr, merge_kv(Args, Block)).

build_identifier({ '.', DotLine, [Expr, { Kind, _, Identifier }] }, Args) when
  Kind == identifier; Kind == punctuated_identifier;
  Kind == paren_identifier; Kind == do_identifier ->
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

%% Interpolation aware

build_string({ string, _Line, [H] }) when is_list(H) -> list_to_binary(H);
build_string({ string, Line, Args }) -> { bitstr, Line, Args }.

build_atom({ atom, _Line, [H] }) when is_atom(H) -> H;
build_atom({ atom, _Line, [H] }) when is_list(H) -> list_to_atom(H);
build_atom({ atom, Line, Args }) -> { binary_to_atom, Line, [{ bitstr, Line, Args}, utf8] }.

%% KV Helpers
% Merge key-value pairs from args and blocks

merge_kv([], Block)   -> [Block];
merge_kv(Args, Block) ->
  { Reverse, Last } = last(Args, []),
  case is_kv(Last) of
    true  ->
      { '[:]', Line, Left } = Last,
      { '[:]', _, Right }  = Block,
      KV = { '[:]', Line, Left ++ Right },
      lists:reverse([KV|Reverse]);
    false ->
      lists:reverse([Block,Last|Reverse])
  end.

last([L], Acc)   -> { Acc, L };
last([H|T], Acc) -> last(T, [H|Acc]).

is_kv({'[:]', _, Args}) -> lists:all(fun is_kv_tuple/1, Args);
is_kv(_) -> false.

is_kv_tuple({ Key, _ }) when is_atom(Key) -> true;
is_kv_tuple(_) -> false.

sort_kv(List) -> lists:sort(fun sort_kv/2, List).
sort_kv({ A, _ }, { B, _ }) -> A =< B.