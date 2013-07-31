Nonterminals
  grammar expr_list
  expr container_expr block_expr no_parens_expr identifier_expr max_expr
  bracket_expr bracket_at_expr base_expr matched_expr unmatched_expr
  op_expr matched_op_expr no_parens_op_expr
  comp_op_eol at_op_eol unary_op_eol and_op_eol or_op_eol tail_op_eol
  add_op_eol mult_op_eol exp_op_eol two_op_eol type_op_eol stab_op_eol
  arrow_op_eol range_op_eol than_op_eol default_op_eol match_op_eol
  when_op_eol in_op_eol inc_op_eol
  open_paren close_paren empty_paren
  open_bracket close_bracket
  open_curly close_curly
  open_bit close_bit
  container_comma_expr_base container_comma_expr container_arg container_args
  call_args_parens_comma_expr call_args_parens parens_call
  call_args_no_parens_one call_args_no_parens_expr call_args_no_parens_comma_expr
  call_args_no_parens_all call_args_no_parens_many call_args_no_parens_many_strict
  stab stab_eol stab_expr stab_maybe_expr stab_parens_many
  kw_eol kw_expr kw_comma kw
  call_args_no_parens_kw_expr call_args_no_parens_kw_comma call_args_no_parens_kw
  dot_op dot_alias dot_identifier dot_op_identifier dot_do_identifier
  dot_paren_identifier dot_punctuated_identifier dot_bracket_identifier
  var list bracket_access bit_string tuple
  do_block fn_eol do_eol end_eol block_eol block_item block_list
  .

Terminals
  identifier kw_identifier punctuated_identifier
  bracket_identifier paren_identifier do_identifier block_identifier
  fn 'end' aliases
  number signed_number atom bin_string list_string sigil
  dot_call_op op_identifier
  comp_op at_op unary_op and_op or_op arrow_op match_op
  range_op in_op inc_op when_op than_op default_op tail_op
  dual_op add_op mult_op exp_op two_op type_op stab_op
  'true' 'false' 'nil' 'do' eol ',' '.' '&'
  '(' ')' '[' ']' '{' '}' '<<' '>>'
  .

Rootsymbol grammar.

%% There are two shift/reduce conflicts coming from call_args_parens.
Expect 2.

Left       5 do.
Right     10 stab_op_eol.     %% ->
Left      20 ','.
Right     30 type_op_eol.     %% ::
Right     40 when_op_eol.     %% when
Left      50 inc_op_eol.      %% inlist, inbits
Right     60 default_op_eol.  %% //
Left      70 tail_op_eol.     %% |
Right     80 match_op_eol.    %% =
Left     130 or_op_eol.       %% ||, |||, or, xor
Left     140 and_op_eol.      %% &&, &&&, and
Left     150 comp_op_eol.     %% <, >, <=, >=, ==, !=, =~, ===, !==
Right    160 arrow_op_eol.    %% < (op), (op) > (e.g <-, |>, <<<, >>>)
Left     170 in_op_eol.       %% in
Left     200 range_op_eol.    %% ..
Left     210 add_op_eol.      %% + (op), - (op)
Left     220 mult_op_eol.     %% * (op), / (op)
Right    230 than_op_eol.     %% < (op) > (e.g <>)
Right    240 two_op_eol.      %% ++, --, **
Left     250 exp_op_eol.      %% ^ (op) (e.g ^^^)
Nonassoc 300 unary_op_eol.    %% +, -, !, ^, not, &, ~~~
Left     310 dot_call_op.
Left     310 dot_op.          %% .
Nonassoc 320 at_op_eol.       %% @ (op)
Nonassoc 330 var.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> eol : [nil].
grammar -> expr_list : lists:reverse('$1').
grammar -> eol expr_list : lists:reverse('$2').
grammar -> expr_list eol : lists:reverse('$1').
grammar -> eol expr_list eol : lists:reverse('$2').
grammar -> '$empty' : [nil].

% Note expressions are on reverse order
expr_list -> expr : ['$1'].
expr_list -> expr_list eol expr : ['$3'|'$1'].

expr -> empty_paren : nil.
expr -> matched_expr : '$1'.
expr -> no_parens_expr : '$1'.
expr -> unmatched_expr : '$1'.

matched_expr -> matched_expr matched_op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
matched_expr -> matched_expr no_parens_op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
matched_expr -> unary_op_eol matched_expr : build_unary_op('$1', '$2').
matched_expr -> unary_op_eol no_parens_expr : build_unary_op('$1', '$2').
matched_expr -> at_op_eol matched_expr : build_unary_op('$1', '$2').
matched_expr -> at_op_eol no_parens_expr : build_unary_op('$1', '$2').
matched_expr -> bracket_at_expr : '$1'.
matched_expr -> identifier_expr : '$1'.

no_parens_expr -> dot_punctuated_identifier call_args_no_parens_many_strict : build_identifier('$1', '$2').
no_parens_expr -> dot_op_identifier call_args_no_parens_many_strict : build_identifier('$1', '$2').
no_parens_expr -> dot_identifier call_args_no_parens_many_strict : build_identifier('$1', '$2').

unmatched_expr -> empty_paren op_expr : build_op(element(1, '$2'), nil, element(2, '$2')).
unmatched_expr -> matched_expr op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
unmatched_expr -> unmatched_expr op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
unmatched_expr -> unary_op_eol expr : build_unary_op('$1', '$2').
unmatched_expr -> at_op_eol expr : build_unary_op('$1', '$2').
unmatched_expr -> block_expr : '$1'.

block_expr -> parens_call call_args_parens do_block : build_identifier('$1', '$2' ++ '$3').
block_expr -> parens_call call_args_parens call_args_parens do_block : build_nested_parens('$1', '$2', '$3' ++ '$4').
block_expr -> dot_do_identifier do_block : build_identifier('$1', '$2').
block_expr -> dot_punctuated_identifier call_args_no_parens_all do_block : build_identifier('$1', '$2' ++ '$3').
block_expr -> dot_identifier call_args_no_parens_all do_block : build_identifier('$1', '$2' ++ '$3').

op_expr -> match_op_eol expr : { '$1', '$2' }.
op_expr -> add_op_eol expr : { '$1', '$2' }.
op_expr -> mult_op_eol expr : { '$1', '$2' }.
op_expr -> exp_op_eol expr : { '$1', '$2' }.
op_expr -> two_op_eol expr : { '$1', '$2' }.
op_expr -> and_op_eol expr : { '$1', '$2' }.
op_expr -> or_op_eol expr : { '$1', '$2' }.
op_expr -> tail_op_eol expr : { '$1', '$2' }.
op_expr -> than_op_eol expr : { '$1', '$2' }.
op_expr -> in_op_eol expr : { '$1', '$2' }.
op_expr -> inc_op_eol expr : { '$1', '$2' }.
op_expr -> when_op_eol expr : { '$1', '$2' }.
op_expr -> range_op_eol expr : { '$1', '$2' }.
op_expr -> default_op_eol expr : { '$1', '$2' }.
op_expr -> type_op_eol expr : { '$1', '$2' }.
op_expr -> comp_op_eol expr : { '$1', '$2' }.
op_expr -> arrow_op_eol expr : { '$1', '$2' }.

no_parens_op_expr -> match_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> add_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> mult_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> exp_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> two_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> and_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> or_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> tail_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> than_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> in_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> inc_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> when_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> range_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> default_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> type_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> comp_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> arrow_op_eol no_parens_expr : { '$1', '$2' }.

matched_op_expr -> match_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> add_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> mult_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> exp_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> two_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> and_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> or_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> tail_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> than_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> in_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> inc_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> when_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> range_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> default_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> type_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> comp_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> arrow_op_eol matched_expr : { '$1', '$2' }.

identifier_expr -> dot_punctuated_identifier call_args_no_parens_one : build_identifier('$1', '$2').
identifier_expr -> dot_op_identifier call_args_no_parens_one : build_identifier('$1', '$2').
identifier_expr -> dot_identifier call_args_no_parens_one : build_identifier('$1', '$2').
identifier_expr -> dot_punctuated_identifier : build_identifier('$1', []).
identifier_expr -> dot_do_identifier : build_identifier('$1', nil).
identifier_expr -> var : build_identifier('$1', nil).
identifier_expr -> max_expr : '$1'.

max_expr -> fn_eol stab end_eol : build_fn('$1', build_stab(lists:reverse('$2'))).
max_expr -> open_paren stab close_paren : build_stab(lists:reverse('$2')).
max_expr -> bracket_expr : '$1'.
max_expr -> parens_call call_args_parens : build_identifier('$1', '$2').
max_expr -> parens_call call_args_parens call_args_parens : build_nested_parens('$1', '$2', '$3').
max_expr -> dot_alias : '$1'.
max_expr -> base_expr : '$1'.

bracket_expr -> dot_bracket_identifier bracket_access : build_access(build_identifier('$1', nil), '$2').
bracket_expr -> max_expr bracket_access : build_access('$1', '$2').

bracket_at_expr -> at_op_eol dot_bracket_identifier bracket_access :
                     build_access(build_unary_op('$1', build_identifier('$2', nil)), '$3').
bracket_at_expr -> at_op_eol max_expr bracket_access :
                     build_access(build_unary_op('$1', '$2'), '$3').
bracket_at_expr -> bracket_at_expr bracket_access :
                     build_access('$1', '$2').

base_expr -> number : ?exprs('$1').
base_expr -> signed_number : { element(4, '$1'), [{line,?line('$1')}], ?exprs('$1') }.
base_expr -> atom : build_atom('$1').
base_expr -> list : '$1'.
base_expr -> tuple : '$1'.
base_expr -> 'true' : ?id('$1').
base_expr -> 'false' : ?id('$1').
base_expr -> 'nil' : ?id('$1').
base_expr -> aliases : { '__aliases__', [{line,?line('$1')}], ?exprs('$1') }.
base_expr -> bin_string  : build_bin_string('$1').
base_expr -> list_string : build_list_string('$1').
base_expr -> bit_string : '$1'.
base_expr -> sigil : build_sigil('$1').
base_expr -> '&' number : { '&', [{line,?line('$1')}], [?exprs('$2')] }.

%% Blocks

do_block -> do_eol 'end' : [[{do,nil}]].
do_block -> do_eol stab end_eol : [[{ do, build_stab(lists:reverse('$2')) }]].
do_block -> do_eol block_list 'end' : [[{ do, nil }|'$2']].
do_block -> do_eol stab_eol block_list 'end' : [[{ do, build_stab(lists:reverse('$2')) }|'$3']].

fn_eol -> 'fn' : '$1'.
fn_eol -> 'fn' eol : '$1'.

do_eol -> 'do' : '$1'.
do_eol -> 'do' eol : '$1'.

end_eol -> 'end' : '$1'.
end_eol -> eol 'end' : '$2'.

block_eol -> block_identifier : '$1'.
block_eol -> block_identifier eol : '$1'.

stab -> stab_expr : ['$1'].
stab -> stab eol stab_expr : ['$3'|'$1'].

stab_eol -> stab : '$1'.
stab_eol -> stab eol : '$1'.

stab_expr -> expr : '$1'.
stab_expr -> stab_op_eol stab_maybe_expr : build_op('$1', [], '$2').
stab_expr -> call_args_no_parens_all stab_op_eol stab_maybe_expr :
               build_op('$2', unwrap_when(unwrap_splice('$1')), '$3').
stab_expr -> stab_parens_many stab_op_eol stab_maybe_expr :
               build_op('$2', unwrap_splice('$1'), '$3').
stab_expr -> stab_parens_many when_op expr stab_op_eol stab_maybe_expr :
              build_op('$4', [{ 'when', [{line,?line('$2')}], unwrap_splice('$1') ++ ['$3'] }], '$5').

stab_maybe_expr -> 'expr' : '$1'.
stab_maybe_expr -> '$empty' : nil.

block_item -> block_eol stab_eol : { ?exprs('$1'), build_stab(lists:reverse('$2')) }.
block_item -> block_eol : { ?exprs('$1'), nil }.

block_list -> block_item : ['$1'].
block_list -> block_item block_list : ['$1'|'$2'].

%% Helpers

var -> dot_identifier : '$1'.

open_paren -> '('      : '$1'.
open_paren -> '(' eol  : '$1'.
close_paren -> ')'     : '$1'.
close_paren -> eol ')' : '$2'.

empty_paren -> open_paren ')' : '$1'.

open_bracket  -> '['     : '$1'.
open_bracket  -> '[' eol : '$1'.
close_bracket -> ']'     : '$1'.
close_bracket -> eol ']' : '$2'.

open_bit  -> '<<'     : '$1'.
open_bit  -> '<<' eol : '$1'.
close_bit -> '>>'     : '$1'.
close_bit -> eol '>>' : '$2'.

open_curly  -> '{'     : '$1'.
open_curly  -> '{' eol : '$1'.
close_curly -> '}'     : '$1'.
close_curly -> eol '}' : '$2'.

% Operators

add_op_eol -> add_op : '$1'.
add_op_eol -> add_op eol : '$1'.
add_op_eol -> dual_op : '$1'.
add_op_eol -> dual_op eol : '$1'.

mult_op_eol -> mult_op : '$1'.
mult_op_eol -> mult_op eol : '$1'.

exp_op_eol -> exp_op : '$1'.
exp_op_eol -> exp_op eol : '$1'.

two_op_eol -> two_op : '$1'.
two_op_eol -> two_op eol : '$1'.

default_op_eol -> default_op : '$1'.
default_op_eol -> default_op eol : '$1'.

type_op_eol -> type_op : '$1'.
type_op_eol -> type_op eol : '$1'.

unary_op_eol -> unary_op : '$1'.
unary_op_eol -> unary_op eol : '$1'.
unary_op_eol -> dual_op : '$1'.
unary_op_eol -> dual_op eol : '$1'.

match_op_eol -> match_op : '$1'.
match_op_eol -> match_op eol : '$1'.

and_op_eol -> and_op : '$1'.
and_op_eol -> and_op eol : '$1'.

or_op_eol -> or_op : '$1'.
or_op_eol -> or_op eol : '$1'.

tail_op_eol -> tail_op : '$1'.
tail_op_eol -> tail_op eol : '$1'.

than_op_eol -> than_op : '$1'.
than_op_eol -> than_op eol : '$1'.

in_op_eol -> in_op : '$1'.
in_op_eol -> in_op eol : '$1'.

inc_op_eol -> inc_op : '$1'.
inc_op_eol -> inc_op eol : '$1'.

when_op_eol -> when_op : '$1'.
when_op_eol -> when_op eol : '$1'.

stab_op_eol -> stab_op : '$1'.
stab_op_eol -> stab_op eol : '$1'.

range_op_eol -> range_op : '$1'.
range_op_eol -> range_op eol : '$1'.

at_op_eol -> at_op : '$1'.
at_op_eol -> at_op eol : '$1'.

comp_op_eol -> comp_op : '$1'.
comp_op_eol -> comp_op eol : '$1'.

arrow_op_eol -> arrow_op : '$1'.
arrow_op_eol -> arrow_op eol : '$1'.

% Dot operator

dot_op -> '.' : '$1'.
dot_op -> '.' eol : '$1'.

dot_identifier -> identifier : '$1'.
dot_identifier -> matched_expr dot_op identifier : build_dot('$2', '$1', '$3').

dot_alias -> matched_expr dot_op aliases : build_dot_alias('$2', '$1', '$3').

dot_op_identifier -> op_identifier : '$1'.
dot_op_identifier -> matched_expr dot_op op_identifier : build_dot('$2', '$1', '$3').

dot_do_identifier -> do_identifier : '$1'.
dot_do_identifier -> matched_expr dot_op do_identifier : build_dot('$2', '$1', '$3').

dot_bracket_identifier -> bracket_identifier : '$1'.
dot_bracket_identifier -> matched_expr dot_op bracket_identifier : build_dot('$2', '$1', '$3').

dot_paren_identifier -> paren_identifier : '$1'.
dot_paren_identifier -> matched_expr dot_op paren_identifier : build_dot('$2', '$1', '$3').

dot_punctuated_identifier -> punctuated_identifier : '$1'.
dot_punctuated_identifier -> matched_expr dot_op punctuated_identifier : build_dot('$2', '$1', '$3').

parens_call -> dot_paren_identifier : '$1'.
parens_call -> matched_expr dot_call_op : { '.', [{line,?line('$2')}], ['$1'] }. % Fun/local calls

% Function calls with no parentheses

call_args_no_parens_expr -> matched_expr : '$1'.
call_args_no_parens_expr -> empty_paren : nil.
call_args_no_parens_expr -> no_parens_expr : throw_no_parens_many_strict('$1').

call_args_no_parens_comma_expr -> matched_expr ',' call_args_no_parens_expr : ['$3', '$1'].
call_args_no_parens_comma_expr -> call_args_no_parens_comma_expr ',' call_args_no_parens_expr : ['$3'|'$1'].

call_args_no_parens_all -> call_args_no_parens_one : '$1'.
call_args_no_parens_all -> call_args_no_parens_many : '$1'.

call_args_no_parens_one -> call_args_no_parens_kw : ['$1'].
call_args_no_parens_one -> matched_expr : ['$1'].
call_args_no_parens_one -> no_parens_expr : ['$1'].

call_args_no_parens_many -> matched_expr ',' call_args_no_parens_kw : ['$1', '$3'].
call_args_no_parens_many -> call_args_no_parens_comma_expr : lists:reverse('$1').
call_args_no_parens_many -> call_args_no_parens_comma_expr ',' call_args_no_parens_kw : lists:reverse(['$3'|'$1']).

call_args_no_parens_many_strict -> call_args_no_parens_many : '$1'.
call_args_no_parens_many_strict -> empty_paren : throw_no_parens_strict('$1').
call_args_no_parens_many_strict -> open_paren call_args_no_parens_kw close_paren : throw_no_parens_strict('$1').
call_args_no_parens_many_strict -> open_paren call_args_no_parens_many close_paren : throw_no_parens_strict('$1').

stab_parens_many -> empty_paren : [].
stab_parens_many -> open_paren call_args_no_parens_kw close_paren : ['$2'].
stab_parens_many -> open_paren call_args_no_parens_many close_paren : '$2'.

% Containers and function calls with parentheses

container_expr -> empty_paren : nil.
container_expr -> matched_expr : '$1'.
container_expr -> unmatched_expr : '$1'.
container_expr -> no_parens_expr : throw_no_parens_many_strict('$1').

container_comma_expr_base -> container_expr ',' : ['$1'].
container_comma_expr_base -> container_comma_expr_base container_expr ',' : ['$2'|'$1'].

container_comma_expr -> container_expr : ['$1'].
container_comma_expr -> kw : ['$1'].
container_comma_expr -> container_comma_expr_base : '$1'.
container_comma_expr -> container_comma_expr_base container_expr : ['$2'|'$1'].
container_comma_expr -> container_comma_expr_base kw : ['$2'|'$1'].

container_arg  -> container_expr : '$1'.
container_arg  -> container_expr ',' : '$1'.
container_args -> container_comma_expr : lists:reverse('$1').

call_args_parens_comma_expr -> container_expr : ['$1'].
call_args_parens_comma_expr -> call_args_parens_comma_expr ',' container_expr : ['$3'|'$1'].

call_args_parens -> empty_paren : [].
call_args_parens -> open_paren no_parens_expr close_paren : ['$2'].
call_args_parens -> open_paren kw close_paren : ['$2'].
call_args_parens -> open_paren call_args_parens_comma_expr close_paren : lists:reverse('$2').
call_args_parens -> open_paren call_args_parens_comma_expr ',' kw close_paren : lists:reverse(['$4'|'$2']).

% KV

kw_eol  -> kw_identifier : '$1'.
kw_eol  -> kw_identifier eol : '$1'.

kw_expr -> kw_eol container_expr : { ?exprs('$1'),'$2' }.
kw_comma -> kw_expr ',' : ['$1'].
kw_comma -> kw_comma kw_expr ',' : ['$2'|'$1'].
kw -> kw_expr : ['$1'].
kw -> kw_comma : lists:reverse('$1').
kw -> kw_comma kw_expr : lists:reverse(['$2'|'$1']).

call_args_no_parens_kw_expr -> kw_eol call_args_no_parens_expr : {?exprs('$1'),'$2'}.
call_args_no_parens_kw_comma -> call_args_no_parens_kw_expr : ['$1'].
call_args_no_parens_kw_comma -> call_args_no_parens_kw_expr ',' call_args_no_parens_kw_comma : ['$1'|'$3'].
call_args_no_parens_kw -> call_args_no_parens_kw_comma : '$1'.

% Lists

bracket_access -> open_bracket ']' : { [], ?line('$1') }.
bracket_access -> open_bracket container_arg close_bracket : { '$2', ?line('$1') }.
bracket_access -> open_bracket kw close_bracket : { '$2', ?line('$1') }.

list -> open_bracket ']' : [].
list -> open_bracket kw close_bracket : '$2'.
list -> open_bracket container_arg close_bracket : ['$2'].
list -> open_bracket container_expr ',' container_args close_bracket : ['$2'|'$4'].

% Tuple

tuple -> open_curly '}' : build_tuple('$1', []).
tuple -> open_curly container_arg close_curly : build_tuple('$1', ['$2']).
tuple -> open_curly container_expr ',' container_args close_curly :  build_tuple('$1', ['$2'|'$4']).

% Bitstrings

bit_string -> open_bit '>>' : { '<<>>', [{line,?line('$1')}], [] }.
bit_string -> open_bit container_args close_bit : { '<<>>', [{line,?line('$1')}], '$2' }.

Erlang code.

-define(id(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(exprs(Node), element(3, Node)).

-define(rearrange_uop(Op), Op == 'not' orelse Op == '!').
-define(rearrange_bop(Op), Op == 'in' orelse Op == 'inlist' orelse Op == 'inbits').

%% The following directive is needed for (significantly) faster
%% compilation of the generated .erl file by the HiPE compiler
-compile([{hipe,[{regalloc,linear_scan}]}]).

%% Operators

build_op({ _Kind, Line, '/' }, { '&', _, [{ Kind, _, Atom } = Left] }, Right) when is_number(Right), is_atom(Atom), is_atom(Kind) ->
  { '&', [{line,Line}], [{ '/', [{line,Line}], [Left, Right] }] };

build_op({ _Kind, Line, '/' }, { '&', _, [{ { '.', _, [_, _] }, _, [] } = Left] }, Right) when is_number(Right) ->
  { '&', [{line,Line}], [{ '/', [{line,Line}], [Left, Right] }] };

build_op({ _Kind, Line, BOp }, { UOp, _, [Left] }, Right) when ?rearrange_bop(BOp), ?rearrange_uop(UOp) ->
  { UOp, [{line,Line}], [{ BOp, [{line,Line}], [Left, Right] }] };

build_op({ _Kind, Line, Op }, Left, Right) ->
  { Op, [{line,Line}], [Left, Right] }.

build_unary_op({ _Kind, Line, Op }, Expr) ->
  { Op, [{line,Line}], [Expr] }.

build_tuple(_Marker, [Left, Right]) ->
  { Left, Right };

build_tuple(Marker, Args) ->
  { '{}', [{line,?line(Marker)}], Args }.

%% Blocks

build_block([nil])                                      -> { '__block__', [], [nil] };
build_block([{Op,_,[_]}]=Exprs) when ?rearrange_uop(Op) -> { '__block__', [], Exprs };
build_block([{unquote_splicing,_,Args}]=Exprs) when
                                      length(Args) =< 2 -> { '__block__', [], Exprs };
build_block([Expr])                                     -> Expr;
build_block(Exprs)                                      -> { '__block__', [], Exprs }.

%% Dots

build_dot_alias(Dot, { '__aliases__', _, Left }, { 'aliases', _, Right }) ->
  { '__aliases__', [{line,?line(Dot)}], Left ++ Right };

build_dot_alias(Dot, Other, { 'aliases', _, Right }) ->
  { '__aliases__', [{line,?line(Dot)}], [Other|Right] }.

build_dot(Dot, Left, Right) ->
  { '.', [{line,?line(Dot)}], [Left, extract_identifier(Right)] }.

%% Identifiers

build_nested_parens(Dot, Args1, Args2) ->
  Identifier = build_identifier(Dot, Args1),
  { Identifier, ?line(Identifier), Args2 }.

build_identifier({ '.', Meta, _ } = Dot, Args) ->
  FArgs = case Args of
    nil -> [];
    _ -> Args
  end,
  { Dot, Meta, FArgs };

build_identifier({ Keyword, Line }, Args) when Keyword == fn ->
  { fn, [{line,Line}], Args };

build_identifier({ op_identifier, Line, Identifier }, [Arg]) ->
  { Identifier, [{ambiguous_op,nil},{line,Line}], [Arg] };

build_identifier({ _, Line, Identifier }, Args) ->
  { Identifier, [{line,Line}], Args }.

extract_identifier({ Kind, _, Identifier }) when
    Kind == identifier; Kind == punctuated_identifier; Kind == bracket_identifier;
    Kind == paren_identifier; Kind == do_identifier; Kind == op_identifier ->
  Identifier;

extract_identifier(Other) -> Other.

%% Fn

build_fn(Op, Stab) ->
  { fn, [{line,?line(Op)}], [[{ do, Stab }]] }.

%% Access

build_access(Expr, Access) ->
  Meta = [{line,?line(Access)}],
  { { '.', Meta, ['Elixir.Kernel', access] }, Meta, [ Expr, ?id(Access) ] }.

%% Interpolation aware

build_sigil({ sigil, Line, Sigil, Parts, Modifiers }) ->
  Meta = [{line,Line}],
  { list_to_atom("sigil_" ++ [Sigil]), Meta, [ { '<<>>', Meta, Parts }, Modifiers ] }.

build_bin_string({ bin_string, _Line, [H] }) when is_binary(H) -> H;
build_bin_string({ bin_string, Line, Args }) -> { '<<>>', [{line,Line}], Args }.

build_list_string({ list_string, _Line, [H] }) when is_binary(H) -> unicode:characters_to_list(H);
build_list_string({ list_string, Line, Args }) ->
  Meta = [{line,Line}],
  { { '.', Meta, [unicode, characters_to_list] }, Meta, [{ '<<>>', Meta, Args}] }.

build_atom({ atom, _Line, Atom }) when is_atom(Atom) -> Atom;
build_atom({ atom, Line, Args }) ->
  Meta = [{line,Line}],
  { { '.', Meta, [erlang, binary_to_atom] }, Meta, [{ '<<>>', Meta, Args }, utf8] }.

%% Keywords

build_stab([{ '->', Meta, [Left, Right] }|T]) ->
  { '->', Meta, build_stab(Meta, T, Left, [Right], []) };

build_stab(Else) ->
  build_block(Else).

build_stab(Old, [{ '->', New, [Left, Right] }|T], Marker, Temp, Acc) ->
  H = { Marker, Old, build_block(lists:reverse(Temp)) },
  build_stab(New, T, Left, [Right], [H|Acc]);

build_stab(Meta, [H|T], Marker, Temp, Acc) ->
  build_stab(Meta, T, Marker, [H|Temp], Acc);

build_stab(Meta, [], Marker, Temp, Acc) ->
  H = { Marker, Meta, build_block(lists:reverse(Temp)) },
  lists:reverse([H|Acc]).

%% Every time the parser sees a (unquote_splicing())
%% it assumes that a block is being spliced, wrapping
%% the splicing in a __block__. But in the stab clause,
%% we can have (unquote_splicing(1,2,3)) -> :ok, in such
%% case, we don't actually want the block, since it is
%% an arg style call. unwrap_splice unwraps the splice
%% from such blocks.
unwrap_splice([{ '__block__', [], [{ unquote_splicing, _, _ }] = Splice }]) ->
  Splice;

unwrap_splice(Other) -> Other.

unwrap_when(Args) ->
  case elixir_tree_helpers:split_last(Args) of
    { Start, { 'when', Meta, [_, _] = End } } ->
      [{ 'when', Meta, Start ++ End }];
    { _, _ } ->
      Args
  end.

%% Errors

throw(Line, Error, Token) ->
  throw({ error, { Line, ?MODULE, [Error, Token] }}).

throw_no_parens_strict(Token) ->
  throw(?line(Token), "unexpected parenthesis. If you are making a "
    "function call, do not insert spaces in between the function name and the "
    "opening parentheses. Syntax error before: ", "'('").

throw_no_parens_many_strict(Token) ->
  Line =
    case lists:keyfind(line, 1, element(2, Token)) of
      { line, L } -> L;
      false -> 0
    end,

  throw(Line, "unexpected comma. Parentheses are required to solve ambiguity "
    "in nested calls. Syntax error before: ", "','").
