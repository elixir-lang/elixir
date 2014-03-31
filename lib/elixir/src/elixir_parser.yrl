Nonterminals
  grammar expr_list
  expr container_expr block_expr no_parens_expr no_parens_one_expr access_expr
  bracket_expr bracket_at_expr matched_expr unmatched_expr max_expr
  op_expr matched_op_expr no_parens_op_expr
  comp_op_eol at_op_eol unary_op_eol and_op_eol or_op_eol
  add_op_eol mult_op_eol exp_op_eol two_op_eol pipe_op_eol stab_op_eol
  arrow_op_eol match_op_eol when_op_eol in_op_eol in_match_op_eol type_op_eol
  open_paren close_paren empty_paren
  list list_args open_bracket close_bracket
  tuple open_curly close_curly
  bit_string open_bit close_bit
  map map_op map_close map_args map_expr struct_op
  assoc_op_eol assoc_expr assoc_base assoc_update assoc_update_kw assoc
  container_args_base container_args call_args_parens_base call_args_parens parens_call
  call_args_no_parens_one call_args_no_parens_expr call_args_no_parens_comma_expr
  call_args_no_parens_all call_args_no_parens_many call_args_no_parens_many_strict
  stab stab_eol stab_expr stab_maybe_expr stab_parens_many
  kw_eol kw_base kw call_args_no_parens_kw_expr call_args_no_parens_kw
  dot_op dot_alias dot_identifier dot_op_identifier dot_do_identifier
  dot_paren_identifier dot_bracket_identifier
  do_block fn_eol do_eol end_eol block_eol block_item block_list
  .

Terminals
  identifier kw_identifier kw_identifier_string bracket_identifier
  paren_identifier do_identifier block_identifier
  fn 'end' aliases
  number signed_number atom atom_string bin_string list_string sigil
  dot_call_op op_identifier
  comp_op at_op unary_op and_op or_op arrow_op match_op in_op in_match_op type_op
  dual_op add_op mult_op exp_op two_op pipe_op stab_op when_op assoc_op
  'true' 'false' 'nil' 'do' eol ',' '.'
  '(' ')' '[' ']' '{' '}' '<<' '>>' '%{}' '%'
  .

Rootsymbol grammar.

%% There are two shift/reduce conflicts coming from call_args_parens.
Expect 2.

%% Changes in ops and precedence should be reflected on lib/elixir/lib/macro.ex
%% Note though the operator => in practice has lower precedence than all others.
%% Its entry in the table is only to support the %{ user | foo => bar } syntax.
Left       5 do.
Right     10 stab_op_eol.     %% ->
Left      20 ','.
Left      40 in_match_op_eol. %% <-, inlist, inbits, \\ (allowed in matches along =)
Right     50 when_op_eol.     %% when
Right     60 type_op_eol.     %% ::
Right     70 pipe_op_eol.     %% |
Right     80 assoc_op_eol.    %% =>
Right     90 match_op_eol.    %% =
Left     130 or_op_eol.       %% ||, |||, or, xor
Left     140 and_op_eol.      %% &&, &&&, and
Left     150 comp_op_eol.     %% <, >, <=, >=, ==, !=, =~, ===, !==
Right    160 arrow_op_eol.    %% < (op), (op) > (e.g |>, <<<, >>>)
Left     170 in_op_eol.       %% in
Right    200 two_op_eol.      %% ++, --, .., <>
Left     210 add_op_eol.      %% + (op), - (op)
Left     220 mult_op_eol.     %% * (op), / (op)
Left     250 exp_op_eol.      %% ^ (op) (e.g ^^^)
Nonassoc 300 unary_op_eol.    %% +, -, !, ^, not, &, ~~~
Left     310 dot_call_op.
Left     310 dot_op.          %% .
Nonassoc 320 at_op_eol.       %% @
Nonassoc 330 dot_identifier.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> eol : nil.
grammar -> expr_list : to_block('$1').
grammar -> eol expr_list : to_block('$2').
grammar -> expr_list eol : to_block('$1').
grammar -> eol expr_list eol : to_block('$2').
grammar -> '$empty' : nil.

% Note expressions are on reverse order
expr_list -> expr : ['$1'].
expr_list -> expr_list eol expr : ['$3'|'$1'].

expr -> empty_paren : nil.
expr -> matched_expr : '$1'.
expr -> no_parens_expr : '$1'.
expr -> unmatched_expr : '$1'.

%% In Elixir we have three main call syntaxes: with parentheses,
%% without parentheses and with do blocks. They are represented
%% in the AST as matched, no_parens and unmatched.
%%
%% The distinction is required because we can't, for example, have
%% a function call with a do block as argument inside another do
%% block call, unless there are parentheses:
%%
%%   if if true do true else false end do  #=> invalid
%%   if(if true do true else false end) do #=> valid
%%
%% Similarly, it is not possible to nest calls without parentheses
%% if their arity is more than 1:
%%
%%   foo a, bar b, c  #=> invalid
%%   foo(a, bar b, c) #=> invalid
%%   foo a, bar b     #=> valid
%%   foo a, bar(b, c) #=> valid
%%
%% So the different grammar rules need to take into account
%% if calls without parentheses are do blocks in particular
%% segments and act accordingly.
matched_expr -> matched_expr matched_op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
matched_expr -> matched_expr no_parens_op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
matched_expr -> unary_op_eol matched_expr : build_unary_op('$1', '$2').
matched_expr -> unary_op_eol no_parens_expr : build_unary_op('$1', '$2').
matched_expr -> at_op_eol matched_expr : build_unary_op('$1', '$2').
matched_expr -> at_op_eol no_parens_expr : build_unary_op('$1', '$2').
matched_expr -> no_parens_one_expr : '$1'.
matched_expr -> access_expr : '$1'.

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
block_expr -> dot_identifier call_args_no_parens_all do_block : build_identifier('$1', '$2' ++ '$3').

op_expr -> match_op_eol expr : { '$1', '$2' }.
op_expr -> add_op_eol expr : { '$1', '$2' }.
op_expr -> mult_op_eol expr : { '$1', '$2' }.
op_expr -> exp_op_eol expr : { '$1', '$2' }.
op_expr -> two_op_eol expr : { '$1', '$2' }.
op_expr -> and_op_eol expr : { '$1', '$2' }.
op_expr -> or_op_eol expr : { '$1', '$2' }.
op_expr -> in_op_eol expr : { '$1', '$2' }.
op_expr -> in_match_op_eol expr : { '$1', '$2' }.
op_expr -> type_op_eol expr : { '$1', '$2' }.
op_expr -> when_op_eol expr : { '$1', '$2' }.
op_expr -> pipe_op_eol expr : { '$1', '$2' }.
op_expr -> comp_op_eol expr : { '$1', '$2' }.
op_expr -> arrow_op_eol expr : { '$1', '$2' }.

no_parens_op_expr -> match_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> add_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> mult_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> exp_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> two_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> and_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> or_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> in_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> in_match_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> type_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> pipe_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> comp_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> arrow_op_eol no_parens_expr : { '$1', '$2' }.

%% Allow when (and only when) with keywords
no_parens_op_expr -> when_op_eol no_parens_expr : { '$1', '$2' }.
no_parens_op_expr -> when_op_eol call_args_no_parens_kw : { '$1', '$2' }.

matched_op_expr -> match_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> add_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> mult_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> exp_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> two_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> and_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> or_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> in_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> in_match_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> type_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> when_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> pipe_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> comp_op_eol matched_expr : { '$1', '$2' }.
matched_op_expr -> arrow_op_eol matched_expr : { '$1', '$2' }.

no_parens_one_expr -> dot_op_identifier call_args_no_parens_one : build_identifier('$1', '$2').
no_parens_one_expr -> dot_identifier call_args_no_parens_one : build_identifier('$1', '$2').
no_parens_one_expr -> dot_do_identifier : build_identifier('$1', nil).
no_parens_one_expr -> dot_identifier : build_identifier('$1', nil).

%% From this point on, we just have constructs that can be
%% used with the access syntax. Notice that (dot_)identifier
%% is not included in this list simply because the tokenizer
%% marks identifiers followed by brackets as bracket_identifier.
access_expr -> bracket_at_expr : '$1'.
access_expr -> bracket_expr : '$1'.
access_expr -> unary_op_eol number : build_unary_op('$1', ?exprs('$2')).
access_expr -> fn_eol stab end_eol : build_fn('$1', build_stab(reverse('$2'))).
access_expr -> open_paren stab close_paren : build_stab(reverse('$2')).
access_expr -> number : ?exprs('$1').
access_expr -> signed_number : { element(4, '$1'), meta('$1'), ?exprs('$1') }.
access_expr -> list : element(1, '$1').
access_expr -> map : '$1'.
access_expr -> tuple : '$1'.
access_expr -> 'true' : ?id('$1').
access_expr -> 'false' : ?id('$1').
access_expr -> 'nil' : ?id('$1').
access_expr -> bin_string  : build_bin_string('$1').
access_expr -> list_string : build_list_string('$1').
access_expr -> bit_string : '$1'.
access_expr -> sigil : build_sigil('$1').
access_expr -> max_expr : '$1'.

%% Aliases and properly formed calls. Used by map_expr.
max_expr -> atom : ?exprs('$1').
max_expr -> atom_string : build_atom_string('$1').
max_expr -> parens_call call_args_parens : build_identifier('$1', '$2').
max_expr -> parens_call call_args_parens call_args_parens : build_nested_parens('$1', '$2', '$3').
max_expr -> dot_alias : '$1'.

bracket_expr -> dot_bracket_identifier list : build_access(build_identifier('$1', nil), '$2').
bracket_expr -> access_expr list : build_access('$1', '$2').

bracket_at_expr -> at_op_eol dot_bracket_identifier list :
                     build_access(build_unary_op('$1', build_identifier('$2', nil)), '$3').
bracket_at_expr -> at_op_eol access_expr list :
                     build_access(build_unary_op('$1', '$2'), '$3').

%% Blocks

do_block -> do_eol 'end' : [[{do,nil}]].
do_block -> do_eol stab end_eol : [[{ do, build_stab(reverse('$2')) }]].
do_block -> do_eol block_list 'end' : [[{ do, nil }|'$2']].
do_block -> do_eol stab_eol block_list 'end' : [[{ do, build_stab(reverse('$2')) }|'$3']].

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
               build_op('$4', [{ 'when', meta('$2'), unwrap_splice('$1') ++ ['$3'] }], '$5').

stab_maybe_expr -> 'expr' : '$1'.
stab_maybe_expr -> '$empty' : nil.

block_item -> block_eol stab_eol : { ?exprs('$1'), build_stab(reverse('$2')) }.
block_item -> block_eol : { ?exprs('$1'), nil }.

block_list -> block_item : ['$1'].
block_list -> block_item block_list : ['$1'|'$2'].

%% Helpers

open_paren -> '('      : '$1'.
open_paren -> '(' eol  : '$1'.
close_paren -> ')'     : '$1'.
close_paren -> eol ')' : '$2'.

empty_paren -> open_paren ')' : '$1'.

open_bracket  -> '['         : '$1'.
open_bracket  -> '[' eol     : '$1'.
close_bracket -> ']'         : '$1'.
close_bracket -> eol ']'     : '$2'.

open_bit  -> '<<'         : '$1'.
open_bit  -> '<<' eol     : '$1'.
close_bit -> '>>'         : '$1'.
close_bit -> eol '>>'     : '$2'.

open_curly  -> '{'     : '$1'.
open_curly  -> '{' eol : '$1'.
close_curly -> '}'         : '$1'.
close_curly -> eol '}'     : '$2'.

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

pipe_op_eol -> pipe_op : '$1'.
pipe_op_eol -> pipe_op eol : '$1'.

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

in_op_eol -> in_op : '$1'.
in_op_eol -> in_op eol : '$1'.

in_match_op_eol -> in_match_op : '$1'.
in_match_op_eol -> in_match_op eol : '$1'.

type_op_eol -> type_op : '$1'.
type_op_eol -> type_op eol : '$1'.

when_op_eol -> when_op : '$1'.
when_op_eol -> when_op eol : '$1'.

stab_op_eol -> stab_op : '$1'.
stab_op_eol -> stab_op eol : '$1'.

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

dot_alias -> aliases : { '__aliases__', meta('$1', 0), ?exprs('$1') }.
dot_alias -> matched_expr dot_op aliases : build_dot_alias('$2', '$1', '$3').

dot_op_identifier -> op_identifier : '$1'.
dot_op_identifier -> matched_expr dot_op op_identifier : build_dot('$2', '$1', '$3').

dot_do_identifier -> do_identifier : '$1'.
dot_do_identifier -> matched_expr dot_op do_identifier : build_dot('$2', '$1', '$3').

dot_bracket_identifier -> bracket_identifier : '$1'.
dot_bracket_identifier -> matched_expr dot_op bracket_identifier : build_dot('$2', '$1', '$3').

dot_paren_identifier -> paren_identifier : '$1'.
dot_paren_identifier -> matched_expr dot_op paren_identifier : build_dot('$2', '$1', '$3').

parens_call -> dot_paren_identifier : '$1'.
parens_call -> matched_expr dot_call_op : { '.', meta('$2'), ['$1'] }. % Fun/local calls

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
call_args_no_parens_many -> call_args_no_parens_comma_expr : reverse('$1').
call_args_no_parens_many -> call_args_no_parens_comma_expr ',' call_args_no_parens_kw : reverse(['$3'|'$1']).

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

container_args_base -> container_expr : ['$1'].
container_args_base -> container_args_base ',' container_expr : ['$3'|'$1'].

container_args -> kw : ['$1'].
container_args -> container_args_base : lists:reverse('$1').
container_args -> container_args_base ',' : lists:reverse('$1').
container_args -> container_args_base ',' kw : lists:reverse(['$3'|'$1']).

call_args_parens_base -> container_expr : ['$1'].
call_args_parens_base -> call_args_parens_base ',' container_expr : ['$3'|'$1'].

call_args_parens -> empty_paren : [].
call_args_parens -> open_paren no_parens_expr close_paren : ['$2'].
call_args_parens -> open_paren kw close_paren : ['$2'].
call_args_parens -> open_paren call_args_parens_base close_paren : reverse('$2').
call_args_parens -> open_paren call_args_parens_base ',' kw close_paren : reverse(['$4'|'$2']).

% KV

kw_eol -> kw_identifier : ?exprs('$1').
kw_eol -> kw_identifier eol : ?exprs('$1').
kw_eol -> kw_identifier_string : build_atom_string('$1').
kw_eol -> kw_identifier_string eol : build_atom_string('$1').

kw_base -> kw_eol container_expr : [{ '$1', '$2' }].
kw_base -> kw_base ',' kw_eol container_expr : [{ '$3', '$4' }|'$1'].

kw -> kw_base : reverse('$1').
kw -> kw_base ',' : reverse('$1').

call_args_no_parens_kw_expr -> kw_eol call_args_no_parens_expr : { '$1','$2' }.
call_args_no_parens_kw -> call_args_no_parens_kw_expr : ['$1'].
call_args_no_parens_kw -> call_args_no_parens_kw_expr ',' call_args_no_parens_kw : ['$1'|'$3'].

% Lists

list_args -> kw : '$1'.
list_args -> container_args_base : reverse('$1').
list_args -> container_args_base ',' : reverse('$1').
list_args -> container_args_base ',' kw : reverse('$1', '$3').

list -> open_bracket ']' : build_list('$1', []).
list -> open_bracket list_args close_bracket : build_list('$1', '$2').

% Tuple

tuple -> open_curly '}' : build_tuple('$1', []).
tuple -> open_curly container_args close_curly :  build_tuple('$1', '$2').

% Bitstrings

bit_string -> open_bit '>>' : build_bit('$1', []).
bit_string -> open_bit container_args close_bit : build_bit('$1', '$2').

% Map and structs

map_expr -> max_expr : '$1'.
map_expr -> dot_identifier : build_identifier('$1', nil).
map_expr -> at_op_eol map_expr : build_unary_op('$1', '$2').

assoc_op_eol -> assoc_op : '$1'.
assoc_op_eol -> assoc_op eol : '$1'.

assoc_expr -> container_expr assoc_op_eol container_expr : { '$1', '$3' }.
assoc_expr -> map_expr : '$1'.

assoc_update -> matched_expr pipe_op_eol matched_expr assoc_op_eol matched_expr : { '$2', '$1', [{ '$3', '$5' }] }.
assoc_update -> unmatched_expr pipe_op_eol expr assoc_op_eol expr : { '$2', '$1', [{ '$3', '$5' }] }.
assoc_update -> matched_expr pipe_op_eol map_expr : { '$2', '$1', ['$3'] }.

assoc_update_kw -> matched_expr pipe_op_eol kw : { '$2', '$1', '$3' }.
assoc_update_kw -> unmatched_expr pipe_op_eol kw : { '$2', '$1', '$3' }.

assoc_base -> assoc_expr : ['$1'].
assoc_base -> assoc_base ',' assoc_expr : ['$3'|'$1'].

assoc -> assoc_base : reverse('$1').
assoc -> assoc_base ',' : reverse('$1').

map_op -> '%{}' : '$1'.
map_op -> '%{}' eol : '$1'.

map_close -> kw close_curly : '$1'.
map_close -> assoc close_curly : '$1'.
map_close -> assoc_base ',' kw close_curly : reverse('$1', '$3').

map_args -> open_curly '}' : build_map('$1', []).
map_args -> open_curly map_close : build_map('$1', '$2').
map_args -> open_curly assoc_update close_curly : build_map_update('$1', '$2', []).
map_args -> open_curly assoc_update ',' close_curly : build_map_update('$1', '$2', []).
map_args -> open_curly assoc_update ',' map_close : build_map_update('$1', '$2', '$4').
map_args -> open_curly assoc_update_kw close_curly : build_map_update('$1', '$2', []).

struct_op -> '%' : '$1'.
struct_op -> '%' eol : '$1'.

map -> map_op map_args : '$2'.
map -> struct_op map_expr map_args : { '%', meta('$1'), ['$2', '$3'] }.
map -> struct_op map_expr eol map_args : { '%', meta('$1'), ['$2', '$4'] }.

Erlang code.

-define(id(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(exprs(Node), element(3, Node)).
-define(rearrange_uop(Op), (Op == 'not' orelse Op == '!')).
-define(is_atom_string(Atom), (Atom == atom_string orelse Atom == kw_identifier_string)).

%% The following directive is needed for (significantly) faster
%% compilation of the generated .erl file by the HiPE compiler
-compile([{hipe,[{regalloc,linear_scan}]}]).
-import(lists, [reverse/1, reverse/2]).

meta(Line, Counter) -> [{counter,Counter}|meta(Line)].
meta(Line) when is_integer(Line) -> [{line,Line}];
meta(Node) -> meta(?line(Node)).

%% Operators

build_op({ _Kind, Line, '/' }, { '&', _, [{ Kind, _, Atom } = Left] }, Right) when is_number(Right), is_atom(Atom), is_atom(Kind) ->
  { '&', meta(Line), [{ '/', meta(Line), [Left, Right] }] };

build_op({ _Kind, Line, '/' }, { '&', _, [{ { '.', _, [_, _] }, _, [] } = Left] }, Right) when is_number(Right) ->
  { '&', meta(Line), [{ '/', meta(Line), [Left, Right] }] };

build_op({ _Kind, Line, 'in' }, { UOp, _, [Left] }, Right) when ?rearrange_uop(UOp) ->
  { UOp, meta(Line), [{ 'in', meta(Line), [Left, Right] }] };

build_op({ _Kind, Line, Op }, Left, Right) ->
  { Op, meta(Line), [Left, Right] }.

build_unary_op({ _Kind, Line, Op }, Expr) ->
  { Op, meta(Line), [Expr] }.

build_list(Marker, Args) ->
  { Args, ?line(Marker) }.

build_tuple(_Marker, [Left, Right]) ->
  { Left, Right };
build_tuple(Marker, Args) ->
  { '{}', meta(Marker), Args }.

build_bit(Marker, Args) ->
  { '<<>>', meta(Marker), Args }.

build_map(Marker, Args) ->
  { '%{}', meta(Marker), Args }.

build_map_update(Marker, { Pipe, Left, Right }, Extra) ->
  { '%{}', meta(Marker), [build_op(Pipe, Left, Right ++ Extra)] }.

%% Blocks

build_block([{Op,_,[_]}]=Exprs) when ?rearrange_uop(Op) -> { '__block__', [], Exprs };
build_block([{unquote_splicing,_,Args}]=Exprs) when
                                      length(Args) =< 2 -> { '__block__', [], Exprs };
build_block([Expr])                                     -> Expr;
build_block(Exprs)                                      -> { '__block__', [], Exprs }.

%% Dots

build_dot_alias(Dot, { '__aliases__', _, Left }, { 'aliases', _, Right }) ->
  { '__aliases__', meta(Dot), Left ++ Right };

build_dot_alias(Dot, Other, { 'aliases', _, Right }) ->
  { '__aliases__', meta(Dot), [Other|Right] }.

build_dot(Dot, Left, Right) ->
  { '.', meta(Dot), [Left, extract_identifier(Right)] }.

extract_identifier({ Kind, _, Identifier }) when
    Kind == identifier; Kind == bracket_identifier; Kind == paren_identifier;
    Kind == do_identifier; Kind == op_identifier ->
  Identifier.

%% Identifiers

build_nested_parens(Dot, Args1, Args2) ->
  Identifier = build_identifier(Dot, Args1),
  Meta = element(2, Identifier),
  { Identifier, Meta, Args2 }.

build_identifier({ '.', Meta, _ } = Dot, Args) ->
  FArgs = case Args of
    nil -> [];
    _ -> Args
  end,
  { Dot, Meta, FArgs };

build_identifier({ Keyword, Line }, Args) when Keyword == fn ->
  { fn, meta(Line), Args };

build_identifier({ op_identifier, Line, Identifier }, [Arg]) ->
  { Identifier, [{ambiguous_op,nil}|meta(Line)], [Arg] };

build_identifier({ _, Line, Identifier }, Args) ->
  { Identifier, meta(Line), Args }.

%% Fn

build_fn(Op, Stab) ->
  { fn, meta(Op), Stab }.

%% Access

build_access(Expr, { List, Line }) ->
  Meta = meta(Line),
  { { '.', Meta, ['Elixir.Kernel', access] }, Meta, [Expr, List] }.

%% Interpolation aware

build_sigil({ sigil, Line, Sigil, Parts, Modifiers }) ->
  Meta = meta(Line),
  { list_to_atom("sigil_" ++ [Sigil]), Meta, [ { '<<>>', Meta, string_parts(Parts) }, Modifiers ] }.

build_bin_string({ bin_string, _Line, [H] }) when is_binary(H) ->
  H;
build_bin_string({ bin_string, Line, Args }) ->
  { '<<>>', meta(Line), string_parts(Args) }.

build_list_string({ list_string, _Line, [H] }) when is_binary(H) ->
  elixir_utils:characters_to_list(H);
build_list_string({ list_string, Line, Args }) ->
  Meta = meta(Line),
  { { '.', Meta, ['Elixir.String', 'to_char_list!'] }, Meta, [{ '<<>>', Meta, string_parts(Args) }] }.

build_atom_string({ Atom, _Line, Safe, [H] }) when ?is_atom_string(Atom) andalso is_binary(H) ->
  Op = binary_to_atom_op(Safe), erlang:Op(H, utf8);
build_atom_string({ Atom, Line, Safe, Args }) when ?is_atom_string(Atom) ->
  Meta = meta(Line),
  { { '.', Meta, [erlang, binary_to_atom_op(Safe)] }, Meta, [{ '<<>>', Meta, string_parts(Args) }, utf8] }.

binary_to_atom_op(true)  -> binary_to_existing_atom;
binary_to_atom_op(false) -> binary_to_atom.

string_parts(Parts) ->
  [string_part(Part) || Part <- Parts].
string_part(Binary) when is_binary(Binary) ->
  Binary;
string_part({ Line, Tokens }) ->
  Form = string_tokens_parse(Tokens),
  Meta = meta(Line),
  { '::', Meta, [{ { '.', Meta, ['Elixir.Kernel', to_string] }, Meta, [Form]}, { binary, Meta, nil }]}.

string_tokens_parse(Tokens) ->
  case parse(Tokens) of
    { ok, Forms } -> Forms;
    { error, _ } = Error -> throw(Error)
  end.

%% Keywords

build_stab([{ '->', Meta, [Left, Right] }|T]) ->
  build_stab(Meta, T, Left, [Right], []);

build_stab(Else) ->
  build_block(Else).

build_stab(Old, [{ '->', New, [Left, Right] }|T], Marker, Temp, Acc) ->
  H = { '->', Old, [Marker, build_block(reverse(Temp))] },
  build_stab(New, T, Left, [Right], [H|Acc]);

build_stab(Meta, [H|T], Marker, Temp, Acc) ->
  build_stab(Meta, T, Marker, [H|Temp], Acc);

build_stab(Meta, [], Marker, Temp, Acc) ->
  H = { '->', Meta, [Marker, build_block(reverse(Temp))] },
  reverse([H|Acc]).

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
  case elixir_utils:split_last(Args) of
    { Start, { 'when', Meta, [_, _] = End } } ->
      [{ 'when', Meta, Start ++ End }];
    { _, _ } ->
      Args
  end.

to_block([One]) when not is_list(One) -> One;
to_block(Other) -> { '__block__', [], reverse(Other) }.

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
