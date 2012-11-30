% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar expr_list
  expr paren_expr block_expr fn_expr bracket_expr call_expr bracket_at_expr max_expr
  base_expr matched_expr matched_op_expr unmatched_expr op_expr
  add_op mult_op unary_op two_op pipeline_op bin_concat_op
  match_op send_op default_op when_op pipe_op in_op inc_op stab_op range_op
  andand_op oror_op and_op or_op comp_expr_op colon_colon_op three_op at_op
  open_paren close_paren empty_paren
  open_bracket close_bracket
  open_curly close_curly
  open_bit close_bit
  base_comma_expr comma_expr optional_comma_expr matched_comma_expr
  call_args call_args_parens call_args_parens_not_one call_args_no_parens parens_call
  stab_expr stab_expr_list
  kw_eol kw_expr kw_comma kw_base
  matched_kw_expr matched_kw_comma matched_kw_base
  dot_op dot_ref dot_identifier dot_op_identifier dot_do_identifier
  dot_paren_identifier dot_punctuated_identifier dot_bracket_identifier
  var list bracket_access bit_string tuple
  do_block fn_eol do_eol end_eol block_eol block_item block_list
  .

Terminals
  identifier kw_identifier punctuated_identifier
  bracket_identifier paren_identifier do_identifier block_identifier
  fn 'end' '__aliases__'
  number signed_number atom bin_string list_string sigil
  dot_call_op comp_op op_identifier
  'not' 'and' 'or' 'xor' 'when' 'in' 'inlist' 'inbits' 'do'
  'true' 'false' 'nil'
  '=' '+' '-' '*' '/' '++' '--' '**' '//'
  '(' ')' '[' ']' '{' '}' '<<' '>>' '::'
  eol ','  '&' '|'  '.' '^' '@' '<-' '<>' '->' '/>' '=~'
  '&&' '||' '!' '...' '..'
  '<<<' '>>>' '&&&' '|||' '^^^' '~~~'
  .

Rootsymbol grammar.

Left       5 do.
Right     10 '->'.
Left      20 ','.  % Solve nested call_args conflicts
Right     30 colon_colon_op.
Right     40 when_op.
Right     50 default_op.
Left      60 pipe_op.
Left      70 inc_op.
Right     80 match_op.
Right    110 send_op.
Left     120 oror_op.
Left     130 andand_op.
Left     140 or_op.
Left     150 and_op.
Left     160 comp_expr_op.
Left     170 in_op.
Left     180 range_op.
Left     190 three_op.
Left     200 add_op.
Left     210 mult_op.
Right    220 bin_concat_op.
Right    230 two_op.
Right    290 pipeline_op.
Nonassoc 300 unary_op.
Left     310 dot_call_op.
Left     310 dot_op.
Nonassoc 320 at_op.
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
expr_list -> empty_paren : [nil].
expr_list -> expr_list eol expr : ['$3'|'$1'].

paren_expr -> empty_paren : nil.
paren_expr -> expr : '$1'.

expr -> matched_expr : '$1'.
expr -> unmatched_expr : '$1'.

matched_expr -> matched_expr matched_op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
matched_expr -> unary_op matched_expr : build_unary_op('$1', '$2').
matched_expr -> at_op matched_expr : build_unary_op('$1', '$2').
matched_expr -> bracket_at_expr : '$1'.
matched_expr -> fn_expr : '$1'.

unmatched_expr -> matched_expr op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
unmatched_expr -> unmatched_expr op_expr : build_op(element(1, '$2'), '$1', element(2, '$2')).
unmatched_expr -> unary_op expr : build_unary_op('$1', '$2').
unmatched_expr -> at_op expr : build_unary_op('$1', '$2').
unmatched_expr -> block_expr : '$1'.

op_expr -> match_op expr : { '$1', '$2' }.
op_expr -> add_op expr : { '$1', '$2' }.
op_expr -> mult_op expr : { '$1', '$2' }.
op_expr -> two_op expr : { '$1', '$2' }.
op_expr -> pipeline_op expr : { '$1', '$2' }.
op_expr -> andand_op expr : { '$1', '$2' }.
op_expr -> three_op expr : { '$1', '$2' }.
op_expr -> oror_op expr : { '$1', '$2' }.
op_expr -> and_op expr : { '$1', '$2' }.
op_expr -> or_op expr : { '$1', '$2' }.
op_expr -> pipe_op expr : { '$1', '$2' }.
op_expr -> bin_concat_op expr : { '$1', '$2' }.
op_expr -> in_op expr : { '$1', '$2' }.
op_expr -> inc_op expr : { '$1', '$2' }.
op_expr -> when_op expr : { '$1', '$2' }.
op_expr -> send_op expr : { '$1', '$2' }.
op_expr -> range_op expr : { '$1', '$2' }.
op_expr -> default_op expr : { '$1', '$2' }.
op_expr -> colon_colon_op expr : { '$1', '$2' }.
op_expr -> comp_expr_op expr : { '$1', '$2' }.

matched_op_expr -> match_op matched_expr : { '$1', '$2' }.
matched_op_expr -> add_op matched_expr : { '$1', '$2' }.
matched_op_expr -> mult_op matched_expr : { '$1', '$2' }.
matched_op_expr -> two_op matched_expr : { '$1', '$2' }.
matched_op_expr -> pipeline_op matched_expr : { '$1', '$2' }.
matched_op_expr -> andand_op matched_expr : { '$1', '$2' }.
matched_op_expr -> three_op matched_expr : { '$1', '$2' }.
matched_op_expr -> oror_op matched_expr : { '$1', '$2' }.
matched_op_expr -> and_op matched_expr : { '$1', '$2' }.
matched_op_expr -> or_op matched_expr : { '$1', '$2' }.
matched_op_expr -> pipe_op matched_expr : { '$1', '$2' }.
matched_op_expr -> bin_concat_op matched_expr : { '$1', '$2' }.
matched_op_expr -> in_op matched_expr : { '$1', '$2' }.
matched_op_expr -> inc_op matched_expr : { '$1', '$2' }.
matched_op_expr -> when_op matched_expr : { '$1', '$2' }.
matched_op_expr -> send_op matched_expr : { '$1', '$2' }.
matched_op_expr -> range_op matched_expr : { '$1', '$2' }.
matched_op_expr -> default_op matched_expr : { '$1', '$2' }.
matched_op_expr -> colon_colon_op matched_expr : { '$1', '$2' }.
matched_op_expr -> comp_expr_op matched_expr : { '$1', '$2' }.

block_expr -> parens_call call_args_parens do_block : build_identifier('$1', '$2' ++ '$3').
block_expr -> parens_call call_args_parens call_args_parens do_block : { build_identifier('$1', '$2'), ?line('$1'), '$3' ++ '$4' }.
block_expr -> dot_punctuated_identifier call_args_no_parens do_block : build_identifier('$1', '$2' ++ '$3').
block_expr -> dot_do_identifier do_block : build_identifier('$1', '$2').
block_expr -> dot_identifier call_args_no_parens do_block : build_identifier('$1', '$2' ++ '$3').

fn_expr -> fn_eol stab_expr_list end_eol : build_fn('$1', build_stab(lists:reverse('$2'))).
fn_expr -> fn_eol '->' grammar 'end' : build_fn('$1', { '->', ?line('$2'), [{ [], build_block('$3') }] }).
fn_expr -> call_expr : '$1'.

call_expr -> dot_punctuated_identifier call_args_no_parens : build_identifier('$1', '$2').
call_expr -> dot_op_identifier call_args_no_parens : build_identifier('$1', '$2').
call_expr -> dot_identifier call_args_no_parens : build_identifier('$1', '$2').
call_expr -> dot_punctuated_identifier : build_identifier('$1', []).
call_expr -> dot_do_identifier : build_identifier('$1', nil).
call_expr -> var : build_identifier('$1', nil).
call_expr -> max_expr : '$1'.

max_expr -> bracket_expr : '$1'.
max_expr -> parens_call call_args_parens : build_identifier('$1', '$2').
max_expr -> parens_call call_args_parens call_args_parens : { build_identifier('$1', '$2'), ?line('$1'), '$3' }.
max_expr -> dot_ref : '$1'.
max_expr -> base_expr : '$1'.
max_expr -> open_paren stab_expr_list close_paren : build_stab(lists:reverse('$2')).

bracket_expr -> dot_bracket_identifier bracket_access : build_access(build_identifier('$1', nil), '$2').
bracket_expr -> max_expr bracket_access : build_access('$1', '$2').

bracket_at_expr -> at_op dot_bracket_identifier bracket_access : build_access(build_unary_op('$1', build_identifier('$2', nil)), '$3').
bracket_at_expr -> at_op max_expr bracket_access : build_access(build_unary_op('$1', '$2'), '$3').
bracket_at_expr -> bracket_at_expr bracket_access : build_access('$1', '$2').

base_expr -> number : ?exprs('$1').
base_expr -> signed_number : { element(4, '$1'), ?line('$1'), ?exprs('$1') }.
base_expr -> atom : build_atom('$1').
base_expr -> list : '$1'.
base_expr -> tuple : '$1'.
base_expr -> 'true' : ?op('$1').
base_expr -> 'false' : ?op('$1').
base_expr -> 'nil' : ?op('$1').
base_expr -> '__aliases__' : '$1'.
base_expr -> bin_string  : build_bin_string('$1').
base_expr -> list_string : build_list_string('$1').
base_expr -> bit_string : '$1'.
base_expr -> '&' : '$1'.
base_expr -> '...' : { ?op('$1'), ?line('$1'), [] }.
base_expr -> sigil : build_sigil('$1').

%% Blocks

do_block -> do_eol 'end' : [[{do,nil}]].
do_block -> do_eol stab_expr_list end_eol : [[{ do, build_stab(lists:reverse('$2')) }]].
do_block -> do_eol block_list 'end' : [[{ do, nil }|'$2']].
do_block -> do_eol stab_expr_list eol block_list 'end' : [[{ do, build_stab(lists:reverse('$2')) }|'$4']].

fn_eol -> 'fn' : '$1'.
fn_eol -> 'fn' eol : '$1'.

do_eol -> 'do' : '$1'.
do_eol -> 'do' eol : '$1'.

end_eol -> 'end' : '$1'.
end_eol -> eol 'end' : '$2'.

block_eol -> block_identifier : '$1'.
block_eol -> block_identifier eol : '$1'.

stab_expr_list -> stab_expr : ['$1'].
stab_expr_list -> stab_expr_list eol stab_expr : ['$3'|'$1'].

stab_expr -> expr : '$1'.
stab_expr -> call_args_no_parens stab_op expr : build_op('$2', '$1', '$3').
stab_expr -> call_args_parens_not_one stab_op expr : build_op('$2', '$1', '$3').

block_item -> block_eol stab_expr_list eol : { ?exprs('$1'), build_stab(lists:reverse('$2')) }.
block_item -> block_eol : { ?exprs('$1'), nil }.

block_list -> block_item : ['$1'].
block_list -> block_item block_list : ['$1'|'$2'].

%% Helpers

var -> dot_identifier : '$1'.

open_paren -> '('      : '$1'.
open_paren -> '(' eol  : '$1'.
close_paren -> ')'     : '$1'.
close_paren -> eol ')' : '$2'.

empty_paren -> open_paren ')' : nil.

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

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> '+' eol : '$1'.
add_op -> '-' eol : '$1'.

mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
mult_op -> '*' eol : '$1'.
mult_op -> '/' eol : '$1'.

two_op -> '++' : '$1'.
two_op -> '--' : '$1'.
two_op -> '++' eol : '$1'.
two_op -> '--' eol : '$1'.
two_op -> '**' : '$1'.
two_op -> '**' eol : '$1'.

pipeline_op -> '=~' : '$1'.
pipeline_op -> '=~' eol : '$1'.
pipeline_op -> '/>' : '$1'.
pipeline_op -> '/>' eol : '$1'.

three_op -> '&&&' : '$1'.
three_op -> '&&&' eol : '$1'.
three_op -> '|||' : '$1'.
three_op -> '|||' eol : '$1'.
three_op -> '<<<' : '$1'.
three_op -> '<<<' eol : '$1'.
three_op -> '>>>' : '$1'.
three_op -> '>>>' eol : '$1'.
three_op -> '^^^' : '$1'.
three_op -> '^^^' eol : '$1'.

default_op -> '//' : '$1'.
default_op -> '//' eol : '$1'.

colon_colon_op -> '::' : '$1'.
colon_colon_op -> '::' eol : '$1'.

unary_op -> '+' : '$1'.
unary_op -> '+' eol : '$1'.
unary_op -> '-' : '$1'.
unary_op -> '-' eol : '$1'.
unary_op -> '!' : '$1'.
unary_op -> '!' eol : '$1'.
unary_op -> '^' : '$1'.
unary_op -> '^' eol : '$1'.
unary_op -> 'not' : '$1'.
unary_op -> 'not' eol : '$1'.
unary_op -> '~~~' : '$1'.
unary_op -> '~~~' eol : '$1'.

at_op -> '@' : '$1'.
at_op -> '@' eol : '$1'.

match_op -> '=' : '$1'.
match_op -> '=' eol : '$1'.

andand_op -> '&&' : '$1'.
andand_op -> '&&' eol : '$1'.

oror_op -> '||' : '$1'.
oror_op -> '||' eol : '$1'.

and_op -> 'and' : '$1'.
and_op -> 'and' eol : '$1'.

or_op -> 'or' : '$1'.
or_op -> 'or' eol : '$1'.
or_op -> 'xor' : '$1'.
or_op -> 'xor' eol : '$1'.

pipe_op -> '|' : '$1'.
pipe_op -> '|' eol : '$1'.

bin_concat_op -> '<>' : '$1'.
bin_concat_op -> '<>' eol : '$1'.

in_op -> 'in' : '$1'.
in_op -> 'in' eol : '$1'.

inc_op -> 'inlist' : '$1'.
inc_op -> 'inlist' eol : '$1'.
inc_op -> 'inbits' : '$1'.
inc_op -> 'inbits' eol : '$1'.

when_op -> 'when' : '$1'.
when_op -> 'when' eol : '$1'.

stab_op -> '->' : '$1'.
stab_op -> '->' eol : '$1'.

send_op -> '<-' : '$1'.
send_op -> '<-' eol : '$1'.

range_op -> '..' : '$1'.
range_op -> '..' eol : '$1'.

comp_expr_op -> comp_op : '$1'.
comp_expr_op -> comp_op eol : '$1'.

% Dot operator

dot_op -> '.' : '$1'.
dot_op -> '.' eol : '$1'.

dot_identifier -> identifier : '$1'.
dot_identifier -> matched_expr dot_op identifier : build_dot('$2', '$1', '$3').

dot_ref -> matched_expr dot_op '__aliases__' : build_dot_ref('$2', '$1', '$3').

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
parens_call -> matched_expr dot_call_op : { '.', ?line('$2'), ['$1'] }. % Fun/local calls

% Function calls

matched_comma_expr -> matched_expr : ['$1'].
matched_comma_expr -> matched_comma_expr ',' matched_expr : ['$3'|'$1'].

call_args_no_parens -> matched_comma_expr : lists:reverse('$1').
call_args_no_parens -> matched_kw_base : ['$1'].
call_args_no_parens -> matched_comma_expr ',' matched_kw_base : lists:reverse(['$3'|'$1']).

call_args_parens_not_one -> open_paren ')' : [].
call_args_parens_not_one -> open_paren matched_kw_base close_paren : ['$2'].
call_args_parens_not_one -> open_paren matched_expr ',' call_args_no_parens close_paren : ['$2'|'$4'].

base_comma_expr -> paren_expr ',' : ['$1'].
base_comma_expr -> base_comma_expr paren_expr ',' : ['$2'|'$1'].

comma_expr -> paren_expr : ['$1'].
comma_expr -> kw_base : ['$1'].
comma_expr -> base_comma_expr : '$1'.
comma_expr -> base_comma_expr paren_expr : ['$2'|'$1'].
comma_expr -> base_comma_expr kw_base : ['$2'|'$1'].

optional_comma_expr -> paren_expr : '$1'.
optional_comma_expr -> paren_expr ',' : '$1'.

call_args -> comma_expr : lists:reverse('$1').

call_args_parens -> open_paren ')' : [].
call_args_parens -> open_paren call_args close_paren : '$2'.

% KV

kw_eol  -> kw_identifier : '$1'.
kw_eol  -> kw_identifier eol : '$1'.
kw_expr -> kw_eol paren_expr : { ?exprs('$1'),'$2' }.

kw_comma -> kw_expr ',' : ['$1'].
kw_comma -> kw_comma kw_expr ',' : ['$2'|'$1'].

kw_base  -> kw_expr : ['$1'].
kw_base  -> kw_comma : lists:reverse('$1').
kw_base  -> kw_comma kw_expr : lists:reverse(['$2'|'$1']).

matched_kw_expr  -> kw_eol matched_expr : {?exprs('$1'),'$2'}.
matched_kw_expr  -> kw_eol empty_paren : {?exprs('$1'),nil}.
matched_kw_comma -> matched_kw_expr : ['$1'].
matched_kw_comma -> matched_kw_expr ',' matched_kw_comma : ['$1'|'$3'].
matched_kw_base  -> matched_kw_comma : '$1'.

% Lists

bracket_access -> open_bracket ']' : { [], ?line('$1') }.
bracket_access -> open_bracket optional_comma_expr close_bracket : { '$2', ?line('$1') }.
bracket_access -> open_bracket kw_base close_bracket : { '$2', ?line('$1') }.

list -> open_bracket ']' : [].
list -> open_bracket kw_base close_bracket : '$2'.
list -> open_bracket optional_comma_expr close_bracket : ['$2'].
list -> open_bracket paren_expr ',' call_args close_bracket : ['$2'|'$4'].

% Tuple

tuple -> open_curly '}' : build_tuple('$1', []).
tuple -> open_curly optional_comma_expr close_curly : build_tuple('$1', ['$2']).
tuple -> open_curly paren_expr ',' call_args close_curly :  build_tuple('$1', ['$2'|'$4']).

% Bitstrings

bit_string -> open_bit '>>' : { '<<>>', ?line('$1'), [] }.
bit_string -> open_bit call_args close_bit : { '<<>>', ?line('$1'), '$2' }.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(exprs(Node), element(3, Node)).

-define(rearrange_uop(Op), Op == 'not' orelse Op == '!').
-define(rearrange_bop(Op), Op == 'in' orelse Op == 'inlist' orelse Op == 'inbits').

%% The following directive is needed for (significantly) faster
%% compilation of the generated .erl file by the HiPE compiler
-compile([{hipe,[{regalloc,linear_scan}]}]).

%% Operators

build_op({ _, _, _ } = Op, Left, Right) ->
  { ?exprs(Op), ?line(Op), [Left, Right] };

build_op({ BOp, Line }, { UOp, _, [Left] }, Right) when ?rearrange_bop(BOp), ?rearrange_uop(UOp) ->
  { UOp, Line, [{ BOp, Line, [Left, Right] }] };

build_op(Op, Left, Right) ->
  { ?op(Op), ?line(Op), [Left, Right] }.

build_unary_op(Op, Expr) ->
  { ?op(Op), ?line(Op), [Expr] }.

build_tuple(_Marker, [Left, Right]) ->
  { Left, Right };

build_tuple(Marker, Args) ->
  { '{}', ?line(Marker), Args }.

%% Blocks

build_block([nil])                                      -> { '__block__', 0, [nil] };
build_block([{Op,_,[_]}]=Exprs) when ?rearrange_uop(Op) -> { '__block__', 0, Exprs };
build_block([Expr]) when not is_list(Expr)              -> Expr;
build_block(Exprs)                                      -> { '__block__', 0, Exprs }.

%% Dots

build_dot_ref(Dot, { '__aliases__', _, Left }, { '__aliases__', _, Right }) ->
  { '__aliases__', ?line(Dot), Left ++ Right };

build_dot_ref(Dot, Other, { '__aliases__', _, Right }) ->
  { '__aliases__', ?line(Dot), [Other|Right] }.

build_dot(Dot, Left, Right) ->
  { '.', ?line(Dot), [Left, extract_identifier(Right)] }.

%% Identifiers

build_identifier({ '.', Line, _ } = Dot, Args) ->
  FArgs = case Args of
    nil -> [];
    _ -> Args
  end,
  { Dot, Line, FArgs };

build_identifier({ Keyword, Line }, Args) when Keyword == fn ->
  { fn, Line, Args };

build_identifier({ op_identifier, Line, Identifier }, Args) ->
  { '__ambiguousop__', Line, [{ Identifier, Line, nil }|Args] };

build_identifier({ _, Line, Identifier }, Args) ->
  { Identifier, Line, Args }.

extract_identifier({ Kind, _, Identifier }) when
    Kind == identifier; Kind == punctuated_identifier; Kind == bracket_identifier;
    Kind == paren_identifier; Kind == do_identifier; Kind == op_identifier ->
  Identifier;

extract_identifier(Other) -> Other.

%% Fn

build_fn(Op, Stab) ->
  { fn, ?line(Op), [[{ do, Stab }]] }.

%% Access

build_access(Expr, Access) ->
  Line = ?line(Access),
  { { '.', Line, ['Elixir-Kernel', access] }, ?line(Access), [ Expr, ?op(Access) ] }.

%% Interpolation aware

build_sigil({ sigil, Line, Sigil, Parts, Modifiers }) ->
  { list_to_atom([$_,$_,Sigil,$_,$_]), Line, [ { '<<>>', Line, Parts }, Modifiers ] }.

build_bin_string({ bin_string, _Line, [H] }) when is_binary(H) -> H;
build_bin_string({ bin_string, Line, Args }) -> { '<<>>', Line, Args }.

build_list_string({ list_string, _Line, [H] }) when is_binary(H) -> binary_to_list(H);
build_list_string({ list_string, Line, Args }) -> { { '.', Line, [erlang, binary_to_list] }, Line, [{ '<<>>', Line, Args}] }.

build_atom({ atom, _Line, Atom }) when is_atom(Atom) -> Atom;
build_atom({ atom, Line, Args }) -> { { '.', Line, [erlang, binary_to_atom] }, Line, [{ '<<>>', Line, Args }, utf8] }.

%% Keywords

build_stab([{ '->', Line, [Left, Right] }|T]) ->
  { '->', Line, build_stab(T, Left, [Right], []) };

build_stab(Else) ->
  build_block(Else).

build_stab([{ '->', _, [Left, Right] }|T], Marker, Temp, Acc) ->
  H = { Marker, build_block(lists:reverse(Temp)) },
  build_stab(T, Left, [Right], [H|Acc]);

build_stab([H|T], Marker, Temp, Acc) ->
  build_stab(T, Marker, [H|Temp], Acc);

build_stab([], Marker, Temp, Acc) ->
  H = { Marker, build_block(lists:reverse(Temp)) },
  lists:reverse([H|Acc]).
