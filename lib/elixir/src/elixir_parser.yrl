Nonterminals
  grammar expr_list
  expr container_expr block_expr access_expr
  no_parens_expr no_parens_zero_expr no_parens_one_expr no_parens_one_ambig_expr
  bracket_expr bracket_at_expr bracket_arg matched_expr unmatched_expr sub_matched_expr
  unmatched_op_expr matched_op_expr no_parens_op_expr no_parens_many_expr
  comp_op_eol at_op_eol unary_op_eol and_op_eol or_op_eol capture_op_eol
  dual_op_eol mult_op_eol power_op_eol concat_op_eol xor_op_eol pipe_op_eol
  stab_op_eol arrow_op_eol match_op_eol when_op_eol in_op_eol in_match_op_eol
  type_op_eol rel_op_eol range_op_eol ternary_op_eol
  open_paren close_paren empty_paren eoe
  list list_args open_bracket close_bracket
  tuple open_curly close_curly
  bitstring open_bit close_bit
  map map_op map_base_expr map_close map_args
  assoc_op_eol assoc_expr assoc_base assoc assoc_update assoc_update_kw
  container_args_base container_args
  call_args_parens_expr call_args_parens_base call_args_parens parens_call
  call_args_no_parens_one call_args_no_parens_ambig call_args_no_parens_expr
  call_args_no_parens_comma_expr call_args_no_parens_all call_args_no_parens_many
  call_args_no_parens_many_strict
  stab stab_eoe stab_expr stab_op_eol_and_expr stab_parens_many
  kw_eol kw_base kw_data kw_call call_args_no_parens_kw_expr call_args_no_parens_kw
  dot_op dot_alias dot_bracket_identifier dot_call_identifier
  dot_identifier dot_op_identifier dot_do_identifier dot_paren_identifier
  do_block fn_eoe do_eoe block_eoe block_item block_list
  .

Terminals
  identifier kw_identifier kw_identifier_safe kw_identifier_unsafe bracket_identifier
  paren_identifier do_identifier block_identifier op_identifier
  fn 'end' alias
  atom atom_quoted atom_safe atom_unsafe bin_string list_string sigil
  bin_heredoc list_heredoc
  comp_op at_op unary_op and_op or_op arrow_op match_op in_op in_match_op ellipsis_op
  type_op dual_op mult_op power_op concat_op range_op xor_op pipe_op stab_op when_op
  capture_int capture_op assoc_op rel_op ternary_op dot_call_op
  'true' 'false' 'nil' 'do' eol ';' ',' '.'
  '(' ')' '[' ']' '{' '}' '<<' '>>' '%{}' '%'
  int flt char
  .

Rootsymbol grammar.

%% Two shift/reduce conflicts coming from call_args_parens and
%% one coming from empty_paren on stab.
Expect 3.

%% Changes in ops and precedence should be reflected on:
%%
%%   1. lib/elixir/lib/code/identifier.ex
%%   2. lib/elixir/pages/operators.md
%%   3. lib/iex/lib/iex/evaluator.ex
%%
%% Note though the operator => in practice has lower precedence
%% than all others, its entry in the table is only to support the
%% %{user | foo => bar} syntax.

Left       5 do.
Right     10 stab_op_eol.     %% ->
Left      20 ','.
Left      40 in_match_op_eol. %% <-, \\ (allowed in matches along =)
Right     50 when_op_eol.     %% when
Right     60 type_op_eol.     %% ::
Right     70 pipe_op_eol.     %% |
Right     80 assoc_op_eol.    %% =>
Nonassoc  90 capture_op_eol.  %% &
Nonassoc  90 ellipsis_op.     %% ...
Right    100 match_op_eol.    %% =
Left     120 or_op_eol.       %% ||, |||, or
Left     130 and_op_eol.      %% &&, &&&, and
Left     140 comp_op_eol.     %% ==, !=, =~, ===, !==
Left     150 rel_op_eol.      %% <, >, <=, >=
Left     160 arrow_op_eol.    %% <<<, >>>, |>, <<~, ~>>, <~, ~>, <~>, <|>
Left     170 in_op_eol.       %% in, not in
Left     180 xor_op_eol.      %% ^^^
Right    190 ternary_op_eol.  %% //
Right    200 concat_op_eol.   %% ++, --, +++, ---, <>
Right    200 range_op_eol.    %% ..
Left     210 dual_op_eol.     %% +, -
Left     220 mult_op_eol.     %% *, /
Left     230 power_op_eol.    %% **
Nonassoc 300 unary_op_eol.    %% +, -, !, ^, not, ~~~
Left     310 dot_call_op.
Left     310 dot_op.          %% .
Nonassoc 320 at_op_eol.       %% @
Nonassoc 330 dot_identifier.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> eoe : {'__block__', meta_from_token('$1'), []}.
grammar -> expr_list : build_block(reverse('$1')).
grammar -> eoe expr_list : build_block(reverse('$2')).
grammar -> expr_list eoe : build_block(reverse(annotate_eoe('$2', '$1'))).
grammar -> eoe expr_list eoe : build_block(reverse(annotate_eoe('$3', '$2'))).
grammar -> '$empty' : {'__block__', [], []}.

% Note expressions are on reverse order
expr_list -> expr : ['$1'].
expr_list -> expr_list eoe expr : ['$3' | annotate_eoe('$2', '$1')].

expr -> matched_expr : '$1'.
expr -> no_parens_expr : '$1'.
expr -> unmatched_expr : '$1'.

%% In Elixir we have three main call syntaxes: with parentheses,
%% without parentheses and with do blocks. They are represented
%% in the AST as matched, no_parens and unmatched.
%%
%% Calls without parentheses are further divided according to how
%% problematic they are:
%%
%% (a) no_parens_one: a call with one unproblematic argument
%% (for example, `f a` or `f g a` and similar) (includes unary operators)
%%
%% (b) no_parens_many: a call with several arguments (for example, `f a, b`)
%%
%% (c) no_parens_one_ambig: a call with one argument which is
%% itself a no_parens_many or no_parens_one_ambig (for example, `f g a, b`,
%% `f g h a, b` and similar)
%%
%% Note, in particular, that no_parens_one_ambig expressions are
%% ambiguous and are interpreted such that the outer function has
%% arity 1. For instance, `f g a, b` is interpreted as `f(g(a, b))` rather
%% than `f(g(a), b)`. Hence the name, no_parens_one_ambig.
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
%%   foo bar a, b     #=> valid
%%   foo a, bar(b, c) #=> valid
%%
%% So the different grammar rules need to take into account
%% if calls without parentheses are do blocks in particular
%% segments and act accordingly.
matched_expr -> matched_expr matched_op_expr : build_op('$1', '$2').
matched_expr -> unary_op_eol matched_expr : build_unary_op('$1', '$2').
matched_expr -> at_op_eol matched_expr : build_unary_op('$1', '$2').
matched_expr -> capture_op_eol matched_expr : build_unary_op('$1', '$2').
matched_expr -> ellipsis_op matched_expr : build_unary_op('$1', '$2').
matched_expr -> no_parens_one_expr : '$1'.
matched_expr -> sub_matched_expr : '$1'.

unmatched_expr -> matched_expr unmatched_op_expr : build_op('$1', '$2').
unmatched_expr -> unmatched_expr matched_op_expr : build_op('$1', '$2').
unmatched_expr -> unmatched_expr unmatched_op_expr : build_op('$1', '$2').
unmatched_expr -> unmatched_expr no_parens_op_expr : warn_no_parens_after_do_op('$2'), build_op('$1', '$2').
unmatched_expr -> unary_op_eol expr : build_unary_op('$1', '$2').
unmatched_expr -> at_op_eol expr : build_unary_op('$1', '$2').
unmatched_expr -> capture_op_eol expr : build_unary_op('$1', '$2').
unmatched_expr -> ellipsis_op expr : build_unary_op('$1', '$2').
unmatched_expr -> block_expr : '$1'.

no_parens_expr -> matched_expr no_parens_op_expr : build_op('$1', '$2').
no_parens_expr -> unary_op_eol no_parens_expr : build_unary_op('$1', '$2').
no_parens_expr -> at_op_eol no_parens_expr : build_unary_op('$1', '$2').
no_parens_expr -> capture_op_eol no_parens_expr : build_unary_op('$1', '$2').
no_parens_expr -> ellipsis_op no_parens_expr : build_unary_op('$1', '$2').
no_parens_expr -> no_parens_one_ambig_expr : '$1'.
no_parens_expr -> no_parens_many_expr : '$1'.

block_expr -> dot_call_identifier call_args_parens do_block : build_parens('$1', '$2', '$3').
block_expr -> dot_call_identifier call_args_parens call_args_parens do_block : build_nested_parens('$1', '$2', '$3', '$4').
block_expr -> dot_do_identifier do_block : build_no_parens_do_block('$1', [], '$2').
block_expr -> dot_op_identifier call_args_no_parens_all do_block : build_no_parens_do_block('$1', '$2', '$3').
block_expr -> dot_identifier call_args_no_parens_all do_block : build_no_parens_do_block('$1', '$2', '$3').

matched_op_expr -> match_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> dual_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> mult_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> power_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> concat_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> range_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> ternary_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> xor_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> and_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> or_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> in_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> in_match_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> type_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> when_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> pipe_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> comp_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> rel_op_eol matched_expr : {'$1', '$2'}.
matched_op_expr -> arrow_op_eol matched_expr : {'$1', '$2'}.

%% We warn exclusively for |> and friends because they are used
%% in other languages with lower precedence than function application,
%% which can be the source of confusion.
matched_op_expr -> arrow_op_eol no_parens_one_expr : warn_pipe('$1', '$2'), {'$1', '$2'}.

unmatched_op_expr -> match_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> dual_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> mult_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> power_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> concat_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> range_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> ternary_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> xor_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> and_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> or_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> in_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> in_match_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> type_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> when_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> pipe_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> comp_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> rel_op_eol unmatched_expr : {'$1', '$2'}.
unmatched_op_expr -> arrow_op_eol unmatched_expr : {'$1', '$2'}.

no_parens_op_expr -> match_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> dual_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> mult_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> power_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> concat_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> range_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> ternary_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> xor_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> and_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> or_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> in_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> in_match_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> type_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> when_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> pipe_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> comp_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> rel_op_eol no_parens_expr : {'$1', '$2'}.
no_parens_op_expr -> arrow_op_eol no_parens_expr : warn_pipe('$1', '$2'), {'$1', '$2'}.

%% Allow when (and only when) with keywords
no_parens_op_expr -> when_op_eol call_args_no_parens_kw : {'$1', '$2'}.

no_parens_one_ambig_expr -> dot_op_identifier call_args_no_parens_ambig : build_no_parens('$1', '$2').
no_parens_one_ambig_expr -> dot_identifier call_args_no_parens_ambig : build_no_parens('$1', '$2').

no_parens_many_expr -> dot_op_identifier call_args_no_parens_many_strict : build_no_parens('$1', '$2').
no_parens_many_expr -> dot_identifier call_args_no_parens_many_strict : build_no_parens('$1', '$2').

no_parens_one_expr -> dot_op_identifier call_args_no_parens_one : build_no_parens('$1', '$2').
no_parens_one_expr -> dot_identifier call_args_no_parens_one : build_no_parens('$1', '$2').
no_parens_zero_expr -> dot_do_identifier : build_identifier('$1').
no_parens_zero_expr -> dot_identifier : build_identifier('$1').

sub_matched_expr -> no_parens_zero_expr : '$1'.
sub_matched_expr -> range_op : build_nullary_op('$1').
sub_matched_expr -> ellipsis_op : build_nullary_op('$1').
sub_matched_expr -> access_expr : '$1'.
sub_matched_expr -> access_expr kw_identifier : error_invalid_kw_identifier('$2').

%% From this point on, we just have constructs that can be
%% used with the access syntax. Note that (dot_)identifier
%% is not included in this list simply because the tokenizer
%% marks identifiers followed by brackets as bracket_identifier.
access_expr -> bracket_at_expr : '$1'.
access_expr -> bracket_expr : '$1'.
access_expr -> capture_int int : build_unary_op('$1', number_value('$2')).
access_expr -> fn_eoe stab_eoe 'end' : build_fn('$1', '$2', '$3').
access_expr -> open_paren stab_eoe ')' : build_paren_stab('$1', '$2', '$3').
access_expr -> open_paren ';' stab_eoe ')' : build_paren_stab('$1', '$3', '$4').
access_expr -> open_paren ';' close_paren : build_paren_stab('$1', [], '$3').
access_expr -> empty_paren : warn_empty_paren('$1'), {'__block__', [], []}.
access_expr -> int : handle_number(number_value('$1'), '$1', ?exprs('$1')).
access_expr -> flt : handle_number(number_value('$1'), '$1', ?exprs('$1')).
access_expr -> char : handle_number(?exprs('$1'), '$1', number_value('$1')).
access_expr -> list : element(1, '$1').
access_expr -> map : '$1'.
access_expr -> tuple : '$1'.
access_expr -> 'true' : handle_literal(?id('$1'), '$1').
access_expr -> 'false' : handle_literal(?id('$1'), '$1').
access_expr -> 'nil' : handle_literal(?id('$1'), '$1').
access_expr -> bin_string : build_bin_string('$1', delimiter(<<$">>)).
access_expr -> list_string : build_list_string('$1', delimiter(<<$'>>)).
access_expr -> bin_heredoc : build_bin_heredoc('$1').
access_expr -> list_heredoc : build_list_heredoc('$1').
access_expr -> bitstring : '$1'.
access_expr -> sigil : build_sigil('$1').
access_expr -> atom : handle_literal(?exprs('$1'), '$1').
access_expr -> atom_quoted : handle_literal(?exprs('$1'), '$1', delimiter(<<$">>)).
access_expr -> atom_safe : build_quoted_atom('$1', true, delimiter(<<$">>)).
access_expr -> atom_unsafe : build_quoted_atom('$1', false, delimiter(<<$">>)).
access_expr -> dot_alias : '$1'.
access_expr -> parens_call : '$1'.

%% Also used by maps and structs
parens_call -> dot_call_identifier call_args_parens : build_parens('$1', '$2', {[], []}).
parens_call -> dot_call_identifier call_args_parens call_args_parens : build_nested_parens('$1', '$2', '$3', {[], []}).

bracket_arg -> open_bracket kw_data close_bracket : build_access_arg('$1', '$2', '$3').
bracket_arg -> open_bracket container_expr close_bracket : build_access_arg('$1', '$2', '$3').
bracket_arg -> open_bracket container_expr ',' close_bracket : build_access_arg('$1', '$2', '$4').
bracket_arg -> open_bracket container_expr ',' container_args close_bracket : error_too_many_access_syntax('$3').

bracket_expr -> dot_bracket_identifier bracket_arg : build_access(build_identifier('$1'), meta_with_from_brackets('$2')).
bracket_expr -> access_expr bracket_arg : build_access('$1', meta_with_from_brackets('$2')).

bracket_at_expr -> at_op_eol dot_bracket_identifier bracket_arg :
                     build_access(build_unary_op('$1', build_identifier('$2')), meta_with_from_brackets('$3')).
bracket_at_expr -> at_op_eol access_expr bracket_arg :
                     build_access(build_unary_op('$1', '$2'), meta_with_from_brackets('$3')).

%% Blocks

do_block -> do_eoe 'end' :
              {do_end_meta('$1', '$2'), [[{handle_literal(do, '$1'), {'__block__', [], []}}]]}.
do_block -> do_eoe stab_eoe 'end' :
              {do_end_meta('$1', '$3'), [[{handle_literal(do, '$1'), build_stab('$2')}]]}.
do_block -> do_eoe block_list 'end' :
              {do_end_meta('$1', '$3'), [[{handle_literal(do, '$1'), {'__block__', [], []}} | '$2']]}.
do_block -> do_eoe stab_eoe block_list 'end' :
              {do_end_meta('$1', '$4'), [[{handle_literal(do, '$1'), build_stab('$2')} | '$3']]}.

eoe -> eol : '$1'.
eoe -> ';' : '$1'.
eoe -> eol ';' : '$1'.

fn_eoe -> 'fn' : '$1'.
fn_eoe -> 'fn' eoe : next_is_eol('$1', '$2').

do_eoe -> 'do' : '$1'.
do_eoe -> 'do' eoe : '$1'.

block_eoe -> block_identifier : '$1'.
block_eoe -> block_identifier eoe : '$1'.

stab -> stab_expr : ['$1'].
stab -> stab eoe stab_expr : ['$3' | annotate_eoe('$2', '$1')].

stab_eoe -> stab : '$1'.
stab_eoe -> stab eoe : annotate_eoe('$2', '$1').

stab_expr -> expr :
               '$1'.
stab_expr -> stab_op_eol_and_expr :
               build_op([], '$1').
stab_expr -> empty_paren stab_op_eol_and_expr :
               build_op([], '$2').
stab_expr -> empty_paren when_op expr stab_op_eol_and_expr :
               build_op([{'when', meta_from_token('$2'), ['$3']}], '$4').
stab_expr -> call_args_no_parens_all stab_op_eol_and_expr :
               build_op(unwrap_when(unwrap_splice('$1')), '$2').
stab_expr -> stab_parens_many stab_op_eol_and_expr :
               build_op(unwrap_splice('$1'), '$2').
stab_expr -> stab_parens_many when_op expr stab_op_eol_and_expr :
               build_op([{'when', meta_from_token('$2'), unwrap_splice('$1') ++ ['$3']}], '$4').

stab_op_eol_and_expr -> stab_op_eol expr : {'$1', '$2'}.
stab_op_eol_and_expr -> stab_op_eol : warn_empty_stab_clause('$1'), {'$1', handle_literal(nil, '$1')}.

block_item -> block_eoe stab_eoe :
                {handle_literal(?exprs('$1'), '$1'), build_stab('$2')}.
block_item -> block_eoe :
                {handle_literal(?exprs('$1'), '$1'), {'__block__', [], []}}.

block_list -> block_item : ['$1'].
block_list -> block_item block_list : ['$1' | '$2'].

%% Helpers

open_paren -> '('      : '$1'.
open_paren -> '(' eol  : next_is_eol('$1', '$2').
close_paren -> ')'     : '$1'.
close_paren -> eol ')' : '$2'.

empty_paren -> open_paren ')' : '$1'.

open_bracket  -> '['     : '$1'.
open_bracket  -> '[' eol : next_is_eol('$1', '$2').
close_bracket -> ']'     : '$1'.
close_bracket -> eol ']' : '$2'.

open_bit  -> '<<'     : '$1'.
open_bit  -> '<<' eol : next_is_eol('$1', '$2').
close_bit -> '>>'     : '$1'.
close_bit -> eol '>>' : '$2'.

open_curly  -> '{'     : '$1'.
open_curly  -> '{' eol : next_is_eol('$1', '$2').
close_curly -> '}'     : '$1'.
close_curly -> eol '}' : '$2'.

% Operators

unary_op_eol -> unary_op : '$1'.
unary_op_eol -> unary_op eol : '$1'.
unary_op_eol -> dual_op : '$1'.
unary_op_eol -> dual_op eol : '$1'.
unary_op_eol -> ternary_op : '$1'.
unary_op_eol -> ternary_op eol : '$1'.

capture_op_eol -> capture_op : '$1'.
capture_op_eol -> capture_op eol : '$1'.

at_op_eol -> at_op : '$1'.
at_op_eol -> at_op eol : '$1'.

match_op_eol -> match_op : '$1'.
match_op_eol -> match_op eol : next_is_eol('$1', '$2').

dual_op_eol -> dual_op : '$1'.
dual_op_eol -> dual_op eol : next_is_eol('$1', '$2').

mult_op_eol -> mult_op : '$1'.
mult_op_eol -> mult_op eol : next_is_eol('$1', '$2').

power_op_eol -> power_op : '$1'.
power_op_eol -> power_op eol : next_is_eol('$1', '$2').

concat_op_eol -> concat_op : '$1'.
concat_op_eol -> concat_op eol : next_is_eol('$1', '$2').

range_op_eol -> range_op : '$1'.
range_op_eol -> range_op eol : next_is_eol('$1', '$2').

ternary_op_eol -> ternary_op : '$1'.
ternary_op_eol -> ternary_op eol : next_is_eol('$1', '$2').

xor_op_eol -> xor_op : '$1'.
xor_op_eol -> xor_op eol : next_is_eol('$1', '$2').

pipe_op_eol -> pipe_op : '$1'.
pipe_op_eol -> pipe_op eol : next_is_eol('$1', '$2').

and_op_eol -> and_op : '$1'.
and_op_eol -> and_op eol : next_is_eol('$1', '$2').

or_op_eol -> or_op : '$1'.
or_op_eol -> or_op eol : next_is_eol('$1', '$2').

in_op_eol -> in_op : '$1'.
in_op_eol -> in_op eol : next_is_eol('$1', '$2').

in_match_op_eol -> in_match_op : '$1'.
in_match_op_eol -> in_match_op eol : next_is_eol('$1', '$2').

type_op_eol -> type_op : '$1'.
type_op_eol -> type_op eol : next_is_eol('$1', '$2').

when_op_eol -> when_op : '$1'.
when_op_eol -> when_op eol : next_is_eol('$1', '$2').

stab_op_eol -> stab_op : '$1'.
stab_op_eol -> stab_op eol : next_is_eol('$1', '$2').

comp_op_eol -> comp_op : '$1'.
comp_op_eol -> comp_op eol : next_is_eol('$1', '$2').

rel_op_eol -> rel_op : '$1'.
rel_op_eol -> rel_op eol : next_is_eol('$1', '$2').

arrow_op_eol -> arrow_op : '$1'.
arrow_op_eol -> arrow_op eol : next_is_eol('$1', '$2').

% Dot operator

dot_op -> '.' : '$1'.
dot_op -> '.' eol : '$1'.

dot_identifier -> identifier : '$1'.
dot_identifier -> matched_expr dot_op identifier : build_dot('$2', '$1', '$3').

dot_alias -> alias : build_alias('$1').
dot_alias -> matched_expr dot_op alias : build_dot_alias('$2', '$1', '$3').
dot_alias -> matched_expr dot_op open_curly '}' : build_dot_container('$2', '$1', [], []).
dot_alias -> matched_expr dot_op open_curly container_args close_curly : build_dot_container('$2', '$1', '$4', newlines_pair('$3', '$5')).

dot_op_identifier -> op_identifier : '$1'.
dot_op_identifier -> matched_expr dot_op op_identifier : build_dot('$2', '$1', '$3').

dot_do_identifier -> do_identifier : '$1'.
dot_do_identifier -> matched_expr dot_op do_identifier : build_dot('$2', '$1', '$3').

dot_bracket_identifier -> bracket_identifier : '$1'.
dot_bracket_identifier -> matched_expr dot_op bracket_identifier : build_dot('$2', '$1', '$3').

dot_paren_identifier -> paren_identifier : '$1'.
dot_paren_identifier -> matched_expr dot_op paren_identifier : build_dot('$2', '$1', '$3').

dot_call_identifier -> dot_paren_identifier : '$1'.
dot_call_identifier -> matched_expr dot_call_op : {'.', meta_from_token('$2'), ['$1']}. % Fun/local calls

% Function calls with no parentheses

call_args_no_parens_expr -> matched_expr : '$1'.
call_args_no_parens_expr -> no_parens_expr : error_no_parens_many_strict('$1').

call_args_no_parens_comma_expr -> matched_expr ',' call_args_no_parens_expr : ['$3', '$1'].
call_args_no_parens_comma_expr -> call_args_no_parens_comma_expr ',' call_args_no_parens_expr : ['$3' | '$1'].

call_args_no_parens_all -> call_args_no_parens_one : '$1'.
call_args_no_parens_all -> call_args_no_parens_ambig : '$1'.
call_args_no_parens_all -> call_args_no_parens_many : '$1'.

call_args_no_parens_one -> call_args_no_parens_kw : ['$1'].
call_args_no_parens_one -> matched_expr : ['$1'].

%% This is the only no parens ambiguity where we don't
%% raise nor warn: "parent_call nested_call 1, 2, 3"
%% always assumes that all arguments are nested.
call_args_no_parens_ambig -> no_parens_expr : ['$1'].

call_args_no_parens_many -> matched_expr ',' call_args_no_parens_kw : ['$1', '$3'].
call_args_no_parens_many -> call_args_no_parens_comma_expr : reverse('$1').
call_args_no_parens_many -> call_args_no_parens_comma_expr ',' call_args_no_parens_kw : reverse(['$3' | '$1']).

call_args_no_parens_many_strict -> call_args_no_parens_many : '$1'.
call_args_no_parens_many_strict -> open_paren call_args_no_parens_kw close_paren : error_no_parens_strict('$1').
call_args_no_parens_many_strict -> open_paren call_args_no_parens_many close_paren : error_no_parens_strict('$1').

stab_parens_many -> open_paren call_args_no_parens_kw close_paren : ['$2'].
stab_parens_many -> open_paren call_args_no_parens_many close_paren : '$2'.

% Containers

container_expr -> matched_expr : '$1'.
container_expr -> unmatched_expr : '$1'.
container_expr -> no_parens_expr : error_no_parens_container_strict('$1').

container_args_base -> container_expr : ['$1'].
container_args_base -> container_args_base ',' container_expr : ['$3' | '$1'].

container_args -> container_args_base : reverse('$1').
container_args -> container_args_base ',' : reverse('$1').
container_args -> container_args_base ',' kw_data : reverse(['$3' | '$1']).

% Function calls with parentheses

call_args_parens_expr -> matched_expr : '$1'.
call_args_parens_expr -> unmatched_expr : '$1'.
call_args_parens_expr -> no_parens_expr : error_no_parens_many_strict('$1').

call_args_parens_base -> call_args_parens_expr : ['$1'].
call_args_parens_base -> call_args_parens_base ',' call_args_parens_expr : ['$3' | '$1'].

call_args_parens -> open_paren ')' :
                      {newlines_pair('$1', '$2'), []}.
call_args_parens -> open_paren no_parens_expr close_paren :
                      {newlines_pair('$1', '$3'), ['$2']}.
call_args_parens -> open_paren kw_call close_paren :
                      {newlines_pair('$1', '$3'), ['$2']}.
call_args_parens -> open_paren call_args_parens_base close_paren :
                      {newlines_pair('$1', '$3'), reverse('$2')}.
call_args_parens -> open_paren call_args_parens_base ',' kw_call close_paren :
                      {newlines_pair('$1', '$5'), reverse(['$4' | '$2'])}.

% KV

kw_eol -> kw_identifier : handle_literal(?exprs('$1'), '$1', [{format, keyword}]).
kw_eol -> kw_identifier eol : handle_literal(?exprs('$1'), '$1', [{format, keyword}]).
kw_eol -> kw_identifier_safe : build_quoted_atom('$1', true, [{format, keyword}]).
kw_eol -> kw_identifier_safe eol : build_quoted_atom('$1', true, [{format, keyword}]).
kw_eol -> kw_identifier_unsafe : build_quoted_atom('$1', false, [{format, keyword}]).
kw_eol -> kw_identifier_unsafe eol : build_quoted_atom('$1', false, [{format, keyword}]).

kw_base -> kw_eol container_expr : [{'$1', '$2'}].
kw_base -> kw_base ',' kw_eol container_expr : [{'$3', '$4'} | '$1'].

kw_call -> kw_base : reverse('$1').
kw_call -> kw_base ',' : warn_trailing_comma('$2'), reverse('$1').
kw_call -> kw_base ',' matched_expr : maybe_bad_keyword_call_follow_up('$2', '$1', '$3').

kw_data -> kw_base : reverse('$1').
kw_data -> kw_base ',' : reverse('$1').
kw_data -> kw_base ',' matched_expr : maybe_bad_keyword_data_follow_up('$2', '$1', '$3').

call_args_no_parens_kw_expr -> kw_eol matched_expr : {'$1', '$2'}.
call_args_no_parens_kw_expr -> kw_eol no_parens_expr : warn_nested_no_parens_keyword('$1', '$2'), {'$1', '$2'}.

call_args_no_parens_kw -> call_args_no_parens_kw_expr : ['$1'].
call_args_no_parens_kw -> call_args_no_parens_kw_expr ',' call_args_no_parens_kw : ['$1' | '$3'].
call_args_no_parens_kw -> call_args_no_parens_kw_expr ',' matched_expr : maybe_bad_keyword_call_follow_up('$2', ['$1'], '$3').

% Lists

list_args -> kw_data : '$1'.
list_args -> container_args_base : reverse('$1').
list_args -> container_args_base ',' : reverse('$1').
list_args -> container_args_base ',' kw_data : reverse('$1', '$3').

list -> open_bracket ']' : build_list('$1', [], '$2').
list -> open_bracket list_args close_bracket : build_list('$1', '$2', '$3').

% Tuple

tuple -> open_curly '}' : build_tuple('$1', [], '$2').
tuple -> open_curly kw_data '}' : bad_keyword('$1', tuple, "'{'").
tuple -> open_curly container_args close_curly :  build_tuple('$1', '$2', '$3').

% Bitstrings

bitstring -> open_bit '>>' : build_bit('$1', [], '$2').
bitstring -> open_bit kw_data '>>' : bad_keyword('$1', bitstring, "'<<'").
bitstring -> open_bit container_args close_bit : build_bit('$1', '$2', '$3').

% Map and structs

map_base_expr -> sub_matched_expr : '$1'.
map_base_expr -> at_op_eol map_base_expr : build_unary_op('$1', '$2').
map_base_expr -> unary_op_eol map_base_expr : build_unary_op('$1', '$2').
map_base_expr -> ellipsis_op map_base_expr : build_unary_op('$1', '$2').

assoc_op_eol -> assoc_op : '$1'.
assoc_op_eol -> assoc_op eol : '$1'.

assoc_expr -> matched_expr assoc_op_eol matched_expr : {'$1', '$3'}.
assoc_expr -> unmatched_expr assoc_op_eol unmatched_expr : {'$1', '$3'}.
assoc_expr -> matched_expr assoc_op_eol unmatched_expr : {'$1', '$3'}.
assoc_expr -> unmatched_expr assoc_op_eol matched_expr : {'$1', '$3'}.
assoc_expr -> map_base_expr : '$1'.

assoc_update -> matched_expr pipe_op_eol assoc_expr : {'$2', '$1', ['$3']}.
assoc_update -> unmatched_expr pipe_op_eol assoc_expr : {'$2', '$1', ['$3']}.

assoc_update_kw -> matched_expr pipe_op_eol kw_data : {'$2', '$1', '$3'}.
assoc_update_kw -> unmatched_expr pipe_op_eol kw_data : {'$2', '$1', '$3'}.

assoc_base -> assoc_expr : ['$1'].
assoc_base -> assoc_base ',' assoc_expr : ['$3' | '$1'].

assoc -> assoc_base : reverse('$1').
assoc -> assoc_base ',' : reverse('$1').

map_op -> '%{}' : '$1'.
map_op -> '%{}' eol : '$1'.

map_close -> kw_data close_curly : {'$1', '$2'}.
map_close -> assoc close_curly : {'$1', '$2'}.
map_close -> assoc_base ',' kw_data close_curly : {reverse('$1', '$3'), '$4'}.

map_args -> open_curly '}' : build_map('$1', [], '$2').
map_args -> open_curly map_close : build_map('$1', element(1, '$2'), element(2, '$2')).
map_args -> open_curly assoc_update close_curly : build_map_update('$1', '$2', '$3', []).
map_args -> open_curly assoc_update ',' close_curly : build_map_update('$1', '$2', '$4', []).
map_args -> open_curly assoc_update ',' map_close : build_map_update('$1', '$2', element(2, '$4'), element(1, '$4')).
map_args -> open_curly assoc_update_kw close_curly : build_map_update('$1', '$2', '$3', []).

map -> map_op map_args : '$2'.
map -> '%' map_base_expr map_args : {'%', meta_from_token('$1'), ['$2', '$3']}.
map -> '%' map_base_expr eol map_args : {'%', meta_from_token('$1'), ['$2', '$4']}.

Erlang code.

-define(columns(), get(elixir_parser_columns)).
-define(token_metadata(), get(elixir_token_metadata)).

-define(id(Token), element(1, Token)).
-define(location(Token), element(2, Token)).
-define(exprs(Token), element(3, Token)).
-define(meta(Node), element(2, Node)).
-define(rearrange_uop(Op), (Op == 'not' orelse Op == '!')).

-compile({inline, meta_from_token/1, meta_from_location/1, is_eol/1}).
-import(lists, [reverse/1, reverse/2]).

meta_from_token(Token) ->
  meta_from_location(?location(Token)).

meta_from_location({Line, Column, _}) ->
  case ?columns() of
    true -> [{line, Line}, {column, Column}];
    false -> [{line, Line}]
  end.

do_end_meta(Do, End) ->
  case ?token_metadata() of
    true ->
      [{do, meta_from_location(?location(Do))}, {'end', meta_from_location(?location(End))}];
    false ->
      []
  end.

meta_from_token_with_closing(Begin, End) ->
  case ?token_metadata() of
    true ->
      [{closing, meta_from_location(?location(End))} | meta_from_token(Begin)];
    false ->
      meta_from_token(Begin)
  end.

append_non_empty(Left, []) -> Left;
append_non_empty(Left, Right) -> Left ++ Right.

%% Handle metadata in literals

handle_literal(Literal, Token) ->
  handle_literal(Literal, Token, []).

handle_literal(Literal, Token, ExtraMeta) ->
  case get(elixir_literal_encoder) of
    false ->
      Literal;

    Fun ->
      Meta = ExtraMeta ++ meta_from_token(Token),
      case Fun(Literal, Meta) of
        {ok, EncodedLiteral} ->
          EncodedLiteral;
        {error, Reason} ->
          return_error(?location(Token), elixir_utils:characters_to_list(Reason) ++ [": "], "literal")
      end
  end.

handle_number(Number, Token, Original) ->
  case ?token_metadata() of
    true -> handle_literal(Number, Token, [{token, elixir_utils:characters_to_binary(Original)}]);
    false -> handle_literal(Number, Token, [])
  end.

number_value({_, {_, _, Value}, _}) ->
  Value.

%% Operators

build_op(Left, {Op, Right}) ->
  build_op(Left, Op, Right).

build_op(AST, {_Kind, Location, '//'}, Right) ->
  case AST of
    {'..', Meta, [Left, Middle]} ->
      {'..//', Meta, [Left, Middle, Right]};

    _ ->
      return_error(Location, "the range step operator (//) must immediately follow the range definition operator (..), for example: 1..9//2. If you wanted to define a default argument, use (\\\\) instead. Syntax error before: ", "'//'")
  end;

build_op({UOp, _, [Left]}, {_Kind, {Line, Column, _} = Location, 'in'}, Right) when ?rearrange_uop(UOp) ->
  %% TODO: Remove "not left in right" rearrangement on v2.0
  warn({Line, Column}, "\"not expr1 in expr2\" is deprecated, use \"expr1 not in expr2\" instead"),
  Meta = meta_from_location(Location),
  {UOp, Meta, [{'in', Meta, [Left, Right]}]};

build_op(Left, {_Kind, Location, 'not in'}, Right) ->
  Meta = meta_from_location(Location),
  {'not', Meta, [{'in', Meta, [Left, Right]}]};

build_op(Left, {_Kind, Location, Op}, Right) ->
  {Op, newlines_op(Location) ++ meta_from_location(Location), [Left, Right]}.

build_unary_op({_Kind, {Line, Column, _}, '//'}, Expr) ->
  {Outer, Inner} =
    case ?columns() of
      true -> {[{column, Column+1}], [{column, Column}]};
      false -> {[], []}
    end,
  {'/', [{line, Line} | Outer], [{'/', [{line, Line} | Inner], nil}, Expr]};

build_unary_op({_Kind, Location, Op}, Expr) ->
  {Op, meta_from_location(Location), [Expr]}.

build_nullary_op({_Kind, Location, Op}) ->
  {Op, meta_from_location(Location), []}.

build_list(Left, Args, Right) ->
  {handle_literal(Args, Left, newlines_pair(Left, Right)), ?location(Left)}.

build_tuple(Left, [Arg1, Arg2], Right) ->
  handle_literal({Arg1, Arg2}, Left, newlines_pair(Left, Right));
build_tuple(Left, Args, Right) ->
  {'{}', newlines_pair(Left, Right) ++ meta_from_token(Left), Args}.

build_bit(Left, Args, Right) ->
  {'<<>>', newlines_pair(Left, Right) ++ meta_from_token(Left), Args}.

build_map(Left, Args, Right) ->
  {'%{}', newlines_pair(Left, Right) ++ meta_from_token(Left), Args}.

build_map_update(Left, {Pipe, Struct, Map}, Right, Extra) ->
  Op = build_op(Struct, Pipe, append_non_empty(Map, Extra)),
  {'%{}', newlines_pair(Left, Right) ++ meta_from_token(Left), [Op]}.

%% Blocks

build_block(Exprs) -> build_block(Exprs, []).

build_block([{unquote_splicing, _, [_]}]=Exprs, Meta) ->
  {'__block__', Meta, Exprs};
build_block([Expr], _Meta) ->
  Expr;
build_block(Exprs, Meta) ->
  {'__block__', Meta, Exprs}.

%% Newlines

newlines_pair(Left, Right) ->
  case ?token_metadata() of
    true ->
      newlines(?location(Left), [{closing, meta_from_location(?location(Right))}]);
    false ->
      []
  end.

newlines_op(Location) ->
  case ?token_metadata() of
    true -> newlines(Location, []);
    false -> []
  end.

next_is_eol(Token, {_, {_, _, Count}}) ->
  {Line, Column, _} = ?location(Token),
  setelement(2, Token, {Line, Column, Count}).

newlines({_, _, Count}, Meta) when is_integer(Count) and (Count > 0) ->
  [{newlines, Count} | Meta];
newlines(_, Meta) ->
  Meta.

annotate_eoe(Token, Stack) ->
  case ?token_metadata() of
    true ->
      case {Token, Stack} of
        {{_, Location}, [{'->', StabMeta, [StabArgs, {Left, Meta, Right}]} | Rest]} when is_list(Meta) ->
          [{'->', StabMeta, [StabArgs, {Left, [{end_of_expression, end_of_expression(Location)} | Meta], Right}]} | Rest];

        {{_, Location}, [{Left, Meta, Right} | Rest]} when is_list(Meta), Left =/= '->' ->
          [{Left, [{end_of_expression, end_of_expression(Location)} | Meta], Right} | Rest];

        _ ->
          Stack
      end;
    false ->
      Stack
  end.

end_of_expression({_, _, Count} = Location) when is_integer(Count) ->
  [{newlines, Count} | meta_from_location(Location)];
end_of_expression(Location) ->
  meta_from_location(Location).

%% Dots

build_alias({'alias', Location, Alias}) ->
  Meta = meta_from_location(Location),
  MetaWithExtra =
    case ?token_metadata() of
      true -> [{last, meta_from_location(Location)} | Meta];
      false -> Meta
    end,
  {'__aliases__', MetaWithExtra, [Alias]}.

build_dot_alias(_Dot, {'__aliases__', Meta, Left}, {'alias', SegmentLocation, Right}) ->
  MetaWithExtra =
    case ?token_metadata() of
      true -> lists:keystore(last, 1, Meta, {last, meta_from_location(SegmentLocation)});
      false -> Meta
    end,
  {'__aliases__', MetaWithExtra, Left ++ [Right]};
build_dot_alias(_Dot, Atom, Right) when is_atom(Atom) ->
  error_bad_atom(Right);
build_dot_alias(Dot, Expr, {'alias', SegmentLocation, Right}) ->
  Meta = meta_from_token(Dot),
  MetaWithExtra =
    case ?token_metadata() of
      true -> [{last, meta_from_location(SegmentLocation)} | Meta];
      false -> Meta
    end,
  {'__aliases__', MetaWithExtra, [Expr, Right]}.

build_dot_container(Dot, Left, Right, Extra) ->
  Meta = meta_from_token(Dot),
  {{'.', Meta, [Left, '{}']}, Extra ++ Meta, Right}.

build_dot(Dot, Left, {_, Location, _} = Right) ->
  Meta = meta_from_token(Dot),
  IdentifierLocation = meta_from_location(Location),
  {'.', Meta, IdentifierLocation, [Left, extract_identifier(Right)]}.

extract_identifier({Kind, _, Identifier}) when
    Kind == identifier; Kind == bracket_identifier; Kind == paren_identifier;
    Kind == do_identifier; Kind == op_identifier ->
  Identifier.

%% Identifiers

build_nested_parens(Dot, Args1, {Args2Meta, Args2}, {BlockMeta, Block}) ->
  Identifier = build_parens(Dot, Args1, {[], []}),
  Meta = BlockMeta ++ Args2Meta ++ ?meta(Identifier),
  {Identifier, Meta, append_non_empty(Args2, Block)}.

build_parens(Expr, {ArgsMeta, Args}, {BlockMeta, Block}) ->
  {BuiltExpr, BuiltMeta, BuiltArgs} = build_call(Expr, append_non_empty(Args, Block)),
  {BuiltExpr, BlockMeta ++ ArgsMeta ++ BuiltMeta, BuiltArgs}.

build_no_parens_do_block(Expr, Args, {BlockMeta, Block}) ->
  {BuiltExpr, BuiltMeta, BuiltArgs} = build_call(Expr, Args ++ Block),
  {BuiltExpr, BlockMeta ++ BuiltMeta, BuiltArgs}.

build_no_parens(Expr, Args) ->
  build_call(Expr, Args).

build_identifier({'.', Meta, IdentifierLocation, DotArgs}) ->
  {{'.', Meta, DotArgs}, [{no_parens, true} | IdentifierLocation], []};

build_identifier({'.', Meta, _} = Dot) ->
  {Dot, [{no_parens, true} | Meta], []};

build_identifier({_, Location, Identifier}) ->
  {Identifier, meta_from_location(Location), nil}.

build_call({'.', Meta, IdentifierLocation, DotArgs}, Args) ->
  {{'.', Meta, DotArgs}, IdentifierLocation, Args};

build_call({'.', Meta, _} = Dot, Args) ->
  {Dot, Meta, Args};

build_call({op_identifier, Location, Identifier}, [Arg]) ->
  {Identifier, [{ambiguous_op, nil} | meta_from_location(Location)], [Arg]};

build_call({_, Location, Identifier}, Args) ->
  {Identifier, meta_from_location(Location), Args}.

%% Fn

build_fn(Fn, Stab, End) ->
  case check_stab(Stab, none) of
    stab ->
      Meta = newlines_op(?location(Fn)) ++ meta_from_token_with_closing(Fn, End),
      {fn, Meta, collect_stab(Stab, [], [])};
    block ->
      return_error(?location(Fn), "expected anonymous functions to be defined with -> inside: ", "'fn'")
  end.

%% Access

build_access_arg(Left, Args, Right) ->
  {Args, newlines_pair(Left, Right) ++ meta_from_token(Left)}.

build_access(Expr, {List, Meta}) ->
  {{'.', Meta, ['Elixir.Access', get]}, Meta, [Expr, List]}.

%% Interpolation aware

build_sigil({sigil, Location, Atom, Parts, Modifiers, Indentation, Delimiter}) ->
  Meta = meta_from_location(Location),
  MetaWithDelimiter = [{delimiter, Delimiter} | Meta],
  MetaWithIndentation = meta_with_indentation(Meta, Indentation),
  {Atom,
   MetaWithDelimiter,
   [{'<<>>', MetaWithIndentation, string_parts(Parts)}, Modifiers]}.

meta_with_indentation(Meta, nil) ->
  Meta;
meta_with_indentation(Meta, Indentation) ->
  [{indentation, Indentation} | Meta].

meta_with_from_brackets({List, Meta}) ->
  {List, [{from_brackets, true} | Meta]}.

build_bin_heredoc({bin_heredoc, Location, Indentation, Args}) ->
  ExtraMeta =
    case ?token_metadata() of
      true -> [{delimiter, <<$", $", $">>}, {indentation, Indentation}];
      false -> []
    end,
  build_bin_string({bin_string, Location, Args}, ExtraMeta).

build_list_heredoc({list_heredoc, Location, Indentation, Args}) ->
  ExtraMeta =
    case ?token_metadata() of
      true -> [{delimiter, <<$', $', $'>>}, {indentation, Indentation}];
      false -> []
    end,
  build_list_string({list_string, Location, Args}, ExtraMeta).

build_bin_string({bin_string, _Location, [H]} = Token, ExtraMeta) when is_binary(H) ->
  handle_literal(H, Token, ExtraMeta);
build_bin_string({bin_string, Location, Args}, ExtraMeta) ->
  Meta =
    case ?token_metadata() of
      true -> ExtraMeta ++ meta_from_location(Location);
      false -> meta_from_location(Location)
    end,
  {'<<>>', Meta, string_parts(Args)}.

build_list_string({list_string, _Location, [H]} = Token, ExtraMeta) when is_binary(H) ->
  handle_literal(elixir_utils:characters_to_list(H), Token, ExtraMeta);
build_list_string({list_string, Location, Args}, ExtraMeta) ->
  Meta = meta_from_location(Location),
  MetaWithExtra =
    case ?token_metadata() of
      true -> ExtraMeta ++ Meta;
      false -> Meta
    end,
  {{'.', Meta, ['Elixir.List', to_charlist]}, MetaWithExtra, [charlist_parts(Args)]}.

build_quoted_atom({_, _Location, [H]} = Token, Safe, ExtraMeta) when is_binary(H) ->
  Op = binary_to_atom_op(Safe),
  handle_literal(erlang:Op(H, utf8), Token, ExtraMeta);
build_quoted_atom({_, Location, Args}, Safe, ExtraMeta) ->
  Meta = meta_from_location(Location),
  MetaWithExtra =
    case ?token_metadata() of
      true -> ExtraMeta ++ Meta;
      false -> Meta
    end,
  {{'.', Meta, [erlang, binary_to_atom_op(Safe)]}, MetaWithExtra, [{'<<>>', Meta, string_parts(Args)}, utf8]}.

binary_to_atom_op(true)  -> binary_to_existing_atom;
binary_to_atom_op(false) -> binary_to_atom.

charlist_parts(Parts) ->
  [charlist_part(Part) || Part <- Parts].
charlist_part(Binary) when is_binary(Binary) ->
  Binary;
charlist_part({Begin, End, Tokens}) ->
  Form = string_tokens_parse(Tokens),
  Meta = meta_from_location(Begin),
  MetaWithExtra =
    case ?token_metadata() of
      true -> [{closing, meta_from_location(End)} | Meta];
      false -> Meta
    end,
  {{'.', Meta, ['Elixir.Kernel', to_string]}, [{from_interpolation, true} | MetaWithExtra], [Form]}.

string_parts(Parts) ->
  [string_part(Part) || Part <- Parts].
string_part(Binary) when is_binary(Binary) ->
  Binary;
string_part({Begin, End, Tokens}) ->
  Form = string_tokens_parse(Tokens),
  Meta = meta_from_location(Begin),
  MetaWithExtra =
    case ?token_metadata() of
      true -> [{closing, meta_from_location(End)} | Meta];
      false -> Meta
    end,
  {'::', Meta, [{{'.', Meta, ['Elixir.Kernel', to_string]}, [{from_interpolation, true} | MetaWithExtra], [Form]}, {binary, Meta, nil}]}.

string_tokens_parse(Tokens) ->
  case parse(Tokens) of
    {ok, Forms} -> Forms;
    {error, _} = Error -> throw(Error)
  end.

delimiter(Delimiter) ->
  case ?token_metadata() of
    true -> [{delimiter, Delimiter}];
    false -> []
  end.

%% Keywords

check_stab([{'->', _, [_, _]}], _) -> stab;
check_stab([], none) -> block;
check_stab([_], none) -> block;
check_stab([_], Meta) -> error_invalid_stab(Meta);
check_stab([{'->', Meta, [_, _]} | T], _) -> check_stab(T, Meta);
check_stab([_ | T], MaybeMeta) -> check_stab(T, MaybeMeta).

build_stab(Stab) ->
  case check_stab(Stab, none) of
    block -> build_block(reverse(Stab));
    stab -> collect_stab(Stab, [], [])
  end.

build_paren_stab(_Before, [{Op, _, [_]}]=Exprs, _After) when ?rearrange_uop(Op) ->
  {'__block__', [], Exprs};
build_paren_stab(Before, Stab, After) ->
  case check_stab(Stab, none) of
    block -> build_block(reverse(Stab), meta_from_token_with_closing(Before, After));
    stab -> handle_literal(collect_stab(Stab, [], []), Before, newlines_pair(Before, After))
  end.

collect_stab([{'->', Meta, [Left, Right]} | T], Exprs, Stabs) ->
  Stab = {'->', Meta, [Left, build_block([Right | Exprs])]},
  collect_stab(T, [], [Stab | Stabs]);

collect_stab([H | T], Exprs, Stabs) ->
  collect_stab(T, [H | Exprs], Stabs);

collect_stab([], [], Stabs) ->
  Stabs.

%% Every time the parser sees a (unquote_splicing())
%% it assumes that a block is being spliced, wrapping
%% the splicing in a __block__. But in the stab clause,
%% we can have (unquote_splicing(1, 2, 3)) -> :ok, in such
%% case, we don't actually want the block, since it is
%% an arg style call. unwrap_splice unwraps the splice
%% from such blocks.
unwrap_splice([{'__block__', _, [{unquote_splicing, _, _}] = Splice}]) ->
  Splice;
unwrap_splice(Other) ->
  Other.

unwrap_when(Args) ->
  case elixir_utils:split_last(Args) of
    {Start, {'when', Meta, [_, _] = End}} ->
      [{'when', Meta, Start ++ End}];
    {_, _} ->
      Args
  end.

%% Warnings and errors

return_error({Line, Column, _}, ErrorMessage, ErrorToken) ->
  return_error([{line, Line}, {column, Column}], [ErrorMessage, ErrorToken]).

%% We should prefer to use return_error as it includes
%% Line and Column but that's not always possible.
return_error_with_meta(Meta, ErrorMessage, ErrorToken) ->
  return_error(Meta, [ErrorMessage, ErrorToken]).

error_invalid_stab(MetaStab) ->
  return_error_with_meta(MetaStab,
    "unexpected operator ->. If you want to define multiple clauses, the first expression must use ->. "
    "Syntax error before: ", "'->'").

error_bad_atom(Token) ->
  return_error(?location(Token), "atom cannot be followed by an alias. "
    "If the '.' was meant to be part of the atom's name, "
    "the atom name must be quoted. Syntax error before: ", "'.'").

bad_keyword(Token, Context, StartString) ->
  return_error(?location(Token),
    "unexpected keyword list inside " ++ atom_to_list(Context) ++ ". "
    "Did you mean to write a map (using %{...}) or a list (using [...]) instead? "
    "Syntax error after: ", StartString).

maybe_bad_keyword_call_follow_up(_Token, KW, {'__cursor__', _, []} = Expr) ->
  reverse([Expr | KW]);
maybe_bad_keyword_call_follow_up(Token, _KW, _Expr) ->
  return_error(?location(Token),
    "unexpected expression after keyword list. Keyword lists must always come as the last argument. Therefore, this is not allowed:\n\n"
    "    function_call(1, some: :option, 2)\n\n"
    "Instead, wrap the keyword in brackets:\n\n"
    "    function_call(1, [some: :option], 2)\n\n"
    "Syntax error after: ", "','").

maybe_bad_keyword_data_follow_up(_Token, KW, {'__cursor__', _, []} = Expr) ->
  reverse([Expr | KW]);
maybe_bad_keyword_data_follow_up(Token, _KW, _Expr) ->
  return_error(?location(Token),
    "unexpected expression after keyword list. Keyword lists must always come last in lists and maps. Therefore, this is not allowed:\n\n"
    "    [some: :value, :another]\n"
    "    %{some: :value, another => value}\n\n"
    "Instead, reorder it to be the last entry:\n\n"
    "    [:another, some: :value]\n"
    "    %{another => value, some: :value}\n\n"
    "Syntax error after: ", "','").

error_no_parens_strict(Token) ->
  return_error(?location(Token), "unexpected parentheses. If you are making a "
    "function call, do not insert spaces between the function name and the "
    "opening parentheses. Syntax error before: ", "'('").

error_no_parens_many_strict(Node) ->
  return_error_with_meta(?meta(Node),
    "unexpected comma. Parentheses are required to solve ambiguity in nested calls.\n\n"
    "This error happens when you have nested function calls without parentheses. "
    "For example:\n\n"
    "    parent_call a, nested_call b, c, d\n\n"
    "In the example above, we don't know if the parameters \"c\" and \"d\" apply "
    "to the function \"parent_call\" or \"nested_call\". You can solve this by "
    "explicitly adding parentheses:\n\n"
    "    parent_call a, nested_call(b, c, d)\n\n"
    "Or by adding commas (in case a nested call is not intended):\n\n"
    "    parent_call a, nested_call, b, c, d\n\n"
    "Elixir cannot compile otherwise. Syntax error before: ", "','").

error_no_parens_container_strict(Node) ->
  return_error_with_meta(?meta(Node),
    "unexpected comma. Parentheses are required to solve ambiguity inside containers.\n\n"
    "This error may happen when you forget a comma in a list or other container:\n\n"
    "    [a, b c, d]\n\n"
    "Or when you have ambiguous calls:\n\n"
    "    [function a, b, c]\n\n"
    "In the example above, we don't know if the values \"b\" and \"c\" "
    "belongs to the list or the function \"function\". You can solve this by explicitly "
    "adding parentheses:\n\n"
    "    [one, function(a, b, c)]\n\n"
    "Elixir cannot compile otherwise. Syntax error before: ", "','").

error_too_many_access_syntax(Comma) ->
  return_error(?location(Comma), "too many arguments when accessing a value. "
    "The value[key] notation in Elixir expects either a single argument or a keyword list. "
    "The following examples are allowed:\n\n"
    "    value[one]\n"
    "    value[one: 1, two: 2]\n"
    "    value[[one, two, three]]\n\n"
    "These are invalid:\n\n"
    "    value[1, 2, 3]\n"
    "    value[one, two, three]\n\n"
    "Syntax error after: ", "','").

error_invalid_kw_identifier({_, Location, do}) ->
  return_error(Location, elixir_tokenizer:invalid_do_error("unexpected keyword: "), "do:");
error_invalid_kw_identifier({_, Location, KW}) ->
  return_error(Location, "syntax error before: ", "'" ++ atom_to_list(KW) ++ ":'").

%% TODO: Make this an error on v2.0
warn_trailing_comma({',', {Line, Column, _}}) ->
  warn({Line, Column}, "trailing commas are not allowed inside function/macro call arguments").

%% TODO: Make this an error on v2.0
warn_pipe({arrow_op, {Line, Column, _}, Op}, {_, [_ | _], [_ | _]}) ->
  warn(
    {Line, Column},
    io_lib:format(
      "parentheses are required when piping into a function call. For example:\n\n"
      "    foo 1 ~ts bar 2 ~ts baz 3\n\n"
      "is ambiguous and should be written as\n\n"
      "    foo(1) ~ts bar(2) ~ts baz(3)\n\n"
      "Ambiguous pipe found at:",
      [Op, Op, Op, Op]
    )
  );
warn_pipe(_Token, _) ->
  ok.

%% TODO: Make this an error on v2.0
warn_no_parens_after_do_op({{_Type, Location, Op}, _}) ->
  {Line, _, _} = Location,

  warn(
    Line,
    "missing parentheses on expression following operator \"" ++ atom_to_list(Op) ++ "\", "
    "you must add parentheses to avoid ambiguities"
  ).

%% TODO: Make this an error on v2.0
warn_nested_no_parens_keyword(Key, Value) when is_atom(Key) ->
  {line, Line} = lists:keyfind(line, 1, ?meta(Value)),
  warn(
    Line,
    "missing parentheses for expression following \"" ++ atom_to_list(Key) ++ ":\" keyword. "
    "Parentheses are required to solve ambiguity inside keywords.\n\n"
    "This error happens when you have function calls without parentheses inside keywords. "
    "For example:\n\n"
    "    function(arg, one: nested_call a, b, c)\n"
    "    function(arg, one: if expr, do: :this, else: :that)\n\n"
    "In the examples above, we don't know if the arguments \"b\" and \"c\" apply "
    "to the function \"function\" or \"nested_call\". Or if the keywords \"do\" and "
    "\"else\" apply to the function \"function\" or \"if\". You can solve this by "
    "explicitly adding parentheses:\n\n"
    "    function(arg, one: if(expr, do: :this, else: :that))\n"
    "    function(arg, one: nested_call(a, b, c))\n\n"
    "Ambiguity found at:"
  );

% Key might not be an atom when using literal_encoder, we just skip the warning
warn_nested_no_parens_keyword(_Key, _Value) ->
  ok.

warn_empty_paren({_, {Line, Column, _}}) ->
  warn(
    {Line, Column},
    "invalid expression (). "
    "If you want to invoke or define a function, make sure there are "
    "no spaces between the function name and its arguments. If you wanted "
    "to pass an empty block or code, pass a value instead, such as a nil or an atom"
  ).

warn_empty_stab_clause({stab_op, {Line, Column, _}, '->'}) ->
  warn(
    {Line, Column},
    "an expression is always required on the right side of ->. "
    "Please provide a value after ->"
  ).

warn(LineColumn, Message) ->
  case get(elixir_parser_warning_file) of
    nil -> ok;
    File -> elixir_errors:erl_warn(LineColumn, File, Message)
  end.
