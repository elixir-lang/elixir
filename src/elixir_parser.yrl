% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  grammar
  expr_list
  expr
  max_expr
  add_op
  mult_op
  break
  .

Terminals
  number
  '+' '-' '*' '/'
  eol ';'
  .

Rootsymbol grammar.

Left     110 add_op.
Left     120 mult_op.

%%% MAIN FLOW OF EXPRESSIONS

grammar -> expr_list : '$1'.
grammar -> '$empty' : [{atom, 0, nil}].

% List of expressions delimited by break
expr_list -> eol : [].
expr_list -> expr : ['$1'].
expr_list -> expr break : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr break expr_list : ['$1'|'$3'].

expr -> expr add_op expr : build_op('$2', '$1', '$3').
expr -> expr mult_op expr : build_op('$2', '$1', '$3').
expr -> max_expr : '$1'.

max_expr -> number : ?exprs('$1').

%% Helpers

break -> eol : '$1'.
break -> ';' : { eol, ?line('$1') }.

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.


Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(exprs(Node), element(3, Node)).

% The following directive is needed for (significantly) faster compilation
% of the generated .erl file by the HiPE compiler. Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

build_op(Op, Left, Right) ->
  { ?op(Op), ?line(Op), Left, Right }.