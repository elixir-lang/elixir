% Grammar for the Elixir language done with yecc
% Copyright (C) 2011 Jose Valim

Nonterminals
  arithmetic
  add_op
  mult_op
  number
  .

Terminals
  float integer
  '+' '-' '*' '/' '(' ')'
  .

Rootsymbol arithmetic.

Left 100 add_op.
Left 200 mult_op.

arithmetic -> arithmetic add_op arithmetic :
  { binary_op, ?line('$1'), ?op('$2'), '$1', '$3' }.

arithmetic -> arithmetic mult_op arithmetic :
  { binary_op, ?line('$1'), ?op('$2'), '$1', '$3' }.

arithmetic -> '(' arithmetic ')' : '$2'.
arithmetic -> number : '$1'.

%% Numbers
number -> float   : '$1'.
number -> integer : '$1'.

%% Addition operators
add_op -> '+' : '$1'.
add_op -> '-' : '$1'.

%% Multiplication operators
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.

Erlang code.

-define(op(Node), element(1, Node)).
-define(line(Node), element(2, Node)).
-define(char(Node), element(3, Node)).