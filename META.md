# Meta-programming in Elixir

Elixir is an homoiconic language. Any Elixir program can be represented using its own data structures. This document describes the Elixir language specification for such data structures.

The building block of Elixir homoiconicity is a tuple with three elements:

    { :sum, 1, [1, 2, 3] }

The tuple above represents a function call to sum passing 1, 2 and 3 as arguments.

The first element of the tuple is always an atom or another tuple in the same representation.

The second element of the tuple is always an integer representing the line number.

The third element of the tuple are the arguments for the function call. The third argument may also be false, meaning that it represents either a variable or a function call. It is up to Elixir interpreters (or your own macros) to decide it.

Overall, we have:

    { Atom | Tuple, Integer, List | false }

You can get the representation of any expression by using the quote macro:

    quote sum(1, 2, 3)
    ;; => { :sum, 0, [1, 2, 3] }

Besides the tuple, Elixir has a few macro literals. Macro literals are elements that when quoted return themselves. They are:

    :sum         ;; Atoms
    1            ;; Integers
    2.0          ;; Floats
    [1,2]        ;; Lists
    {key, value} ;; Key-value pairs (i.e. a tuple with two elements)

While most homoiconic programming languages requires you to group a list of expressions, Elixir automatically handles such scenarios by the use of parenthesis:

    quote((
      1
      2
      3
    ))
    ;; => { :block, 0, [1,2,3] }

