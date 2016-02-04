# Scoping Rules

This document will describe Elixir scoping rules both for variables and its lexical directives.

## Variable scoping

Elixir constructs can be split in three categories:

  * The ones that do not introduce a new scope (like `if`/`case`/`cond`/`receive`)
  * The ones that introduce a new scope (like `fn`/`try`/`for`/`with`)
  * The ones that introduce an empty new scope (like `def`/`defp`/`defmacro`)

For instance, `if` does not introduce a new scope. Therefore rebinding a variable inside `if` "inner" scope will affect its "outer" scope:

    x = 0
    if true do
      x = 1
    end
    x #=> 1

Since "if" does not introduce a new scope, there is no "inner" and "outer" in this case. It has been the same scope all along.

Constructs like `fn` introduce a new scope, on top of the current one. Defining a variable inside `fn` won't affect its outer scope:

    x = 0
    (fn -> x = 1 end).()
    x #=> 0

Besides the basic constructs like `fn`, `for` and `try`, any other construct built on top of those will keep those properties. For example, `defmodule` is implemented on top of `fn`:

    x = 0
    defmodule Foo do
      x = 1
    end
    x #=> 0

Finally, there are constructs that introduce a new *empty* scope. Such as `def` and friends that are used to define functions in modules:

    defmodule Bar do
      x = 0
      def bar do
        x #=> is not defined here
      end
    end

Those are the three categories regarding variables scopes in Elixir. Any other construct will be built on top of those basic rules and consequently belong to one of those three categories.

## Lexical scoping

Elixir's `import`, `alias` and `require` follow lexical scoping rules. This means it is possible to determine the scope of those constructs only by looking at the source code.

When you `alias` a module, it will affect all upcoming code in the same code branch:

    # U is not available here
    alias URI, as: U
    # U is available here

If you alias at the top of a file, the alias will be available inside of any module, as well as any function or construct in that file:

    alias URI, as: U        T -----------------+
                                               |
    defmodule M do            M -------------+ | 
      @moduledoc "M module"                  | | 
                                             | | 
      def abs(n) do             F ---------+ | | # T: top level / file scope
        cond do                   C -----+ | | | # M: module's scope
          n < 0  -> -n                   | | | | # F: function clause scope
          n >= 0 -> n                    | | | | # C: cond's scope
          _                              | | | |
        end                         -----+ | | | 
      end                         ---------+ | | 
    end                         -------------+ | 
                              -----------------+

On the other hand, importing, aliasing or requiring a module in a nested scope won't affect the outer one. For example, aliasing inside the module scope won't affect the file scope:

    # U is NOT available here

    defmodule M do
      alias URI, as: U
      # U is available here

      def my_fun do
        # U is available here
      end
    end

    # U is NOT available here

Similarly, applying any of those directives inside a function will be available to the expressions inside that particular function:

    defmodule M do
      def my_fun do
        alias URI, as: U
        # U is available here
      end

      def other_fun do
        # U is NOT available here
      end
    end

The same is true for constructs with multiple clauses like `case`, `cond` and `fn`. The directive will only affect the clause it is in: 

    # U is NOT available here

    cond do
      n < 0 ->
        alias URI, as: U
        # U is available here
      n >= 0 ->
        # U is NOT available here
    end

    # U is NOT available here

All of those rules are tied to Elixir syntax. `do`/`end` blocks define precisely where each scope starts and ends. Constructs with multiple clauses are properly indented on each clause, making it straight-forward for Elixir developers to understand at a glance the effect of Elixir directives.
