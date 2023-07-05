# Design-related anti-patterns

This document outlines anti-patterns related to your modules, functions, and the role they
play within a codebase.

## Primitive obsession

TODO

## Boolean obsession

TODO

## Working with invalid data

  * **Problem:** This anti-pattern refers to a function that does not validate its parameters' types and therefore can produce internal non-predicted behavior. When an error is raised inside a function due to an invalid parameter value, this can confuse the developers and make it harder to locate and fix the error.

  * **Example:** An example of this anti-pattern is when a function receives an invalid parameter and then passes it to other functions, either from the same library or a third-party. This will cause an error raised deep inside, which may be confusing for the developer who is working with invalid data. As shown next, the function `foo/1` is a user-facing API which doesn't validate its parameters at the boundary. In this way, it is possible that invalid data will be passed through, causing a "mysterious error".

    ```elixir
    defmodule MyLibrary do
      def foo(invalid_data) do
        ...
        MyLibrary.Internal.sum(1, invalid_data)
        ...
      end
    end

    #...Use examples...

    # with valid data is ok
    iex(1)> MyLibrary.foo(2)
    3

    #with invalid data cause a confusing error deep inside
    iex(2)> MyLibrary.foo("José")
    ** (ArithmeticError) bad argument in arithmetic expression: 1 + "José"
      :erlang.+(1, "José")
      my_library.ex:4: MyLibrary.Internal.sum/2
    ```

  * **Refactoring:** To remove this anti-pattern, the client code must validate input parameters at the boundary with the user, via guard clauses or pattern matching. This will prevent errors from occurring deeply, making them easier to understand. This refactoring will also allow libraries to be implemented without worrying about creating internal protection mechanisms. The next code illustrates the refactoring of `foo/1`, removing this anti-pattern:

    ```elixir
    defmodule MyLibrary do
      def foo(data) when is_integer(data) do
        ...
        MyLibrary.Internal.sum(1, data)
        ...
      end
    end

    #...Use examples...

    #with valid data is ok
    iex(1)> MyLibrary.foo(2)
    3

    # with invalid data errors are easy to locate and fix
    iex(2)> MyLibrary.foo("José")
    ** (FunctionClauseError) no function clause matching in MyLibrary.foo/1
      The following arguments were given to MyLibrary.foo/1:
          # 1
          "José"
      my_library.ex:2: MyLibrary.foo/1
    ```

## Alternative return types

TODO

## Unrelated multi-clause function

TODO

## Feature envy

TODO

## Excessive side-effects

TODO

## Using exceptions for control-flow

TODO

## Using application configuration for libraries

TODO
