# Writing Documentation

Elixir treats documentation as a first-class citizen. This means documentation should be easy to write and easy to read. In this document you will learn how to write documentation in Elixir, covering constructs like module attributes, style practices and doctests.

## Markdown

Elixir documentation is written using Markdown. There are plenty of guides on Markdown online, we recommend the ones available at GitHub as a getting started point:

  * [Basic writing and formatting syntax](https://help.github.com/articles/basic-writing-and-formatting-syntax/)
  * [Mastering Markdown](https://guides.github.com/features/mastering-markdown/)

## Module Attributes

Documentation in Elixir is usually attached to module attributes. Let's see an example:

    defmodule MyApp.Hello do
      @moduledoc """
      This is the Hello module.
      """
      @moduledoc since: "1.0.0"

      @doc """
      Says hello to the given `name`.

      Returns `:ok`.

      ## Examples

          iex> MyApp.Hello.world(:john)
          :ok

      """
      @doc since: "1.3.0"
      def world(name) do
        IO.puts "hello #{name}"
      end
    end

The `@moduledoc` attribute is used to add documentation to the module. `@doc` is used before a function to provide documentation for it. Besides the attributes above, `@typedoc` can also be used to attach documentation to types defined as part of typespecs. Elixir also allows metadata to be attached to documentation, by passing a keyword list to `@doc` and friends.

## Function Arguments

When documenting a function, argument names are inferred by the compiler. For example:

    def size(%{size: size}) do
      size
    end

The compiler will infer this argument as `map`. Sometimes the inference will be suboptimal, especially if the function contains multiple clauses with the argument matching on different values each time. You can specify the proper names for documentation by declaring only the function head at any moment before the implementation:

    def size(map_with_size)
    def size(%{size: size}) do
      size
    end

## Documentation metadata

Elixir allows developers to attach arbitrary metadata to the documentation. This is done by passing a keyword list to the relevant attribute (such as `@moduledoc`, `@typedoc`, and `@doc`). A commonly used metadata is `:since`, which annotates in which version that particular module, function, type, or callback was added, as shown in the example above.

Another common metadata is `:deprecated`, which emits a warning in the documentation, explaining that its usage of is discouraged:

    @doc deprecated: "Use Foo.bar/2 instead"

Note the `:deprecated` key does not warn when a developer invokes the functions. If you want the code to also emit a warning, you can use the `@deprecated` attribute:

    @deprecated "Use Foo.bar/2 instead"

Metadata can have any key. Documentation tools often use metadata to provide more data to readers and to enrich the user experience.

## Recommendations

When writing documentation:

  * Keep the first paragraph of the documentation concise and simple, typically one-line. Tools like [ExDoc](https://github.com/elixir-lang/ex_doc/) use the first line to generate a summary.

  * Reference modules by their full name.

    Markdown uses backticks (`` ` ``) to quote code. Elixir builds on top of that to automatically generate links when module or function names are referenced. For this reason, always use full module names. If you have a module called `MyApp.Hello`, always reference it as `` `MyApp.Hello` `` and never as `` `Hello` ``.

  * Reference functions by name and arity if they are local, as in `` `world/1` ``, or by module, name and arity if pointing to an external module: `` `MyApp.Hello.world/1` ``.

  * Reference a `@callback` by prepending `c:`, as in `` `c:world/1` ``.

  * Reference a `@type` by prepending `t:`, as in `` `t:values/0` ``.

  * Start new sections with second level Markdown headers `##`. First level headers are reserved for module and function names.

  * Place documentation before the first clause of multi-clause functions. Documentation is always per function and arity and not per clause.
  
  * Use the `:since` key in the documentation metadata to annotate whenever new functions or modules are added to your API.

## Doctests

We recommend that developers include examples in their documentation, often under their own `## Examples` heading. To ensure examples do not get out of date, Elixir's test framework (ExUnit) provides a feature called doctests that allows developers to test the examples in their documentation. Doctests work by parsing out code samples starting with `iex>` from the documentation. You can read more about it at `ExUnit.DocTest`.

Notice doctests have limitations. When you cannot doctest a function, because it relies on state or side-effects, we recommend developers include examples directly without the `iex>` prompt.

## Documentation != Code comments

Elixir treats documentation and code comments as different concepts. Documentation is an explicit contract between you and users of your Application Programming Interface (API), be them third-party developers, co-workers, or your future self. Modules and functions must always be documented if they are part of your API.

Code comments are aimed at developers reading the code. They are useful to mark improvements, leave notes (for example, why you had to resort to a workaround due to a bug in a library), and so forth. Code comments are tied to the source code. You can completely rewrite a function, remove all existing code comments, and it will continue to behave the same, with no change to either its behaviour or its documentation.

Because private functions cannot be accessed externally, Elixir will warn if a private function has a `@doc` attribute and will discard its content. However, you can add code comments to private functions, as with any other piece of code, and we recommend developers to do so whenever they believe it will add relevant information to the readers and maintainers of such code.

Finally, beware of redundant code comments, such as code comments that are describing the exact same as the code:

    # Total is the sum of the batch and individual entries
    total = batch_sum + individual_sum

In summary, documentation is a contract with users of your API, who may not necessarily have access to the source code; whereas code comments are for those who interact directly with the source. You can learn and express different guarantees about your software by separating those two concepts.

## Hiding Internal Modules and Functions

Besides the modules and functions libraries provide as part of their public interface, libraries may also implement important functionality that is not part of their API. While these modules and functions can be accessed, they are meant to be internal to the library and thus should not have documentation for end users.

Conveniently, Elixir allows developers to hide modules and functions from the documentation, by setting `@doc false` to hide a particular function, or `@moduledoc false` to hide the whole module. If a module is hidden, you may even document the functions in the module, but the module itself won't be listed in the documentation:

    defmodule MyApp.Hidden do
      @moduledoc false

      @doc """
      This function won't be listed in docs.
      """
      def function_that_wont_be_listed_in_docs do
        # ...
      end
    end

However, keep in mind that adding `@doc false` does not make the function private. The function above can still be invoked as `MyApp.Sample.add(1, 2)`. Not only that, if `MyApp.Sample` is imported, the `add/2` function will also be imported into the caller. For those reasons, be cautious when adding `@doc false` to functions, instead use one of these two options:

  * Move the undocumented function to a module with `@moduledoc false`, like `MyApp.Hidden`, ensuring the function won't be accidentally exposed or imported. Remember you can use `@moduledoc false` to hide a whole module and still document each function with `@doc`. Tools will still ignore the module.

  * Start the function name with one or two underscores, for example, `__add__/2`, and add `@doc false`. The compiler does not import functions with leading underscores and they hint to anyone reading the code of their intended private usage.

## Code.fetch_docs/1

Elixir stores documentation inside pre-defined chunks in the bytecode. It can be accessed from Elixir by using the `Code.fetch_docs/1` function. This also means documentation is only accessed when required and not when modules are loaded by the Virtual Machine. The only downside is that modules defined in-memory, like the ones defined in IEx, cannot have their documentation accessed as they do not have their bytecode written to disk.
