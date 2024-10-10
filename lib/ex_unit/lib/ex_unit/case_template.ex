defmodule ExUnit.CaseTemplate do
  @moduledoc """
  Defines a module template to be used throughout your test suite.

  This is useful when there are a set of setup callbacks or a set
  of functions that should be shared between test modules.

  Let's imagine that you create a `MyCase` module that calls `use
  ExUnit.CaseTemplate`. When a test case module calls `use MyCase`, the
  following things hold true:

    * All the functionality that `MyCase` would have had available from
      `use ExUnit.Case` is available (same as if `MyCase` called `use
      ExUnit.Case` directly)

    * The `setup` and `setup_all` callbacks that you define in `MyCase`
      get used in the test case module

  The options that you pass to `use MyCase` get also passed to `use
  ExUnit.Case` under the hood. This means you can do things like `use
  MyCase, async: true`. You can also access this options in `using/2`.

  > #### `use ExUnit.CaseTemplate` {: .info}
  >
  > When you `use ExUnit.CaseTemplate`, it will import the functionality
  > from `ExUnit.Assertions`, `ExUnit.Callbacks`, and this module itself.
  > It will also define a `__using__` callback, so the module itself can
  > be used as a template instead of `ExUnit.Case`.

  ## Example

      defmodule MyCase do
        use ExUnit.CaseTemplate

        setup do
          IO.puts("This will run before each test that uses this case")
        end
      end

      defmodule MyTest do
        use MyCase, async: true

        test "truth" do
          assert true
        end
      end

  If you need to "hook" into `use MyCase` and do other things as well,
  you can use the `using/2` macro. See its documentation for more
  information and examples.

      defmodule MyCase do
        use ExUnit.CaseTemplate

        using do
          quote do
            import MyApp.TestHelpers
          end
        end
      end

  """

  @doc false
  defmacro __using__(_) do
    quote do
      ExUnit.Callbacks.__register__(__MODULE__)
      @before_compile ExUnit.Callbacks
      import ExUnit.Callbacks

      import ExUnit.Assertions
      import unquote(__MODULE__)

      defmacro __using__(opts) do
        unquote(__MODULE__).__proxy__(__MODULE__, opts)
      end

      defoverridable __using__: 1
    end
  end

  @doc false
  # We inject this code in the module that calls "use MyTemplate".
  def __proxy__(module, opts) do
    quote do
      use ExUnit.Case, unquote(opts)

      setup_all context do
        unquote(module).__ex_unit__(:setup_all, context)
      end

      setup context do
        unquote(module).__ex_unit__(:setup, context)
      end
    end
  end

  @doc """
  Allows a developer to customize the using block
  when the case template is used.

  You can use an optional `var` argument when calling `using/2`. ExUnit
  will pass whatever argument you pass to `use MyCase` as this `var` argument. See the examples below for clarification.

  ## Example

      defmodule MyCase do
        use ExUnit.CaseTemplate

        using do
          quote do
            # This code is injected into every case that calls "use MyCase"
            alias MyApp.FunModule
          end
        end
      end

  You can specify an argument to `using/2`:

      defmodule MyCase do
        use ExUnit.CaseTemplate

        using options do
          quote do
            if unquote(options)[:import_helpers] do
              import MyApp.TestHelpers
            end
          end
        end
      end

  The second argument passed to `use MyCase` gets forwarded to `using/2` too:

      defmodule SomeTestCase do
        use MyCase, async: true, import_helpers: true

        test "the truth" do
          # truth/0 comes from MyApp.TestHelpers:
          assert truth()
        end
      end

  > #### Sharing options with `use ExUnit.Case` {: .warning}
  >
  > The second argument that you pass to `use MyCase` is *also* passed
  > as the second argument to `use ExUnit.Case`.

  """
  defmacro using(var \\ quote(do: _), do: block) do
    quote do
      defmacro __using__(unquote(var) = opts) do
        parent = unquote(__MODULE__).__proxy__(__MODULE__, opts)
        result = unquote(block)
        {:__block__, [], [parent, result]}
      end
    end
  end
end
