defmodule ExUnit.CaseTemplate do
  @moduledoc """
  This module allows a developer to define a test case
  template to be used throughout his tests. This is useful
  when there are a set of functions that should be shared
  between tests or a set of setup/teardown callbacks.

  By using this module, the callbacks and assertions
  available for regular test cases will also be available.

  ## Example

      defmodule MyCase do
        use ExUnit.CaseTemplate

        setup do
          IO.puts "This will run before each test that uses this case"
        end
      end

      defmodule MyTest do
        use MyCase, async: true

        test "truth" do
          assert true
        end
      end

  """

  @doc false
  defmacro __using__(_) do
    quote do
      use ExUnit.Callbacks, parent: ExUnit.Case

      import unquote(__MODULE__)
      import ExUnit.Assertions

      defmacro __using__(opts) do
        unquote(__MODULE__).__parent__(__MODULE__, opts)
      end

      defoverridable [__using__: 1]
    end
  end

  @doc false
  def __parent__(module, opts) do
    opts = Keyword.put(opts, :parent, module)

    quote do
      use ExUnit.Case, unquote(opts)
    end
  end

  @doc """
  Allows a developer to code to be invoked when
  this module is used.
  """
  defmacro using(var // quote(do: _), do: block) do
    quote location: :keep do
      defmacro __using__(unquote(var) = opts) do
        parent = unquote(__MODULE__).__parent__(__MODULE__, opts)
        result = unquote(block)
        { :__block__, [], [parent, result] }
      end
    end
  end
end
