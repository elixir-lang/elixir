Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ImplTest do
  use ExUnit.Case

  defp capture_err(fun) do
    ExUnit.CaptureIO.capture_io(:stderr, fun)
  end

  defp purge(module) do
    :code.purge(module)
    :code.delete(module)
  end

  setup do
    on_exit fn -> purge(Kernel.ImplTest.ImplAttributes) end
  end

  defmodule Behaviour do
    @callback foo :: any
  end

  defmodule MacroBehaviour do
    @macrocallback bar :: any
  end

  test "sets impl to boolean" do
    defmodule ImplAttributes do
      @behaviour Behaviour

      @impl true
      def foo(), do: :ok

      @impl false
      def foo(term) do
        term
      end
    end
  end

  test "sets impl to nil" do
    assert_raise ArgumentError, ~r/expected impl attribute to contain a module or a boolean/, fn ->
      defmodule ImplAttributes do
        @behaviour Behaviour
        @impl nil
        def foo(), do: :ok
      end
    end
  end

  test "sets impl to behaviour" do
    defmodule ImplAttributes do
      @behaviour Behaviour
      @impl Behaviour
      def foo(), do: :ok
    end
  end

  test "does not set impl" do
    defmodule ImplAttributes do
      @behaviour Behaviour
      def foo(), do: :ok
    end
  end

  test "warns for undefined value" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour :abc

        @impl :abc
        def foo(), do: :ok
      end
      """
    end) =~ "got @impl :abc for def foo/0 but the behaviour does not specify this callback. There are no known callbacks"
  end

  test "warns for callbacks without impl and @impl has been set before" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @behaviour Kernel.ImplTest.MacroBehaviour

        @impl true
        def foo(), do: :ok

        defmacro bar(), do: :ok
      end
      """
    end) =~ "module attribute @impl was not set for callback defmacro bar/0 (callback specified in Kernel.ImplTest.MacroBehaviour)"
  end

  test "warns for callbacks without impl and @impl has been set after" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @behaviour Kernel.ImplTest.MacroBehaviour

        defmacro bar(), do: :ok

        @impl true
        def foo(), do: :ok
      end
      """
    end) =~ "module attribute @impl was not set for callback defmacro bar/0 (callback specified in Kernel.ImplTest.MacroBehaviour)"
  end

  test "warns when impl is set on private function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @impl true
        defp foo(), do: :ok
      end
      """
    end) =~ "defp foo/0 is private, @impl is always discarded for private functions/macros"
  end

  test "warns when @impl is set and no function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @impl true
      end
      """
    end) =~ "module attribute @impl was set but no definition follows it"
  end

  test "warns for @impl true and no behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @impl true
        def foo(), do: :ok
      end
      """
    end) =~ "got @impl true for def foo/0 but no behaviour was declared"
  end

  test "warns for @impl true with callback name not in behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @impl true
        def bar(), do: :ok
      end
      """
    end) =~ "got @impl true for def bar/0 but no behaviour specifies this callback"
  end

  test "warns for @impl true with macro callback name not in behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.MacroBehaviour
        @impl true
        defmacro foo(), do: :ok
      end
      """
    end) =~ "got @impl true for defmacro foo/0 but no behaviour specifies this callback"
  end

  test "warns for @impl true with callback kind not in behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.MacroBehaviour
        @impl true
        def foo(), do: :ok
      end
      """
    end) =~ "got @impl true for def foo/0 but no behaviour specifies this callback"
  end

  test "warns for @impl true with wrong arity" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @impl true
        def foo(arg), do: arg
      end
      """
    end) =~ "got @impl true for def foo/1 but no behaviour specifies this callback"
  end

  test "warns for @impl false and there are no callbacks" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @impl false
        def baz(term), do: term
      end
      """
    end) =~ "got @impl false for def baz/1 but no behaviour was declared"
  end

  test "warns for @impl false and it is a callback" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @impl false
        def foo(), do: :ok
      end
      """
    end) =~ "got @impl false for def foo/0 but it is a callback specified in Kernel.ImplTest.Behaviour"
  end

  test "warns for @impl module and no behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @impl Kernel.ImplTest.Behaviour
        def foo(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.ImplTest.Behaviour for def foo/0 but no behaviour was declared"
  end

  test "warns for @impl module with callback name not in behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @impl Kernel.ImplTest.Behaviour
        def bar(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.ImplTest.Behaviour for def bar/0 but the behaviour does not specify this callback"
  end

  test "warns for @impl module with macro callback name not in behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.MacroBehaviour
        @impl Kernel.ImplTest.MacroBehaviour
        defmacro foo(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.ImplTest.MacroBehaviour for defmacro foo/0 but the behaviour does not specify this callback"
  end

  test "warns for @impl module with macro callback kind not in behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.MacroBehaviour
        @impl Kernel.ImplTest.MacroBehaviour
        def foo(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.ImplTest.MacroBehaviour for def foo/0 but the behaviour does not specify this callback"
  end

  test "warns for @impl module and callback belongs to another known module" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @behaviour Kernel.ImplTest.MacroBehaviour
        @impl Kernel.ImplTest.Behaviour
        def bar(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.ImplTest.Behaviour for def bar/0 but the behaviour does not specify this callback"
  end

  test "warns for @impl module and callback belongs to another unknown module" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @impl Kernel.ImplTest.MacroBehaviour
        def bar(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.ImplTest.MacroBehaviour for def bar/0 but the given behaviour was not declared with @behaviour"
  end

  test "does not warn for no @impl when overriding callback" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour

        def foo(), do: :overridable

        defoverridable Kernel.ImplTest.Behaviour

        def foo(), do: :overridden
      end
      """
    end) == ""
  end

  test "does not warn for overridable function missing @impl" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour

        def foo(), do: :overridable

        defoverridable Kernel.ImplTest.Behaviour

        @impl Kernel.ImplTest.Behaviour
        def foo(), do: :overridden
      end
      """
    end) == ""
  end

  test "warns correctly for missing @impl only for end-user implemented function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @behaviour Kernel.ImplTest.MacroBehaviour

        def foo(), do: :overridable

        defoverridable Kernel.ImplTest.Behaviour

        def foo(), do: :overridden

        @impl true
        defmacro bar(), do: :overridden
      end
      """
    end) =~ "module attribute @impl was not set for callback def foo/0 (callback specified in Kernel.ImplTest.Behaviour)"
  end

  test "warns correctly for missing @impl even if it was set in overridable callback" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @behaviour Kernel.ImplTest.MacroBehaviour

        @impl Kernel.ImplTest.Behaviour
        def foo(), do: :overridable

        defoverridable Kernel.ImplTest.Behaviour

        def foo(), do: :overridden

        @impl Kernel.ImplTest.MacroBehaviour
        defmacro bar(), do: :overridden
      end
      """
    end) =~ "module attribute @impl was not set for callback def foo/0 (callback specified in Kernel.ImplTest.Behaviour)"
  end

  test "warns correctly for incorrect @impl in overridable callback" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Kernel.ImplTest.ImplAttributes do
        @behaviour Kernel.ImplTest.Behaviour
        @behaviour Kernel.ImplTest.MacroBehaviour

        @impl Kernel.ImplTest.MacroBehaviour
        def foo(), do: :overridable

        defoverridable Kernel.ImplTest.Behaviour

        @impl Kernel.ImplTest.Behaviour
        def foo(), do: :overridden
      end
      """
    end) =~ "got @impl Kernel.ImplTest.MacroBehaviour for def foo/0 but the behaviour does not specify this callback"
  end

  test "does not warn for overridable callback when using __before_compile__/1 hook" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule BeforeCompile do
        defmacro __before_compile__(_) do
          quote do
            @behaviour Kernel.ImplTest.Behaviour

            def foo(), do: :overridable

            defoverridable Kernel.ImplTest.Behaviour
          end
        end
      end

      defmodule Kernel.ImplTest.ImplAttributes do
        @before_compile BeforeCompile
        @behaviour Kernel.ImplTest.MacroBehaviour

        defmacro bar(), do: :overridable

        defoverridable Kernel.ImplTest.MacroBehaviour

        @impl Kernel.ImplTest.MacroBehaviour
        defmacro bar(), do: :overridden
      end
      """
    end) == ""
  end
end
