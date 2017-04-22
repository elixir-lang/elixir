Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ImplTest.Behaviour do
  @callback required_foo :: any
  @callback optional_bar :: any
  @callback optional_baz(integer) :: any
  @optional_callbacks [:optional_bar, :optional_baz]
end

defmodule Kernel.ImplTest do
  use ExUnit.Case

  defp purge(module) do
    :code.purge(module)
    :code.delete(module)
  end

  test "attributes format" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @impl Kernel.ImplTest.Behaviour
      assert @impl == Kernel.ImplTest.Behaviour
      assert Module.get_attribute(__MODULE__, :impl) == {20, Kernel.ImplTest.Behaviour}
      def required_foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "sets @doc false" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @impl Kernel.ImplTest.Behaviour
      assert @doc == false
      assert Module.get_attribute(__MODULE__, :doc) == {33, false}
      def required_foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not set @doc false if doc was specified before @impl" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @doc "An irrelevant function doc"
      def quz(), do: :ok

      @doc "Function doc"
      @impl Kernel.ImplTest.Behaviour
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {49, "Function doc"}
      def required_foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not explode if multiple @doc directives were used" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @doc "First function doc"
      @doc "Function doc"
      @impl Kernel.ImplTest.Behaviour
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {64, "Function doc"}
      def required_foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not set @doc false if doc was specified after @impl" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @impl Kernel.ImplTest.Behaviour
      @doc "Function doc"
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {79, "Function doc"}
      def required_foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end
end
