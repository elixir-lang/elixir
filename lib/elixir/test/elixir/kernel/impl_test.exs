Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ImplTest.Behaviour do
  @callback required_foo :: any
  @callback optional_bar :: any
  @callback optional_baz(integer) :: any
  @optional_callbacks [:optional_bar, :optional_baz]
end

defmodule Kernel.ImplTest do
  use ExUnit.Case

  test "attributes format" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @impl Kernel.ImplTest.Behaviour
      assert @impl == Kernel.ImplTest.Behaviour
      assert Module.get_attribute(__MODULE__, :impl) == {15, Kernel.ImplTest.Behaviour}
      def required_foo() do
        :ok
      end
    end
  end

  test "sets @doc false" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @impl Kernel.ImplTest.Behaviour
      assert @doc == false
      assert Module.get_attribute(__MODULE__, :doc) == {26, false}
      def required_foo() do
        :ok
      end
    end
  end

  test "does not set @doc false if doc was specified before @impl" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @doc "Function doc"
      @impl Kernel.ImplTest.Behaviour
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {37, "Function doc"}
      def required_foo() do
        :ok
      end
    end
  end

  test "does not set @doc false if doc was specified after @impl" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @impl Kernel.ImplTest.Behaviour
      @doc "Function doc"
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {50, "Function doc"}
      def required_foo() do
        :ok
      end
    end
  end
end
