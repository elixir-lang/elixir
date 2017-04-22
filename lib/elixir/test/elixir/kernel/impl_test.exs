Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ImplTest do
  use ExUnit.Case

  defp purge(module) do
    :code.purge(module)
    :code.delete(module)
  end

  defmodule Kernel.ImplTest.Behaviour do
    @callback foo :: any
  end

  test "attributes format" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl Kernel.ImplTest.Behaviour
      assert @impl == Kernel.ImplTest.Behaviour
      assert Module.get_attribute(__MODULE__, :impl) == {19, Kernel.ImplTest.Behaviour}
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "sets @doc false" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl Kernel.ImplTest.Behaviour
      assert @doc == false
      assert Module.get_attribute(__MODULE__, :doc) == {34, false}
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not set @doc false if doc was specified before @impl" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @doc "An irrelevant function doc"
      def quz(), do: :ok

      @doc "Function doc"
      @impl Kernel.ImplTest.Behaviour
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {52, "Function doc"}
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not explode if multiple @doc directives were used" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @doc "First function doc"
      @doc "Function doc"
      @impl Kernel.ImplTest.Behaviour
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {69, "Function doc"}
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not set @doc false if doc was specified after @impl" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl Kernel.ImplTest.Behaviour
      @doc "Function doc"
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {86, "Function doc"}
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end
end
