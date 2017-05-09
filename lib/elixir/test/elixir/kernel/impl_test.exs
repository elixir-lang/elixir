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

  test "no error for undefined module" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl :foobar
      assert @impl == :foobar
      assert Module.get_attribute(__MODULE__, :impl) == :foobar
      def foo() do
        :ok
      end
    end
  end

  test "sets impl with nil" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl nil
      assert @impl == nil
      assert Module.get_attribute(__MODULE__, :impl) == nil
      assert @doc == nil
      assert Module.get_attribute(__MODULE__, :doc) == nil
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "sets impl with behaviour" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl Kernel.ImplTest.Behaviour
      assert @impl == Kernel.ImplTest.Behaviour
      assert Module.get_attribute(__MODULE__, :impl) == Kernel.ImplTest.Behaviour
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
      assert Module.get_attribute(__MODULE__, :doc) == {67, "Function doc"}
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
      assert Module.get_attribute(__MODULE__, :doc) == {84, "Function doc"}
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
      assert Module.get_attribute(__MODULE__, :doc) == {101, "Function doc"}
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "sets impl with false" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      def foo() do
        :ok
      end

      @doc "Function doc"
      @impl false
      assert @impl == false
      assert Module.get_attribute(__MODULE__, :impl) == false
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {120, "Function doc"}
      def foo(term) do
        term
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "impl behaviour sets @doc false" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl Kernel.ImplTest.Behaviour
      # assert @doc == false
      # assert Module.get_attribute(__MODULE__, :doc) == false
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "impl true sets @doc false" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl true
      # assert @doc == false
      # assert Module.get_attribute(__MODULE__, :doc) == false
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end
end
