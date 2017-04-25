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

  test "sets @doc false" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl Kernel.ImplTest.Behaviour
      def foo() do
        :ok
      end
    end

    # TODO: This always returns nil... how to test it?
    assert Code.get_docs(Kernel.ImplTest.ImplAttributes, :docs) == false

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not set @doc false if doc was specified before @impl" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @doc "An irrelevant function doc"
      def quz(), do: :ok

      @doc "Function doc"
      @impl Kernel.ImplTest.Behaviour
      def foo() do
        :ok
      end
    end

    assert Code.get_docs(Kernel.ImplTest.ImplAttributes, :docs) == "Function doc"

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not explode if multiple @doc directives were used" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @doc "First function doc"
      @doc "Function doc"
      @impl Kernel.ImplTest.Behaviour
      def foo() do
        :ok
      end
    end

    assert Code.get_docs(Kernel.ImplTest.ImplAttributes, :docs) == "Function doc"

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "does not set @doc false if doc was specified after @impl" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl Kernel.ImplTest.Behaviour
      @doc "Function doc"
      def foo() do
        :ok
      end
    end

    assert Code.get_docs(Kernel.ImplTest.ImplAttributes, :docs) == "Function doc"

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "sets impl with false" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      def foo() do
        :ok
      end

      @impl false
      assert @impl == false
      assert Module.get_attribute(__MODULE__, :impl) == false
      def foo(term) do
        term
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
  end

  test "sets impl with nil" do
    defmodule Kernel.ImplTest.ImplAttributes do
      @behaviour Kernel.ImplTest.Behaviour

      @impl nil
      assert @impl == nil
      assert Module.get_attribute(__MODULE__, :impl) == nil
      def foo() do
        :ok
      end
    end

    purge(Kernel.ImplTest.ImplAttributes)
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
end
