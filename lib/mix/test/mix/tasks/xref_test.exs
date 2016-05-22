Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.XrefTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "reports nothing with no references" do
    assert_no_warnings "defmodule A do end"
  end

  test "reports missing functions" do
    assert_warnings """
    defmodule A do
      def a, do: A.no_func
      def b, do: A.a()
    end
    """, """
    \e[33mwarning: \e[0mRemote function A.no_func/0 cannot be found
      lib/a.ex:2

    """
  end

  test "reports missing functions respecting arity" do
    assert_warnings """
    defmodule A do
      def a, do: :ok
      def b, do: A.a(1)
    end
    """, """
    \e[33mwarning: \e[0mRemote function A.a/1 cannot be found
      lib/a.ex:3

    """
  end

  test "reports missing modules" do
    assert_warnings """
    defmodule A do
      def a, do: D.no_module
    end
    """, """
    \e[33mwarning: \e[0mModule D cannot be found

    In remote call to D.no_module/0 at:
      lib/a.ex:2

    """
  end

  test "doesn't report missing funcs at compile time" do
    assert_no_warnings """
      Enum.map([], fn _ -> BadReferencer.no_func4() end)

      if function_exported?(List, :flatten, 1) do
        List.flatten([1, 2, 3])
      else
        List.old_flatten([1, 2, 3])
      end
    """
  end

  test "protocols are checked, ignoring missing builtin impls" do
    assert_warnings """
    defprotocol AProtocol do
      def func(arg)
    end

    defmodule AImplementation do
      defimpl AProtocol do
        def func(_), do: B.no_func
      end
    end
    """, """
    \e[33mwarning: \e[0mRemote function B.no_func/0 cannot be found
      lib/a.ex:7

    """
  end

  test "handles erlang ops" do
    assert_no_warnings """
    defmodule A do
      def a(a, b), do: a and b
      def b(a, b), do: a or b
    end
    """
  end

  test "handles erlang modules" do
    assert_warnings """
    defmodule A do
      def a, do: :not_a_module.no_module
      def b, do: :lists.no_func
    end
    """, """
    \e[33mwarning: \e[0mModule :not_a_module cannot be found

    In remote call to :not_a_module.no_module/0 at:
      lib/a.ex:2

    \e[33mwarning: \e[0mRemote function :lists.no_func/0 cannot be found
      lib/a.ex:3

    """
  end

  test "handles multiple modules in one file" do
    assert_warnings """
    defmodule A do
      def a, do: A2.no_func
      def b, do: A2.a
    end

    defmodule A2 do
      def a, do: A.no_func
      def b, do: A.b
    end
    """, """
    \e[33mwarning: \e[0mRemote function A2.no_func/0 cannot be found
      lib/a.ex:2

    \e[33mwarning: \e[0mRemote function A.no_func/0 cannot be found
      lib/a.ex:7

    """
  end

  test "handles module body conditionals" do
    assert_warnings """
    defmodule A do
      if function_exported?(List, :flatten, 1) do
        List.flatten([1, 2, 3])
      else
        List.old_flatten([1, 2, 3])
      end

      if function_exported?(List, :flatten, 1) do
        def flatten(arg), do: List.flatten(arg)
      else
        def flatten(arg), do: List.old_flatten(arg)
      end

      if function_exported?(List, :flatten, 1) do
        def flatten2(arg), do: List.old_flatten(arg)
      else
        def flatten2(arg), do: List.flatten(arg)
      end
    end
    """, """
    \e[33mwarning: \e[0mRemote function List.old_flatten/1 cannot be found
      lib/a.ex:15

    """
  end

  test "imports" do
    assert_no_warnings """
    defmodule A do
      import Record

      def a(a, b), do: extract(a, b)
      def b(arg), do: is_record(arg)
    end
    """
  end

  test "requires" do
    assert_no_warnings """
    defmodule A do
      require Integer

      def a(a), do: Integer.is_even(a)
    end
    """
  end

  defp assert_warnings(contents, expected) do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      assert capture_io(:stderr, fn ->
        assert Mix.Task.run("xref", []) == :error
      end) == expected
    end
  end
  defp assert_no_warnings(contents) do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      assert capture_io(:stderr, fn ->
        assert Mix.Task.run("xref", []) == :ok
      end) == ""
    end
  end
end
