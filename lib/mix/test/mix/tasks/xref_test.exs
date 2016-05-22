Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.XrefTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  setup_all do
    previous = Application.get_env(:elixir, :ansi_enabled, false)
    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit fn -> Application.put_env(:elixir, :ansi_enabled, previous) end
  end

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  ## Warnings

  test "warnings: reports nothing with no references" do
    assert_no_warnings "defmodule A do end"
  end

  test "warnings: reports missing functions" do
    assert_warnings """
    defmodule A do
      def a, do: A.no_func
      def b, do: A.a()
    end
    """, """
    warning: function A.no_func/0 is undefined or private
      lib/a.ex:2

    """
  end

  test "warnings: reports missing functions respecting arity" do
    assert_warnings """
    defmodule A do
      def a, do: :ok
      def b, do: A.a(1)
    end
    """, """
    warning: function A.a/1 is undefined or private
      lib/a.ex:3

    """
  end

  test "warnings: reports missing modules" do
    assert_warnings """
    defmodule A do
      def a, do: D.no_module
    end
    """, """
    warning: function D.no_module/0 is undefined (module D is not available)
      lib/a.ex:2

    """
  end

  test "warnings: doesn't report missing funcs at compile time" do
    assert_no_warnings """
      Enum.map([], fn _ -> BadReferencer.no_func4() end)

      if function_exported?(List, :flatten, 1) do
        List.flatten([1, 2, 3])
      else
        List.old_flatten([1, 2, 3])
      end
    """
  end

  test "warnings: protocols are checked, ignoring missing builtin impls" do
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
    warning: function B.no_func/0 is undefined or private
      lib/a.ex:7

    """
  end

  test "warnings: handles erlang ops" do
    assert_no_warnings """
    defmodule A do
      def a(a, b), do: a and b
      def b(a, b), do: a or b
    end
    """
  end

  test "warnings: handles erlang modules" do
    assert_warnings """
    defmodule A do
      def a, do: :not_a_module.no_module
      def b, do: :lists.no_func
    end
    """, """
    warning: function :not_a_module.no_module/0 is undefined (module :not_a_module is not available)
      lib/a.ex:2

    warning: function :lists.no_func/0 is undefined or private
      lib/a.ex:3

    """
  end

  test "warnings: handles multiple modules in one file" do
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
    warning: function A2.no_func/0 is undefined or private
      lib/a.ex:2

    warning: function A.no_func/0 is undefined or private
      lib/a.ex:7

    """
  end

  test "warnings: groups multiple warnings in one file" do
    assert_warnings """
    defmodule A do
      def a, do: A.no_func
      def b, do: A2.no_func
      def c, do: A.no_func
      def d, do: A2.no_func
    end
    """, """
    warning: function A.no_func/0 is undefined or private
    Found at 2 locations:
      lib/a.ex:2
      lib/a.ex:4

    warning: function A2.no_func/0 is undefined (module A2 is not available)
    Found at 2 locations:
      lib/a.ex:3
      lib/a.ex:5

    """
  end

  test "warnings: handles module body conditionals" do
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
    warning: function List.old_flatten/1 is undefined or private
      lib/a.ex:15

    """
  end

  test "warnings: imports" do
    assert_no_warnings """
    defmodule A do
      import Record

      def a(a, b), do: extract(a, b)
      def b(arg), do: is_record(arg)
    end
    """
  end

  test "warnings: requires" do
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
        assert Mix.Task.run("xref", ["--warnings"]) == :error
      end) == expected
    end
  end

  defp assert_no_warnings(contents) do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      assert capture_io(:stderr, fn ->
        assert Mix.Task.run("xref", ["--warnings"]) == :ok
      end) == ""
    end
  end

  ## Unreachable

  test "unreachble: reports missing functions" do
    assert_unreachable """
    defmodule A do
      def a, do: A.no_func
      def b, do: A.a()
    end
    """, """
    lib/a.ex:2: A.no_func/0
    """
  end

  defp assert_unreachable(contents, expected) do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      assert capture_io(fn ->
        assert Mix.Task.run("xref", ["--unreachable"]) == :error
      end) == expected
    end
  end

  ## Exclude

  test "exclude: excludes specified modules and MFAs" do
    defmodule ExcludeSample do
      def project do
        [app: :sample,
         version: "0.1.0",
         xref: [exclude: [MissingModule, {MissingModule2, :no_func, 2}]]]
      end
    end

    Mix.Project.push ExcludeSample

    assert_warnings """
    defmodule A do
      def a, do: MissingModule.no_func(1)
      def b, do: MissingModule2.no_func(1, 2)
      def c, do: MissingModule2.no_func(1)
      def d, do: MissingModule3.no_func(1, 2)
    end
    """, """
    warning: function MissingModule2.no_func/1 is undefined (module MissingModule2 is not available)
      lib/a.ex:4

    warning: function MissingModule3.no_func/2 is undefined (module MissingModule3 is not available)
      lib/a.ex:5

    """
  end
end
