Code.require_file("../test_helper.exs", __DIR__)

defmodule Module.CheckerTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  setup_all do
    previous = Application.get_env(:elixir, :ansi_enabled, false)
    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit(fn -> Application.put_env(:elixir, :ansi_enabled, previous) end)
  end

  test "handles Erlang modules" do
    files = %{
      "a.ex" => """
      defmodule A do
        def a, do: :not_a_module.no_module
        def b, do: :lists.no_func
      end
      """
    }

    warning = """
    warning: :not_a_module.no_module/0 is undefined (module :not_a_module is not available or is yet to be defined)
      a.ex:2: A.a/0

    warning: :lists.no_func/0 is undefined or private
      a.ex:3: A.b/0

    """

    assert_warnings(files, warning)
  end

  test "handles module body conditionals" do
    files = %{
      "a.ex" => """
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
      """
    }

    warning = """
    warning: List.old_flatten/1 is undefined or private. Did you mean one of:

          * flatten/1
          * flatten/2

      a.ex:15: A.flatten2/1

    """

    assert_warnings(files, warning)
  end

  # test "aliases" do
  #   files = %{
  #     "a.ex" => """
  #     defmodule A do
  #       alias Enum, as: E
  #
  #       def a(a, b), do: E.map2(a, b)
  #       def b, do: &E.map2/2
  #
  #       @file "external_source.ex"
  #       def c do
  #         alias Enum, as: EE
  #         &EE.map2/2
  #       end
  #     end
  #     """
  #   }
  #
  #   warning = """
  #   warning: function Enum.map2/2 is undefined or private. Did you mean one of:
  #
  #         * map/2
  #
  #   Found at 3 locations:
  #     a.ex:4
  #     a.ex:5
  #     external_source.ex:10
  #
  #   """
  #
  #   assert_warnings(files, warning)
  # end

  # test "reports missing functions" do
  #   files = %{
  #     "a.ex" => """
  #     defmodule A do
  #       def a, do: A.no_func
  #       def b, do: A.a()
  #
  #       @file "external_source.ex"
  #       def c, do: &A.no_func/1
  #     end
  #     """
  #   }
  #
  #   warning = """
  #   warning: A.no_func/0 is undefined or private
  #     a.ex:2: A.a/0
  #
  #   warning: A.no_func/1 is undefined or private
  #     external_source.ex:6: A.c/0
  #
  #   """
  #
  #   assert_warnings(files, warning)
  # end

  test " reports missing functions respecting arity" do
    files = %{
      "a.ex" => """
      defmodule A do
        def a, do: :ok
        def b, do: A.a(1)

        @file "external_source.ex"
        def c, do: A.b(1)
      end
      """
    }

    warning = """
    warning: A.a/1 is undefined or private. Did you mean one of:

          * a/0

      a.ex:3: A.b/0

    warning: A.b/1 is undefined or private. Did you mean one of:

          * b/0

      external_source.ex:6: A.c/0

    """

    assert_warnings(files, warning)
  end

  test "reports missing modules" do
    files = %{
      "a.ex" => """
      defmodule A do
        def a, do: D.no_module

        @file "external_source.ex"
        def c, do: E.no_module
      end
      """
    }

    warning = """
    warning: D.no_module/0 is undefined (module D is not available or is yet to be defined)
      a.ex:2: A.a/0

    warning: E.no_module/0 is undefined (module E is not available or is yet to be defined)
      external_source.ex:5: A.c/0

    """

    assert_warnings(files, warning)
  end

  # test "reports missing captures" do
  #   files = %{
  #     "a.ex" => """
  #     defmodule A do
  #       def a, do: &A.no_func/0
  #
  #       @file "external_source.ex"
  #       def c, do: &A.no_func/1
  #     end
  #     """
  #   }
  #
  #   warning = """
  #   warning: A.no_func/0 is undefined or private
  #     a.ex:2: A.a./0
  #
  #   warning: A.no_func/1 is undefined or private
  #     external_source.ex:5: A.c/0
  #
  #   """
  #
  #   assert_warnings(files, warning)
  # end

  test "doesn't report missing funcs at compile time" do
    files = %{
      "a.ex" => """
        Enum.map([], fn _ -> BadReferencer.no_func4() end)

        if function_exported?(List, :flatten, 1) do
          List.flatten([1, 2, 3])
        else
          List.old_flatten([1, 2, 3])
        end
      """
    }

    assert_no_warnings(files)
  end

  test "handles multiple modules in one file" do
    files = %{
      "a.ex" => """
      defmodule A do
        def a, do: B.no_func
        def b, do: B.a
      end
      """,
      "b.ex" => """
      defmodule B do
        def a, do: A.no_func
        def b, do: A.b
      end
      """
    }

    warning = """
    warning: B.no_func/0 is undefined or private
      a.ex:2: A.a/0

    warning: A.no_func/0 is undefined or private
      b.ex:2: B.a/0

    """

    assert_warnings(files, warning)
  end

  # test "doesn't load unloaded modules" do
  #   files = %{
  #     "a.ex" => """
  #     defmodule A do
  #       @compile {:autoload, false}
  #       @on_load :init
  #       def init do
  #         raise "oops"
  #       end
  #     end
  #     """,
  #     "b.ex" => """
  #     defmodule B do
  #       def a, do: A.no_func
  #       def b, do: A.init
  #     end
  #     """
  #   }
  #
  #   warning = """
  #   warning: function A.no_func/0 is undefined or private
  #     b.ex:2
  #
  #   """
  #
  #   assert_warnings(files, warning)
  # end

  # test "groups multiple warnings in one file" do
  #   files = %{
  #     "a.ex" => """
  #     defmodule A do
  #       def a, do: A.no_func
  #
  #       @file "external_source.ex"
  #       def b, do: A2.no_func
  #
  #       def c, do: A.no_func
  #       def d, do: A2.no_func
  #     end
  #     """
  #   }
  #
  #   """
  #   warning: A.no_func/0 is undefined or private
  #     a.ex:2: A.a/0
  #
  #   warning: A2.no_func/0 is undefined (module A2 is not available or is yet to be defined)
  #     external_source.ex:5: A.b/0
  #
  #   warning: A.no_func/0 is undefined or private
  #     a.ex:7: A.c/0
  #
  #   warning: A2.no_func/0 is undefined (module A2 is not available or is yet to be defined)
  #     a.ex:8: A.d/0
  #   """
  #
  #   warning = """
  #   warning: A.no_func/0 is undefined or private
  #   Found at 2 locations:
  #     a.ex:2
  #     a.ex:7
  #
  #   warning: A2.no_func/0 is undefined (module A2 is not available or is yet to be defined)
  #   Found at 2 locations:
  #     a.ex:8
  #     external_source.ex:5
  #
  #   """
  #
  #   assert_warnings(files, warning)
  # end

  test "protocols are checked, ignoring missing built-in impls" do
    files = %{
      "a.ex" => """
      defprotocol AProtocol do
        def func(arg)
      end

      defmodule AImplementation do
        defimpl AProtocol do
          def func(_), do: B.no_func
        end
      end
      """
    }

    warning = """
    warning: B.no_func/0 is undefined (module B is not available or is yet to be defined)
      a.ex:7: AProtocol.AImplementation.func/1

    """

    assert_warnings(files, warning)
  end

  test "handles Erlang ops" do
    files = %{
      "a.ex" => """
      defmodule A do
        def a(a, b), do: a and b
        def b(a, b), do: a or b
      end
      """
    }

    assert_no_warnings(files)
  end

  test "hints exclude deprecated functions" do
    files = %{
      "a.ex" => """
      defmodule A do
        def to_charlist(a), do: a

        @deprecated "Use String.to_charlist/1 instead"
        def to_char_list(a), do: a

        def c(a), do: A.to_list(a)
      end
      """
    }

    warning = """
    warning: A.to_list/1 is undefined or private. Did you mean one of:

          * to_charlist/1

      a.ex:7: A.c/1

    """

    assert_warnings(files, warning)
  end

  test "imports" do
    files = %{
      "a.ex" => """
      defmodule A do
        import Record

        def a(a, b), do: extract(a, b)
        def b(arg), do: is_record(arg)
      end
      """
    }

    assert_no_warnings(files)
  end

  test "requires" do
    files = %{
      "a.ex" => """
      defmodule A do
        require Integer

        def a(a), do: Integer.is_even(a)
      end
      """
    }

    assert_no_warnings(files)
  end

  defp assert_warnings(files, expected) do
    in_tmp(fn ->
      files = generate_files(files)

      output = capture_io(:stderr, fn ->
        {:ok, modules, _warnings} = Kernel.ParallelCompiler.compile(files)

        Enum.each(modules, fn module ->
          :code.purge(module)
          :code.delete(module)
        end)
      end)

      assert output == expected
    end)
  end

  defp assert_no_warnings(files) do
    in_tmp(fn ->
      files = generate_files(files)

      output = capture_io(:stderr, fn ->
        {:ok, modules, _warnings} = Kernel.ParallelCompiler.compile(files)

        Enum.each(modules, fn module ->
          :code.purge(module)
          :code.delete(module)
        end)
      end)

      assert output == ""
    end)
  end

  defp generate_files(files) do
    for {file, contents} <- files do
      File.write!(file, contents)
      file
    end
  end

  defp in_tmp(fun) do
    tmp = Path.expand("../../../tmp", __DIR__)
    path = Path.join(tmp, Integer.to_string(System.unique_integer([:positive])))

    File.rm_rf!(path)
    File.mkdir_p!(path)
    File.cd!(path, fun)
  end
end
