Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.WarningTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  test :unused_variable do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello(arg), do: nil
      end
      """
    end) =~ %r"variable arg is unused"
  after
    purge Sample
  end

  test :unused_function do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample1 do
        defp hello, do: nil
      end
      """
    end) =~ %r"function hello/0 is unused"

    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample2 do
        defp hello(0), do: hello(1)
        defp hello(1), do: :ok
      end
      """
    end) =~ %r"function hello/1 is unused"

    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample3 do
        def a, do: nil
        def b, do: d(10)
        defp c(x, y // 1), do: [x, y]
        defp d(x), do: x
      end
      """
    end) =~ %r"function c/2 is unused"

  after
    purge [Sample1, Sample2, Sample3]
  end

  test :unused_cyclic_functions do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample do
        defp a, do: b
        defp b, do: a
      end
      """
    end) =~ %r"function a/0 is unused"
  after
    purge Sample
  end

  test :unused_macro do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample do
        defmacrop hello, do: nil
      end
      """
    end) =~ %r"macro hello/0 is unused"
  after
    purge Sample
  end

  test :unused_default_args do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample1 do
        def a, do: b(1,2,3)
        defp b(arg1 // 1, arg2 // 2, arg3 // 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ %r"default arguments in b/3 are never used"

    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample2 do
        def a, do: b(1,2)
        defp b(arg1 // 1, arg2 // 2, arg3 // 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ %r"the first 2 default arguments in b/3 are never used"

    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample3 do
        def a, do: b(1)
        defp b(arg1 // 1, arg2 // 2, arg3 // 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ %r"the first default argument in b/3 is never used"

    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample4 do
        def a, do: b(1)
        defp b(arg1 // 1, arg2, arg3 // 3), do: [arg1, arg2, arg3]
      end
      """
    end) == nil
  after
    purge [Sample1, Sample2, Sample3, Sample4]
  end

  test :unused_import do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample1 do
        def hello, do: nil
      end

      defmodule Sample2 do
        import Sample1
        def a, do: nil
      end
      """
    end) =~ %r"unused import Sample1"
  after
    purge [Sample1, Sample2]
  end

  test :clause_not_match do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello, do: nil
        def hello, do: nil
      end
      """
    end) =~ %r"this clause cannot match because a previous clause at line 2 always matches"
  after
    purge Sample
  end

  test :clause_with_defaults_should_be_first do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello(arg), do: nil
        def hello(arg // 0), do: nil
      end
      """
    end) =~ %r"clause with defaults should be the first clause in def hello/1"
  after
    purge Sample
  end

  test :multiple_clauses_with_defaults do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello(arg // 0), do: nil
        def hello(arg // 1), do: nil
      end
      """
    end) =~ %r"def hello/1 has default values and multiple clauses, use a separate clause for declaring defaults"
  after
    purge Sample
  end

  test :unused_with_local_with_overridable do
    assert capture_io(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello, do: world
        defp world, do: :ok
        defoverridable [hello: 0]
        def hello, do: :ok
      end
      """
    end) =~ %r"function world/0 is unused"
  after
    purge Sample
  end

  test :used_with_local_with_reattached_overridable do
    assert nil? capture_io(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello, do: world
        defp world, do: :ok
        defoverridable [hello: 0, world: 0]
      end
      """
    end)
  after
    purge Sample
  end

  defp purge(list) when is_list(list) do
    Enum.each list, purge(&1)
  end

  defp purge(module) when is_atom(module) do
    :code.delete module
    :code.purge module
  end
end
