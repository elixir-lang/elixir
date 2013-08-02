Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.WarningTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  defp capture_err(fun) do
    capture_io(:stderr, fun)
  end

  test :unused_variable do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello(arg), do: nil
      end
      """
    end) =~ "variable arg is unused"
  after
    purge Sample
  end

  test :unused_function do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        defp hello, do: nil
      end
      """
    end) =~ "function hello/0 is unused"

    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample2 do
        defp hello(0), do: hello(1)
        defp hello(1), do: :ok
      end
      """
    end) =~ "function hello/1 is unused"

    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample3 do
        def a, do: nil
        def b, do: d(10)
        defp c(x, y // 1), do: [x, y]
        defp d(x), do: x
      end
      """
    end) =~ "function c/2 is unused"
  after
    purge [Sample1, Sample2, Sample3]
  end

  test :unused_cyclic_functions do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        defp a, do: b
        defp b, do: a
      end
      """
    end) =~ "function a/0 is unused"
  after
    purge Sample
  end

  test :unused_macro do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        defmacrop hello, do: nil
      end
      """
    end) =~ "macro hello/0 is unused"
  after
    purge Sample
  end

  test :shadowing do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def test(x) do
          case x do
            {:file, fid} -> fid
            {:path, _}   -> fn(fid) -> fid end
          end
        end
      end
      """
    end) == nil
  after
    purge Sample
  end

  test :unused_default_args do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        def a, do: b(1, 2, 3)
        defp b(arg1 // 1, arg2 // 2, arg3 // 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ "default arguments in b/3 are never used"

    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample2 do
        def a, do: b(1, 2)
        defp b(arg1 // 1, arg2 // 2, arg3 // 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ "the first 2 default arguments in b/3 are never used"

    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample3 do
        def a, do: b(1)
        defp b(arg1 // 1, arg2 // 2, arg3 // 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ "the first default argument in b/3 is never used"

    assert capture_err(fn ->
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
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        def hello, do: nil
      end

      defmodule Sample2 do
        import Sample1
        def a, do: nil
      end
      """
    end) =~ "unused import Sample1"
  after
    purge [Sample1, Sample2]
  end

  test :unused_guard do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        def is_atom_case do
          v = "bc"
          case v do
            _ when is_atom(v) -> :ok
            _ -> :fail
          end
        end
      end
      """
    end) =~ "nofile:5: the guard for this clause evaluates to 'false'"

    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample2 do
        def is_binary_cond do
          v = "bc"
          cond do
            is_binary(v) -> :bin
            true -> :ok
          end
        end
      end
      """
    end) =~ "nofile:6: this clause cannot match because a previous clause at line 5 always matches"

    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample3 do
        def is_binary_cond_case do
          v = "bc"
          case(is_binary(v)) do
            _ in [false, nil] -> :ok
            _ -> :bin
          end
        end
      end
      """
    end) =~ "nofile:5: this clause cannot match because a previous clause at line 6 always matches"
  after
    purge [Sample1, Sample2, Sample3]
  end

  test :unused_docs do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        @doc "Oops"
        def hello
      end
      """
    end) =~ "docs provided for nonexistent function or macro hello/0"
  after
    purge [Sample1]
  end

  test :used_import_via_alias do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        import List, only: [flatten: 1]

        defmacro generate do
          List.duplicate(quote(do: flatten([1,2,3])), 100)
        end
      end

      defmodule Sample2 do
        import Sample1
        generate
      end
      """
    end) == nil
  after
    purge [Sample1, Sample2]
  end

  test :clause_not_match do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello, do: nil
        def hello, do: nil
      end
      """
    end) =~ "this clause cannot match because a previous clause at line 2 always matches"
  after
    purge Sample
  end

  test :clause_with_defaults_should_be_first do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello(arg), do: nil
        def hello(arg // 0), do: nil
      end
      """
    end) =~ "clause with defaults should be the first clause in def hello/1"
  after
    purge Sample
  end

  test :unused_with_local_with_overridable do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello, do: world
        defp world, do: :ok
        defoverridable [hello: 0]
        def hello, do: :ok
      end
      """
    end) =~ "function world/0 is unused"
  after
    purge Sample
  end

  test :used_with_local_with_reattached_overridable do
    assert nil? capture_err(fn ->
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

  test :undefined_module_attribute do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @foo
      end
      """
    end) =~ "undefined module attribute @foo, please remove access to @foo or explicitly set it to nil before access"
  after
    purge Sample
  end

  test :in_guard_empty_list do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def a(x) when x in [], do: x
      end
      """
    end) =~ "the guard for this clause evaluates to 'false'"
  after
    purge Sample
  end

  test :no_effect_operator do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def a(x) do
          x != :foo
          :ok
        end
      end
      """
    end) =~ "use of operator != has no effect"
  after
    purge Sample
  end

  test :undefined_function_for_behaviour do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        use Behaviour
        defcallback foo
      end

      defmodule Sample2 do
        @behaviour Sample1
      end
      """
    end) =~ "undefined behaviour function foo/0 (for behaviour Sample1)"
  after
    purge [Sample1, Sample2, Sample3]
  end

  test :undefined_macro_for_behaviour do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        use Behaviour
        defmacrocallback foo
      end

      defmodule Sample2 do
        @behaviour Sample1
      end
      """
    end) =~ "undefined behaviour macro foo/0 (for behaviour Sample1)"
  after
    purge [Sample1, Sample2, Sample3]
  end

  test :undefined_macro_for_protocol do
    assert capture_err(fn ->
      Code.eval_string """
      defprotocol Sample1 do
        def foo(subject)
      end

      defimpl Sample1, for: Atom do
      end
      """
    end) =~ "undefined protocol function foo/1 (for protocol Sample1)"
  after
    purge [Sample1, Sample1.Atom]
  end

  defp purge(list) when is_list(list) do
    Enum.each list, purge(&1)
  end

  defp purge(module) when is_atom(module) do
    :code.delete module
    :code.purge module
  end
end
