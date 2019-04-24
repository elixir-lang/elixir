Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.TypesTest do
  use ExUnit.Case, async: true

  import Kernel.Types, only: [context: 0]

  defp of_pattern(pattern) do
    case Kernel.Types.of_pattern(pattern, context()) do
      {:ok, type, context} -> {:ok, resolve_types(type, nil, context)}
      {:error, reason} -> {:error, reason}
    end
  end

  defmacrop quoted_pattern(expr) do
    quote do
      case Kernel.Types.of_pattern(unquote(Macro.escape(expr)), context()) do
        {:ok, type, context} -> {:ok, resolve_types(type, nil, context)}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp resolve_types({:var, var} = new_parent, parent, context) do
    case Map.fetch(context.types, var) do
      {:ok, :unbound} -> {:var, var}
      {:ok, ^parent} -> {:var, var}
      # Inadequate infinite recursion check
      {:ok, type} -> resolve_types(type, new_parent, context)
      :error -> {:var, var}
    end
  end

  defp resolve_types({:tuple, types} = parent, _parent, context) do
    {:tuple, Enum.map(types, &resolve_types(&1, parent, context))}
  end

  defp resolve_types({:cons, left, right} = parent, _parent, context) do
    {:cons, resolve_types(left, parent, context), resolve_types(right, parent, context)}
  end

  defp resolve_types({:fn, params, return} = parent, _parent, context) do
    params = Enum.map(params, &resolve_types(&1, parent, context))
    return = resolve_types(return, parent, context)
    {:fn, params, return}
  end

  defp resolve_types(other, _parent, _context) do
    other
  end

  describe "of_pattern/1" do
    test "literals" do
      assert of_pattern(true) == {:ok, {:literal, true}}
      assert of_pattern(false) == {:ok, {:literal, false}}
      assert of_pattern(:foo) == {:ok, {:literal, :foo}}
      assert of_pattern(0) == {:ok, :integer}

      assert of_pattern("foo") == {:error, {:unsupported_pattern, "foo"}}
    end

    test "list" do
      assert of_pattern([]) == {:ok, :null}
      assert of_pattern([123]) == {:ok, {:cons, :integer, :null}}
      assert of_pattern([123 | []]) == {:ok, {:cons, :integer, :null}}
      assert of_pattern([123 | 456]) == {:ok, {:cons, :integer, :integer}}
      assert of_pattern([123, 456 | 789]) == {:ok, {:cons, :integer, {:cons, :integer, :integer}}}
    end

    test "tuple" do
      assert quoted_pattern({}) == {:ok, {:tuple, []}}
      assert quoted_pattern({:a}) == {:ok, {:tuple, [{:literal, :a}]}}
      assert quoted_pattern({:a, 123}) == {:ok, {:tuple, [{:literal, :a}, :integer]}}
    end

    test "variables" do
      assert quoted_pattern(foo) == {:ok, {:var, 0}}
      assert quoted_pattern({foo}) == {:ok, {:tuple, [{:var, 0}]}}
      assert quoted_pattern({foo, bar}) == {:ok, {:tuple, [{:var, 0}, {:var, 1}]}}
    end

    test "assignment" do
      assert quoted_pattern(x = y) == {:ok, {:var, 0}}
      assert quoted_pattern(x = 123) == {:ok, :integer}
      assert quoted_pattern({foo}) == {:ok, {:tuple, [{:var, 0}]}}
      assert quoted_pattern({x = y}) == {:ok, {:tuple, [{:var, 0}]}}

      assert quoted_pattern(x = y = 123) == {:ok, :integer}
      assert quoted_pattern(x = 123 = y) == {:ok, :integer}
      assert quoted_pattern(123 = x = y) == {:ok, :integer}

      assert quoted_pattern({x} = x) == {:ok, {:tuple, [var: 0]}}
    end
  end
end
