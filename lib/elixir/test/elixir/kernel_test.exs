Code.require_file "test_helper.exs", __DIR__

defmodule KernelTest do
  use ExUnit.Case, async: true

  test :match do
    assert ("abcd" =~ ~r/c(d)/) == true
    assert ("abcd" =~ ~r/e/) == false

    string = "^ab+cd*$"
    assert (string =~ "ab+") == true
    assert (string =~ "bb") == false

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ "hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ ~r"hello"
    end
  end

  test :^ do
    x = List.first([1])

    assert_raise MatchError, fn ->
      { x, ^x } = { 2, 2 }
      x
    end
  end

  test :match? do
    assert match?(_, List.first(1)) == true
    assert binding([:x]) == []

    a = List.first([0])
    assert match?(b when b > a, 1) == true
    assert binding([:b]) == []

    assert match?(b when b > a, -1) == false
    assert binding([:b]) == []
  end

  test :nil? do
    assert nil?(nil) == true
    assert nil?(0) == false
    assert nil?(false) == false
  end

  test :in do
    assert 2 in [1, 2, 3]
    assert 2 in 1..3
    refute 4 in [1, 2, 3]
    refute 4 in 1..3

    list = [1, 2, 3]
    assert 2 in list
    refute 4 in list
  end

  @at_list  [4,5]
  @at_range [6,7,8]
  def fun_in(x) when x in [0],       do: :list
  def fun_in(x) when x in 1..3,      do: :range
  def fun_in(x) when x in @at_list,  do: :at_list
  def fun_in(x) when x in @at_range, do: :at_range

  test :in_in_function_guards do
    assert fun_in(0) == :list
    assert fun_in(2) == :range
    assert fun_in(5) == :at_list
    assert fun_in(8) == :at_range
  end

  defmacrop case_in(x, y) do
    quote do
      case 0 do
        _ when unquote(x) in unquote(y) -> true
        _ -> false
      end
    end
  end

  test :in_in_case_guards do
    assert case_in(1, [1,2,3]) == true
    assert case_in(1, 1..3) == true
    assert case_in(2, 1..3) == true
    assert case_in(3, 1..3) == true
    assert case_in(-3, -1..-3) == true
  end

  test :paren do
    assert nil?(()) == true
    assert ((); ();) == nil
    assert [ 1, (), 3 ] == [1, nil, 3 ]
    assert [do: ()] == [do: nil]
    assert { 1, (), 3 } == { 1, nil, 3 }
    assert (Kernel.&& nil, ()) == nil
    assert (Kernel.&& nil, ()) == nil
    assert (() && ()) == nil
    assert (if(() && ()) do
      :ok
    else
      :error
    end) == :error
  end

  test :__info__ do
    assert { :in, 2 } in Kernel.__info__(:macros)
  end

  test :__info__not_included do
    assert not ({ :__info__, 1 } in Kernel.__info__(:functions))
  end

  test :macro_exported? do
    assert macro_exported?(Kernel, :in, 2) == true
    assert macro_exported?(Kernel, :def, 1) == true
    assert macro_exported?(Kernel, :def, 2) == true
    assert macro_exported?(Kernel, :def, 3) == false
    assert macro_exported?(Kernel, :no_such_macro, 2) == false
  end

  test :debug_info do
    assert :debug_info in Kernel.__info__(:compile)[:options]
  end

  test :apply do
    assert apply(Enum, :reverse, [[1|[2, 3]]]) == [3, 2, 1]
    assert apply(fn x -> x * 2 end, [2]) == 4
  end

  test :function_from___ENV__ do
    assert __ENV__.function == { :test_function_from___ENV__, 1 }
  end

  test :binding do
    x = 1
    assert binding == [x: 1]
    assert binding([:x, :y]) == [x: 1]
    assert binding([:x, :y], nil) == [x: 1]

    x = 2
    assert binding == [x: 2]

    y = 3
    assert binding == [x: 2, y: 3]

    var!(x, :foo) = 2
    assert binding(:foo) == [x: 2]
    assert binding([:x, :y], :foo) == [x: 2]
  end

  test :binding_on_match do
    x = 1
    assert binding() == [x: 1]
    refute binding() == [x: 2]
  end

  defmodule User do
    defstruct name: "jose"
  end

  test :struct do
    assert struct(User) == %User{name: "jose"}

    user = struct(User, name: "eric")
    assert user == %User{ name: "eric" }

    assert struct(user, unknown: "key") == user
    assert struct(user, name: "jose") == %User{ name: "jose" }
    assert struct(user, name: "other") == %User{ name: "other" }
  end

  defmodule Conversions do
    use ExUnit.Case, async: true

    test :binary_to_integer do
      assert binary_to_integer("123") == 123
    end

    test :binary_to_integer_with_base do
      assert binary_to_integer("755", 8) == 493
      assert binary_to_integer("3FF", 16) == 1023
    end

    test :binary_to_float do
      assert binary_to_float("2.2017764e+0") == 2.2017764
    end

    test :integer_to_binary do
      assert integer_to_binary(77) == "77"
    end

    test :integer_to_binary_with_base do
      assert integer_to_binary(493, 8) == "755"
      assert integer_to_binary(1023, 16) == "3FF"
    end

    test :float_to_binary do
      assert float_to_binary(7.0) == "7.00000000000000000000e+00"
    end

    test :float_to_binary_with_options do
      assert float_to_binary(7.1, [decimals: 2]) == "7.10"
      assert float_to_binary(7.1, [scientific: 2]) == "7.10e+00"
      assert float_to_binary(7.1, [decimals: 2, compact: true]) == "7.1"
      assert float_to_binary(7.1, [scientific: 2, compact: true]) == "7.10e+00"
      assert float_to_binary(7.1, [decimals: 2, compact: false]) == "7.10"
    end

    test :atom_to_binary_defaults_to_utf8 do
      assert atom_to_binary(:another_atom) == "another_atom"
    end

    test :binary_to_atom_defaults_to_utf8 do
      assert binary_to_atom("another_binary") == :another_binary
    end

    test :binary_to_existing_atom_defaults_to_utf8 do
      :existing_atom
      assert binary_to_existing_atom("existing_atom") == :existing_atom

      assert_raise ArgumentError, fn ->
        binary_to_existing_atom "nonexisting_atom"
      end
    end
  end

  defmodule DefDelegate do
    use ExUnit.Case, async: true

    defdelegate my_flatten(list), to: List, as: :flatten
    defdelegate [map(callback, list)], to: :lists, append_first: true

    dynamic = :dynamic_flatten
    defdelegate unquote(dynamic)(list), to: List, as: :flatten

    test :defdelegate_with_function do
      assert my_flatten([[1]]) == [1]
    end

    test :defdelegate_with_appended_handle do
      assert map([1], fn(x) -> x + 1 end) == [2]
    end

    test :dynamic_defdelegate do
      assert dynamic_flatten([[1]]) == [1]
    end
  end

  defmodule PipelineOp do
    use ExUnit.Case, async: true

    test "simple" do
      assert [1, [2], 3] |> List.flatten == [1, 2, 3]
    end

    test "nested pipelines" do
      assert [1, [2], 3] |> List.flatten |> Enum.map(&(&1 * 2)) == [2, 4, 6]
    end

    test "local call" do
      assert [1, [2], 3] |> List.flatten |> local == [2, 4, 6]
    end

    test "pipeline with capture" do
      assert Enum.map([1, 2, 3], &(&1 |> twice |> twice)) == [4, 8, 12]
    end

    test "non-call" do
      assert  1  |> (&(&1*2)).() == 2
      assert [1] |> (&hd(&1)).() == 1

      import CompileAssertion
      assert_compile_fail ArgumentError, "cannot pipe 1 into 2", "1 |> 2"
    end

    defp twice(a), do: a * 2

    defp local(list) do
      Enum.map(list, &(&1 * 2))
    end
  end

  defmodule IfScope do
    use ExUnit.Case, async: true

    test :variables_on_nested_if do
      if true do
        a = 1
        if true do
          b = 2
        end
      end

      assert a == 1
      assert b == 2
    end

    test :variables_on_siblings_if do
      if true do
        a = 1

        if true do
          b = 2
        end

        if true do
          c = 3
        end
      end

      assert a == 1
      assert b == 2
      assert c == 3
    end

    test :variables_counter_on_nested_if do
      r = (fn() -> 3 end).() # supresses warning at (if r < 0...)
      r = r - 1
      r = r - 1
      r = r - 1

      if true do
        r = r - 1
        if r < 0, do: r = 0
      end

      assert r == 0
    end
  end

  defmodule Destructure do
    use ExUnit.Case, async: true

    test :less do
      destructure [x, y, z], [1, 2, 3, 4, 5]
      assert x == 1
      assert y == 2
      assert z == 3
    end

    test :more do
      destructure [a, b, c, d, e], [1, 2, 3]
      assert a == 1
      assert b == 2
      assert c == 3
      assert d == nil
      assert e == nil
    end

    test :equal do
      destructure [a, b, c], [1, 2, 3]
      assert a == 1
      assert b == 2
      assert c == 3
    end

    test :none do
      destructure [a, b, c], []
      assert a == nil
      assert b == nil
      assert c == nil
    end

    test :match do
      destructure [1, b, _], [1, 2, 3]
      assert b == 2
    end

    test :nil do
      destructure [a, b, c], a_nil
      assert a == nil
      assert b == nil
      assert c == nil
    end

    test :invalid_match do
      a = List.first([3])
      assert_raise CaseClauseError, fn ->
        destructure [^a, _b, _c], a_list
      end
    end

    defp a_list, do: [1, 2, 3]
    defp a_nil, do: nil
  end
end
