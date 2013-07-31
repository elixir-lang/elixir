Code.require_file "test_helper.exs", __DIR__

defmodule KernelTest do
  use ExUnit.Case, async: true

  test :match do
    assert ("abcd" =~ %r/c(d)/) == true
    assert ("abcd" =~ %r/e/) == false

    string = "^ab+cd*$"
    assert (string =~ "ab+") == true
    assert (string =~ "bb") == false

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ "hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ %r"hello"
    end
  end

  test :^ do
    x = 1

    assert_raise MatchError, fn ->
      { x, ^x } = { 2, 2 }
      x
    end
  end

  test :match? do
    assert match?(x, 1)
    assert binding([:x]) == []

    a = 0
    assert match?(b when b > a, 1)
    assert binding([:b]) == []

    refute match?(b when b > a, -1)
    assert binding([:b]) == []
  end

  test :nil? do
    assert nil?(nil)
    refute nil?(0)
    refute nil?(false)
  end

  test :in do
    assert x(1)
    refute x(4)
    refute x([])

    assert 2 in [1, 2, 3]
    assert 2 in 1..3
    refute 4 in [1, 2, 3]
    refute 4 in 1..3

    list = [1, 2, 3]
    assert 2 in list
    refute 4 in list
  end

  test :paren do
    assert nil?(())
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
    assert macro_exported?(Kernel, :def, 4) == true
    assert macro_exported?(Kernel, :no_such_macro, 2) == false
  end

  test :debug_info do
    assert :debug_info in Kernel.__info__(:compile)[:options]
  end

  test :apply do
    assert apply(Enum, :reverse, [[1|[2, 3]]]) == [3, 2, 1]
    assert apply(fn x -> x * 2 end, [2]) == 4
  end

  test :__MODULE__ do
    assert __MODULE__ == :"Elixir.KernelTest"
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
    assert binding() = [x: 1]
    refute binding() = [x: 2]
  end

  defp x(value) when value in [1, 2, 3], do: true
  defp x(_),                             do: false

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
      expected  = atom_to_binary :some_binary, :utf8
      actual    = atom_to_binary :some_binary

      assert actual == expected
      assert atom_to_binary(:another_atom) == "another_atom"
    end

    test :binary_to_atom_defaults_to_utf8 do
      expected  = binary_to_atom "some_binary", :utf8
      actual    = binary_to_atom "some_binary"

      assert actual == expected
      assert binary_to_atom("another_binary") == :another_binary
    end

    test :binary_to_existing_atom_defaults_to_utf8 do
      expected = binary_to_atom "existing_atom", :utf8
      actual   = binary_to_existing_atom "existing_atom"

      assert actual == expected

      :existing_atom
      assert binary_to_existing_atom("existing_atom") == :existing_atom

      assert_raise ArgumentError, fn ->
        binary_to_existing_atom "nonexisting_atom"
      end
    end
  end

  defmodule Runtime do
    use ExUnit.Case, async: true
    defp kernel, do: Kernel

    test :not do
      assert kernel.not(true) == false
      assert kernel.not(false) == true
    end

    test :! do
      assert kernel.!(true) == false
      assert kernel.!(nil) == true
      assert kernel.!(false) == true
    end

    test :list do
      assert kernel.++([1], [2]) == [1, 2]
      assert kernel.--([1], [1]) == []
    end

    test :math do
      assert kernel.+(1, 2) == 3
      assert kernel.-(2, 2) == 0
      assert kernel.*(2, 2) == 4
      assert kernel./(2, 2) == 1.0
    end

    test :unary do
      assert kernel.+(1) == 1
      assert kernel.-(1) == -1
    end

    test :send do
      kernel.<-(self, :hello)
      assert_received :hello
    end

    test :comp do
      assert kernel.==(1, 1)
      refute kernel.==(1, 2)

      assert kernel.!=(1, 2)
      refute kernel.!=(1, 1)

      assert kernel.===(1, 1)
      refute kernel.===(1, 2)

      assert kernel.!==(1, 2)
      refute kernel.!==(1, 1)

      assert kernel.>(1, 0)
      refute kernel.>(1, 2)

      assert kernel.<(0, 1)
      refute kernel.<(2, 1)

      assert kernel.>=(1, 0)
      refute kernel.>=(1, 2)

      assert kernel.<=(0, 1)
      refute kernel.<=(2, 1)
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
      assert [1, [2], 3] |> List.flatten |> Enum.map(&1 * 2) == [2, 4, 6]
    end

    test "local call" do
      assert [1, [2], 3] |> List.flatten |> local == [2, 4, 6]
    end

    test "pipeline with capture" do
      assert Enum.map([1, 2, 3], &1 |> twice |> twice) == [4, 8, 12]
    end

    test "non-call" do
      assert  1  |> (&1*2).() == 2
      assert [1] |> hd(&1).() == 1

      import CompileAssertion
      assert_compile_fail ArgumentError, "unsupported expression in pipeline |> operator: 2", "1 |> 2"
    end

    defp twice(a), do: a * 2

    defp local(list) do
      Enum.map(list, &1 * 2)
    end
  end

  defmodule Bang do
    use ExUnit.Case, async: true

    test :bang do
      assert bang(1)     == :truthy
      assert bang(true)  == :truthy
      assert bang(nil)   == :falsy
      assert bang(false) == :falsy
    end

    test :bangbang do
      assert bangbang(1)     == :truthy
      assert bangbang(true)  == :truthy
      assert bangbang(nil)   == :falsy
      assert bangbang(false) == :falsy
    end

    defp bangbang(value) when !!value, do: :truthy
    defp bangbang(_), do: :falsy

    defp bang(value) when !value, do: :falsy
    defp bang(_), do: :truthy
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
      a = 3
      assert_raise CaseClauseError, fn ->
        destructure [^a, _b, _c], a_list
      end
    end

    defp a_list, do: [1, 2, 3]
    defp a_nil, do: nil
  end
end
