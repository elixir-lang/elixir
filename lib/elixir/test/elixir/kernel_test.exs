Code.require_file "../test_helper.exs", __FILE__

defmodule KernelTest do
  use ExUnit.Case, async: true

  test :match do
    assert "abcd" =~ %r/c(d)/
    refute "abcd" =~ %r/e/
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
  end

  test :paren do
    assert nil?(())
    assert [ 1, (), 3 ] == [1, nil, 3 ]
    assert [do: ()] == [do: nil]
    assert { 1, (), 3 } == { 1, nil, 3 }
  end

  test :__info__ do
    assert { :in, 2 } inlist Kernel.__info__(:macros)
  end

  test :__info__not_included do
    assert not ({ :__info__, 1 } inlist Kernel.__info__(:functions))
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
    assert :debug_info inlist Kernel.__info__(:compile)[:options]
  end

  test :apply do
    assert apply(Enum, :reverse, [[1|[2,3]]]) == [3,2,1]
    assert apply(fn x -> x * 2 end, [2]) == 4
  end

  test :__MODULE__ do
    assert __MODULE__ == :"Elixir-KernelTest"
  end

  test :function_from___ENV__ do
    assert __ENV__.function == { :test_function_from___ENV__, 1 }
  end

  doctest Kernel

  defp x(value) when value in [1,2,3], do: true
  defp x(_),                           do: false

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

    case :proplists.get_value(:float_to_binary,
                              :proplists.get_value(:exports, :erlang.module_info, [])) do
      2 ->
        # We can only test this where this functionality is available
        test :float_to_binary_with_options do
          assert float_to_binary(7.1, [decimals: 2]) == "7.10"
          assert float_to_binary(7.1, [scientific: 2]) == "7.10e+00"
          assert float_to_binary(7.1, [decimals: 2, compact: true]) == "7.1"
          assert float_to_binary(7.1, [scientific: 2, compact: true]) == "7.10e+00"
          assert float_to_binary(7.1, [decimals: 2, compact: false]) == "7.10"
        end
      _ ->
        :ok
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

  defmodule Function do
    use ExUnit.Case, async: true

    test :retrieve_remote_function do
      assert is_function(function(:erlang, :atom_to_list, 1))
      assert :erlang.fun_info(function(:erlang, :atom_to_list, 1), :arity) == {:arity, 1}
      assert function(:erlang, :atom_to_list, 1).(:a) == 'a'
    end

    test :remote_syntax_function do
      assert function(:erlang.atom_to_list/1) ==
             function(:erlang, :atom_to_list, 1)
      assert function(Enum.map/2) == function(Enum, :map, 2)
    end

    test :retrieve_local_function do
      assert is_function(function(:atl, 1))
      assert :erlang.fun_info(function(:atl, 1), :arity) == {:arity, 1}
      assert function(:atl, 1).(:a) == 'a'
    end

    test :local_syntax_function do
      assert function(atl/1).(:a) == 'a'
    end

    test :retrieve_imported_function do
      assert is_function(function(:atom_to_list, 1))
      assert :erlang.fun_info(function(:atom_to_list, 1), :arity) == {:arity, 1}
      assert function(:atom_to_list, 1).(:a) == 'a'
    end

    test :retrieve_dynamic_function do
      a = :erlang
      b = :atom_to_list
      c = 1

      assert is_function(function(a, b, c))
      assert :erlang.fun_info(function(a, b, c), :arity) == {:arity, 1}
      assert function(a, b, c).(:a) == 'a'
    end

    test :function do
      f = function do
        x, y when y > 0 -> x + y
        x, y -> x - y
      end

      assert f.(1, 2)  == 3
      assert f.(1, -2) == 3
    end

    defp atl(arg) do
      :erlang.atom_to_list arg
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

    test :simple do
      assert [1,[2],3] |> List.flatten == [1,2,3]
    end

    test :nested do
      assert [1,[2],3] |> List.flatten |> Enum.map(&1 * 2) == [2,4,6]
    end

    test :local do
      assert [1,[2],3] |> List.flatten |> local == [2,4,6]
    end

    test :map do
      assert Enum.map([1,2,3], &1 |> twice |> twice) == [4,8,12]
    end

    test :atom do
      assert __MODULE__ |> :constant == 13
    end

    def constant, do: 13

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
      destructure [x,y,z], [1,2,3,4,5]
      assert x == 1
      assert y == 2
      assert z == 3
    end

    test :more do
      destructure [a,b,c,d,e], [1,2,3]
      assert a == 1
      assert b == 2
      assert c == 3
      assert d == nil
      assert e == nil
    end

    test :equal do
      destructure [a,b,c], [1,2,3]
      assert a == 1
      assert b == 2
      assert c == 3
    end

    test :none do
      destructure [a,b,c], []
      assert a == nil
      assert b == nil
      assert c == nil
    end

    test :match do
      destructure [1,b,_], [1,2,3]
      assert b == 2
    end

    test :nil do
      destructure [a,b,c], a_nil
      assert a == nil
      assert b == nil
      assert c == nil
    end

    test :invalid_match do
      a = 3
      assert_raise CaseClauseError, fn ->
        destructure [^a,_b,_c], a_list
      end
    end

    defp a_list, do: [1,2,3]
    defp a_nil, do: nil
  end
end
