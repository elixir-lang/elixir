Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.QuoteTest do
  use ExUnit.Case, async: true

  defmacrop custom_file do
    quote file: "HELLO", do: __FILE__
  end

  test :file do
    assert custom_file == "HELLO"
  end

  test :list do
    assert quote(do: [1,2,3]) == [1,2,3]
  end

  test :tuple do
    assert quote(do: { :a, 1 }) == {:a,1}
  end

  test :keep_line do
    ## DO NOT MOVE THIS LINE
    assert quote(line: :keep, do: bar(1,2,3)) == { :bar, [line: 24], [1,2,3] }
  end

  test :fixed_line do
    assert quote(line: 3, do: bar(1,2,3)) == { :bar, [line: 3], [1,2,3] }
  end

  test :keep_location do
    ## DO NOT MOVE THIS LINE
    assert quote(location: :keep, do: bar(1,2,3)) == {
      :__scope__,
      [line: 33],
      [
        [file: __FILE__],
        [do: { :bar, [line: 33], [1,2,3] }]
      ]
    }
  end

  test :quote_line_var do
    ## DO NOT MOVE THIS LINE
    line = __ENV__.line
    assert quote(line: line, do: bar(1,2,3)) == { :bar, [line: 45], [1,2,3] }
  end

  test :unquote_call do
    assert quote(do: foo(bar)[:baz])
    assert quote(do: unquote(:bar)()) == quote(do: bar())
    assert quote(do: unquote(:bar)(1) do 2 + 3 end) == quote(do: bar(1) do 2 + 3 end)
    assert quote(do: foo.unquote(:bar)) == quote(do: foo.bar)
    assert quote(do: foo.unquote(:bar)(1)) == quote(do: foo.bar(1))
    assert quote(do: foo.unquote(:bar)(1) do 2 + 3 end) == quote(do: foo.bar(1) do 2 + 3 end)

    assert Code.eval_quoted(quote(do: Foo.unquote(Bar)))  == { Elixir.Foo.Bar, [] }
    assert Code.eval_quoted(quote(do: Foo.unquote(quote do: Bar))) == { Elixir.Foo.Bar, [] }

    assert_raise SyntaxError, fn ->
      quote(do: foo.unquote(1))
    end
  end

  test :nested_quote do
    assert { :quote, _, [[do: { :unquote, _, _ }]] } = quote(do: quote(do: unquote(x)))
  end

  defmacrop nested_quote_in_macro do
    x = 1
    quote do
      x = unquote(x)
      quote do
        unquote(x)
      end
    end
  end

  test :nested_quote_in_macro do
    assert nested_quote_in_macro == 1
  end

  Enum.each [foo: 1, bar: 2, baz: 3], fn { k, v } ->
    def unquote(k)(arg) do
      unquote(v) + arg
    end
  end

  test :dynamic_definition_with_unquote do
    assert foo(1) == 2
    assert bar(2) == 4
    assert baz(3) == 6
  end

  test :splice_on_root do
    contents = [1, 2, 3]
    assert quote(do: unquote_splicing(contents)) == quote do: (1; 2; 3)
  end

  test :splice_on_pipe do
    contents = [1, 2, 3]
    assert quote(do: [unquote_splicing(contents)|[1,2,3]]) == [1,2,3,1,2,3]
  end

  test :stab do
    assert { :->, _, [{[],_}] } = (quote do -> end)
    assert { :->, _, [{[],_}] } = (quote do: (->))

    assert { :->, _, [{[1],_}] } = (quote do 1 -> end)
    assert { :->, _, [{[1],_}] } = (quote do: (1 ->))

    assert { :->, _, [{[],1}] } = (quote do -> 1 end)
    assert { :->, _, [{[],1}] } = (quote do: (-> 1))
  end
end

defmodule Kernel.QuoteTest.VarHygiene do
  defmacro no_interference do
    quote do: a = 1
  end

  defmacro no_hygiene do
    quote [hygiene: [vars: false]] do
      a = 1
    end
  end

  defmacro write_interference do
    quote do: var!(a) = 1
  end

  defmacro read_interference do
    quote do: 10 = var!(a)
  end

  defmacro cross_module_interference do
    quote do: var!(:a, Kernel.QuoteTest.VarHygieneTest) = 1
  end
end

defmodule Kernel.QuoteTest.VarHygieneTest do
  use ExUnit.Case, async: true
  import Kernel.QuoteTest.VarHygiene

  defmacrop cross_module_no_interference do
    quote do: a = 10
  end

  defmacrop read_cross_module do
    quote do: var!(a, __MODULE__)
  end

  test :no_interference do
    a = 10
    no_interference
    assert a == 10
  end

  test :no_hygiene do
    no_hygiene
    assert a == 1
  end

  test :cross_module_no_interference do
    cross_module_no_interference
    no_interference
    assert read_cross_module == 10
  end

  test :cross_module_interference do
    cross_module_no_interference
    cross_module_interference
    assert read_cross_module == 1
  end

  test :write_interference do
    write_interference
    assert a == 1
  end

  test :read_interference do
    a = 10
    read_interference
  end
end

defmodule Kernel.QuoteTest.AliasHygieneTest do
  use ExUnit.Case, async: true

  alias Dict, as: SuperDict

  defmacro dict do
    quote do: Dict.Bar
  end

  defmacro super_dict do
    quote do: SuperDict.Bar
  end

  test :annotate_aliases do
    assert quote(do: Foo.Bar) == { :__aliases__, [alias: false], [:Foo, :Bar] }
    assert quote(do: Dict.Bar) == { :__aliases__, [alias: false], [:Dict, :Bar] }
    assert quote(do: SuperDict.Bar) == { :__aliases__, [alias: Dict.Bar], [:SuperDict, :Bar] }
    assert quote(do: alias!(SuperDict.Bar)) == { :__aliases__, [], [:SuperDict, :Bar] }
  end

  test :expand_aliases do
    assert Code.eval_quoted(quote do: SuperDict.Bar) == { Elixir.Dict.Bar, [] }
    assert Code.eval_quoted(quote do: alias!(SuperDict.Bar)) == { Elixir.SuperDict.Bar, [] }

    assert Code.eval_quoted(quote do
      alias HashDict, as: SuperDict
      SuperDict.Bar
    end) == { Elixir.HashDict.Bar, [] }

    assert Code.eval_quoted(quote do
      alias HashDict, as: Dict
      require Kernel.QuoteTest.AliasHygieneTest
      Kernel.QuoteTest.AliasHygieneTest.dict
    end) == { Elixir.Dict.Bar, [] }

    assert Code.eval_quoted(quote do
      alias HashDict, as: SuperDict
      require Kernel.QuoteTest.AliasHygieneTest
      Kernel.QuoteTest.AliasHygieneTest.super_dict
    end) == { Elixir.Dict.Bar, [] }
  end
end

defmodule Kernel.QuoteTest.ImportsHygieneTest do
  use ExUnit.Case, async: true

  defmacrop get_bin_size do
    quote do
      size("hello")
    end
  end

  defmacrop get_bin_size_with_partial do
    quote do
      size(&1).("hello")
    end
  end

  defmacrop get_bin_size_with_function do
    quote do
      function(size/1).("hello")
    end
  end

  defmacrop get_bin_size_with_kernel_function do
    quote do
      Kernel.function(size/1).("hello")
    end
  end

  test :expand_imports do
    import Kernel, except: [size: 1]
    assert get_bin_size == 5
    assert get_bin_size_with_partial == 5
    assert get_bin_size_with_function == 5
    assert get_bin_size_with_kernel_function == 5
  end

  defmacrop get_dict_size do
    import Kernel, except: [size: 1]

    quote do
      size([a: 1, b: 2])
    end
  end

  test :lazy_expand_imports do
    import Kernel, except: [size: 1]
    import Dict, only: [size: 1]
    assert get_dict_size == 2
  end

  defmacrop with_size do
    quote do
      import Kernel, except: [size: 1]
      import Dict, only: [size: 1]
      size([a: 1, b: 2])
    end
  end

  defmacrop with_nested_size do
    quote do
      with_size
    end
  end

  test :explicitly_overriden_imports do
    assert with_size == 2
    assert with_nested_size == 2
  end
end
