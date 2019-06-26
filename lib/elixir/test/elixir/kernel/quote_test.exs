Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.QuoteTest do
  use ExUnit.Case, async: true

  test "list" do
    assert quote(do: [1, 2, 3]) == [1, 2, 3]
  end

  test "tuple" do
    assert quote(do: {:a, 1}) == {:a, 1}
  end

  test "keep line" do
    # DO NOT MOVE THIS LINE
    assert quote(location: :keep, do: bar(1, 2, 3)) ==
             {:bar, [keep: {Path.relative_to_cwd(__ENV__.file), 16}], [1, 2, 3]}
  end

  test "fixed line" do
    assert quote(line: 3, do: bar(1, 2, 3)) == {:bar, [line: 3], [1, 2, 3]}
  end

  test "quote line var" do
    # DO NOT MOVE THIS LINE
    line = __ENV__.line
    assert quote(line: line, do: bar(1, 2, 3)) == {:bar, [line: 26], [1, 2, 3]}
  end

  test "generated" do
    assert quote(generated: true, do: bar(1)) == {:bar, [generated: true], [1]}
  end

  test "unquote call" do
    assert quote(do: foo(bar)[unquote(:baz)]) == quote(do: foo(bar)[:baz])
    assert quote(do: unquote(:bar)()) == quote(do: bar())

    assert (quote do
              unquote(:bar)(1) do
                2 + 3
              end
            end) ==
             (quote do
                bar 1 do
                  2 + 3
                end
              end)

    assert quote(do: foo.unquote(:bar)) == quote(do: foo.bar)
    assert quote(do: foo.unquote(:bar)(1)) == quote(do: foo.bar(1))

    assert (quote do
              foo.unquote(:bar)(1) do
                2 + 3
              end
            end) ==
             (quote do
                foo.bar 1 do
                  2 + 3
                end
              end)

    assert quote(do: foo.unquote({:bar, [], nil})) == quote(do: foo.bar)
    assert quote(do: foo.unquote({:bar, [], [1, 2]})) == quote(do: foo.bar(1, 2))

    assert Code.eval_quoted(quote(do: Foo.unquote(Bar))) == {Elixir.Foo.Bar, []}
    assert Code.eval_quoted(quote(do: Foo.unquote(quote(do: Bar)))) == {Elixir.Foo.Bar, []}

    assert_raise ArgumentError, fn ->
      quote(do: foo.unquote(1))
    end
  end

  test "nested quote" do
    assert {:quote, _, [[do: {:unquote, _, _}]]} = quote(do: quote(do: unquote(x)))
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

  test "nested quote in macro" do
    assert nested_quote_in_macro() == 1
  end

  defmodule Dyn do
    for {k, v} <- [foo: 1, bar: 2, baz: 3] do
      # Local call unquote
      def unquote(k)(), do: unquote(v)

      # Remote call unquote
      def unquote(k)(arg), do: __MODULE__.unquote(k)() + arg
    end
  end

  test "dynamic definition with unquote" do
    assert Dyn.foo() == 1
    assert Dyn.bar() == 2
    assert Dyn.baz() == 3

    assert Dyn.foo(1) == 2
    assert Dyn.bar(2) == 4
    assert Dyn.baz(3) == 6
  end

  test "splice on root" do
    contents = [1, 2, 3]

    assert quote(do: (unquote_splicing(contents))) ==
             (quote do
                1
                2
                3
              end)
  end

  test "splice with tail" do
    contents = [1, 2, 3]

    assert quote(do: [unquote_splicing(contents) | [1, 2, 3]]) == [1, 2, 3, 1, 2, 3]

    assert quote(do: [unquote_splicing(contents) | val]) == quote(do: [1, 2, 3 | val])

    assert quote(do: [unquote_splicing(contents) | unquote([4])]) == quote(do: [1, 2, 3, 4])
  end

  test "splice on stab" do
    {fun, []} = Code.eval_quoted(quote(do: fn unquote_splicing([1, 2, 3]) -> :ok end), [])
    assert fun.(1, 2, 3) == :ok

    {fun, []} = Code.eval_quoted(quote(do: fn 1, unquote_splicing([2, 3]) -> :ok end), [])
    assert fun.(1, 2, 3) == :ok
  end

  @compile {:no_warn_undefined, Kernel.QuoteTest.Hello}

  test "splice on definition" do
    defmodule Hello do
      def world([unquote_splicing(["foo", "bar"]) | rest]) do
        rest
      end
    end

    assert Hello.world(["foo", "bar", "baz"]) == ["baz"]
  end

  test "splice on map" do
    assert %{unquote_splicing(foo: :bar)} == %{foo: :bar}
    assert %{unquote_splicing(foo: :bar), baz: :bat} == %{foo: :bar, baz: :bat}
    assert %{unquote_splicing(foo: :bar), :baz => :bat} == %{foo: :bar, baz: :bat}
    assert %{:baz => :bat, unquote_splicing(foo: :bar)} == %{foo: :bar, baz: :bat}

    map = %{foo: :default}
    assert %{map | unquote_splicing(foo: :bar)} == %{foo: :bar}
  end

  test "when" do
    assert [{:->, _, [[{:when, _, [1, 2, 3, 4]}], 5]}] = quote(do: (1, 2, 3 when 4 -> 5))
    assert [{:->, _, [[{:when, _, [1, 2, 3, 4]}], 5]}] = quote(do: (1, 2, 3 when 4 -> 5))

    assert [{:->, _, [[{:when, _, [1, 2, 3, {:when, _, [4, 5]}]}], 6]}] =
             quote(do: (1, 2, 3 when 4 when 5 -> 6))
  end

  test "stab" do
    assert [{:->, _, [[], 1]}] =
             (quote do
                () -> 1
              end)

    assert [{:->, _, [[], 1]}] = quote(do: (() -> 1))
  end

  test "bind quoted" do
    args = [
      {:=, [], [{:foo, [], Kernel.QuoteTest}, 3]},
      {:foo, [], Kernel.QuoteTest}
    ]

    quoted = quote(bind_quoted: [foo: 1 + 2], do: foo)
    assert quoted == {:__block__, [], args}
  end

  test "literals" do
    assert quote(do: []) == []
    assert quote(do: nil) == nil

    assert (quote do
              []
            end) == []

    assert (quote do
              nil
            end) == nil
  end

  defmacrop dynamic_opts do
    [line: 3]
  end

  test "with dynamic opts" do
    assert quote(dynamic_opts(), do: bar(1, 2, 3)) == {:bar, [line: 3], [1, 2, 3]}
  end

  test "unary with integer precedence" do
    assert quote(do: +1.foo) == quote(do: +1.foo)
    assert quote(do: (@1).foo) == quote(do: (@1).foo)
    assert quote(do: &1.foo) == quote(do: &1.foo)
  end

  test "operators slash arity" do
    assert {:/, _, [{:+, _, _}, 2]} = quote(do: + / 2)
    assert {:/, _, [{:&&, _, _}, 3]} = quote(do: && / 3)
  end

  test "pipe precedence" do
    assert {:|>, _, [{:|>, _, [{:foo, _, _}, {:bar, _, _}]}, {:baz, _, _}]} =
             quote(do: foo |> bar |> baz)

    assert {:|>, _, [{:|>, _, [{:foo, _, _}, {:bar, _, _}]}, {:baz, _, _}]} =
             (quote do
                foo do
                end
                |> bar
                |> baz
              end)

    assert {:|>, _, [{:|>, _, [{:foo, _, _}, {:bar, _, _}]}, {:baz, _, _}]} =
             (quote do
                foo
                |> bar do
                end
                |> baz
              end)

    assert {:|>, _, [{:|>, _, [{:foo, _, _}, {:bar, _, _}]}, {:baz, _, _}]} =
             (quote do
                foo
                |> bar
                |> baz do
                end
              end)

    assert {:|>, _, [{:|>, _, [{:foo, _, _}, {:bar, _, _}]}, {:baz, _, _}]} =
             (quote do
                foo do
                end
                |> bar
                |> baz do
                end
              end)

    assert {:|>, _, [{:|>, _, [{:foo, _, _}, {:bar, _, _}]}, {:baz, _, _}]} =
             (quote do
                foo do
                end
                |> bar do
                end
                |> baz do
                end
              end)
  end
end

# DO NOT MOVE THIS LINE
defmodule Kernel.QuoteTest.Errors do
  defmacro defraise do
    quote location: :keep do
      def will_raise(_a, _b), do: raise("oops")
    end
  end

  defmacro will_raise do
    quote(location: :keep, do: raise("oops"))
  end
end

defmodule Kernel.QuoteTest.ErrorsTest do
  use ExUnit.Case, async: true
  import Kernel.QuoteTest.Errors

  # Defines the add function
  defraise()

  test "inside function error" do
    try do
      will_raise(:a, :b)
    rescue
      RuntimeError ->
        mod = Kernel.QuoteTest.ErrorsTest
        file = __ENV__.file |> Path.relative_to_cwd() |> String.to_charlist()
        assert [{^mod, :will_raise, 2, [file: ^file, line: 277]} | _] = __STACKTRACE__
    else
      _ -> flunk("expected failure")
    end
  end

  test "outside function error" do
    try do
      will_raise()
    rescue
      RuntimeError ->
        mod = Kernel.QuoteTest.ErrorsTest
        file = __ENV__.file |> Path.relative_to_cwd() |> String.to_charlist()
        assert [{^mod, _, _, [file: ^file, line: 308]} | _] = __STACKTRACE__
    else
      _ -> flunk("expected failure")
    end
  end
end

defmodule Kernel.QuoteTest.VarHygiene do
  defmacro no_interference do
    quote(do: a = 1)
  end

  defmacro write_interference do
    quote(do: var!(a) = 1)
  end

  defmacro read_interference do
    quote(do: 10 = var!(a))
  end

  defmacro cross_module_interference do
    quote(do: var!(a, Kernel.QuoteTest.VarHygieneTest) = 1)
  end
end

defmodule Kernel.QuoteTest.VarHygieneTest do
  use ExUnit.Case, async: true
  import Kernel.QuoteTest.VarHygiene

  defmacrop cross_module_no_interference do
    quote(do: a = 10)
  end

  defmacrop read_cross_module do
    quote(do: var!(a, __MODULE__))
  end

  defmacrop nested(var, do: block) do
    quote do
      var = unquote(var)
      unquote(block)
      var
    end
  end

  defmacrop hat do
    quote do
      var = 1
      ^var = 1
      var
    end
  end

  test "no interference" do
    a = 10
    no_interference()
    assert a == 10
  end

  test "cross module interference" do
    cross_module_no_interference()
    cross_module_interference()
    assert read_cross_module() == 1
  end

  test "write interference" do
    write_interference()
    assert a == 1
  end

  test "read interference" do
    a = 10
    read_interference()
  end

  test "hat" do
    assert hat() == 1
  end

  test "nested macro" do
    assert (nested 1 do
              nested 2 do
                _ = :ok
              end
            end) == 1
  end

  @compile {:no_warn_undefined, Kernel.QuoteTest.VarHygieneTest.UseNestedQuote}

  test "nested quoted" do
    defmodule NestedQuote do
      defmacro __using__(_) do
        quote unquote: false do
          arg = quote(do: arg)

          def test(arg) do
            unquote(arg)
          end
        end
      end
    end

    defmodule UseNestedQuote do
      use NestedQuote
    end

    assert UseNestedQuote.test("foo") == "foo"
  end

  @compile {:no_warn_undefined, Kernel.QuoteTest.VarHygieneTest.UseNestedBindQuoted}

  test "nested bind quoted" do
    defmodule NestedBindQuoted do
      defmacrop macro(arg) do
        quote bind_quoted: [arg: arg] do
          quote bind_quoted: [arg: arg], do: String.duplicate(arg, 2)
        end
      end

      defmacro __using__(_) do
        quote do
          def test do
            unquote(macro("foo"))
          end
        end
      end
    end

    defmodule UseNestedBindQuoted do
      use NestedBindQuoted
    end

    assert UseNestedBindQuoted.test() == "foofoo"
  end
end

defmodule Kernel.QuoteTest.AliasHygiene do
  alias Dict, as: SuperDict

  defmacro dict do
    quote(do: Dict.Bar)
  end

  defmacro super_dict do
    quote(do: SuperDict.Bar)
  end
end

defmodule Kernel.QuoteTest.AliasHygieneTest do
  use ExUnit.Case, async: true

  alias Dict, as: SuperDict

  test "annotate aliases" do
    assert {:__aliases__, [alias: false], [:Foo, :Bar]} = quote(do: Foo.Bar)
    assert {:__aliases__, [alias: false], [:Dict, :Bar]} = quote(do: Dict.Bar)
    assert {:__aliases__, [alias: Dict.Bar], [:SuperDict, :Bar]} = quote(do: SuperDict.Bar)
  end

  test "expand aliases" do
    assert Code.eval_quoted(quote(do: SuperDict.Bar)) == {Elixir.Dict.Bar, []}
    assert Code.eval_quoted(quote(do: alias!(SuperDict.Bar))) == {Elixir.SuperDict.Bar, []}
  end

  test "expand aliases without macro" do
    alias HashDict, as: SuperDict
    assert SuperDict.Bar == Elixir.HashDict.Bar
  end

  test "expand aliases with macro does not expand source alias" do
    alias HashDict, as: Dict, warn: false
    require Kernel.QuoteTest.AliasHygiene
    assert Kernel.QuoteTest.AliasHygiene.dict() == Elixir.Dict.Bar
  end

  test "expand aliases with macro has higher preference" do
    alias HashDict, as: SuperDict, warn: false
    require Kernel.QuoteTest.AliasHygiene
    assert Kernel.QuoteTest.AliasHygiene.super_dict() == Elixir.Dict.Bar
  end
end

defmodule Kernel.QuoteTest.ImportsHygieneTest do
  use ExUnit.Case, async: true

  # We are redefining |> and using it inside the quote
  # and only inside the quote. This code should still compile.
  defmacro x |> f do
    quote do
      unquote(x) |> unquote(f)
    end
  end

  defmacrop get_list_length do
    quote do
      length('hello')
    end
  end

  defmacrop get_list_length_with_partial do
    quote do
      (&length(&1)).('hello')
    end
  end

  defmacrop get_list_length_with_function do
    quote do
      (&length/1).('hello')
    end
  end

  test "expand imports" do
    import Kernel, except: [length: 1]
    assert get_list_length() == 5
    assert get_list_length_with_partial() == 5
    assert get_list_length_with_function() == 5
  end

  defmacrop get_string_length do
    import Kernel, except: [length: 1]

    quote do
      length("hello")
    end
  end

  test "lazy expand imports" do
    import Kernel, except: [length: 1]
    import String, only: [length: 1]
    assert get_string_length() == 5
  end

  test "lazy expand imports no conflicts" do
    import Kernel, except: [length: 1]
    import String, only: [length: 1]

    assert get_list_length() == 5
    assert get_list_length_with_partial() == 5
    assert get_list_length_with_function() == 5
  end

  defmacrop with_length do
    quote do
      import Kernel, except: [length: 1]
      import String, only: [length: 1]
      length('hello')
    end
  end

  test "explicitly overridden imports" do
    assert with_length() == 5
  end

  defmodule BinaryUtils do
    defmacro int32 do
      quote do
        integer - size(32)
      end
    end
  end

  test "checks the context also for variables to zero-arity functions" do
    import BinaryUtils
    {:int32, meta, __MODULE__} = quote(do: int32)
    assert meta[:import] == BinaryUtils
  end
end
