Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.ExprTest do
  use ExUnit.Case, async: true

  import TypeHelper

  defmodule :"Elixir.Module.Types.ExprTest.Struct" do
    defstruct foo: :atom, bar: 123, baz: %{}
  end

  test "literal" do
    assert quoted_expr(true) == {:ok, {:atom, true}}
    assert quoted_expr(false) == {:ok, {:atom, false}}
    assert quoted_expr(:foo) == {:ok, {:atom, :foo}}
    assert quoted_expr(0) == {:ok, :integer}
    assert quoted_expr(0.0) == {:ok, :float}
    assert quoted_expr("foo") == {:ok, :binary}
  end

  describe "list" do
    test "proper" do
      assert quoted_expr([]) == {:ok, {:list, :dynamic}}
      assert quoted_expr([123]) == {:ok, {:list, :integer}}
      assert quoted_expr([123, 456]) == {:ok, {:list, :integer}}
      assert quoted_expr([123 | []]) == {:ok, {:list, :integer}}
      assert quoted_expr([123, "foo"]) == {:ok, {:list, {:union, [:integer, :binary]}}}
      assert quoted_expr([123 | ["foo"]]) == {:ok, {:list, {:union, [:integer, :binary]}}}
    end

    test "improper" do
      assert quoted_expr([123 | 456]) == {:ok, {:list, :integer}}
      assert quoted_expr([123, 456 | 789]) == {:ok, {:list, :integer}}
      assert quoted_expr([123 | "foo"]) == {:ok, {:list, {:union, [:integer, :binary]}}}
    end

    test "keyword" do
      assert quoted_expr(a: 1, b: 2) ==
               {:ok,
                {:list,
                 {:union,
                  [
                    {:tuple, 2, [{:atom, :a}, :integer]},
                    {:tuple, 2, [{:atom, :b}, :integer]}
                  ]}}}
    end
  end

  test "tuple" do
    assert quoted_expr({}) == {:ok, {:tuple, 0, []}}
    assert quoted_expr({:a}) == {:ok, {:tuple, 1, [{:atom, :a}]}}
    assert quoted_expr({:a, 123}) == {:ok, {:tuple, 2, [{:atom, :a}, :integer]}}
  end

  # Use module attribute to avoid formatter adding parentheses
  @mix_module Mix

  test "module call" do
    assert quoted_expr(@mix_module.shell) == {:ok, :dynamic}
    assert quoted_expr(@mix_module.shell.info) == {:ok, {:var, 0}}
  end

  describe "binary" do
    test "literal" do
      assert quoted_expr(<<"foo"::binary>>) == {:ok, :binary}
      assert quoted_expr(<<123::integer>>) == {:ok, :binary}
      assert quoted_expr(<<123::utf8>>) == {:ok, :binary}
      assert quoted_expr(<<"foo"::utf8>>) == {:ok, :binary}
    end

    test "variable" do
      assert quoted_expr([foo], <<foo::little>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::integer>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::integer()>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::integer-little>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::little-integer>>) == {:ok, :binary}
    end

    test "infer" do
      assert quoted_expr(
               (
                 foo = 0.0
                 <<foo::float>>
               )
             ) == {:ok, :binary}

      assert quoted_expr(
               (
                 foo = 0
                 <<foo::float>>
               )
             ) == {:ok, :binary}

      assert quoted_expr([foo], {<<foo::integer>>, foo}) ==
               {:ok, {:tuple, 2, [:binary, :integer]}}

      assert quoted_expr([foo], {<<foo::binary>>, foo}) == {:ok, {:tuple, 2, [:binary, :binary]}}

      assert quoted_expr([foo], {<<foo::utf8>>, foo}) ==
               {:ok, {:tuple, 2, [:binary, {:union, [:integer, :binary]}]}}

      assert {:error, {:unable_unify, {:integer, :binary, _}}} =
               quoted_expr(
                 (
                   foo = 0
                   <<foo::binary>>
                 )
               )

      assert {:error, {:unable_unify, {:binary, :integer, _}}} =
               quoted_expr([foo], <<foo::binary-0, foo::integer>>)
    end
  end

  test "variables" do
    assert quoted_expr([foo], foo) == {:ok, {:var, 0}}
    assert quoted_expr([foo], {foo}) == {:ok, {:tuple, 1, [{:var, 0}]}}
    assert quoted_expr([foo, bar], {foo, bar}) == {:ok, {:tuple, 2, [{:var, 0}, {:var, 1}]}}
  end

  test "pattern match" do
    assert {:error, _} = quoted_expr(:foo = 1)
    assert {:error, _} = quoted_expr(1 = :foo)

    assert quoted_expr(:foo = :foo) == {:ok, {:atom, :foo}}
    assert quoted_expr(1 = 1) == {:ok, :integer}
  end

  test "block" do
    assert quoted_expr(
             (
               a = 1
               a
             )
           ) == {:ok, :integer}

    assert quoted_expr(
             (
               a = :foo
               a
             )
           ) == {:ok, {:atom, :foo}}

    assert {:error, _} =
             quoted_expr(
               (
                 a = 1
                 :foo = a
               )
             )
  end

  describe "case" do
    test "infer pattern" do
      assert quoted_expr(
               [a],
               case a do
                 :foo = b -> :foo = b
               end
             ) == {:ok, :dynamic}

      assert {:error, _} =
               quoted_expr(
                 [a],
                 case a do
                   :foo = b -> :bar = b
                 end
               )
    end

    test "do not leak pattern/guard inference between clauses" do
      assert quoted_expr(
               [a],
               case a do
                 :foo = b -> b
                 :bar = b -> b
               end
             ) == {:ok, :dynamic}

      assert quoted_expr(
               [a],
               case a do
                 b when is_atom(b) -> b
                 b when is_integer(b) -> b
               end
             ) == {:ok, :dynamic}

      assert quoted_expr(
               [a],
               case a do
                 :foo = b -> :foo = b
                 :bar = b -> :bar = b
               end
             ) == {:ok, :dynamic}
    end

    test "do not leak body inference between clauses" do
      assert quoted_expr(
               [a],
               case a do
                 :foo ->
                   b = :foo
                   b

                 :bar ->
                   b = :bar
                   b
               end
             ) == {:ok, :dynamic}

      assert quoted_expr(
               [a, b],
               case a do
                 :foo -> :foo = b
                 :bar -> :bar = b
               end
             ) == {:ok, :dynamic}

      assert quoted_expr(
               [a, b],
               case a do
                 :foo when is_binary(b) -> b <> ""
                 :foo when is_list(b) -> b
               end
             ) == {:ok, :dynamic}
    end
  end

  test "fn" do
    assert quoted_expr(fn :foo = b -> :foo = b end) == {:ok, :dynamic}

    assert {:error, _} = quoted_expr(fn :foo = b -> :bar = b end)
  end

  test "receive" do
    assert quoted_expr(
             receive do
             after
               0 -> :ok
             end
           ) == {:ok, :dynamic}
  end

  test "with" do
    assert quoted_expr(
             [a, b],
             with(
               :foo <- a,
               :bar <- b,
               c = :baz,
               do: c
             )
           ) == {:ok, :dynamic}

    assert quoted_expr(
             [a],
             (
               with(a = :baz, do: a)
               a
             )
           ) == {:ok, {:var, 0}}
  end

  describe "for comprehension" do
    test "with generators and filters" do
      assert quoted_expr(
               [list],
               for(
                 foo <- list,
                 is_integer(foo),
                 do: foo == 123
               )
             ) == {:ok, :dynamic}
    end

    test "with unused return" do
      assert quoted_expr(
               [list, bar],
               (
                 for(
                   foo <- list,
                   is_integer(bar),
                   do: foo == 123
                 )

                 bar
               )
             ) == {:ok, {:var, 0}}
    end

    test "with reduce" do
      assert quoted_expr(
               [],
               for(i <- [1, 2, 3], do: (acc -> i + acc), reduce: 0)
             ) == {:ok, :dynamic}

      assert quoted_expr(
               [],
               for(i <- [1, 2, 3], do: (_ -> i), reduce: nil)
             ) == {:ok, :dynamic}
    end
  end
end
