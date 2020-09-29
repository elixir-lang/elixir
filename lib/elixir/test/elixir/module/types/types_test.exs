Code.require_file("../../test_helper.exs", __DIR__)

defmodule Module.Types.TypesTest do
  use ExUnit.Case, async: true
  alias Module.Types
  alias Module.Types.{Pattern, Expr, Unify}

  defmacro warning(patterns \\ [], guards \\ [], body) do
    min_line = min_line(patterns ++ guards ++ [body])
    patterns = reset_line(patterns, min_line)
    guards = reset_line(guards, min_line)
    body = reset_line(body, min_line)
    expr = TypeHelper.expand_expr(patterns, guards, body, __CALLER__)

    quote do
      unquote(Macro.escape(expr))
      |> Module.Types.TypesTest.__expr__()
      |> to_warning()
    end
  end

  def __expr__({patterns, guards, body}) do
    with {:ok, _types, context} <-
           Pattern.of_head(patterns, guards, TypeHelper.new_stack(), TypeHelper.new_context()),
         {:ok, type, context} <- Expr.of_expr(body, TypeHelper.new_stack(), context) do
      flunk("expexted error, got: #{inspect([type] |> Unify.lift_types(context) |> hd())}")
    else
      {:error, {type, reason, context}} ->
        {:error, {type, reason, context}}
    end
  end

  defp reset_line(ast, min_line) do
    Macro.prewalk(ast, fn ast ->
      Macro.update_meta(ast, fn meta ->
        Keyword.update!(meta, :line, &(&1 - min_line + 1))
      end)
    end)
  end

  defp min_line(ast) do
    {_ast, min} =
      Macro.prewalk(ast, :infinity, fn
        {_fun, meta, _args} = ast, min -> {ast, min(min, Keyword.get(meta, :line, 1))}
        other, min -> {other, min}
      end)

    min
  end

  defp to_warning({:error, {type, reason, context}}) do
    {Module.Types, error, _location} = Module.Types.error_to_warning(type, reason, context)

    error
    |> Module.Types.format_warning()
    |> List.to_string()
    |> String.trim_trailing("\nConflict found at")
  end

  test "expr_to_string/1" do
    assert Types.expr_to_string({1, 2}) == "{1, 2}"
    assert Types.expr_to_string(quote(do: Foo.bar(arg))) == "Foo.bar(arg)"
    assert Types.expr_to_string(quote(do: :erlang.band(a, b))) == "Bitwise.band(a, b)"
    assert Types.expr_to_string(quote(do: :erlang.orelse(a, b))) == "a or b"
    assert Types.expr_to_string(quote(do: :erlang."=:="(a, b))) == "a === b"
    assert Types.expr_to_string(quote(do: :erlang.list_to_atom(a))) == "List.to_atom(a)"
    assert Types.expr_to_string(quote(do: :maps.remove(a, b))) == "Map.delete(b, a)"
    assert Types.expr_to_string(quote(do: :erlang.element(1, a))) == "elem(a, 0)"
    assert Types.expr_to_string(quote(do: :erlang.element(:erlang.+(a, 1), b))) == "elem(b, a)"
  end

  describe "function head warnings" do
    test "warns on literals" do
      string = warning([var = 123, var = "abc"], var)

      assert string == """
             incompatible types:

                 integer() !~ binary()

             in expression:

                 # types_test.ex:1
                 var = "abc"

             where "var" was given the type integer() in:

                 # types_test.ex:1
                 var = 123

             where "var" was given the type binary() in:

                 # types_test.ex:1
                 var = "abc"
             """
    end

    test "warns on binary patterns" do
      string = warning([<<var::integer, var::binary>>], var)

      assert string == """
             incompatible types:

                 integer() !~ binary()

             in expression:

                 # types_test.ex:1
                 <<..., var::binary()>>

             where "var" was given the type integer() in:

                 # types_test.ex:1
                 <<var::integer(), ...>>

             where "var" was given the type binary() in:

                 # types_test.ex:1
                 <<..., var::binary()>>
             """
    end

    test "warns on recursive patterns" do
      string = warning([{var} = var], var)

      assert string == """
             incompatible types:

                 {var0} !~ var0

             in expression:

                 # types_test.ex:1
                 {var} = var

             where "var" was given the type {var0} in:

                 # types_test.ex:1
                 {var} = var
             """
    end

    test "warns on guards" do
      string = warning([var], [is_integer(var) and is_binary(var)], var)

      assert string == """
             incompatible types:

                 integer() !~ binary()

             in expression:

                 # types_test.ex:1
                 is_binary(var)

             where "var" was given the type integer() in:

                 # types_test.ex:1
                 is_integer(var)

             where "var" was given the type binary() in:

                 # types_test.ex:1
                 is_binary(var)
             """
    end

    test "warns on guards with multiple variables" do
      string = warning([x = y], [is_integer(x) and is_binary(y)], {x, y})

      assert string == """
             incompatible types:

                 integer() !~ binary()

             in expression:

                 # types_test.ex:1
                 is_binary(y)

             where "y" was given the same type as "x" in:

                 # types_test.ex:1
                 x = y

             where "y" was given the type binary() in:

                 # types_test.ex:1
                 is_binary(y)

             where "x" was given the type integer() in:

                 # types_test.ex:1
                 is_integer(x)
             """
    end

    test "only show relevant traces in warning" do
      string = warning([x = y, z], [is_integer(x) and is_binary(y) and is_boolean(z)], {x, y, z})

      assert string == """
             incompatible types:

                 integer() !~ binary()

             in expression:

                 # types_test.ex:1
                 is_binary(y)

             where "y" was given the same type as "x" in:

                 # types_test.ex:1
                 x = y

             where "y" was given the type binary() in:

                 # types_test.ex:1
                 is_binary(y)

             where "x" was given the type integer() in:

                 # types_test.ex:1
                 is_integer(x)
             """
    end

    test "check body" do
      string = warning([x], [is_integer(x)], :foo = x)

      assert string == """
             incompatible types:

                 integer() !~ :foo

             in expression:

                 # types_test.ex:1
                 :foo = x

             where "x" was given the type integer() in:

                 # types_test.ex:1
                 is_integer(x)

             where "x" was given the type :foo in:

                 # types_test.ex:1
                 :foo = x
             """
    end

    test "check binary" do
      string = warning([foo], [is_binary(foo)], <<foo>>)

      assert string == """
             incompatible types:

                 binary() !~ integer()

             in expression:

                 # types_test.ex:1
                 <<foo>>

             where "foo" was given the type binary() in:

                 # types_test.ex:1
                 is_binary(foo)

             where "foo" was given the type integer() in:

                 # types_test.ex:1
                 <<foo>>

             HINT: all expressions given to binaries are assumed to be of type \
             integer() unless said otherwise. For example, <<expr>> assumes "expr" \
             is an integer. Pass a modifier, such as <<expr::float>> or <<expr::binary>>, \
             to change the default behaviour.
             """

      string = warning([foo], [is_binary(foo)], <<foo::integer>>)

      assert string == """
             incompatible types:

                 binary() !~ integer()

             in expression:

                 # types_test.ex:1
                 <<foo::integer()>>

             where "foo" was given the type binary() in:

                 # types_test.ex:1
                 is_binary(foo)

             where "foo" was given the type integer() in:

                 # types_test.ex:1
                 <<foo::integer()>>
             """
    end
  end

  describe "map warnings" do
    test "handling of non-singleton types in maps" do
      string =
        warning(
          [],
          (
            event = %{"type" => "order"}
            %{"amount" => amount} = event
            %{"user" => user} = event
            %{"id" => user_id} = user
            {:order, user_id, amount}
          )
        )

      assert string == """
             incompatible types:

                 binary() !~ map()

             in expression:

                 # types_test.ex:5
                 %{"id" => user_id} = user

             where "user" was given the same type as "amount" in:

                 # types_test.ex:4
                 %{"user" => user} = event

             where "user" was given the type map() in:

                 # types_test.ex:5
                 %{"id" => user_id} = user

             where "amount" was given the type binary() in:

                 # types_test.ex:3
                 %{"amount" => amount} = event
             """
    end

    test "show map() when comparing against non-map" do
      string =
        warning(
          [foo],
          (
            foo.bar
            :atom = foo
          )
        )

      assert string == """
             incompatible types:

                 map() !~ :atom

             in expression:

                 # types_test.ex:4
                 :atom = foo

             where "foo" was given the type map() (due to calling var.field) in:

                 # types_test.ex:3
                 foo.bar

             where "foo" was given the type :atom in:

                 # types_test.ex:4
                 :atom = foo

             HINT: "var.field" (without parentheses) implies "var" is a map() while \
             "var.fun()" (with parentheses) implies "var" is an atom()
             """
    end

    test "use module as map (without parentheses)" do
      string =
        warning(
          [foo],
          (
            %module{} = foo
            module.__struct__
          )
        )

      assert string == """
             incompatible types:

                 map() !~ atom()

             in expression:

                 # types_test.ex:4
                 module.__struct__

             where "module" was given the type atom() in:

                 # types_test.ex:3
                 %module{}

             where "module" was given the type map() (due to calling var.field) in:

                 # types_test.ex:4
                 module.__struct__

             HINT: "var.field" (without parentheses) implies "var" is a map() while \
             "var.fun()" (with parentheses) implies "var" is an atom()
             """
    end

    test "use map as module (with parentheses)" do
      string = warning([foo], [is_map(foo)], foo.__struct__())

      assert string == """
             incompatible types:

                 map() !~ atom()

             in expression:

                 # types_test.ex:1
                 foo.__struct__()

             where "foo" was given the type map() in:

                 # types_test.ex:1
                 is_map(foo)

             where "foo" was given the type atom() (due to calling var.fun()) in:

                 # types_test.ex:1
                 foo.__struct__()

             HINT: "var.field" (without parentheses) implies "var" is a map() while \
             "var.fun()" (with parentheses) implies "var" is an atom()
             """
    end

    test "non-existant map field warning" do
      string =
        warning(
          (
            map = %{foo: 1}
            map.bar
          )
        )

      assert string == """
             undefined field "bar" in expression:

                 # types_test.ex:3
                 map.bar

             expected one of the following fields: foo

             where "map" was given the type map() in:

                 # types_test.ex:2
                 map = %{foo: 1}
             """
    end

    test "non-existant struct field warning" do
      string =
        warning(
          [foo],
          (
            %URI{} = foo
            foo.bar
          )
        )

      assert string == """
             undefined field "bar" in expression:

                 # types_test.ex:4
                 foo.bar

             expected one of the following fields: __struct__, authority, fragment, host, path, port, query, scheme, userinfo

             where "foo" was given the type %URI{} in:

                 # types_test.ex:3
                 %URI{} = foo
             """
    end

    test "expands type variables" do
      string =
        warning(
          [%{foo: key} = event, other_key],
          [is_integer(key) and is_atom(other_key)],
          %{foo: ^other_key} = event
        )

      assert string == """
             incompatible types:

                 %{foo: integer()} !~ %{foo: atom()}

             in expression:

                 # types_test.ex:3
                 %{foo: ^other_key} = event

             where "event" was given the type %{foo: integer(), optional(dynamic()) => dynamic()} in:

                 # types_test.ex:1
                 %{foo: key} = event

             where "event" was given the type %{foo: atom(), optional(dynamic()) => dynamic()} in:

                 # types_test.ex:3
                 %{foo: ^other_key} = event
             """
    end
  end
end
