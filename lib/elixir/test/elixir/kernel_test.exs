Code.require_file("test_helper.exs", __DIR__)

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Kernel

  defp empty_list(), do: []

  test "=~/2" do
    assert "abcd" =~ ~r/c(d)/ == true
    assert "abcd" =~ ~r/e/ == false
    assert "abcd" =~ ~R/c(d)/ == true
    assert "abcd" =~ ~R/e/ == false

    string = "^ab+cd*$"
    assert string =~ "ab+" == true
    assert string =~ "bb" == false

    assert "abcd" =~ ~r// == true
    assert "abcd" =~ ~R// == true
    assert "abcd" =~ "" == true

    assert "" =~ ~r// == true
    assert "" =~ ~R// == true
    assert "" =~ "" == true

    assert "" =~ "abcd" == false
    assert "" =~ ~r/abcd/ == false
    assert "" =~ ~R/abcd/ == false

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ "hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ ~r"hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ ~R"hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      ~r"hello" =~ "hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      ~r"hello" =~ ~r"hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      :abcd =~ ~r//
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      :abcd =~ ""
    end

    assert_raise FunctionClauseError, "no function clause matching in Regex.match?/2", fn ->
      "abcd" =~ nil
    end

    assert_raise FunctionClauseError, "no function clause matching in Regex.match?/2", fn ->
      "abcd" =~ :abcd
    end
  end

  test "^" do
    x = List.first([1])

    assert_raise MatchError, fn ->
      {x, ^x} = {2, 2}
      x
    end
  end

  test "match?/2" do
    a = List.first([0])
    assert match?(b when b > a, 1) == true
    assert binding() == [a: 0]

    assert match?(b when b > a, -1) == false
    assert binding() == [a: 0]
  end

  def exported?, do: not_exported?()
  defp not_exported?, do: true

  test "function_exported?/3" do
    assert function_exported?(__MODULE__, :exported?, 0)
    refute function_exported?(__MODULE__, :not_exported?, 0)
  end

  test "macro_exported?/3" do
    assert macro_exported?(Kernel, :in, 2) == true
    assert macro_exported?(Kernel, :def, 1) == true
    assert macro_exported?(Kernel, :def, 2) == true
    assert macro_exported?(Kernel, :def, 3) == false
    assert macro_exported?(Kernel, :no_such_macro, 2) == false
    assert macro_exported?(:erlang, :abs, 1) == false
  end

  test "apply/3 and apply/2" do
    assert apply(Enum, :reverse, [[1 | [2, 3]]]) == [3, 2, 1]
    assert apply(fn x -> x * 2 end, [2]) == 4
  end

  test "binding/0 and binding/1" do
    x = 1
    assert binding() == [x: 1]

    x = 2
    assert binding() == [x: 2]

    y = 3
    assert binding() == [x: 2, y: 3]

    var!(x, :foo) = 4
    assert binding() == [x: 2, y: 3]
    assert binding(:foo) == [x: 4]

    # No warnings
    _x = 1
    assert binding() == [_x: 1, x: 2, y: 3]
  end

  defmodule User do
    assert is_map(defstruct name: "john")
  end

  defmodule UserTuple do
    def __struct__({UserTuple, :ok}) do
      %User{}
    end
  end

  test "struct/1 and struct/2" do
    assert struct(User) == %User{name: "john"}

    user = struct(User, name: "meg")
    assert user == %User{name: "meg"}

    assert struct(user, unknown: "key") == user
    assert struct(user, %{name: "john"}) == %User{name: "john"}
    assert struct(user, name: "other", __struct__: Post) == %User{name: "other"}
  end

  test "struct!/1 and struct!/2" do
    assert struct!(User) == %User{name: "john"}

    user = struct!(User, name: "meg")
    assert user == %User{name: "meg"}

    assert_raise KeyError, fn ->
      struct!(user, unknown: "key")
    end

    assert struct!(user, %{name: "john"}) == %User{name: "john"}
    assert struct!(user, name: "other", __struct__: Post) == %User{name: "other"}
  end

  test "if/2 with invalid keys" do
    error_message =
      "invalid or duplicate keys for if, only \"do\" and an optional \"else\" are permitted"

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("if true, foo: 7")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("if true, do: 6, boo: 7")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("if true, do: 7, do: 6")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("if true, do: 8, else: 7, else: 6")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("if true, else: 6")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("if true, []")
    end
  end

  test "unless/2 with invalid keys" do
    error_message =
      "invalid or duplicate keys for unless, only \"do\" " <>
        "and an optional \"else\" are permitted"

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("unless true, foo: 7")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("unless true, do: 6, boo: 7")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("unless true, do: 7, do: 6")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("unless true, do: 8, else: 7, else: 6")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("unless true, else: 6")
    end

    assert_raise ArgumentError, error_message, fn ->
      Code.eval_string("unless true, []")
    end
  end

  test "and/2" do
    assert (true and false) == false
    assert (true and true) == true
    assert (true and 0) == 0
    assert (false and false) == false
    assert (false and true) == false
    assert (false and 0) == false
    assert (false and raise("oops")) == false
    assert ((x = true) and not x) == false
    assert_raise BadBooleanError, fn -> 0 and 1 end
  end

  test "or/2" do
    assert (true or false) == true
    assert (true or true) == true
    assert (true or 0) == true
    assert (true or raise("foo")) == true
    assert (false or false) == false
    assert (false or true) == true
    assert (false or 0) == 0
    assert ((x = false) or not x) == true
    assert_raise BadBooleanError, fn -> 0 or 1 end
  end

  test "if/2 boolean optimization does not leak variables during expansion" do
    if false do
      :ok
    else
      assert Macro.Env.vars(__ENV__) == []
    end
  end

  describe "in/2" do
    test "with literals on right side" do
      assert 2 in [1, 2, 3]
      assert 2 in 1..3
      refute 4 in [1, 2, 3]
      refute 4 in 1..3
    end

    test "with expressions on right side" do
      list = [1, 2, 3]
      assert 2 in list
      refute 4 in list

      assert 2 in [1 | [2, 3]]
      assert 3 in [1 | list]
    end

    @at_list1 [4, 5]
    @at_range 6..8
    @at_list2 [13, 14]
    def fun_in(x) when x in [0], do: :list
    def fun_in(x) when x in 1..3, do: :range
    def fun_in(x) when x in @at_list1, do: :at_list
    def fun_in(x) when x in @at_range, do: :at_range
    def fun_in(x) when x in [9 | [10, 11]], do: :list_cons
    def fun_in(x) when x in [12 | @at_list2], do: :list_cons_at
    def fun_in(_), do: :none

    test "in function guard" do
      assert fun_in(0) == :list
      assert fun_in(1) == :range
      assert fun_in(2) == :range
      assert fun_in(3) == :range
      assert fun_in(5) == :at_list
      assert fun_in(6) == :at_range
      assert fun_in(7) == :at_range
      assert fun_in(8) == :at_range
      assert fun_in(9) == :list_cons
      assert fun_in(10) == :list_cons
      assert fun_in(11) == :list_cons
      assert fun_in(12) == :list_cons_at
      assert fun_in(13) == :list_cons_at
      assert fun_in(14) == :list_cons_at

      assert fun_in(0.0) == :none
      assert fun_in(1.0) == :none
      assert fun_in(2.0) == :none
      assert fun_in(3.0) == :none
      assert fun_in(6.0) == :none
      assert fun_in(7.0) == :none
      assert fun_in(8.0) == :none
      assert fun_in(9.0) == :none
      assert fun_in(10.0) == :none
      assert fun_in(11.0) == :none
      assert fun_in(12.0) == :none
      assert fun_in(13.0) == :none
      assert fun_in(14.0) == :none
    end

    def dynamic_in(x, y, z) when x in y..z, do: true
    def dynamic_in(_x, _y, _z), do: false

    test "in dynamic function guard" do
      assert dynamic_in(1, 1, 3)
      assert dynamic_in(2, 1, 3)
      assert dynamic_in(3, 1, 3)

      assert dynamic_in(1, 3, 1)
      assert dynamic_in(2, 3, 1)
      assert dynamic_in(3, 3, 1)

      refute dynamic_in(0, 1, 3)
      refute dynamic_in(4, 1, 3)
      refute dynamic_in(0, 3, 1)
      refute dynamic_in(4, 3, 1)

      refute dynamic_in(2, 1.0, 3)
      refute dynamic_in(2, 1, 3.0)
      refute dynamic_in(2.0, 1, 3)
    end

    defmacrop case_in(x, y) do
      quote do
        case 0 do
          _ when unquote(x) in unquote(y) -> true
          _ -> false
        end
      end
    end

    test "in case guard" do
      assert case_in(1, [1, 2, 3]) == true
      assert case_in(1, 1..3) == true
      assert case_in(2, 1..3) == true
      assert case_in(3, 1..3) == true
      assert case_in(-3, -1..-3) == true
    end

    test "performs all side-effects" do
      assert 1 in [1, send(self(), 2)]
      assert_received 2

      assert 1 in [1 | send(self(), [2])]
      assert_received [2]

      assert 2 in [1 | send(self(), [2])]
      assert_received [2]
    end

    test "has proper evaluation order" do
      a = 1
      assert 1 in [a = 2, a]
      # silence unused var warning
      _ = a
    end

    test "in module body" do
      defmodule InSample do
        @foo [:a, :b]
        true = :a in @foo
      end
    after
      purge(InSample)
    end

    test "inside and/2" do
      response = %{code: 200}

      if is_map(response) and response.code in 200..299 do
        :pass
      end

      # This module definition copies internal variable
      # defined during in/2 expansion.
      Module.create(InVarCopy, nil, __ENV__)
      purge(InVarCopy)
    end

    test "with a non-literal non-escaped compile-time range in guards" do
      message = "non-literal range in guard should be escaped with Macro.escape/2"

      assert_eval_raise(ArgumentError, message, """
      defmodule InErrors do
        range = 1..3
        def foo(x) when x in unquote(range), do: :ok
      end
      """)
    end

    test "with a non-compile-time range in guards" do
      message = ~r/invalid args for operator "in", .* got: :hello/

      assert_eval_raise(ArgumentError, message, """
      defmodule InErrors do
        def foo(x) when x in :hello, do: :ok
      end
      """)
    end

    test "with a non-compile-time list cons in guards" do
      message = ~r/invalid args for operator "in", .* got: \[1 | list\(\)\]/

      assert_eval_raise(ArgumentError, message, """
      defmodule InErrors do
        def list, do: [1]
        def foo(x) when x in [1 | list()], do: :ok
      end
      """)
    end

    test "with a compile-time non-list in tail in guards" do
      message = ~r/invalid args for operator "in", .* got: \[1 | 1..3\]/

      assert_eval_raise(ArgumentError, message, """
      defmodule InErrors do
        def foo(x) when x in [1 | 1..3], do: :ok
      end
      """)
    end

    test "with a non-integer range" do
      message = "ranges (first..last) expect both sides to be integers, got: 0..5.0"

      assert_raise ArgumentError, message, fn ->
        last = 5.0
        1 in 0..last
      end
    end

    test "hoists variables" do
      result = expand_to_string(quote(do: rand() in 1..2))
      assert result =~ "var = rand()"

      assert result =~
               ":erlang.andalso(:erlang.is_integer(var), :erlang.andalso(:erlang.>=(var, 1), :erlang.\"=<\"(var, 2)))"

      result = expand_to_string(quote(do: rand() in [1, 2]))
      assert result =~ "var = rand()"
      assert result =~ ":erlang.orelse(:erlang.\"=:=\"(var, 2), :erlang.\"=:=\"(var, 1))"

      result = expand_to_string(quote(do: rand() in [1 | [2]]))
      assert result =~ "var = rand()"
      assert result =~ ":erlang.orelse(:erlang.\"=:=\"(var, 1), :erlang.\"=:=\"(var, 2))"

      result = expand_to_string(quote(do: rand() in [1 | some_call()]))
      assert result =~ "var = rand()"
      assert result =~ "{arg0} = {some_call()}"
      assert result =~ ":erlang.orelse(:erlang.\"=:=\"(var, 1), :lists.member(var, arg0))"
    end

    test "is optimized" do
      assert expand_to_string(quote(do: foo in [])) == "Enum.member?([], foo)"

      assert expand_to_string(quote(do: foo in 0..1)) ==
               ":erlang.andalso(:erlang.is_integer(foo), :erlang.andalso(:erlang.>=(foo, 0), :erlang.\"=<\"(foo, 1)))"

      assert expand_to_string(quote(do: foo in -1..0)) ==
               ":erlang.andalso(:erlang.is_integer(foo), :erlang.andalso(:erlang.>=(foo, -1), :erlang.\"=<\"(foo, 0)))"
    end

    defp expand_to_string(ast) do
      ast
      |> Macro.prewalk(&Macro.expand(&1, __ENV__))
      |> Macro.to_string()
    end

    defp assert_eval_raise(error, msg, string) do
      assert_raise error, msg, fn ->
        Code.eval_string(string)
      end
    end
  end

  describe "__info__" do
    test ":macros" do
      assert {:in, 2} in Kernel.__info__(:macros)
    end

    test ":functions" do
      refute {:__info__, 1} in Kernel.__info__(:functions)
    end

    test "others" do
      assert Kernel.__info__(:module) == Kernel
      assert is_list(Kernel.__info__(:compile))
      assert is_list(Kernel.__info__(:attributes))
    end
  end

  describe "defdelegate" do
    defdelegate my_flatten(list), to: List, as: :flatten

    dynamic = :dynamic_flatten
    defdelegate unquote(dynamic)(list), to: List, as: :flatten

    test "dispatches to delegated functions" do
      assert my_flatten([[1]]) == [1]
    end

    test "with unquote" do
      assert dynamic_flatten([[1]]) == [1]
    end

    test "raises with non-variable arguments" do
      msg = "defdelegate/2 only accepts function parameters, got: 1"

      assert_raise ArgumentError, msg, fn ->
        string = """
        defmodule IntDelegate do
          defdelegate foo(1), to: List
        end
        """

        Code.eval_string(string, [], __ENV__)
      end

      assert_raise ArgumentError, msg, fn ->
        string = """
        defmodule IntOptionDelegate do
          defdelegate foo(1 \\\\ 1), to: List
        end
        """

        Code.eval_string(string, [], __ENV__)
      end
    end

    defdelegate my_reverse(list \\ []), to: :lists, as: :reverse
    defdelegate my_get(map \\ %{}, key, default \\ ""), to: Map, as: :get

    test "accepts variable with optional arguments" do
      assert my_reverse() == []
      assert my_reverse([1, 2, 3]) == [3, 2, 1]

      assert my_get("foo") == ""
      assert my_get(%{}, "foo") == ""
      assert my_get(%{"foo" => "bar"}, "foo") == "bar"
      assert my_get(%{}, "foo", "not_found") == "not_found"
    end
  end

  describe "access" do
    test "get_in/2" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      assert get_in(users, ["john", :age]) == 27
      assert get_in(users, ["dave", :age]) == nil
      assert get_in(nil, ["john", :age]) == nil

      map = %{"fruits" => ["banana", "apple", "orange"]}
      assert get_in(map, ["fruits", by_index(0)]) == "banana"
      assert get_in(map, ["fruits", by_index(3)]) == nil
      assert get_in(map, ["unknown", by_index(3)]) == :oops

      assert_raise FunctionClauseError, fn ->
        get_in(users, [])
      end
    end

    test "put_in/3" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

      assert put_in(users, ["john", :age], 28) == %{"john" => %{age: 28}, "meg" => %{age: 23}}

      assert_raise FunctionClauseError, fn ->
        put_in(users, [], %{})
      end

      assert_raise ArgumentError, "could not put/update key \"john\" on a nil value", fn ->
        put_in(nil, ["john", :age], 28)
      end
    end

    test "put_in/2" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

      assert put_in(users["john"][:age], 28) == %{"john" => %{age: 28}, "meg" => %{age: 23}}

      assert put_in(users["john"].age, 28) == %{"john" => %{age: 28}, "meg" => %{age: 23}}

      assert_raise BadMapError, fn ->
        put_in(users["dave"].age, 19)
      end

      assert_raise KeyError, fn ->
        put_in(users["meg"].unknown, "value")
      end
    end

    test "update_in/3" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

      assert update_in(users, ["john", :age], &(&1 + 1)) ==
               %{"john" => %{age: 28}, "meg" => %{age: 23}}

      assert_raise FunctionClauseError, fn ->
        update_in(users, [], fn _ -> %{} end)
      end

      assert_raise ArgumentError, "could not put/update key \"john\" on a nil value", fn ->
        update_in(nil, ["john", :age], fn _ -> %{} end)
      end

      assert_raise UndefinedFunctionError, fn ->
        pop_in(struct(Sample, []), [:name])
      end
    end

    test "update_in/2" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

      assert update_in(users["john"][:age], &(&1 + 1)) ==
               %{"john" => %{age: 28}, "meg" => %{age: 23}}

      assert update_in(users["john"].age, &(&1 + 1)) ==
               %{"john" => %{age: 28}, "meg" => %{age: 23}}

      assert_raise BadMapError, fn ->
        update_in(users["dave"].age, &(&1 + 1))
      end

      assert_raise KeyError, fn ->
        put_in(users["meg"].unknown, &(&1 + 1))
      end
    end

    test "get_and_update_in/3" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

      assert get_and_update_in(users, ["john", :age], &{&1, &1 + 1}) ==
               {27, %{"john" => %{age: 28}, "meg" => %{age: 23}}}

      map = %{"fruits" => ["banana", "apple", "orange"]}

      assert get_and_update_in(map, ["fruits", by_index(0)], &{&1, String.reverse(&1)}) ==
               {"banana", %{"fruits" => ["ananab", "apple", "orange"]}}

      assert get_and_update_in(map, ["fruits", by_index(3)], &{&1, &1}) ==
               {nil, %{"fruits" => ["banana", "apple", "orange"]}}

      assert get_and_update_in(map, ["unknown", by_index(3)], &{&1, []}) ==
               {:oops, %{"fruits" => ["banana", "apple", "orange"], "unknown" => []}}

      assert_raise FunctionClauseError, fn ->
        update_in(users, [], fn _ -> %{} end)
      end
    end

    test "get_and_update_in/2" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

      assert get_and_update_in(users["john"].age, &{&1, &1 + 1}) ==
               {27, %{"john" => %{age: 28}, "meg" => %{age: 23}}}

      assert_raise ArgumentError, "could not put/update key \"john\" on a nil value", fn ->
        get_and_update_in(nil["john"][:age], fn nil -> {:ok, 28} end)
      end

      assert_raise BadMapError, fn ->
        get_and_update_in(users["dave"].age, &{&1, &1 + 1})
      end

      assert_raise KeyError, fn ->
        get_and_update_in(users["meg"].unknown, &{&1, &1 + 1})
      end
    end

    test "pop_in/2" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

      assert pop_in(users, ["john", :age]) == {27, %{"john" => %{}, "meg" => %{age: 23}}}

      assert pop_in(users, ["bob", :age]) == {nil, %{"john" => %{age: 27}, "meg" => %{age: 23}}}

      assert pop_in([], [:foo, :bar]) == {nil, []}

      assert_raise FunctionClauseError, fn ->
        pop_in(users, [])
      end

      assert_raise FunctionClauseError, "no function clause matching in Kernel.pop_in/2", fn ->
        pop_in(users, :not_a_list)
      end
    end

    test "pop_in/2 with paths" do
      map = %{"fruits" => ["banana", "apple", "orange"]}

      assert pop_in(map, ["fruits", by_index(0)]) ==
               {"banana", %{"fruits" => ["apple", "orange"]}}

      assert pop_in(map, ["fruits", by_index(3)]) == {nil, map}

      map = %{"fruits" => [%{name: "banana"}, %{name: "apple"}]}

      assert pop_in(map, ["fruits", by_index(0), :name]) ==
               {"banana", %{"fruits" => [%{}, %{name: "apple"}]}}

      assert pop_in(map, ["fruits", by_index(3), :name]) == {nil, map}
    end

    test "pop_in/1" do
      users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

      assert pop_in(users["john"][:age]) == {27, %{"john" => %{}, "meg" => %{age: 23}}}
      assert pop_in(users["john"][:name]) == {nil, %{"john" => %{age: 27}, "meg" => %{age: 23}}}
      assert pop_in(users["bob"][:age]) == {nil, %{"john" => %{age: 27}, "meg" => %{age: 23}}}

      users = %{john: [age: 27], meg: [age: 23]}

      assert pop_in(users.john[:age]) == {27, %{john: [], meg: [age: 23]}}
      assert pop_in(users.john[:name]) == {nil, %{john: [age: 27], meg: [age: 23]}}

      assert pop_in([][:foo][:bar]) == {nil, []}
      assert_raise KeyError, fn -> pop_in(users.bob[:age]) end
    end

    test "pop_in/1/2 with nils" do
      users = %{"john" => nil, "meg" => %{age: 23}}
      assert pop_in(users["john"][:age]) == {nil, %{"meg" => %{age: 23}}}
      assert pop_in(users, ["john", :age]) == {nil, %{"meg" => %{age: 23}}}

      users = %{john: nil, meg: %{age: 23}}
      assert pop_in(users.john[:age]) == {nil, %{john: nil, meg: %{age: 23}}}
      assert pop_in(users, [:john, :age]) == {nil, %{meg: %{age: 23}}}

      x = nil
      assert_raise ArgumentError, fn -> pop_in(x["john"][:age]) end
      assert_raise ArgumentError, fn -> pop_in(nil["john"][:age]) end
      assert_raise ArgumentError, fn -> pop_in(nil, ["john", :age]) end
    end

    test "with dynamic paths" do
      map = empty_map()

      assert put_in(map[:foo], "bar") == %{foo: "bar"}
      assert put_in(empty_map()[:foo], "bar") == %{foo: "bar"}
      assert put_in(KernelTest.empty_map()[:foo], "bar") == %{foo: "bar"}
      assert put_in(__MODULE__.empty_map()[:foo], "bar") == %{foo: "bar"}

      assert_raise ArgumentError, ~r"access at least one element,", fn ->
        Code.eval_quoted(quote(do: put_in(map, "bar")), [])
      end

      assert_raise ArgumentError, ~r"must start with a variable, local or remote call", fn ->
        Code.eval_quoted(quote(do: put_in(map.foo(1, 2)[:bar], "baz")), [])
      end
    end

    def empty_map, do: %{}

    def by_index(index) do
      fn
        _, nil, next ->
          next.(:oops)

        :get, data, next ->
          next.(Enum.at(data, index))

        :get_and_update, data, next ->
          current = Enum.at(data, index)

          case next.(current) do
            {get, update} -> {get, List.replace_at(data, index, update)}
            :pop -> {current, List.delete_at(data, index)}
          end
      end
    end
  end

  describe "pipeline" do
    test "simple" do
      assert [1, [2], 3] |> List.flatten() == [1, 2, 3]
    end

    test "nested" do
      assert [1, [2], 3] |> List.flatten() |> Enum.map(&(&1 * 2)) == [2, 4, 6]
    end

    test "local call" do
      assert [1, [2], 3] |> List.flatten() |> local == [2, 4, 6]
    end

    test "with capture" do
      assert Enum.map([1, 2, 3], &(&1 |> twice |> twice)) == [4, 8, 12]
    end

    test "with anonymous functions" do
      assert 1 |> (&(&1 * 2)).() == 2
      assert [1] |> (&hd(&1)).() == 1
    end

    defp twice(a), do: a * 2

    defp local(list) do
      Enum.map(list, &(&1 * 2))
    end
  end

  describe "destructure" do
    test "less args" do
      destructure [x, y, z], [1, 2, 3, 4, 5]
      assert x == 1
      assert y == 2
      assert z == 3
    end

    test "more args" do
      destructure [a, b, c, d, e], [1, 2, 3]
      assert a == 1
      assert b == 2
      assert c == 3
      assert d == nil
      assert e == nil
    end

    test "equal args" do
      destructure [a, b, c], [1, 2, 3]
      assert a == 1
      assert b == 2
      assert c == 3
    end

    test "no values" do
      destructure [a, b, c], []
      assert a == nil
      assert b == nil
      assert c == nil
    end

    test "works as match" do
      destructure [1, b, _], [1, 2, 3]
      assert b == 2
    end

    test "nil values" do
      destructure [a, b, c], a_nil()
      assert a == nil
      assert b == nil
      assert c == nil
    end

    test "invalid match" do
      a = List.first([3])

      assert_raise MatchError, fn ->
        destructure [^a, _b, _c], a_list()
      end
    end

    defp a_list, do: [1, 2, 3]
    defp a_nil, do: nil
  end

  describe "use/2" do
    import ExUnit.CaptureIO

    defmodule SampleA do
      defmacro __using__(opts) do
        prefix = Keyword.get(opts, :prefix, "")
        IO.puts(prefix <> "A")
      end
    end

    defmodule SampleB do
      defmacro __using__(_) do
        IO.puts("B")
      end
    end

    test "invalid argument is literal" do
      message = "invalid arguments for use, expected a compile time atom or alias, got: 42"

      assert_raise ArgumentError, message, fn ->
        Code.eval_string("use 42")
      end
    end

    test "invalid argument is variable" do
      message = "invalid arguments for use, expected a compile time atom or alias, got: variable"

      assert_raise ArgumentError, message, fn ->
        Code.eval_string("use variable")
      end
    end

    test "multi-call" do
      assert capture_io(fn ->
               Code.eval_string("use KernelTest.{SampleA, SampleB,}", [], __ENV__)
             end) == "A\nB\n"
    end

    test "multi-call with options" do
      assert capture_io(fn ->
               Code.eval_string(~S|use KernelTest.{SampleA}, prefix: "-"|, [], __ENV__)
             end) == "-A\n"
    end

    test "multi-call with unquote" do
      assert capture_io(fn ->
               string = """
               defmodule TestMod do
                 def main() do
                   use KernelTest.{SampleB, unquote(:SampleA)}
                 end
               end
               """

               Code.eval_string(string, [], __ENV__)
             end) == "B\nA\n"
    after
      purge(KernelTest.TestMod)
    end
  end

  test "tl/1" do
    assert tl([:one]) == []
    assert tl([1, 2, 3]) == [2, 3]

    assert_raise ArgumentError, "argument error", fn ->
      tl(empty_list())
    end

    assert tl([:a | :b]) == :b
    assert tl([:a, :b | :c]) == [:b | :c]
  end

  test "hd/1" do
    assert hd([1, 2, 3, 4]) == 1

    assert_raise ArgumentError, "argument error", fn ->
      hd(empty_list())
    end

    assert hd([1 | 2]) == 1
  end

  test "floor/1" do
    assert floor(1) === 1
    assert floor(1.0) === 1
    assert floor(0) === 0
    assert floor(0.0) === 0
    assert floor(-0.0) === 0
    assert floor(1.123) === 1
    assert floor(-10.123) === -11
    assert floor(-10) === -10
    assert floor(-10.0) === -10

    assert match?(x when floor(x) == 0, 0.2)
  end

  test "ceil/1" do
    assert ceil(1) === 1
    assert ceil(1.0) === 1
    assert ceil(0) === 0
    assert ceil(0.0) === 0
    assert ceil(-0.0) === 0
    assert ceil(1.123) === 2
    assert ceil(-10.123) === -10
    assert ceil(-10) === -10
    assert ceil(-10.0) === -10

    assert match?(x when ceil(x) == 1, 0.2)
  end

  test "sigil_U/2" do
    assert_raise ArgumentError, ~r"reason: :invalid_format", fn ->
      Code.eval_string(~s{~U[2015-01-13 13:00]})
    end

    assert_raise ArgumentError, ~r"reason: :missing_offset", fn ->
      Code.eval_string(~s{~U[2015-01-13 13:00:07]})
    end

    assert_raise ArgumentError, ~r"reason: :invalid_offset", fn ->
      Code.eval_string(~s{~U[2015-01-13 13:00:07+00:30]})
    end
  end

  defp purge(module) do
    :code.delete(module)
    :code.purge(module)
  end
end
