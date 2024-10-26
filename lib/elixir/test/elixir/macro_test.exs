Code.require_file("test_helper.exs", __DIR__)

defmodule Macro.ExternalTest do
  defmacro external do
    line = 18
    file = __ENV__.file
    ^line = __CALLER__.line
    ^file = __CALLER__.file
    ^line = Macro.Env.location(__CALLER__)[:line]
    ^file = Macro.Env.location(__CALLER__)[:file]
  end

  defmacro oror(left, right) do
    quote(do: unquote(left) || unquote(right))
  end
end

defmodule CustomIf do
  def if(_cond, _expr) do
    "custom if result"
  end
end

defmodule MacroTest do
  use ExUnit.Case, async: true
  doctest Macro

  # Changing the lines above will make compilation
  # fail since we are asserting on the caller lines
  import Macro.ExternalTest

  describe "escape/2" do
    test "returns tuples with size equal to two" do
      assert Macro.escape({:a, :b}) == {:a, :b}
    end

    test "returns lists" do
      assert Macro.escape([1, 2, 3]) == [1, 2, 3]
    end

    test "escapes tuples with size different than two" do
      assert Macro.escape({:a}) == {:{}, [], [:a]}
      assert Macro.escape({:a, :b, :c}) == {:{}, [], [:a, :b, :c]}
      assert Macro.escape({:a, {1, 2, 3}, :c}) == {:{}, [], [:a, {:{}, [], [1, 2, 3]}, :c]}

      # False positives
      assert Macro.escape({:quote, :foo, [:bar]}) == {:{}, [], [:quote, :foo, [:bar]]}
      assert Macro.escape({:quote, :foo, [:bar, :baz]}) == {:{}, [], [:quote, :foo, [:bar, :baz]]}
    end

    test "escapes maps" do
      assert Macro.escape(%{a: 1}) == {:%{}, [], [a: 1]}
    end

    test "escapes bitstring" do
      assert {:<<>>, [], args} = Macro.escape(<<300::12>>)
      assert [{:"::", [], [1, {:size, [], [4]}]}, {:"::", [], [",", {:binary, [], nil}]}] = args
    end

    test "escapes recursively" do
      assert Macro.escape([1, {:a, :b, :c}, 3]) == [1, {:{}, [], [:a, :b, :c]}, 3]
    end

    test "escapes improper lists" do
      assert Macro.escape([1 | 2]) == [{:|, [], [1, 2]}]
      assert Macro.escape([1, 2 | 3]) == [1, {:|, [], [2, 3]}]
    end

    test "prunes metadata" do
      meta = [nothing: :important, counter: 1]
      assert Macro.escape({:foo, meta, []}) == {:{}, [], [:foo, meta, []]}
      assert Macro.escape({:foo, meta, []}, prune_metadata: true) == {:{}, [], [:foo, [], []]}
    end

    test "with unquote" do
      contents = quote(unquote: false, do: unquote(1))
      assert Macro.escape(contents, unquote: true) == 1

      contents = quote(unquote: false, do: unquote(x))
      assert Macro.escape(contents, unquote: true) == {:x, [], MacroTest}
    end

    defp eval_escaped(contents) do
      {eval, []} = Code.eval_quoted(Macro.escape(contents, unquote: true))
      eval
    end

    test "with remote unquote" do
      contents = quote(unquote: false, do: Kernel.unquote(:is_atom)(:ok))
      assert eval_escaped(contents) == quote(do: Kernel.is_atom(:ok))
    end

    test "with nested unquote" do
      contents =
        quote do
          quote(do: unquote(x))
        end

      assert eval_escaped(contents) == quote(do: quote(do: unquote(x)))
    end

    test "with alias or no arguments remote unquote" do
      contents = quote(unquote: false, do: Kernel.unquote(:self)())
      assert eval_escaped(contents) == quote(do: Kernel.self())

      contents = quote(unquote: false, do: x.unquote(Foo))
      assert eval_escaped(contents) == quote(do: x.unquote(Foo))
    end

    test "with splicing" do
      contents = quote(unquote: false, do: [1, 2, 3, 4, 5])
      assert Macro.escape(contents, unquote: true) == [1, 2, 3, 4, 5]

      contents = quote(unquote: false, do: [1, 2, unquote_splicing([3, 4, 5])])
      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents = quote(unquote: false, do: [unquote_splicing([1, 2, 3]), 4, 5])
      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [unquote_splicing([1, 2, 3]), unquote_splicing([4, 5])])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4]), 5])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4]) | [5]])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]
    end

    test "does not add context to quote" do
      assert Macro.escape({:quote, [], [[do: :foo]]}) == {:{}, [], [:quote, [], [[do: :foo]]]}
    end
  end

  describe "expand_once/2" do
    test "with external macro" do
      assert {:||, _, [1, false]} = Macro.expand_once(quote(do: oror(1, false)), __ENV__)
    end

    test "with raw atom" do
      assert Macro.expand_once(quote(do: :foo), __ENV__) == :foo
    end

    test "with current module" do
      assert Macro.expand_once(quote(do: __MODULE__), __ENV__) == __MODULE__
    end

    test "with main" do
      assert Macro.expand_once(quote(do: Elixir), __ENV__) == Elixir
    end

    test "with simple alias" do
      assert Macro.expand_once(quote(do: Foo), __ENV__) == Foo
    end

    test "with current module plus alias" do
      assert Macro.expand_once(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
    end

    test "with main plus alias" do
      assert Macro.expand_once(quote(do: Elixir.Foo), __ENV__) == Foo
    end

    test "with custom alias" do
      alias Foo, as: Bar
      assert Macro.expand_once(quote(do: Bar.Baz), __ENV__) == Foo.Baz
    end

    test "with main plus custom alias" do
      alias Foo, as: Bar, warn: false
      assert Macro.expand_once(quote(do: Elixir.Bar.Baz), __ENV__) == Elixir.Bar.Baz
    end

    test "with call in alias" do
      assert Macro.expand_once(quote(do: Foo.bar().Baz), __ENV__) == quote(do: Foo.bar().Baz)
    end

    test "env" do
      env = %{__ENV__ | line: 0, lexical_tracker: self()}

      expanded = Macro.expand_once(quote(do: __ENV__), env)
      assert Macro.validate(expanded) == :ok
      assert Code.eval_quoted(expanded) == {env, []}

      assert Macro.expand_once(quote(do: __ENV__.file), env) == env.file
      assert Macro.expand_once(quote(do: __ENV__.unknown), env) == quote(do: __ENV__.unknown)

      expanded = Macro.expand_once(quote(do: __ENV__.versioned_vars), env)
      assert Macro.validate(expanded) == :ok
      assert Code.eval_quoted(expanded) == {env.versioned_vars, []}
    end

    test "env in :match context does not expand" do
      env = %{__ENV__ | line: 0, lexical_tracker: self(), context: :match}

      expanded = Macro.expand_once(quote(do: __ENV__), env)
      assert expanded == quote(do: __ENV__)

      expanded = Macro.expand_once(quote(do: __ENV__.file), env)
      assert expanded == quote(do: __ENV__.file)
    end

    defmacro local_macro(), do: raise("ignored")

    test "vars" do
      expr = {:local_macro, [], nil}
      assert Macro.expand_once(expr, __ENV__) == expr
    end

    defp expand_once_and_clean(quoted, env) do
      cleaner = &Keyword.drop(&1, [:counter])

      quoted
      |> Macro.expand_once(env)
      |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
    end

    test "with imported macro" do
      temp_var = {:x, [], Kernel}

      quoted =
        quote context: Kernel do
          case 1 do
            unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
            unquote(temp_var) -> unquote(temp_var)
          end
        end

      assert expand_once_and_clean(quote(do: 1 || false), __ENV__) == quoted
    end

    test "with require macro" do
      temp_var = {:x, [], Kernel}

      quoted =
        quote context: Kernel do
          case 1 do
            unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
            unquote(temp_var) -> unquote(temp_var)
          end
        end

      assert expand_once_and_clean(quote(do: Kernel.||(1, false)), __ENV__) == quoted
    end

    test "with not expandable expression" do
      expr = quote(do: other(1, 2, 3))
      assert Macro.expand_once(expr, __ENV__) == expr
    end

    test "propagates generated" do
      assert {:||, meta, [1, false]} = Macro.expand_once(quote(do: oror(1, false)), __ENV__)
      refute meta[:generated]

      assert {:||, meta, [1, false]} =
               Macro.expand_once(quote(generated: true, do: oror(1, false)), __ENV__)

      assert meta[:generated]
    end

    test "does not expand module attributes" do
      message =
        "could not call Module.get_attribute/2 because the module #{inspect(__MODULE__)} " <>
          "is already compiled. Use the Module.__info__/1 callback or Code.fetch_docs/1 instead"

      assert_raise ArgumentError, message, fn ->
        Macro.expand_once(quote(do: @foo), __ENV__)
      end
    end
  end

  defp expand_and_clean(quoted, env) do
    cleaner = &Keyword.drop(&1, [:counter])

    quoted
    |> Macro.expand(env)
    |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
  end

  test "expand/2" do
    temp_var = {:x, [], Kernel}

    quoted =
      quote context: Kernel do
        case 1 do
          unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
          unquote(temp_var) -> unquote(temp_var)
        end
      end

    assert expand_and_clean(quote(do: oror(1, false)), __ENV__) == quoted
  end

  test "expand_literals/2" do
    assert Macro.expand_literals(quote(do: Foo), __ENV__) == Foo
    assert Macro.expand_literals(quote(do: Foo + Bar), __ENV__) == quote(do: Foo + Bar)
    assert Macro.expand_literals(quote(do: __MODULE__), __ENV__) == __MODULE__
    assert Macro.expand_literals(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
    assert Macro.expand_literals(quote(do: [Foo, 1 + 2]), __ENV__) == [Foo, quote(do: 1 + 2)]
  end

  test "expand_literals/3" do
    fun = fn node, acc ->
      expanded = Macro.expand(node, __ENV__)
      {expanded, [expanded | acc]}
    end

    assert Macro.expand_literals(quote(do: Foo), [], fun) == {Foo, [Foo]}
    assert Macro.expand_literals(quote(do: Foo + Bar), [], fun) == {quote(do: Foo + Bar), []}
    assert Macro.expand_literals(quote(do: __MODULE__), [], fun) == {__MODULE__, [__MODULE__]}

    assert Macro.expand_literals(quote(do: __MODULE__.Foo), [], fun) ==
             {__MODULE__.Foo, [__MODULE__.Foo, __MODULE__]}
  end

  test "var/2" do
    assert Macro.var(:foo, nil) == {:foo, [], nil}
    assert Macro.var(:foo, Other) == {:foo, [], Other}
  end

  describe "dbg/3" do
    defmacrop dbg_format(ast, options \\ quote(do: [syntax_colors: []])) do
      quote do
        {result, formatted} =
          ExUnit.CaptureIO.with_io(fn ->
            unquote(Macro.dbg(ast, options, __CALLER__))
          end)

        # Make sure there's an empty line after the output.
        assert String.ends_with?(formatted, "\n\n") or
                 String.ends_with?(formatted, "\n\n" <> IO.ANSI.reset())

        {result, formatted}
      end
    end

    test "with a simple expression" do
      {result, formatted} = dbg_format(1 + 1)
      assert result == 2
      assert formatted =~ "1 + 1 #=> 2"
    end

    test "with variables" do
      my_var = 1 + 1
      {result, formatted} = dbg_format(my_var)
      assert result == 2
      assert formatted =~ "my_var #=> 2"
    end

    test "with a function call" do
      {result, formatted} = dbg_format(Atom.to_string(:foo))

      assert result == "foo"
      assert formatted =~ ~s[Atom.to_string(:foo) #=> "foo"]
    end

    test "with a multiline input" do
      {result, formatted} =
        dbg_format(
          case 1 + 1 do
            2 -> :two
            _other -> :math_is_broken
          end
        )

      assert result == :two

      assert formatted =~ """
             case 1 + 1 do
               2 -> :two
               _other -> :math_is_broken
             end #=> :two
             """
    end

    defp abc, do: [:a, :b, :c]

    test "with a pipeline on a single line" do
      {result, formatted} = dbg_format(abc() |> tl() |> tl |> Kernel.hd())
      assert result == :c

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             \nabc() #=> [:a, :b, :c]
             |> tl() #=> [:b, :c]
             |> tl #=> [:c]
             |> Kernel.hd() #=> :c
             """

      # Regression for pipes sometimes erroneously ending with three newlines (one
      # extra than needed).
      assert formatted =~ ~r/[^\n]\n\n$/
    end

    test "with a pipeline on multiple lines" do
      {result, formatted} =
        dbg_format(
          abc()
          |> tl()
          |> tl
          |> Kernel.hd()
        )

      assert result == :c

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             \nabc() #=> [:a, :b, :c]
             |> tl() #=> [:b, :c]
             |> tl #=> [:c]
             |> Kernel.hd() #=> :c
             """

      # Regression for pipes sometimes erroneously ending with three newlines (one
      # extra than needed).
      assert formatted =~ ~r/[^\n]\n\n$/
    end

    test "with simple boolean expressions" do
      {result, formatted} = dbg_format(:rand.uniform() < 0.0 and length([]) == 0)
      assert result == false

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             :rand.uniform() < 0.0 #=> false
             :rand.uniform() < 0.0 and length([]) == 0 #=> false
             """
    end

    test "with left-associative operators" do
      {result, formatted} = dbg_format(List.first([]) || "yes" || raise("foo"))
      assert result == "yes"

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             List.first([]) #=> nil
             List.first([]) || "yes" #=> "yes"
             List.first([]) || "yes" || raise "foo" #=> "yes"
             """
    end

    test "with composite boolean expressions" do
      true1 = length([]) == 0
      true2 = length([]) == 0
      {result, formatted} = dbg_format((true1 and true2) or (List.first([]) || true1))

      assert result == true

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             true1 #=> true
             true1 and true2 #=> true
             (true1 and true2) or (List.first([]) || true1) #=> true
             """
    end

    test "with block of code" do
      {result, formatted} =
        dbg_format(
          (
            a = 1
            b = a + 2
            a + b
          )
        )

      assert result == 4

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             Code block:
             (
               a = 1 #=> 1
               b = a + 2 #=> 3
               a + b #=> 4
             )
             """
    end

    test "with case" do
      list = [1, 2, 3]

      {result, formatted} =
        dbg_format(
          case list do
            [] -> nil
            _ -> Enum.sum(list)
          end
        )

      assert result == 6

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             Case argument:
             list #=> [1, 2, 3]

             Case expression (clause #2 matched):
             case list do
               [] -> nil
               _ -> Enum.sum(list)
             end #=> 6
             """
    end

    test "with case - guard" do
      {result, formatted} =
        dbg_format(
          case 0..100//5 do
            %{first: first, last: last, step: step} when last > first ->
              count = div(last - first, step)
              {:ok, count}

            _ ->
              :error
          end
        )

      assert result == {:ok, 20}

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             Case argument:
             0..100//5 #=> 0..100//5

             Case expression (clause #1 matched):
             case 0..100//5 do
               %{first: first, last: last, step: step} when last > first ->
                 count = div(last - first, step)
                 {:ok, count}

               _ ->
                 :error
             end #=> {:ok, 20}
             """
    end

    test "with cond" do
      map = %{b: 5}

      {result, formatted} =
        dbg_format(
          cond do
            a = map[:a] -> a + 1
            b = map[:b] -> b * 2
            true -> nil
          end
        )

      assert result == 10

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             Cond clause (clause #2 matched):
             b = map[:b] #=> 5

             Cond expression:
             cond do
               a = map[:a] -> a + 1
               b = map[:b] -> b * 2
               true -> nil
             end #=> 10
             """
    end

    test "if expression" do
      x = true
      map = %{a: 5, b: 1}

      {result, formatted} =
        dbg_format(
          if true and x do
            map[:a] * 2
          else
            map[:b]
          end
        )

      assert result == 10

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             If condition:
             true and x #=> true

             If expression:
             if true and x do
               map[:a] * 2
             else
               map[:b]
             end #=> 10
             """
    end

    test "if expression without else" do
      x = true
      map = %{a: 5, b: 1}

      {result, formatted} =
        dbg_format(
          if false and x do
            map[:a] * 2
          end
        )

      assert result == nil

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             If condition:
             false and x #=> false

             If expression:
             if false and x do
               map[:a] * 2
             end #=> nil
             """
    end

    test "custom if definition" do
      import Kernel, except: [if: 2]
      import CustomIf, only: [if: 2]

      {result, formatted} =
        dbg_format(
          if true do
            "something"
          end
        )

      assert result == "custom if result"

      assert formatted =~ """
             if true do
               "something"
             end #=> "custom if result"
             """
    end

    test "with with/1 (all clauses match)" do
      opts = %{width: 10, height: 15}

      {result, formatted} =
        dbg_format(
          with {:ok, width} <- Map.fetch(opts, :width),
               double_width = width * 2,
               IO.puts("just a side effect"),
               {:ok, height} <- Map.fetch(opts, :height) do
            {:ok, double_width * height}
          end
        )

      assert result == {:ok, 300}

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             With clauses:
             Map.fetch(opts, :width) #=> {:ok, 10}
             width * 2 #=> 20
             Map.fetch(opts, :height) #=> {:ok, 15}

             With expression:
             with {:ok, width} <- Map.fetch(opts, :width),
                  double_width = width * 2,
                  IO.puts("just a side effect"),
                  {:ok, height} <- Map.fetch(opts, :height) do
               {:ok, double_width * height}
             end #=> {:ok, 300}
             """
    end

    test "with with/1 (no else)" do
      opts = %{width: 10}

      {result, formatted} =
        dbg_format(
          with {:ok, width} <- Map.fetch(opts, :width),
               {:ok, height} <- Map.fetch(opts, :height) do
            {:ok, width * height}
          end
        )

      assert result == :error

      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             With clauses:
             Map.fetch(opts, :width) #=> {:ok, 10}
             Map.fetch(opts, :height) #=> :error

             With expression:
             with {:ok, width} <- Map.fetch(opts, :width),
                  {:ok, height} <- Map.fetch(opts, :height) do
               {:ok, width * height}
             end #=> :error
             """
    end

    test "with with/1 (else clause)" do
      opts = %{width: 10}

      {result, formatted} =
        dbg_format(
          with {:ok, width} <- Map.fetch(opts, :width),
               {:ok, height} <- Map.fetch(opts, :height) do
            width * height
          else
            :error -> 0
          end
        )

      assert result == 0
      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             With clauses:
             Map.fetch(opts, :width) #=> {:ok, 10}
             Map.fetch(opts, :height) #=> :error

             With expression:
             with {:ok, width} <- Map.fetch(opts, :width),
                  {:ok, height} <- Map.fetch(opts, :height) do
               width * height
             else
               :error -> 0
             end #=> 0
             """
    end

    test "with with/1 (guard)" do
      opts = %{width: 10, height: 0.0}

      {result, formatted} =
        dbg_format(
          with {:ok, width} when is_integer(width) <- Map.fetch(opts, :width),
               {:ok, height} when is_integer(height) <- Map.fetch(opts, :height) do
            width * height
          else
            _ -> nil
          end
        )

      assert result == nil
      assert formatted =~ "macro_test.exs"

      assert formatted =~ """
             With clauses:
             Map.fetch(opts, :width) #=> {:ok, 10}
             Map.fetch(opts, :height) #=> {:ok, 0.0}

             With expression:
             with {:ok, width} when is_integer(width) <- Map.fetch(opts, :width),
                  {:ok, height} when is_integer(height) <- Map.fetch(opts, :height) do
               width * height
             else
               _ -> nil
             end #=> nil
             """
    end

    test "with with/1 (guard in else)" do
      opts = %{}

      {result, _formatted} =
        dbg_format(
          with {:ok, width} <- Map.fetch(opts, :width) do
            width
          else
            other when is_integer(other) -> :int
            other when is_atom(other) -> :atom
          end
        )

      assert result == :atom
    end

    test "with with/1 respects the WithClauseError" do
      value = Enum.random([:unexpected])

      error =
        assert_raise WithClauseError, fn ->
          dbg(
            with :ok <- value do
              true
            else
              :error -> false
            end
          )
        end

      assert error.term == :unexpected
    end

    test "with \"syntax_colors: []\" it doesn't print any color sequences" do
      {_result, formatted} = dbg_format("hello")
      refute formatted =~ "\e["
    end

    test "with \"syntax_colors: [...]\" it forces color sequences" do
      {_result, formatted} = dbg_format("hello", syntax_colors: [string: :cyan])
      assert formatted =~ IO.iodata_to_binary(IO.ANSI.format([:cyan, ~s("hello")]))
    end

    test "forwards options to the underlying inspect calls" do
      value = ~c"hello"
      assert {^value, formatted} = dbg_format(value, syntax_colors: [], charlists: :as_lists)
      assert formatted =~ "value #=> [104, 101, 108, 108, 111]\n"
    end

    test "with the :print_location option set to false, doesn't print any header" do
      {result, formatted} = dbg_format("hello", print_location: false)
      assert result == "hello"
      refute formatted =~ Path.basename(__ENV__.file)
    end
  end

  describe "to_string/1" do
    test "converts quoted to string" do
      assert Macro.to_string(quote do: hello(world)) == "hello(world)"
    end

    test "converts invalid AST with inspect" do
      assert Macro.to_string(quote do: unquote(1..3)) == "1..3"
    end
  end

  describe "to_string/2" do
    defp macro_to_string(var, fun \\ fn _ast, string -> string end) do
      module = String.to_atom("Elixir.Macro")
      module.to_string(var, fun)
    end

    test "variable" do
      assert macro_to_string(quote(do: foo)) == "foo"
    end

    test "local call" do
      assert macro_to_string(quote(do: foo(1, 2, 3))) == "foo(1, 2, 3)"
      assert macro_to_string(quote(do: foo([1, 2, 3]))) == "foo([1, 2, 3])"
    end

    test "remote call" do
      assert macro_to_string(quote(do: foo.bar(1, 2, 3))) == "foo.bar(1, 2, 3)"
      assert macro_to_string(quote(do: foo.bar([1, 2, 3]))) == "foo.bar([1, 2, 3])"

      quoted =
        quote do
          (foo do
             :ok
           end).bar([1, 2, 3])
        end

      assert macro_to_string(quoted) == "(foo do\n  :ok\nend).bar([1, 2, 3])"
    end

    test "nullary remote call" do
      assert macro_to_string(quote do: foo.bar) == "foo.bar"
      assert macro_to_string(quote do: foo.bar()) == "foo.bar()"
    end

    test "atom remote call" do
      assert macro_to_string(quote(do: :foo.bar(1, 2, 3))) == ":foo.bar(1, 2, 3)"
    end

    test "remote and fun call" do
      assert macro_to_string(quote(do: foo.bar().(1, 2, 3))) == "foo.bar().(1, 2, 3)"
      assert macro_to_string(quote(do: foo.bar().([1, 2, 3]))) == "foo.bar().([1, 2, 3])"
    end

    test "unusual remote atom fun call" do
      assert macro_to_string(quote(do: Foo."42"())) == ~s/Foo."42"()/
      assert macro_to_string(quote(do: Foo."Bar"())) == ~s/Foo."Bar"()/
      assert macro_to_string(quote(do: Foo."bar baz"().""())) == ~s/Foo."bar baz"().""()/
      assert macro_to_string(quote(do: Foo."%{}"())) == ~s/Foo."%{}"()/
      assert macro_to_string(quote(do: Foo."..."())) == ~s/Foo."..."()/
    end

    test "atom fun call" do
      assert macro_to_string(quote(do: :foo.(1, 2, 3))) == ":foo.(1, 2, 3)"
    end

    test "aliases call" do
      assert macro_to_string(quote(do: Elixir)) == "Elixir"
      assert macro_to_string(quote(do: Foo)) == "Foo"
      assert macro_to_string(quote(do: Foo.Bar.baz(1, 2, 3))) == "Foo.Bar.baz(1, 2, 3)"
      assert macro_to_string(quote(do: Foo.Bar.baz([1, 2, 3]))) == "Foo.Bar.baz([1, 2, 3])"
      assert macro_to_string(quote(do: Foo.bar(<<>>, []))) == "Foo.bar(<<>>, [])"
    end

    test "keyword call" do
      assert macro_to_string(quote(do: Foo.bar(foo: :bar))) == "Foo.bar(foo: :bar)"
      assert macro_to_string(quote(do: Foo.bar("Elixir.Foo": :bar))) == "Foo.bar([{Foo, :bar}])"
    end

    test "sigil call" do
      assert macro_to_string(quote(do: ~r"123")) == ~S/~r"123"/
      assert macro_to_string(quote(do: ~r"\n123")) == ~S/~r"\n123"/
      assert macro_to_string(quote(do: ~r"12\"3")) == ~S/~r"12\"3"/
      assert macro_to_string(quote(do: ~r/12\/3/u)) == ~S"~r/12\/3/u"
      assert macro_to_string(quote(do: ~r{\n123})) == ~S/~r{\n123}/
      assert macro_to_string(quote(do: ~r((1\)(2\)3))) == ~S/~r((1\)(2\)3)/
      assert macro_to_string(quote(do: ~r{\n1{1\}23})) == ~S/~r{\n1{1\}23}/
      assert macro_to_string(quote(do: ~r|12\|3|)) == ~S"~r|12\|3|"

      assert macro_to_string(quote(do: ~r[1#{two}3])) == ~S/~r[1#{two}3]/
      assert macro_to_string(quote(do: ~r[1[#{two}\]3])) == ~S/~r[1[#{two}\]3]/
      assert macro_to_string(quote(do: ~r'1#{two}3'u)) == ~S/~r'1#{two}3'u/

      assert macro_to_string(quote(do: ~R"123")) == ~S/~R"123"/
      assert macro_to_string(quote(do: ~R"123"u)) == ~S/~R"123"u/
      assert macro_to_string(quote(do: ~R"\n123")) == ~S/~R"\n123"/

      assert macro_to_string(quote(do: ~S["'(123)'"])) == ~S/~S["'(123)'"]/
      assert macro_to_string(quote(do: ~s"#{"foo"}")) == ~S/~s"#{"foo"}"/

      assert macro_to_string(quote(do: ~HTML[hi])) == ~S/~HTML[hi]/

      assert macro_to_string(
               quote do
                 ~s"""
                 "\""foo"\""
                 """
               end
             ) == ~s[~s"""\n"\\""foo"\\""\n"""]

      assert macro_to_string(
               quote do
                 ~s'''
                 '\''foo'\''
                 '''
               end
             ) == ~s[~s'''\n'\\''foo'\\''\n''']

      assert macro_to_string(
               quote do
                 ~s"""
                 "\"foo\""
                 """
               end
             ) == ~s[~s"""\n"\\"foo\\""\n"""]

      assert macro_to_string(
               quote do
                 ~s'''
                 '\"foo\"'
                 '''
               end
             ) == ~s[~s'''\n'\\"foo\\"'\n''']

      assert macro_to_string(
               quote do
                 ~S"""
                 "123"
                 """
               end
             ) == ~s[~S"""\n"123"\n"""]

      assert macro_to_string(
               quote do
                 ~HTML"""
                 "123"
                 """
               end
             ) == ~s[~HTML"""\n"123"\n"""]
    end

    test "tuple call" do
      assert macro_to_string(quote(do: alias(Foo.{Bar, Baz, Bong}))) ==
               "alias(Foo.{Bar, Baz, Bong})"

      assert macro_to_string(quote(do: foo(Foo.{}))) == "foo(Foo.{})"
    end

    test "arrow" do
      assert macro_to_string(quote(do: foo(1, (2 -> 3)))) == "foo(1, (2 -> 3))"
    end

    test "block" do
      quoted =
        quote do
          1
          2

          (
            :foo
            :bar
          )

          3
        end

      expected = """
      (
        1
        2
        (
          :foo
          :bar
        )
        3
      )
      """

      assert macro_to_string(quoted) <> "\n" == expected
    end

    test "not in" do
      assert macro_to_string(quote(do: false not in [])) == "false not in []"
    end

    test "if else" do
      expected = """
      if(foo) do
        bar
      else
        baz
      end
      """

      assert macro_to_string(quote(do: if(foo, do: bar, else: baz))) <> "\n" == expected
    end

    test "case" do
      quoted =
        quote do
          case foo do
            true ->
              0

            false ->
              1
              2
          end
        end

      expected = """
      case(foo) do
        true ->
          0
        false ->
          1
          2
      end
      """

      assert macro_to_string(quoted) <> "\n" == expected
    end

    test "try" do
      quoted =
        quote do
          try do
            foo
          catch
            _, _ ->
              2
          rescue
            ArgumentError ->
              1
          after
            4
          else
            _ ->
              3
          end
        end

      expected = """
      try do
        foo
      rescue
        ArgumentError ->
          1
      catch
        _, _ ->
          2
      else
        _ ->
          3
      after
        4
      end
      """

      assert macro_to_string(quoted) <> "\n" == expected
    end

    test "fn" do
      assert macro_to_string(quote(do: fn -> 1 + 2 end)) == "fn -> 1 + 2 end"
      assert macro_to_string(quote(do: fn x -> x + 1 end)) == "fn x -> x + 1 end"

      quoted =
        quote do
          fn x ->
            y = x + 1
            y
          end
        end

      expected = """
      fn x ->
        y = x + 1
        y
      end
      """

      assert macro_to_string(quoted) <> "\n" == expected

      quoted =
        quote do
          fn
            x ->
              y = x + 1
              y

            z ->
              z
          end
        end

      expected = """
      fn
        x ->
          y = x + 1
          y
        z ->
          z
      end
      """

      assert macro_to_string(quoted) <> "\n" == expected

      assert macro_to_string(quote(do: (fn x -> x end).(1))) == "(fn x -> x end).(1)"

      quoted =
        quote do
          (fn
             %{} -> :map
             _ -> :other
           end).(1)
        end

      expected = """
      (fn
        %{} ->
          :map
        _ ->
          :other
      end).(1)
      """

      assert macro_to_string(quoted) <> "\n" == expected
    end

    test "range" do
      assert macro_to_string(quote(do: unquote(-1..+2))) == "-1..2"
      assert macro_to_string(quote(do: Foo.integer()..3)) == "Foo.integer()..3"
      assert macro_to_string(quote(do: unquote(-1..+2//-3))) == "-1..2//-3"

      assert macro_to_string(quote(do: Foo.integer()..3//Bar.bat())) ==
               "Foo.integer()..3//Bar.bat()"
    end

    test "when" do
      assert macro_to_string(quote(do: (-> x))) == "(() -> x)"
      assert macro_to_string(quote(do: (x when y -> z))) == "(x when y -> z)"
      assert macro_to_string(quote(do: (x, y when z -> w))) == "((x, y) when z -> w)"
      assert macro_to_string(quote(do: (x, y when z -> w))) == "((x, y) when z -> w)"
    end

    test "nested" do
      quoted =
        quote do
          defmodule Foo do
            def foo do
              1 + 1
            end
          end
        end

      expected = """
      defmodule(Foo) do
        def(foo) do
          1 + 1
        end
      end
      """

      assert macro_to_string(quoted) <> "\n" == expected
    end

    test "operator precedence" do
      assert macro_to_string(quote(do: (1 + 2) * (3 - 4))) == "(1 + 2) * (3 - 4)"
      assert macro_to_string(quote(do: (1 + 2) * 3 - 4)) == "(1 + 2) * 3 - 4"
      assert macro_to_string(quote(do: 1 + 2 + 3)) == "1 + 2 + 3"
      assert macro_to_string(quote(do: 1 + 2 - 3)) == "1 + 2 - 3"
    end

    test "capture operator" do
      assert macro_to_string(quote(do: &foo/0)) == "&foo/0"
      assert macro_to_string(quote(do: &Foo.foo/0)) == "&Foo.foo/0"
      assert macro_to_string(quote(do: &(&1 + &2))) == "&(&1 + &2)"
      assert macro_to_string(quote(do: & &1)) == "&(&1)"
      assert macro_to_string(quote(do: & &1.(:x))) == "&(&1.(:x))"
      assert macro_to_string(quote(do: (& &1).(:x))) == "(&(&1)).(:x)"
    end

    test "containers" do
      assert macro_to_string(quote(do: {})) == "{}"
      assert macro_to_string(quote(do: [])) == "[]"
      assert macro_to_string(quote(do: {1, 2, 3})) == "{1, 2, 3}"
      assert macro_to_string(quote(do: [1, 2, 3])) == "[1, 2, 3]"
      assert macro_to_string(quote(do: ["Elixir.Foo": :bar])) == "[{Foo, :bar}]"
      assert macro_to_string(quote(do: %{})) == "%{}"
      assert macro_to_string(quote(do: %{:foo => :bar})) == "%{foo: :bar}"
      assert macro_to_string(quote(do: %{:"Elixir.Foo" => :bar})) == "%{Foo => :bar}"
      assert macro_to_string(quote(do: %{{1, 2} => [1, 2, 3]})) == "%{{1, 2} => [1, 2, 3]}"
      assert macro_to_string(quote(do: %{map | "a" => "b"})) == "%{map | \"a\" => \"b\"}"
      assert macro_to_string(quote(do: [1, 2, 3])) == "[1, 2, 3]"
    end

    test "struct" do
      assert macro_to_string(quote(do: %Test{})) == "%Test{}"
      assert macro_to_string(quote(do: %Test{foo: 1, bar: 1})) == "%Test{foo: 1, bar: 1}"
      assert macro_to_string(quote(do: %Test{struct | foo: 2})) == "%Test{struct | foo: 2}"
      assert macro_to_string(quote(do: %Test{} + 1)) == "%Test{} + 1"
      assert macro_to_string(quote(do: %Test{foo(1)} + 2)) == "%Test{foo(1)} + 2"
    end

    test "binary operators" do
      assert macro_to_string(quote(do: 1 + 2)) == "1 + 2"
      assert macro_to_string(quote(do: [1, 2 | 3])) == "[1, 2 | 3]"
      assert macro_to_string(quote(do: [h | t] = [1, 2, 3])) == "[h | t] = [1, 2, 3]"
      assert macro_to_string(quote(do: (x ++ y) ++ z)) == "(x ++ y) ++ z"
      assert macro_to_string(quote(do: (x +++ y) +++ z)) == "(x +++ y) +++ z"
    end

    test "unary operators" do
      assert macro_to_string(quote(do: not 1)) == "not(1)"
      assert macro_to_string(quote(do: not foo)) == "not(foo)"
      assert macro_to_string(quote(do: -1)) == "-1"
      assert macro_to_string(quote(do: +(+1))) == "+(+1)"
      assert macro_to_string(quote(do: !(foo > bar))) == "!(foo > bar)"
      assert macro_to_string(quote(do: @foo(bar))) == "@foo(bar)"
      assert macro_to_string(quote(do: identity(&1))) == "identity(&1)"
    end

    test "access" do
      assert macro_to_string(quote(do: a[b])) == "a[b]"
      assert macro_to_string(quote(do: a[1 + 2])) == "a[1 + 2]"
      assert macro_to_string(quote(do: (a || [a: 1])[:a])) == "(a || [a: 1])[:a]"
      assert macro_to_string(quote(do: Map.put(%{}, :a, 1)[:a])) == "Map.put(%{}, :a, 1)[:a]"
    end

    test "keyword list" do
      assert macro_to_string(quote(do: [a: a, b: b])) == "[a: a, b: b]"
      assert macro_to_string(quote(do: [a: 1, b: 1 + 2])) == "[a: 1, b: 1 + 2]"
      assert macro_to_string(quote(do: ["a.b": 1, c: 1 + 2])) == "[\"a.b\": 1, c: 1 + 2]"
    end

    test "interpolation" do
      assert macro_to_string(quote(do: "foo#{bar}baz")) == ~S["foo#{bar}baz"]
    end

    test "bit syntax" do
      ast = quote(do: <<1::8*4>>)
      assert macro_to_string(ast) == "<<1::8*4>>"

      ast = quote(do: @type(foo :: <<_::8, _::_*4>>))
      assert macro_to_string(ast) == "@type(foo :: <<_::8, _::_*4>>)"

      ast = quote(do: <<69 - 4::bits-size(8 - 4)-unit(1), 65>>)
      assert macro_to_string(ast) == "<<69 - 4::bits-size(8 - 4)-unit(1), 65>>"

      ast = quote(do: <<(<<65>>), 65>>)
      assert macro_to_string(ast) == "<<(<<65>>), 65>>"

      ast = quote(do: <<65, (<<65>>)>>)
      assert macro_to_string(ast) == "<<65, (<<65>>)>>"

      ast = quote(do: for(<<(a::4 <- <<1, 2>>)>>, do: a))
      assert macro_to_string(ast) == "for(<<(a :: 4 <- <<1, 2>>)>>) do\n  a\nend"
    end

    test "charlist" do
      assert macro_to_string(quote(do: [])) == "[]"
      assert macro_to_string(quote(do: ~c"abc")) == ~S/~c"abc"/
      assert macro_to_string(quote(do: [?a, ?b, ?c])) == ~S/~c"abc"/
    end

    test "string" do
      assert macro_to_string(quote(do: "")) == ~S/""/
      assert macro_to_string(quote(do: "abc")) == ~S/"abc"/
      assert macro_to_string(quote(do: "#{"abc"}")) == ~S/"#{"abc"}"/
    end

    test "last arg keyword list" do
      assert macro_to_string(quote(do: foo([]))) == "foo([])"
      assert macro_to_string(quote(do: foo(x: y))) == "foo(x: y)"
      assert macro_to_string(quote(do: foo(x: 1 + 2))) == "foo(x: 1 + 2)"
      assert macro_to_string(quote(do: foo(x: y, p: q))) == "foo(x: y, p: q)"
      assert macro_to_string(quote(do: foo(a, x: y, p: q))) == "foo(a, x: y, p: q)"

      assert macro_to_string(quote(do: {[]})) == "{[]}"
      assert macro_to_string(quote(do: {[a: b]})) == "{[a: b]}"
      assert macro_to_string(quote(do: {x, a: b})) == "{x, [a: b]}"
      assert macro_to_string(quote(do: foo(else: a))) == "foo(else: a)"
      assert macro_to_string(quote(do: foo(catch: a))) == "foo(catch: a)"
    end

    test "with fun" do
      assert macro_to_string(quote(do: foo(1, 2, 3)), fn _, string -> ":#{string}:" end) ==
               ":foo(:1:, :2:, :3:):"

      assert macro_to_string(quote(do: Bar.foo(1, 2, 3)), fn _, string -> ":#{string}:" end) ==
               "::Bar:.foo(:1:, :2:, :3:):"
    end
  end

  test "validate/1" do
    ref = make_ref()

    assert Macro.validate(1) == :ok
    assert Macro.validate(1.0) == :ok
    assert Macro.validate(:foo) == :ok
    assert Macro.validate("bar") == :ok
    assert Macro.validate(<<0::8>>) == :ok
    assert Macro.validate(self()) == :ok
    assert Macro.validate({1, 2}) == :ok
    assert Macro.validate({:foo, [], :baz}) == :ok
    assert Macro.validate({:foo, [], []}) == :ok
    assert Macro.validate([1, 2, 3]) == :ok

    assert Macro.validate(<<0::4>>) == {:error, <<0::4>>}
    assert Macro.validate(ref) == {:error, ref}
    assert Macro.validate({1, ref}) == {:error, ref}
    assert Macro.validate({ref, 2}) == {:error, ref}
    assert Macro.validate([1, ref, 3]) == {:error, ref}
    assert Macro.validate({:foo, [], 0}) == {:error, {:foo, [], 0}}
    assert Macro.validate({:foo, 0, []}) == {:error, {:foo, 0, []}}
  end

  test "decompose_call/1" do
    assert Macro.decompose_call(quote(do: foo)) == {:foo, []}
    assert Macro.decompose_call(quote(do: foo())) == {:foo, []}
    assert Macro.decompose_call(quote(do: foo(1, 2, 3))) == {:foo, [1, 2, 3]}

    assert Macro.decompose_call(quote(do: M.N.foo(1, 2, 3))) ==
             {{:__aliases__, [alias: false], [:M, :N]}, :foo, [1, 2, 3]}

    assert Macro.decompose_call(quote(do: :foo.foo(1, 2, 3))) == {:foo, :foo, [1, 2, 3]}
    assert Macro.decompose_call(quote(do: 1.(1, 2, 3))) == :error
    assert Macro.decompose_call(quote(do: "some string")) == :error
    assert Macro.decompose_call(quote(do: {:foo, :bar, :baz})) == :error
    assert Macro.decompose_call(quote(do: {:foo, :bar, :baz, 42})) == :error
  end

  ## pipe/unpipe

  test "pipe/3" do
    assert Macro.pipe(1, quote(do: foo), 0) == quote(do: foo(1))
    assert Macro.pipe(1, quote(do: foo(2)), 0) == quote(do: foo(1, 2))
    assert Macro.pipe(1, quote(do: foo), -1) == quote(do: foo(1))
    assert Macro.pipe(2, quote(do: foo(1)), -1) == quote(do: foo(1, 2))

    assert Macro.pipe(quote(do: %{foo: "bar"}), quote(do: Access.get(:foo)), 0) ==
             quote(do: Access.get(%{foo: "bar"}, :foo))

    assert_raise ArgumentError, ~r"cannot pipe 1 into 2", fn ->
      Macro.pipe(1, 2, 0)
    end

    assert_raise ArgumentError, ~r"cannot pipe 1 into \{2, 3\}", fn ->
      Macro.pipe(1, {2, 3}, 0)
    end

    assert_raise ArgumentError, ~r"cannot pipe 1 into 1 \+ 1, the :\+ operator can", fn ->
      Macro.pipe(1, quote(do: 1 + 1), 0) == quote(do: foo(1))
    end

    assert_raise ArgumentError, ~r"cannot pipe 1 into <<1>>", fn ->
      Macro.pipe(1, quote(do: <<1>>), 0)
    end

    assert_raise ArgumentError, ~r"cannot pipe 1 into the special form unquote/1", fn ->
      Macro.pipe(1, quote(do: unquote()), 0)
    end

    assert_raise ArgumentError, ~r"piping into a unary operator is not supported", fn ->
      Macro.pipe(1, quote(do: +1), 0)
    end

    assert_raise ArgumentError, ~r"cannot pipe Macro into Env", fn ->
      Macro.pipe(Macro, quote(do: Env), 0)
    end

    assert_raise ArgumentError, ~r"cannot pipe 1 into 2 && 3", fn ->
      Macro.pipe(1, quote(do: 2 && 3), 0)
    end

    message = ~r"cannot pipe :foo into an anonymous function without calling"

    assert_raise ArgumentError, message, fn ->
      Macro.pipe(:foo, quote(do: fn x -> x end), 0)
    end

    message = ~r"wrong operator precedence when piping into bracket-based access"

    assert_raise ArgumentError, message, fn ->
      Macro.pipe(:foo, quote(do: %{foo: bar}[:foo]), 0)
    end
  end

  test "unpipe/1" do
    assert Macro.unpipe(quote(do: foo)) == quote(do: [{foo, 0}])
    assert Macro.unpipe(quote(do: foo |> bar)) == quote(do: [{foo, 0}, {bar, 0}])
    assert Macro.unpipe(quote(do: foo |> bar |> baz)) == quote(do: [{foo, 0}, {bar, 0}, {baz, 0}])
  end

  ## traverse/pre/postwalk

  test "traverse/4" do
    assert traverse({:foo, [], nil}) == [{:foo, [], nil}, {:foo, [], nil}]

    assert traverse({:foo, [], [1, 2, 3]}) ==
             [{:foo, [], [1, 2, 3]}, 1, 1, 2, 2, 3, 3, {:foo, [], [1, 2, 3]}]

    assert traverse({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
             [
               {{:., [], [:foo, :bar]}, [], [1, 2, 3]},
               {:., [], [:foo, :bar]},
               :foo,
               :foo,
               :bar,
               :bar,
               {:., [], [:foo, :bar]},
               1,
               1,
               2,
               2,
               3,
               3,
               {{:., [], [:foo, :bar]}, [], [1, 2, 3]}
             ]

    assert traverse({[1, 2, 3], [4, 5, 6]}) ==
             [
               {[1, 2, 3], [4, 5, 6]},
               [1, 2, 3],
               1,
               1,
               2,
               2,
               3,
               3,
               [1, 2, 3],
               [4, 5, 6],
               4,
               4,
               5,
               5,
               6,
               6,
               [4, 5, 6],
               {[1, 2, 3], [4, 5, 6]}
             ]
  end

  defp traverse(ast) do
    Macro.traverse(ast, [], &{&1, [&1 | &2]}, &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse()
  end

  test "prewalk/3" do
    assert prewalk({:foo, [], nil}) == [{:foo, [], nil}]

    assert prewalk({:foo, [], [1, 2, 3]}) == [{:foo, [], [1, 2, 3]}, 1, 2, 3]

    assert prewalk({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
             [
               {{:., [], [:foo, :bar]}, [], [1, 2, 3]},
               {:., [], [:foo, :bar]},
               :foo,
               :bar,
               1,
               2,
               3
             ]

    assert prewalk({[1, 2, 3], [4, 5, 6]}) ==
             [{[1, 2, 3], [4, 5, 6]}, [1, 2, 3], 1, 2, 3, [4, 5, 6], 4, 5, 6]
  end

  defp prewalk(ast) do
    Macro.prewalk(ast, [], &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse()
  end

  test "postwalk/3" do
    assert postwalk({:foo, [], nil}) == [{:foo, [], nil}]

    assert postwalk({:foo, [], [1, 2, 3]}) == [1, 2, 3, {:foo, [], [1, 2, 3]}]

    assert postwalk({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
             [
               :foo,
               :bar,
               {:., [], [:foo, :bar]},
               1,
               2,
               3,
               {{:., [], [:foo, :bar]}, [], [1, 2, 3]}
             ]

    assert postwalk({[1, 2, 3], [4, 5, 6]}) ==
             [1, 2, 3, [1, 2, 3], 4, 5, 6, [4, 5, 6], {[1, 2, 3], [4, 5, 6]}]
  end

  test "generate_arguments/2" do
    assert Macro.generate_arguments(0, __MODULE__) == []
    assert Macro.generate_arguments(1, __MODULE__) == [{:arg1, [], __MODULE__}]
    assert Macro.generate_arguments(4, __MODULE__) |> length == 4
  end

  defp postwalk(ast) do
    Macro.postwalk(ast, [], &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse()
  end

  test "struct_info!/2 expands structs multiple levels deep" do
    defmodule StructBang do
      defstruct [:a, :b]

      assert Macro.struct_info!(StructBang, __ENV__) == [
               %{field: :a, required: false, default: nil},
               %{field: :b, required: false, default: nil}
             ]

      def within_function do
        assert Macro.struct_info!(StructBang, __ENV__) == [
                 %{field: :a, required: false, default: nil},
                 %{field: :b, required: false, default: nil}
               ]
      end

      defmodule Nested do
        assert Macro.struct_info!(StructBang, __ENV__) == [
                 %{field: :a, required: false, default: nil},
                 %{field: :b, required: false, default: nil}
               ]
      end
    end

    assert Macro.struct_info!(StructBang, __ENV__) == [
             %{field: :a, required: false, default: nil},
             %{field: :b, required: false, default: nil}
           ]
  end

  test "prewalker/1" do
    ast = quote do: :mod.foo(bar({1, 2}), [3, 4, five])
    map = Enum.map(Macro.prewalker(ast), & &1)

    assert map == [
             {{:., [], [:mod, :foo]}, [], [{:bar, [], [{1, 2}]}, [3, 4, {:five, [], MacroTest}]]},
             {:., [], [:mod, :foo]},
             :mod,
             :foo,
             {:bar, [], [{1, 2}]},
             {1, 2},
             1,
             2,
             [3, 4, {:five, [], MacroTest}],
             3,
             4,
             {:five, [], MacroTest}
           ]

    assert map == ast |> Macro.prewalk([], &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse()
    assert Enum.zip(Macro.prewalker(ast), []) == Enum.zip(map, [])

    for i <- 0..(length(map) + 1) do
      assert Enum.take(Macro.prewalker(ast), i) == Enum.take(map, i)
    end
  end

  test "postwalker/1" do
    ast = quote do: :mod.foo(bar({1, 2}), [3, 4, five])
    map = Enum.map(Macro.postwalker(ast), & &1)

    assert map == [
             :mod,
             :foo,
             {:., [], [:mod, :foo]},
             1,
             2,
             {1, 2},
             {:bar, [], [{1, 2}]},
             3,
             4,
             {:five, [], MacroTest},
             [3, 4, {:five, [], MacroTest}],
             {{:., [], [:mod, :foo]}, [], [{:bar, [], [{1, 2}]}, [3, 4, {:five, [], MacroTest}]]}
           ]

    assert map == ast |> Macro.postwalk([], &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse()
    assert Enum.zip(Macro.postwalker(ast), []) == Enum.zip(map, [])

    for i <- 0..(length(map) + 1) do
      assert Enum.take(Macro.postwalker(ast), i) == Enum.take(map, i)
    end
  end

  test "operator?/2" do
    assert Macro.operator?(:+, 2)
    assert Macro.operator?(:+, 1)
    refute Macro.operator?(:+, 0)
  end

  test "quoted_literal?/1" do
    assert Macro.quoted_literal?(quote(do: "foo"))
    assert Macro.quoted_literal?(quote(do: {"foo", 1}))
    assert Macro.quoted_literal?(quote(do: %{foo: "bar"}))
    assert Macro.quoted_literal?(quote(do: %URI{path: "/"}))
    assert Macro.quoted_literal?(quote(do: <<>>))
    assert Macro.quoted_literal?(quote(do: <<1, "foo", "bar"::utf16>>))
    assert Macro.quoted_literal?(quote(do: <<1000::size(8)-unit(4)>>))
    assert Macro.quoted_literal?(quote(do: <<1000::8*4>>))
    assert Macro.quoted_literal?(quote(do: <<102::unsigned-big-integer-size(8)>>))
    refute Macro.quoted_literal?(quote(do: {"foo", var}))
    refute Macro.quoted_literal?(quote(do: <<"foo"::size(name_size)>>))
    refute Macro.quoted_literal?(quote(do: <<"foo"::binary-size(name_size)>>))
    refute Macro.quoted_literal?(quote(do: <<"foo"::custom_modifier()>>))
    refute Macro.quoted_literal?(quote(do: <<102, rest::binary>>))
  end

  test "underscore/1" do
    assert Macro.underscore("foo") == "foo"
    assert Macro.underscore("foo_bar") == "foo_bar"
    assert Macro.underscore("Foo") == "foo"
    assert Macro.underscore("FooBar") == "foo_bar"
    assert Macro.underscore("FOOBar") == "foo_bar"
    assert Macro.underscore("FooBAR") == "foo_bar"
    assert Macro.underscore("FOO_BAR") == "foo_bar"
    assert Macro.underscore("FoBaZa") == "fo_ba_za"
    assert Macro.underscore("Foo10") == "foo10"
    assert Macro.underscore("FOO10") == "foo10"
    assert Macro.underscore("10Foo") == "10_foo"
    assert Macro.underscore("FooBar10") == "foo_bar10"
    assert Macro.underscore("FooBAR10") == "foo_bar10"
    assert Macro.underscore("Foo10Bar") == "foo10_bar"
    assert Macro.underscore("Foo.Bar") == "foo/bar"
    assert Macro.underscore(Foo.Bar) == "foo/bar"
    assert Macro.underscore("API.V1.User") == "api/v1/user"
    assert Macro.underscore("") == ""
  end

  test "camelize/1" do
    assert Macro.camelize("Foo") == "Foo"
    assert Macro.camelize("FooBar") == "FooBar"
    assert Macro.camelize("foo") == "Foo"
    assert Macro.camelize("foo_bar") == "FooBar"
    assert Macro.camelize("foo_") == "Foo"
    assert Macro.camelize("_foo") == "Foo"
    assert Macro.camelize("foo10") == "Foo10"
    assert Macro.camelize("_10foo") == "10foo"
    assert Macro.camelize("foo_10") == "Foo10"
    assert Macro.camelize("foo__10") == "Foo10"
    assert Macro.camelize("foo__bar") == "FooBar"
    assert Macro.camelize("foo/bar") == "Foo.Bar"
    assert Macro.camelize("Foo.Bar") == "Foo.Bar"
    assert Macro.camelize("foo1_0") == "Foo10"
    assert Macro.camelize("foo_123_4_567") == "Foo1234567"
    assert Macro.camelize("FOO_BAR") == "FOO_BAR"
    assert Macro.camelize("FOO.BAR") == "FOO.BAR"
    assert Macro.camelize("") == ""
  end
end
