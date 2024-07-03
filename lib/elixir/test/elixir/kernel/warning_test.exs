Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.WarningTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  defp capture_err(fun) when is_function(fun), do: capture_io(:stderr, fun)

  defp assert_messages_match(messages, output) when is_list(messages) do
    for message <- messages do
      assert output =~ message
    end
  end

  defp assert_warn_eval(messages, source) do
    captured =
      capture_err(fn ->
        quoted = Code.string_to_quoted!(source, columns: true)
        Code.eval_quoted(quoted)
      end)

    assert_messages_match(messages, captured)
  end

  defp assert_warn_quoted(messages, source) do
    captured =
      capture_err(fn ->
        Code.string_to_quoted!(source, columns: true)
      end)

    assert_messages_match(messages, captured)
  end

  defp assert_warn_compile(messages, source) do
    captured =
      capture_err(fn ->
        quoted = Code.string_to_quoted!(source, columns: true)
        Code.compile_quoted(quoted)
      end)

    assert_messages_match(messages, captured)
  end

  defp capture_eval(source) do
    capture_err(fn ->
      quoted = Code.string_to_quoted!(source, columns: true)
      Code.eval_quoted(quoted)
    end)
  end

  defp capture_compile(source) do
    capture_err(fn ->
      quoted = Code.string_to_quoted!(source, columns: true)
      Code.compile_quoted(quoted)
    end)
  end

  defmacro will_warn do
    quote file: "demo", line: true do
      %{dup: 1, dup: 2}
    end
  end

  test "warnings from macro" do
    assert_warn_eval(
      ["demo:60\n", "key :dup will be overridden in map\n"],
      """
      import Kernel.WarningTest
      will_warn()
      """
    )
  end

  test "outdented heredoc" do
    assert_warn_eval(
      ["nofile:2:3", "outdented heredoc line"],
      """
        '''
      outdented
        '''
      """
    )
  end

  test "does not warn on incomplete tokenization" do
    assert {:error, _} = Code.string_to_quoted(~s[:"foobar" do])
  end

  describe "unicode identifier security" do
    test "warns on confusables" do
      assert_warn_quoted(
        ["nofile:1:6", "confusable identifier: 'a' looks like 'а' on line 1"],
        "а=1; a=1"
      )

      assert_warn_quoted(
        ["nofile:1:12", "confusable identifier: 'a' looks like 'а' on line 1"],
        "[{:а, 1}, {:a, 1}]"
      )

      assert_warn_quoted(
        ["nofile:1:8", "confusable identifier: 'a' looks like 'а' on line 1"],
        "[а: 1, a: 1]"
      )

      assert_warn_quoted(
        ["nofile:1:18", "confusable identifier: 'a' looks like 'а' on line 1"],
        "quote do: [а(1), a(1)]"
      )

      assert_warn_quoted(
        ["nofile:1:6", "confusable identifier: 'カ' looks like '力' on line 1"],
        "力=1; カ=1"
      )

      # by convention, doesn't warn on ascii-only confusables
      assert capture_eval("x0 = xO = 1") == ""
      assert capture_eval("l1 = ll = 1") == ""

      # works with a custom atom encoder
      assert capture_err(fn ->
               Code.string_to_quoted("[{:а, 1}, {:a, 1}]",
                 static_atoms_encoder: fn token, _ -> {:ok, {:wrapped, token}} end
               )
             end) =~
               "confusable identifier: 'a' looks like 'а' on line 1"
    end

    test "warns on LTR-confusables" do
      # warning outputs in byte order (vs bidi algo display order, uax9), mentions presence of rtl
      assert_warn_quoted(
        ["nofile:1:9", "'_1א' looks like '_א1'", "right-to-left characters"],
        "_א1 and _1א"
      )

      assert_warn_quoted(
        [
          "'a_1א' includes right-to-left characters",
          "\\u0061 a ltr",
          "\\u005F _ neutral",
          "\\u0031 1 weak_number",
          "\\u05D0 א rtl",
          "'a_א1' includes right-to-left characters:",
          "\\u0061 a ltr",
          "\\u005F _ neutral",
          "\\u05D0 א rtl",
          "\\u0031 1 weak_number"
        ],
        "a_א1 or a_1א"
      )
    end
  end

  test "operators formed by many of the same character followed by that character" do
    assert_warn_eval(
      [
        "nofile:1:12",
        "found \"+++\" followed by \"+\", please use a space between \"+++\" and the next \"+\""
      ],
      "quote do: 1++++1"
    )
  end

  test "identifier that ends in ! followed by the = operator without a space in between" do
    assert_warn_eval(
      ["nofile:1:1", "found identifier \"foo!\", ending with \"!\""],
      "foo!= 1"
    )

    assert_warn_eval(
      ["nofile:1:1", "found atom \":foo!\", ending with \"!\""],
      ":foo!= :foo!"
    )
  end

  describe "unnecessary quotes" do
    test "does not warn for unnecessary quotes in uppercase atoms/keywords" do
      assert capture_eval(~s/:"Foo"/) == ""
      assert capture_eval(~s/["Foo": :bar]/) == ""
      assert capture_eval(~s/:"Foo"/) == ""
      assert capture_eval(~s/:"foo@bar"/) == ""
      assert capture_eval(~s/:"héllò"/) == ""
      assert capture_eval(~s/:"3L1X1R"/) == ""
    end

    test "warns for unnecessary quotes" do
      assert_warn_eval(
        ["nofile:1:1", "found quoted atom \"foo\" but the quotes are not required"],
        ~s/:"foo"/
      )

      assert_warn_eval(
        ["nofile:1:3", "found quoted keyword \"foo\" but the quotes are not required"],
        ~s/["foo": :bar]/
      )

      assert_warn_eval(
        ["nofile:1:9", "found quoted call \"length\" but the quotes are not required"],
        ~s/[Kernel."length"([])]/
      )
    end
  end

  test "warns on :: as atom" do
    assert_warn_eval(
      [
        "nofile:1:1",
        "atom ::: must be written between quotes, as in :\"::\", to avoid ambiguity"
      ],
      ~s/:::/
    )
  end

  test "unused variable" do
    # Note we use compile_string because eval_string does not emit unused vars warning
    assert_warn_compile(
      [
        "nofile:2:3",
        "variable \"module\" is unused",
        "nofile:3:13",
        "variable \"arg\" is unused",
        "nofile:5:1",
        "variable \"file\" is unused"
      ],
      """
      defmodule Sample do
        module = 1
        def hello(arg), do: nil
      end
      file = 2
      file = 3
      file
      """
    )
  after
    purge(Sample)
  end

  test "unused variable that could be pinned" do
    # Note we use compile_string because eval_string does not emit unused vars warning
    assert_warn_compile(
      [
        "nofile:4:12",
        "variable \"compare_local\" is unused (there is a variable with the same name in the context, use the pin operator (^) to match on it or prefix this variable with underscore if it is not meant to be used)",
        "nofile:8:7",
        "variable \"compare_nested\" is unused (there is a variable with the same name in the context, use the pin operator (^) to match on it or prefix this variable with underscore if it is not meant to be used)"
      ],
      """
      defmodule Sample do
        def test do
          compare_local = "hello"
          match?(compare_local, "hello")

          compare_nested = "hello"
          case "hello" do
            compare_nested -> true
            _other -> false
          end
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused compiler variable" do
    output =
      capture_eval("""
      defmodule Sample do
        def hello(__MODULE___), do: :ok
        def world(_R), do: :ok
      end
      """)

    assert output =~ "unknown compiler variable \"__MODULE___\""
    assert output =~ "nofile:2:13"
    refute output =~ "unknown compiler variable \"_R\""
  after
    purge(Sample)
  end

  test "nested unused variable" do
    messages = ["undefined variable \"x\"", "variable \"x\" is unused"]

    assert_compile_error(
      ["nofile:5:1", "nofile:2:11" | messages],
      """
      case false do
        true -> x = 1
        _ -> 1
      end
      x
      """
    )

    assert_compile_error(
      ["nofile:1:12", "nofile:2:1" | messages],
      """
      false and (x = 1)
      x
      """
    )

    assert_compile_error(
      ["nofile:1:10", "nofile:2:1" | messages],
      """
      true or (x = 1)
      x
      """
    )

    assert_compile_error(
      ["nofile:2:3", "nofile:4:1" | messages],
      """
      if false do
        x = 1
      end
      x
      """
    )

    assert_compile_error(
      ["nofile:2:12", "nofile:5:1" | messages],
      """
      cond do
        false -> x = 1
        true -> 1
      end
      x
      """
    )

    assert_compile_error(
      ["nofile:2:11", "nofile:6:1" | messages],
      """
      receive do
        :foo -> x = 1
      after
        0 -> 1
      end
      x
      """
    )

    assert_compile_error(
      ["nofile:1:11", "nofile:2:1" | messages],
      """
      false && (x = 1)
      x
      """
    )

    assert_compile_error(
      ["nofile:1:10", "nofile:2:1" | messages],
      """
      true || (x = 1)
      x
      """
    )

    assert_compile_error(
      ["nofile:2:3", "nofile:4:1" | messages],
      """
      with true <- true do
        x = false
      end
      x
      """
    )

    assert_compile_error(
      ["nofile:2:3", "nofile:4:1" | messages],
      """
      fn ->
        x = true
      end
      x
      """
    )
  end

  test "unused variable in redefined function in different file" do
    output =
      capture_err(fn ->
        Code.eval_string("""
        defmodule Sample do
          defmacro __using__(_) do
            quote location: :keep do
              def function(arg)
            end
          end
        end
        """)

        code = """
          defmodule RedefineSample do
            use Sample
            def function(var123), do: nil
          end
        """

        Code.eval_string(code, [], file: "redefine_sample.ex")
      end)

    assert output =~ "redefine_sample.ex:3: "
    assert output =~ "variable \"var123\" is unused"
  after
    purge(Sample)
    purge(RedefineSample)
  end

  test "unused variable because re-declared in a match? pattern" do
    assert_warn_eval(
      [
        "nofile:1:16",
        "variable \"x\" is unused (there is a variable with the same name in the context,",
        "variable \"x\" is unused (if the variable is not meant to be used,"
      ],
      """
      fn x -> match?(x, :value) end
      """
    )
  end

  test "useless literal" do
    message = "code block contains unused literal \"oops\""

    assert_warn_eval(
      ["nofile:1\n", message],
      """
      "oops"
      :ok
      """
    )

    assert_warn_eval(
      ["nofile:1\n", message],
      """
      fn ->
        "oops"
        :ok
      end
      """
    )

    assert_warn_eval(
      ["nofile:1\n", message],
      """
      try do
        "oops"
        :ok
      after
        :ok
      end
      """
    )
  end

  test "useless attr" do
    assert_warn_eval(
      [
        "nofile:4:3",
        "module attribute @foo in code block has no effect as it is never returned ",
        "nofile:7:5",
        "module attribute @bar in code block has no effect as it is never returned "
      ],
      """
      defmodule Sample do
        @foo 1
        @bar 1
        @foo

        def bar do
          @bar
          :ok
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "useless var" do
    message = "variable foo in code block has no effect as it is never returned "

    assert_warn_eval(
      ["nofile:2:1", message],
      """
      foo = 1
      foo
      :ok
      """
    )

    assert_warn_eval(
      ["nofile:3:3", message],
      """
      fn ->
        foo = 1
        foo
        :ok
      end
      """
    )

    assert_warn_eval(
      ["nofile:3:3", message],
      """
      try do
        foo = 1
        foo
        :ok
      after
        :ok
      end
      """
    )

    assert capture_eval("""
           node()
           :ok
           """) == ""
  end

  test "underscored variable on match" do
    assert_warn_eval(
      ["nofile:1:8", "the underscored variable \"_arg\" appears more than once in a match"],
      """
      {_arg, _arg} = {1, 1}
      """
    )
  end

  test "underscored variable on use" do
    assert_warn_eval(
      ["nofile:1:12", "the underscored variable \"_var\" is used after being set"],
      """
      fn _var -> _var + 1 end
      """
    )

    assert capture_eval("""
           fn var!(_var, Foo) -> var!(_var, Foo) + 1 end
           """) == ""
  end

  test "unused function" do
    assert_warn_eval(
      ["nofile:2:8: ", "function hello/0 is unused\n"],
      """
      defmodule Sample1 do
        defp hello, do: nil
      end
      """
    )

    assert_warn_eval(
      ["nofile:2:8: ", "function hello/1 is unused\n"],
      """
      defmodule Sample2 do
        defp hello(0), do: hello(1)
        defp hello(1), do: :ok
      end
      """
    )

    assert_warn_eval(
      ["nofile:4:8: ", "function c/2 is unused\n"],
      ~S"""
      defmodule Sample3 do
        def a, do: nil
        def b, do: d(10)
        defp c(x, y \\ 1), do: [x, y]
        defp d(x), do: x
      end
      """
    )

    assert_warn_eval(
      ["nofile:3:8: ", "function b/2 is unused\n"],
      ~S"""
      defmodule Sample4 do
        def a, do: nil
        defp b(x \\ 1, y \\ 1)
        defp b(x, y), do: [x, y]
      end
      """
    )

    assert_warn_eval(
      ["nofile:3:8: ", "function b/0 is unused\n"],
      ~S"""
      defmodule Sample5 do
        def a, do: nil
        defp b(), do: unquote(1)
      end
      """
    )
  after
    purge([Sample1, Sample2, Sample3, Sample4, Sample5])
  end

  test "unused cyclic functions" do
    assert_warn_eval(
      [
        "nofile:2:8: ",
        "function a/0 is unused\n",
        "nofile:3:8: ",
        "function b/0 is unused\n"
      ],
      """
      defmodule Sample do
        defp a, do: b()
        defp b, do: a()
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused macro" do
    assert_warn_eval(
      ["nofile:2:13: ", "macro hello/0 is unused"],
      """
      defmodule Sample do
        defmacrop hello, do: nil
      end
      """
    )

    assert_warn_eval(
      ["nofile:2:13: ", "macro hello/0 is unused\n"],
      ~S"""
      defmodule Sample2 do
        defmacrop hello do
          quote do: unquote(1)
        end
      end
      """
    )
  after
    purge([Sample, Sample2])
  end

  test "shadowing" do
    assert capture_eval("""
           defmodule Sample do
             def test(x) do
               case x do
                 {:file, fid} -> fid
                 {:path, _}   -> fn(fid) -> fid end
               end
             end
           end
           """) == ""
  after
    purge(Sample)
  end

  test "unused default args" do
    assert_warn_eval(
      ["nofile:3:8: ", "default values for the optional arguments in b/3 are never used"],
      ~S"""
      defmodule Sample1 do
        def a, do: b(1, 2, 3)
        defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3), do: [arg1, arg2, arg3]
      end
      """
    )

    assert_warn_eval(
      [
        "nofile:3:8: ",
        "the default value for the last optional argument in b/3 is never used"
      ],
      ~S"""
      defmodule Sample2 do
        def a, do: b(1, 2)
        defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3), do: [arg1, arg2, arg3]
      end
      """
    )

    assert_warn_eval(
      [
        "nofile:3:8: ",
        "the default values for the last 2 optional arguments in b/4 are never used"
      ],
      ~S"""
      defmodule Sample3 do
        def a, do: b(1, 2)
        defp b(arg1, arg2 \\ 2, arg3 \\ 3, arg4 \\ 4), do: [arg1, arg2, arg3, arg4]
      end
      """
    )

    assert capture_eval(~S"""
           defmodule Sample4 do
             def a, do: b(1)
             defp b(arg1 \\ 1, arg2, arg3 \\ 3), do: [arg1, arg2, arg3]
           end
           """) == ""

    assert_warn_eval(
      ["nofile:3:8: ", "the default value for the last optional argument in b/3 is never used"],
      ~S"""
      defmodule Sample5 do
        def a, do: b(1, 2)
        defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3)

        defp b(arg1, arg2, arg3), do: [arg1, arg2, arg3]
      end
      """
    )
  after
    purge([Sample1, Sample2, Sample3, Sample4, Sample5])
  end

  test "unused import" do
    assert_warn_compile(
      ["nofile:2:3", "unused import :lists"],
      """
      defmodule Sample do
        import :lists
        def a, do: nil
      end
      """
    )

    assert_warn_compile(
      ["nofile:1:1", "unused import :lists"],
      """
      import :lists
      """
    )
  after
    purge(Sample)
  end

  test "unknown import" do
    assert_warn_compile(
      ["nofile:1:1", "cannot import Kernel.invalid/1 because it is undefined or private"],
      """
      import(Kernel, only: [invalid: 1])
      """
    )
  end

  test "unused import of one of the functions in :only" do
    assert_warn_compile(
      [
        "nofile:2:3",
        "unused import String.downcase/1",
        "nofile:2:3",
        "unused import String.trim/1"
      ],
      """
      defmodule Sample do
        import String, only: [upcase: 1, downcase: 1, trim: 1]
        def a, do: upcase("hello")
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused import of any of the functions in :only" do
    assert_warn_compile(
      ["nofile:1:1", "unused import String"],
      """
      import String, only: [upcase: 1, downcase: 1]
      """
    )
  end

  def with(a, b, c), do: [a, b, c]

  test "import matches special form" do
    assert_warn_compile(
      [
        "nofile:1:1",
        "cannot import Kernel.WarningTest.with/3 because it conflicts with Elixir special forms"
      ],
      """
      import Kernel.WarningTest, only: [with: 3]
      :ok = with true <- true, true <- true, do: :ok
      """
    )
  end

  test "duplicated function on import options" do
    assert_warn_compile(
      ["nofile:2:3", "invalid :only option for import, wrap/1 is duplicated"],
      """
      defmodule Kernel.WarningsTest.DuplicatedFunctionOnImportOnly do
        import List, only: [wrap: 1, keyfind: 3, wrap: 1]
      end
      """
    )

    assert_warn_compile(
      ["nofile:2:3", "invalid :except option for import, wrap/1 is duplicated"],
      """
      defmodule Kernel.WarningsTest.DuplicatedFunctionOnImportExcept do
        import List, except: [wrap: 1, keyfind: 3, wrap: 1]
      end
      """
    )
  end

  test "unused alias" do
    assert_warn_compile(
      ["nofile:2:3", "unused alias List"],
      """
      defmodule Sample do
        alias :lists, as: List
        def a, do: nil
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused alias when also import" do
    assert_warn_compile(
      ["nofile:2:3", "unused alias List"],
      """
      defmodule Sample do
        alias :lists, as: List
        import MapSet
        new()
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused inside dynamic module" do
    import List, only: [flatten: 1], warn: false

    assert capture_err(fn ->
             defmodule Sample do
               import String, only: [downcase: 1]

               def world do
                 flatten([1, 2, 3])
               end
             end
           end) =~ "unused import String"
  after
    purge(Sample)
  end

  test "duplicate map keys" do
    assert_warn_eval(
      [
        "key :a will be overridden in map",
        "nofile:4:10\n",
        "key :m will be overridden in map",
        "nofile:5:10\n",
        "key 1 will be overridden in map",
        "nofile:6:10\n"
      ],
      """
      defmodule DuplicateMapKeys do
        import ExUnit.Assertions

        assert %{a: :b, a: :c} == %{a: :c}
        assert %{m: :n, m: :o, m: :p} == %{m: :p}
        assert %{1 => 2, 1 => 3} == %{1 => 3}
      end
      """
    )

    assert map_size(%{System.unique_integer() => 1, System.unique_integer() => 2}) == 2
  end

  test "unused guard" do
    assert_warn_eval(
      ["nofile:5\n", "this check/guard will always yield the same result"],
      """
      defmodule Sample do
        def atom_case do
          v = "bc"
          case v do
            _ when is_atom(v) -> :ok
            _ -> :fail
          end
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "length(list) == 0 in guard" do
    assert_warn_eval(
      [
        "nofile:5:24",
        "do not use \"length(v) == 0\" to check if a list is empty",
        "Prefer to pattern match on an empty list or use \"v == []\" as a guard"
      ],
      """
      defmodule Sample do
        def list_case do
          v = []
          case v do
            _ when length(v) == 0 -> :ok
            _ -> :fail
          end
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "length(list) > 0 in guard" do
    assert_warn_eval(
      [
        "nofile:5:24",
        "do not use \"length(v) > 0\" to check if a list is not empty",
        "Prefer to pattern match on a non-empty list, such as [_ | _], or use \"v != []\" as a guard"
      ],
      """
      defmodule Sample do
        def list_case do
          v = []
          case v do
            _ when length(v) > 0 -> :ok
            _ -> :fail
          end
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "late function heads" do
    assert_warn_eval(
      [
        "nofile:4:7\n",
        "function head for def add/2 must come at the top of its direct implementation"
      ],
      """
      defmodule Sample do
        def add(a, b), do: a + b
        @doc "hello"
        def add(a, b)
      end
      """
    )
  after
    purge(Sample)
  end

  test "late function heads do not warn for meta programming" do
    assert capture_eval("""
           defmodule Sample1 do
             defmacro __using__(_) do
               quote do
                 def add(a, b), do: a + b
               end
             end
           end

           defmodule Sample2 do
             use Sample1
             @doc "hello"
             def add(a, b)
           end
           """) == ""

    assert capture_eval("""
           defmodule Sample3 do
            for fun <- [:foo, :bar] do
              def unquote(fun)(), do: unquote(fun)
            end

             def foo()
             def bar()
           end
           """) == ""
  after
    purge([Sample1, Sample2, Sample3])
  end

  test "used import via alias" do
    assert capture_eval("""
           defmodule Sample1 do
             import List, only: [flatten: 1]

             defmacro generate do
               List.duplicate(quote(do: flatten([1, 2, 3])), 100)
             end
           end

           defmodule Sample2 do
             import Sample1
             generate()
           end
           """) == ""
  after
    purge([Sample1, Sample2])
  end

  test "clause not match" do
    assert_warn_eval(
      [
        "nofile:3\n",
        ~r"this clause( for hello/0)? cannot match because a previous clause at line 2 always matches"
      ],
      """
      defmodule Sample do
        def hello, do: nil
        def hello, do: nil
      end
      """
    )
  after
    purge(Sample)
  end

  test "generated clause not match" do
    assert_warn_eval(
      [
        "nofile:10\n",
        ~r"this clause( for hello/0)? cannot match because a previous clause at line 10 always matches"
      ],
      """
      defmodule Sample do
        defmacro __using__(_) do
          quote do
            def hello, do: nil
            def hello, do: nil
          end
        end
      end
      defmodule UseSample do
        use Sample
      end
      """
    )
  after
    purge(Sample)
    purge(UseSample)
  end

  test "deprecated closing sigil delimiter" do
    assert_warn_eval(["nofile:1:7", "deprecated"], "~S(foo\\))")
  end

  test "deprecated not left in right" do
    assert_warn_eval(["nofile:1:7", "deprecated"], "not 1 in [1, 2, 3]")
  end

  test "clause with defaults should be first" do
    message = "def hello/1 has multiple clauses and also declares default values"

    assert_warn_eval(
      ["nofile:3\n", message],
      ~S"""
      defmodule Sample1 do
        def hello(arg), do: arg
        def hello(arg \\ 0), do: arg
      end
      """
    )

    assert_warn_eval(
      ["nofile:3:7\n", message],
      ~S"""
      defmodule Sample2 do
        def hello(_arg)
        def hello(arg \\ 0), do: arg
      end
      """
    )
  after
    purge([Sample1, Sample2])
  end

  test "clauses with default should use header" do
    assert_warn_eval(
      ["nofile:3\n", "def hello/1 has multiple clauses and also declares default values"],
      ~S"""
      defmodule Sample do
        def hello(arg \\ 0), do: arg
        def hello(arg), do: arg
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused with local with overridable" do
    assert_warn_eval(
      ["nofile:3:8: ", "function world/0 is unused"],
      """
      defmodule Sample do
        def hello, do: world()
        defp world, do: :ok
        defoverridable [hello: 0]
        def hello, do: :ok
      end
      """
    )
  after
    purge(Sample)
  end

  test "undefined module attribute" do
    assert_warn_eval(
      [
        "nofile:2: ",
        "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
      ],
      """
      defmodule Sample do
        @foo
      end
      """
    )
  after
    purge(Sample)
  end

  test "parens with module attribute" do
    assert_warn_eval(
      [
        "nofile:3: ",
        "the @foo() notation (with parentheses) is deprecated, please use @foo (without parentheses) instead"
      ],
      """
      defmodule Sample do
        @foo 13
        @foo()
      end
      """
    )
  after
    purge(Sample)
  end

  test "undefined module attribute in function" do
    assert_warn_eval(
      [
        "nofile:3: ",
        "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
      ],
      """
      defmodule Sample do
        def hello do
          @foo
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "undefined module attribute with file" do
    assert_warn_eval(
      [
        "nofile:2: ",
        "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
      ],
      """
      defmodule Sample do
        @foo
      end
      """
    )
  after
    purge(Sample)
  end

  test "parse transform" do
    assert_warn_eval(
      ["nofile:1: ", "@compile {:parse_transform, :ms_transform} is deprecated"],
      """
      defmodule Sample do
        @compile {:parse_transform, :ms_transform}
      end
      """
    )
  after
    purge(Sample)
  end

  test "@compile inline no warning for unreachable function" do
    refute capture_eval("""
           defmodule Sample do
             @compile {:inline, foo: 1}

             defp foo(_), do: :ok
           end
           """) =~ "inlined function foo/1 undefined"
  after
    purge(Sample)
  end

  test "in guard empty list" do
    assert_warn_eval(
      ["nofile:2\n", "this check/guard will always yield the same result"],
      """
      defmodule Sample do
        def a(x) when x in [], do: x
      end
      """
    )
  after
    purge(Sample)
  end

  test "no effect operator" do
    assert_warn_eval(
      ["nofile:3\n", "use of operator != has no effect"],
      """
      defmodule Sample do
        def a(x) do
          x != :foo
          :ok
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "eval failure warning" do
    assert_warn_eval(
      ["nofile:2\n", "the call to Atom.to_string/1 will fail with ArgumentError"],
      """
      defmodule Sample1 do
        def foo, do: Atom.to_string "abc"
      end
      """
    )

    assert_warn_eval(
      ["nofile:2\n", "the call to +/2 will fail with ArithmeticError"],
      """
      defmodule Sample2 do
        def foo, do: 1 + nil
      end
      """
    )
  after
    purge([Sample1, Sample2])
  end

  test "undefined function for behaviour" do
    assert_warn_eval(
      [
        "nofile:5: ",
        "function foo/0 required by behaviour Sample1 is not implemented (in module Sample2)"
      ],
      """
      defmodule Sample1 do
        @callback foo :: term
      end

      defmodule Sample2 do
        @behaviour Sample1
      end
      """
    )
  after
    purge([Sample1, Sample2])
  end

  test "undefined macro for behaviour" do
    assert_warn_eval(
      [
        "nofile:5: ",
        "macro foo/0 required by behaviour Sample1 is not implemented (in module Sample2)"
      ],
      """
      defmodule Sample1 do
        @macrocallback foo :: Macro.t
      end

      defmodule Sample2 do
        @behaviour Sample1
      end
      """
    )
  after
    purge([Sample1, Sample2])
  end

  test "wrong kind for behaviour" do
    assert_warn_eval(
      [
        "nofile:5: ",
        "function foo/0 required by behaviour Sample1 was implemented as \"defmacro\" but should have been \"def\""
      ],
      """
      defmodule Sample1 do
        @callback foo :: term
      end

      defmodule Sample2 do
        @behaviour Sample1
        defmacro foo, do: :ok
      end
      """
    )
  after
    purge([Sample1, Sample2])
  end

  test "conflicting behaviour" do
    assert_warn_eval(
      [
        "nofile:9: ",
        "conflicting behaviours found. Callback function foo/0 is defined by both Sample1 and Sample2 (in module Sample3)"
      ],
      """
      defmodule Sample1 do
        @callback foo :: term
      end

      defmodule Sample2 do
        @callback foo :: term
      end

      defmodule Sample3 do
        @behaviour Sample1
        @behaviour Sample2
      end
      """
    )
  after
    purge([Sample1, Sample2, Sample3])
  end

  test "conflicting behaviour (but one optional callback)" do
    message =
      capture_compile("""
      defmodule Sample1 do
        @callback foo :: term
      end

      defmodule Sample2 do
        @callback foo :: term
        @callback bar :: term
        @optional_callbacks foo: 0
      end

      defmodule Sample3 do
        @behaviour Sample1
        @behaviour Sample2

        @impl Sample1
        def foo, do: 1
        @impl Sample2
        def bar, do: 2
      end
      """)

    assert message =~
             "conflicting behaviours found. Callback function foo/0 is defined by both Sample1 and Sample2 (in module Sample3)"

    refute message =~ "module attribute @impl was not set"
    refute message =~ "this behaviour does not specify such callback"
  after
    purge([Sample1, Sample2, Sample3])
  end

  test "duplicate behaviour" do
    assert_warn_eval(
      [
        "nofile:5: ",
        "the behaviour Sample1 has been declared twice (conflict in function foo/0 in module Sample2)"
      ],
      """
      defmodule Sample1 do
        @callback foo :: term
      end

      defmodule Sample2 do
        @behaviour Sample1
        @behaviour Sample1
      end
      """
    )
  after
    purge([Sample1, Sample2])
  end

  test "unknown remote call" do
    assert capture_compile("""
           defmodule Sample do
             def perform(), do: Unknown.call()
           end
           """) =~
             "Unknown.call/0 is undefined (module Unknown is not available or is yet to be defined)"
  after
    purge(Sample)
  end

  test "undefined behaviour" do
    assert_warn_eval(
      ["nofile:1: ", "@behaviour UndefinedBehaviour does not exist (in module Sample)"],
      """
      defmodule Sample do
        @behaviour UndefinedBehaviour
      end
      """
    )
  after
    purge(Sample)
  end

  test "empty behaviours" do
    assert_warn_eval(
      ["nofile:3: ", "module EmptyBehaviour is not a behaviour (in module Sample)"],
      """
      defmodule EmptyBehaviour do
      end
      defmodule Sample do
        @behaviour EmptyBehaviour
      end
      """
    )
  after
    purge(Sample)
    purge(EmptyBehaviour)
  end

  test "undefined function for protocol" do
    assert_warn_eval(
      [
        "nofile:5: ",
        "function foo/1 required by protocol Sample1 is not implemented (in module Sample1.Atom)"
      ],
      """
      defprotocol Sample1 do
        def foo(subject)
      end

      defimpl Sample1, for: Atom do
      end
      """
    )
  after
    purge([Sample1, Sample1.Atom])
  end

  test "ungrouped def name" do
    assert_warn_eval(
      [
        "nofile:4:7\n",
        "clauses with the same name should be grouped together, \"def foo/2\" was previously defined (nofile:2)"
      ],
      """
      defmodule Sample do
        def foo(x, 1), do: x + 1
        def foo(), do: nil
        def foo(x, 2), do: x * 2
      end
      """
    )
  after
    purge(Sample)
  end

  test "ungrouped def name and arity" do
    assert_warn_eval(
      [
        "nofile:4:7\n",
        "clauses with the same name and arity (number of arguments) should be grouped together, \"def foo/2\" was previously defined (nofile:2)"
      ],
      """
      defmodule Sample do
        def foo(x, 1), do: x + 1
        def bar(), do: nil
        def foo(x, 2), do: x * 2
      end
      """
    )
  after
    purge(Sample)
  end

  test "ungrouped defs do not warn for meta programming" do
    assert capture_eval("""
           defmodule Sample do
             for atom <- [:foo, :bar] do
               def from_string(unquote(to_string(atom))), do: unquote(atom)
               def to_string(unquote(atom)), do: unquote(to_string(atom))
             end
           end
           """) == ""
  after
    purge(Sample)
  end

  test "warning with overridden file" do
    assert_warn_eval(
      ["sample:3:11:", "variable \"x\" is unused"],
      """
      defmodule Sample do
        @file "sample"
        def foo(x), do: :ok
      end
      """
    )
  after
    purge(Sample)
  end

  test "warning on unnecessary code point escape" do
    assert capture_eval("?\\n + ?\\\\") == ""

    assert_warn_eval(
      ["nofile:1:1", "unknown escape sequence ?\\w, use ?w instead"],
      "?\\w"
    )
  end

  test "warning on code point escape" do
    assert_warn_eval(
      ["nofile:1:1", "found ? followed by code point 0x20 (space), please use ?\\s instead"],
      "? "
    )

    assert_warn_eval(
      ["nofile:1:1", "found ?\\ followed by code point 0x20 (space), please use ?\\s instead"],
      "?\\ "
    )
  end

  test "duplicated docs in the same clause" do
    output =
      capture_eval("""
      defmodule Sample do
        @doc "Something"
        @doc "Another"
        def foo, do: :ok
      end
      """)

    assert output =~ "redefining @doc attribute previously set at line 2"
    assert output =~ "nofile:3: Sample (module)"
  after
    purge(Sample)
  end

  test "duplicate docs across clauses" do
    assert capture_eval("""
           defmodule Sample1 do
             defmacro __using__(_) do
               quote do
                 @doc "hello"
                 def add(a, 1), do: a + 1
               end
             end
           end

           defmodule Sample2 do
             use Sample1
             @doc "world"
             def add(a, 2), do: a + 2
           end
           """) == ""

    assert_warn_eval(
      ["nofile:4: ", "redefining @doc attribute previously set at line"],
      """
      defmodule Sample3 do
        @doc "hello"
        def add(a, 1), do: a + 1
        @doc "world"
        def add(a, 2), do: a + 2
      end
      """
    )
  after
    purge([Sample1, Sample2, Sample3])
  end

  test "reserved doc metadata keys" do
    {output, diagnostics} =
      Code.with_diagnostics([log: true], fn ->
        capture_eval("""
        defmodule Sample do
          @typedoc opaque: false
          @type t :: binary

          @doc defaults: 3, since: "1.2.3"
          def foo(a), do: a
        end
        """)
      end)

    assert output =~ "ignoring reserved documentation metadata key: :opaque"
    assert output =~ "nofile:2: "
    assert output =~ "ignoring reserved documentation metadata key: :defaults"
    assert output =~ "nofile:5: "
    refute output =~ ":since"

    assert [
             %{
               message: "ignoring reserved documentation metadata key: :opaque",
               position: 2,
               file: "nofile",
               severity: :warning
             },
             %{
               message: "ignoring reserved documentation metadata key: :defaults",
               position: 5,
               file: "nofile",
               severity: :warning
             }
           ] = diagnostics
  after
    purge(Sample)
  end

  describe "typespecs" do
    test "unused types" do
      output =
        capture_eval("""
        defmodule Sample do
          @type pub :: any
          @opaque op :: any
          @typep priv :: any
          @typep priv_args(var1, var2) :: {var1, var2}
          @typep priv2 :: any
          @typep priv3 :: priv2 | atom

          @spec my_fun(priv3) :: pub
          def my_fun(var), do: var
        end
        """)

      assert output =~ "nofile:4: "
      assert output =~ "type priv/0 is unused"
      assert output =~ "nofile:5: "
      assert output =~ "type priv_args/2 is unused"
      refute output =~ "type pub/0 is unused"
      refute output =~ "type op/0 is unused"
      refute output =~ "type priv2/0 is unused"
      refute output =~ "type priv3/0 is unused"
    after
      purge(Sample)
    end

    test "underspecified opaque types" do
      output =
        capture_eval("""
        defmodule Sample do
          @opaque op1 :: term
          @opaque op2 :: any
          @opaque op3 :: atom
        end
        """)

      assert output =~ "nofile:2: "
      assert output =~ "@opaque type op1/0 is underspecified and therefore meaningless"
      assert output =~ "nofile:3: "
      assert output =~ "@opaque type op2/0 is underspecified and therefore meaningless"
      refute output =~ "nofile:4: "
      refute output =~ "op3"
    after
      purge(Sample)
    end

    test "underscored types variables" do
      output =
        capture_eval("""
        defmodule Sample do
          @type in_typespec_vars(_var1, _var1) :: atom
          @type in_typespec(_var2) :: {atom, _var2}

          @spec in_spec(_var3) :: {atom, _var3} when _var3: var
          def in_spec(a), do: {:ok, a}
        end
        """)

      assert output =~ "nofile:2: "
      assert output =~ ~r/the underscored type variable "_var1" is used more than once/
      assert output =~ "nofile:3: "
      assert output =~ ~r/the underscored type variable "_var2" is used more than once/
      assert output =~ "nofile:5: "
      assert output =~ ~r/the underscored type variable "_var3" is used more than once/
    after
      purge(Sample)
    end

    test "typedoc on typep" do
      assert_warn_eval(
        [
          "nofile:2: ",
          "type priv/0 is private, @typedoc's are always discarded for private types"
        ],
        """
        defmodule Sample do
          @typedoc "Something"
          @typep priv :: any
          @spec foo() :: priv
          def foo(), do: nil
        end
        """
      )
    after
      purge(Sample)
    end

    test "discouraged types" do
      string_discouraged =
        "string() type use is discouraged. " <>
          "For character lists, use charlist() type, for strings, String.t()\n"

      nonempty_string_discouraged =
        "nonempty_string() type use is discouraged. " <>
          "For non-empty character lists, use nonempty_charlist() type, for strings, String.t()\n"

      assert_warn_eval(
        [
          "nofile:2: ",
          string_discouraged,
          "nofile:3: ",
          nonempty_string_discouraged
        ],
        """
        defmodule Sample do
          @type foo :: string()
          @type bar :: nonempty_string()
        end
        """
      )
    after
      purge(Sample)
    end

    test "nested type annotations" do
      message = "invalid type annotation. Type annotations cannot be nested"

      assert_warn_eval(
        ["nofile:2: ", message],
        """
        defmodule Sample1 do
          @type my_type :: ann_type :: nested_ann_type :: atom
        end
        """
      )

      purge(Sample1)

      assert_warn_eval(
        ["nofile:2: ", message],
        """
        defmodule Sample2 do
          @type my_type :: ann_type :: nested_ann_type :: atom | port
        end
        """
      )

      purge(Sample2)

      assert_warn_eval(
        ["nofile:2: ", message],
        """
        defmodule Sample3 do
          @spec foo :: {pid, ann_type :: nested_ann_type :: atom}
          def foo, do: nil
        end
        """
      )
    after
      purge([Sample1, Sample2, Sample3])
    end

    test "invalid type annotations" do
      assert_warn_eval(
        [
          "nofile:2: ",
          "invalid type annotation. The left side of :: must be a variable, got: pid()"
        ],
        """
        defmodule Sample1 do
          @type my_type :: (pid() :: atom)
        end
        """
      )

      message =
        "invalid type annotation. The left side of :: must be a variable, got: pid | ann_type. " <>
          "Note \"left | right :: ann\" is the same as \"(left | right) :: ann\""

      assert_warn_eval(
        ["nofile:2: ", message],
        """
        defmodule Sample2 do
          @type my_type :: pid | ann_type :: atom
        end
        """
      )
    after
      purge([Sample1, Sample2])
    end
  end

  test "attribute with no use" do
    assert_warn_eval(
      ["nofile:2: ", "module attribute @at was set but never used"],
      """
      defmodule Sample do
        @at "Something"
      end
      """
    )
  after
    purge(Sample)
  end

  test "registered attribute with no use" do
    assert_warn_eval(
      ["nofile:3: ", "module attribute @at was set but never used"],
      """
      defmodule Sample do
        Module.register_attribute(__MODULE__, :at, [])
        @at "Something"
      end
      """
    )
  after
    purge(Sample)
  end

  test "typedoc with no type" do
    assert_warn_eval(
      ["nofile:2: ", "module attribute @typedoc was set but no type follows it"],
      """
      defmodule Sample do
        @typedoc "Something"
      end
      """
    )
  after
    purge(Sample)
  end

  test "doc with no function" do
    assert_warn_eval(
      ["nofile:2: ", "module attribute @doc was set but no definition follows it"],
      """
      defmodule Sample do
        @doc "Something"
      end
      """
    )
  after
    purge(Sample)
  end

  test "pipe without explicit parentheses" do
    assert_warn_eval(
      ["nofile:2:1", "parentheses are required when piping into a function call"],
      """
      [5, 6, 7, 3]
      |> Enum.map_join "", &(Integer.to_string(&1))
      |> String.to_integer
      """
    )
  end

  test "keywords without explicit parentheses" do
    assert_warn_eval(
      ["nofile:2\n", "missing parentheses for expression following \"label:\" keyword. "],
      """
      quote do
        IO.inspect arg, label: if true, do: "foo", else: "baz"
      end
      """
    )
  end

  test "do+end with operator without explicit parentheses" do
    assert_warn_eval(
      ["nofile:3\n", "missing parentheses on expression following operator \"||\""],
      """
      quote do
        case do
        end || raise 1, 2
      end
      """
    )
  end

  test "variable is being expanded to function call (on_undefined_variable: warn)" do
    Code.put_compiler_option(:on_undefined_variable, :warn)

    output =
      capture_eval("""
      self
      defmodule Sample do
        def my_node(), do: node
      end
      """)

    assert output =~ "variable \"self\" does not exist and is being expanded to \"self()\""
    assert output =~ "nofile:1:1"
    assert output =~ "variable \"node\" does not exist and is being expanded to \"node()\""
    assert output =~ "nofile:3:22"
  after
    Code.put_compiler_option(:on_undefined_variable, :raise)
    purge(Sample)
  end

  defmodule User do
    defstruct [:name]
  end

  test ":__struct__ is ignored when using structs" do
    assert capture_err(fn ->
             code = """
             assert %Kernel.WarningTest.User{__struct__: Ignored, name: "joe"} ==
                    %Kernel.WarningTest.User{name: "joe"}
             """

             Code.eval_string(code, [], __ENV__)
           end) =~ "key :__struct__ is ignored when using structs"

    assert capture_err(fn ->
             code = """
             user = %Kernel.WarningTest.User{name: "meg"}
             assert %Kernel.WarningTest.User{user | __struct__: Ignored, name: "joe"} ==
                    %Kernel.WarningTest.User{__struct__: Kernel.WarningTest.User, name: "joe"}
             """

             Code.eval_string(code, [], __ENV__)
           end) =~ "key :__struct__ is ignored when using structs"
  end

  test "catch comes before rescue in try block" do
    assert_warn_eval(
      ["nofile:1:1\n", ~s("catch" should always come after "rescue" in try)],
      """
      try do
        :trying
      catch
        _ -> :caught
      rescue
        _ -> :error
      end
      """
    )
  end

  test "catch comes before rescue in def" do
    assert_warn_eval(
      ["nofile:2:7\n", ~s("catch" should always come after "rescue" in def)],
      """
      defmodule Sample do
        def foo do
          :trying
        catch
          _, _ -> :caught
        rescue
          _ -> :error
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused variable in defguard" do
    assert_warn_eval(
      ["nofile:2:21", "variable \"baz\" is unused"],
      """
      defmodule Sample do
        defguard foo(bar, baz) when bar
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused import in defguard" do
    assert_warn_eval(
      ["nofile:2:3", "unused import Record\n"],
      """
      defmodule Sample do
        import Record
        defguard is_record(baz) when baz
      end
      """
    )
  after
    purge(Sample)
  end

  test "unused private guard" do
    assert_warn_eval(
      ["nofile:2:13: ", "macro foo/2 is unused\n"],
      """
      defmodule Sample do
        defguardp foo(bar, baz) when bar + baz
      end
      """
    )
  after
    purge(Sample)
  end

  test "defguard overriding defmacro" do
    assert_warn_eval(
      [
        "nofile:3\n",
        ~r"this clause( for foo/1)? cannot match because a previous clause at line 2 always matches"
      ],
      """
      defmodule Sample do
        defmacro foo(bar), do: bar == :bar
        defguard foo(baz) when baz == :baz
      end
      """
    )
  after
    purge(Sample)
  end

  test "defmacro overriding defguard" do
    assert_warn_eval(
      [
        "nofile:3\n",
        ~r"this clause( for foo/1)? cannot match because a previous clause at line 2 always matches"
      ],
      """
      defmodule Sample do
        defguard foo(baz) when baz == :baz
        defmacro foo(bar), do: bar == :bar
      end
      """
    )
  after
    purge(Sample)
  end

  test "deprecated GenServer super on callbacks" do
    assert_warn_eval(
      ["nofile:1: ", "calling super for GenServer callback handle_call/3 is deprecated"],
      """
      defmodule Sample do
        use GenServer

        def handle_call(a, b, c) do
          super(a, b, c)
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "super is allowed on GenServer.child_spec/1" do
    refute capture_eval("""
           defmodule Sample do
             use GenServer

             def child_spec(opts) do
               super(opts)
             end
           end
           """) =~ "calling super for GenServer callback child_spec/1 is deprecated"
  after
    purge(Sample)
  end

  test "def warns if only clause is else" do
    assert_warn_compile(
      ["nofile:2:7\n", "\"else\" shouldn't be used as the only clause in \"def\""],
      """
      defmodule Sample do
        def foo do
          :bar
        else
          _other -> :ok
        end
      end
      """
    )
  after
    purge(Sample)
  end

  test "try warns if only clause is else" do
    assert_warn_compile(
      ["nofile:1:1\n", "\"else\" shouldn't be used as the only clause in \"try\""],
      """
      try do
        :ok
      else
        other -> other
      end
      """
    )
  end

  test "sigil w/W warns on trailing comma at macro expansion time" do
    for sigil <- ~w(w W),
        modifier <- ~w(a s c) do
      output =
        capture_err(fn ->
          {:ok, ast} =
            "~#{sigil}(foo, bar baz)#{modifier}"
            |> Code.string_to_quoted()

          Macro.expand(ast, __ENV__)
        end)

      assert output =~ "the sigils ~w/~W do not allow trailing commas"
    end
  end

  test "warnings on trailing comma on call" do
    assert_warn_eval(
      ["nofile:1:25\n", "trailing commas are not allowed inside function/macro call arguments"],
      "Keyword.merge([], foo: 1,)"
    )
  end

  test "defstruct warns with duplicate keys" do
    assert_warn_eval(
      ["nofile:2: TestMod", "duplicate key :foo found in struct"],
      """
      defmodule TestMod do
        defstruct [:foo, :bar, foo: 1]
      end
      """
    )
  after
    purge(TestMod)
  end

  test "deprecate nullary remote zero-arity capture with parens" do
    assert capture_eval("""
           import System, only: [pid: 0]
           &pid/0
           """) == ""

    assert_warn_eval(
      [
        "nofile:1:1\n",
        "extra parentheses on a remote function capture &System.pid()/0 have been deprecated. Please remove the parentheses: &System.pid/0"
      ],
      """
      &System.pid()/0
      """
    )
  end

  defp assert_compile_error(messages, string) do
    captured =
      capture_err(fn ->
        assert_raise CompileError, fn ->
          ast = Code.string_to_quoted!(string, columns: true)
          Code.eval_quoted(ast)
        end
      end)

    for message <- List.wrap(messages) do
      assert captured =~ message
    end
  end

  defp purge(list) when is_list(list) do
    Enum.each(list, &purge/1)
  end

  defp purge(module) when is_atom(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
