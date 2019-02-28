Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Formatter.CallsTest do
  use ExUnit.Case, async: true

  import CodeFormatterHelpers

  @short_length [line_length: 10]
  @medium_length [line_length: 20]

  describe "next break fits" do
    test "does not apply to function calls" do
      bad = "foo(very_long_call(bar))"

      good = """
      foo(
        very_long_call(
          bar
        )
      )
      """

      assert_format bad, good, @short_length
    end

    test "does not apply to strings" do
      bad = "foo(\"very long string\")"

      good = """
      foo(
        "very long string"
      )
      """

      assert_format bad, good, @short_length
    end

    test "for functions" do
      assert_same """
      foo(fn x -> y end)
      """

      assert_same """
      foo(fn
        a1 -> :ok
        b2 -> :error
      end)
      """

      assert_same """
      foo(bar, fn
        a1 -> :ok
        b2 -> :error
      end)
      """

      assert_same """
                  foo(fn x ->
                    :really_long_atom
                  end)
                  """,
                  @medium_length

      assert_same """
                  foo(bar, fn
                    a1 ->
                      :ok

                    b2 ->
                      :really_long_error
                  end)
                  """,
                  @medium_length
    end

    test "for heredocs" do
      assert_same """
      foo('''
      bar
      ''')
      """

      assert_same to_string('''
                  foo("""
                  bar
                  """)
                  ''')

      assert_same """
      foo(~S'''
      bar
      ''')
      """

      assert_same """
                  foo(~S'''
                  very long line does trigger another break
                  ''')
                  """,
                  @short_length
    end

    test "for lists" do
      bad = "foo([1, 2, 3, 4])"

      good = """
      foo([
        1,
        2,
        3,
        4
      ])
      """

      assert_format bad, good, @short_length
    end

    test "for {} calls" do
      bad = """
      alias Foo.{
              Bar, Baz
            }
      """

      good = """
      alias Foo.{
        Bar,
        Baz
      }
      """

      assert_format bad, good, @medium_length
    end

    test "for binaries only on eol" do
      bad = "foo(<<1, 2, 3, 4>>)"

      good = """
      foo(
        <<1, 2,
          3, 4>>
      )
      """

      assert_format bad, good, @short_length

      bad = """
      foo(<<
        # foo
        1,
        2,
        3,
        4>>)
      """

      good = """
      foo(<<
        # foo
        1,
        2,
        3,
        4
      >>)
      """

      assert_format bad, good, @short_length
    end
  end

  describe "local calls" do
    test "without arguments" do
      assert_format "foo( )", "foo()"
    end

    test "without arguments doesn't split on line limit" do
      assert_same "very_long_function_name()", @short_length
    end

    test "removes outer parens except for unquote_splicing/1" do
      assert_format "(foo())", "foo()"
      assert_same "(unquote_splicing(123))"
    end

    test "with arguments" do
      assert_format "foo( :one ,:two,\n   :three)", "foo(:one, :two, :three)"
    end

    test "with arguments splits on line limit" do
      bad = """
      fun(x, y, z)
      """

      good = """
      fun(
        x,
        y,
        z
      )
      """

      assert_format bad, good, @short_length
    end

    test "with arguments on comma limit" do
      bad = """
      import(foo(abc, cde), :next)
      """

      good = """
      import(
        foo(abc, cde),
        :next
      )
      """

      assert_format bad, good, @medium_length
    end

    test "with keyword lists" do
      assert_same "foo(foo: 1, bar: 2)"
      assert_same "foo(:hello, foo: 1, bar: 2)"

      bad = """
      foo(:hello, foo: 1, bar: 2)
      """

      good = """
      foo(
        :hello,
        foo: 1,
        bar: 2
      )
      """

      assert_format bad, good, @short_length

      bad = """
      foo(:hello, foo: 1,
        bar: 2, baz: 3)
      """

      assert_format bad, """
      foo(:hello, foo: 1, bar: 2, baz: 3)
      """
    end

    test "with lists maybe rewritten as keyword lists" do
      assert_format "foo([foo: 1, bar: 2])", "foo(foo: 1, bar: 2)"
      assert_format "foo(:arg, [foo: 1, bar: 2])", "foo(:arg, foo: 1, bar: 2)"
      assert_same "foo(:arg, [:elem, foo: 1, bar: 2])"
    end

    test "without parens" do
      assert_same "import :foo, :bar"
      assert_same "bar = if foo, do: bar, else: baz"

      assert_same """
      for :one,
          :two,
          :three,
          fn ->
            :ok
          end
      """

      assert_same """
      for :one, fn ->
        :ok
      end
      """
    end

    test "without parens on line limit" do
      bad = "import :long_atom, :other_arg"

      good = """
      import :long_atom,
             :other_arg
      """

      assert_format bad, good, @short_length
    end

    test "without parens on comma limit" do
      bad = """
      import foo(abc, cde), :next
      """

      good = """
      import foo(
               abc,
               cde
             ),
             :next
      """

      assert_format bad, good, @medium_length
    end

    test "without parens and with keyword lists preserves multiline" do
      assert_same """
      defstruct foo: 1,
                bar: 2
      """

      assert_same """
      config :app,
        foo: 1
      """

      assert_same """
      config :app,
        foo: 1,
        bar: 2
      """

      assert_same """
      config :app, :key,
        foo: 1,
        bar: 2
      """

      assert_same """
      config :app,
             :key,
             foo: 1,
             bar: 2
      """

      bad = """
      config :app, foo: 1,
        bar: 2
      """

      assert_format bad, """
      config :app,
        foo: 1,
        bar: 2
      """
    end

    test "without parens and with keyword lists on comma limit" do
      bad = """
      import foo(abc, cde), opts: :next
      """

      good = """
      import foo(
               abc,
               cde
             ),
             opts: :next
      """

      assert_format bad, good, @medium_length
    end

    test "without parens and with keyword lists on line limit" do
      assert_same "import :atom, opts: [foo: :bar]"

      bad = "import :atom, opts: [foo: :bar]"

      good = """
      import :atom,
        opts: [foo: :bar]
      """

      assert_format bad, good, @medium_length

      bad = "import :atom, really_long_key: [foo: :bar]"

      good = """
      import :atom,
        really_long_key: [
          foo: :bar
        ]
      """

      assert_format bad, good, @medium_length

      assert_same """
                  import :foo,
                    one: two,
                    three: four,
                    five: [6, 7, 8, 9]
                  """,
                  @medium_length

      assert_same """
                  import :really_long_atom_but_no_breaks,
                    one: two,
                    three: four
                  """,
                  @medium_length

      bad = "with :really_long_atom1, :really_long_atom2, opts: [foo: :bar]"

      good = """
      with :really_long_atom1,
           :really_long_atom2,
           opts: [
             foo: :bar
           ]
      """

      assert_format bad, good, @medium_length
    end

    test "without parens from option" do
      assert_format "foo bar", "foo(bar)"
      assert_same "foo bar", locals_without_parens: [foo: 1]
      assert_same "foo(bar)", locals_without_parens: [foo: 1]
      assert_same "foo bar", locals_without_parens: [foo: :*]
      assert_same "foo(bar)", locals_without_parens: [foo: :*]
    end

    test "without parens on unique argument" do
      assert_same "foo(for 1, 2, 3)"
      assert_same "foo(bar, for(1, 2, 3))"
      assert_same "assert for 1, 2, 3"
      assert_same "assert foo, for(1, 2, 3)"

      assert_same """
      assert for 1, 2, 3 do
        :ok
      end
      """

      assert_same """
      assert foo, for(1, 2, 3) do
        :ok
      end
      """

      assert_same """
      assert for(1, 2, 3) do
        :ok
      end
      """

      assert_same """
      assert (for 1, 2, 3 do
                :ok
              end)
      """
    end

    test "call on call" do
      assert_same "unquote(call)()"
      assert_same "unquote(call)(one, two)"

      assert_same """
      unquote(call)(one, two) do
        :ok
      end
      """
    end

    test "call on call on line limit" do
      bad = "foo(bar)(one, two, three)"

      good = """
      foo(bar)(
        one,
        two,
        three
      )
      """

      assert_format bad, good, @short_length
    end

    test "with generators" do
      assert_same "foo(bar <- baz, is_bat(bar))"
      assert_same "for bar <- baz, is_bat(bar)"

      assert_same """
      foo(
        bar <- baz,
        is_bat(bar),
        bat <- bar
      )
      """

      assert_same """
      for bar <- baz,
          is_bat(bar),
          bat <- bar
      """

      assert_same """
      for bar <- baz,
          is_bat(bar),
          bat <- bar do
        :ok
      end
      """

      assert_same """
      for bar <- baz,
          is_bat(bar),
          bat <- bar,
          into: %{}
      """
    end

    test "preserves user choice on parens even when it fits" do
      assert_same """
      call(
        :hello,
        :foo,
        :bar
      )
      """

      assert_same """
      call(
        :hello,
        :foo,
        :bar
      ) do
        1 + 2
      end
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format "call(\nfoo, bar, baz)", "call(foo, bar, baz)"

      # Doesn't preserve because there are no args
      bad = """
      call() do
        1 + 2
      end
      """

      assert_format bad, """
      call do
        1 + 2
      end
      """

      # Doesn't preserve because we have a single argument with next break fits
      bad = """
      call(
        %{
          key: :value
        }
      )
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format bad, """
      call(%{
        key: :value
      })
      """
    end
  end

  describe "remote calls" do
    test "with no arguments" do
      assert_format "Foo . Bar . baz", "Foo.Bar.baz()"
      assert_format ":erlang.\nget_stacktrace", ":erlang.get_stacktrace()"
      assert_format "@foo.bar", "@foo.bar"
      assert_format "@foo.bar()", "@foo.bar()"
      assert_format "(@foo).bar()", "@foo.bar()"
      assert_format "__MODULE__.start_link", "__MODULE__.start_link()"
      assert_format "Foo.bar.baz.bong", "Foo.bar().baz.bong"
      assert_format "(1 + 2).foo", "(1 + 2).foo"
      assert_format "(1 + 2).foo()", "(1 + 2).foo()"
    end

    test "with arguments" do
      assert_format "Foo . Bar. baz(1, 2, 3)", "Foo.Bar.baz(1, 2, 3)"
      assert_format ":erlang.\nget(\n:some_key)", ":erlang.get(:some_key)"
      assert_format ":erlang.\nget(:some_key\n)", ":erlang.get(:some_key)"
      assert_same "@foo.bar(1, 2, 3)"
      assert_same "__MODULE__.start_link(1, 2, 3)"
      assert_same "foo.bar(1).baz(2, 3)"
    end

    test "inspects function names correctly" do
      assert_same ~S[MyModule."my function"(1, 2)]
      assert_same ~S[MyModule."Foo.Bar"(1, 2)]
      assert_same ~S[Kernel.+(1, 2)]
      assert_same ~S[:erlang.+(1, 2)]
      assert_same ~S[foo."bar baz"(1, 2)]
    end

    test "splits on arguments and dot on line limit" do
      bad = """
      MyModule.Foo.bar(:one, :two, :three)
      """

      good = """
      MyModule.Foo.bar(
        :one,
        :two,
        :three
      )
      """

      assert_format bad, good, @medium_length

      bad = """
      My_function.foo().bar(2, 3).baz(4, 5)
      """

      good = """
      My_function.foo().bar(
        2,
        3
      ).baz(4, 5)
      """

      assert_format bad, good, @medium_length
    end

    test "doesn't split on parens on empty arguments" do
      assert_same "Mod.func()", @short_length
    end

    test "with keyword lists" do
      assert_same "mod.foo(foo: 1, bar: 2)"

      assert_same "mod.foo(:hello, foo: 1, bar: 2)"

      assert_same """
                  mod.really_long_function_name(
                    :hello,
                    foo: 1,
                    bar: 2
                  )
                  """,
                  @short_length

      assert_same """
                  really_long_module_name.foo(
                    :hello,
                    foo: 1,
                    bar: 2
                  )
                  """,
                  @short_length
    end

    test "wraps left side in parens if it is an anonymous function" do
      assert_same "(fn -> :ok end).foo"
    end

    test "wraps left side in parens if it is a do-end block" do
      assert_same """
      (if true do
         :ok
       end).foo
      """
    end

    test "wraps left side in parens if it is a do-end block as an argument" do
      assert_same """
      import (if true do
                :ok
              end).foo
      """
    end

    test "call on call" do
      assert_same "foo.bar(call)()"
      assert_same "foo.bar(call)(one, two)"

      assert_same """
      foo.bar(call)(one, two) do
        :ok
      end
      """
    end

    test "call on call on line limit" do
      bad = "a.b(foo)(one, two, three)"

      good = """
      a.b(foo)(
        one,
        two,
        three
      )
      """

      assert_format bad, good, @short_length
    end

    test "on vars" do
      assert_same "foo.bar"
      assert_same "foo.bar()"
    end

    test "on vars before blocks" do
      assert_same """
      if var.field do
        raise "oops"
      end
      """
    end

    test "on vars before brackets" do
      assert_same """
      exception.opts[:foo]
      """
    end

    test "preserves user choice on parens even when it fits" do
      assert_same """
      Remote.call(
        :hello,
        :foo,
        :bar
      )
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format "Remote.call(\nfoo, bar, baz)", "Remote.call(foo, bar, baz)"

      assert_same """
      Remote.call(
        :hello,
        :foo,
        fn -> :bar end
      )
      """
    end
  end

  describe "anonymous function calls" do
    test "without arguments" do
      assert_format "foo . ()", "foo.()"
      assert_format "(foo.()).().()", "foo.().().()"
      assert_same "@foo.()"
      assert_same "(1 + 1).()"
      assert_same ":foo.()"
    end

    test "with arguments" do
      assert_format "foo . (1, 2  ,  3 )", "foo.(1, 2, 3)"
      assert_format "foo . (1, 2 ).(3,4)", "foo.(1, 2).(3, 4)"
      assert_same "@foo.(:one, :two)"
      assert_same "foo.(1 + 1).(hello)"
    end

    test "does not split on dot on line limit" do
      assert_same "my_function.()", @short_length
    end

    test "splits on arguments on line limit" do
      bad = """
      my_function.(1, 2, 3)
      """

      good = """
      my_function.(
        1,
        2,
        3
      )
      """

      assert_format bad, good, @short_length

      bad = """
      my_function.(1, 2).f(3, 4).(5, 6)
      """

      good = """
      my_function.(
        1,
        2
      ).f(3, 4).(
        5,
        6
      )
      """

      assert_format bad, good, @short_length
    end

    test "with keyword lists" do
      assert_same "foo.(foo: 1, bar: 2)"

      assert_same "foo.(:hello, foo: 1, bar: 2)"

      assert_same """
                  foo.(
                    :hello,
                    foo: 1,
                    bar: 2
                  )
                  """,
                  @short_length
    end

    test "wraps left side in parens if it is an anonymous function" do
      assert_same "(fn -> :ok end).()"
    end

    test "wraps left side in parens if it is a do-end block" do
      assert_same """
      (if true do
         :ok
       end).()
      """
    end

    test "wraps left side in parens if it is a do-end block as an argument" do
      assert_same """
      import (if true do
                :ok
              end).()
      """
    end

    test "preserves user choice on parens even when it fits" do
      assert_same """
      call.(
        :hello,
        :foo,
        :bar
      )
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format "call.(\nfoo, bar, baz)", "call.(foo, bar, baz)"
    end
  end

  describe "do-end blocks" do
    test "with non-block keywords" do
      assert_same "foo(do: nil)"
    end

    test "with multiple keywords" do
      assert_same """
      foo do
        :do
      rescue
        :rescue
      catch
        :catch
      else
        :else
      after
        :after
      end
      """
    end

    test "with multiple keywords and arrows" do
      assert_same """
      foo do
        a1 -> a2
        b1 -> b2
      rescue
        a1 -> a2
        b1 -> b2
      catch
        a1 -> a2
        b1 -> b2
      else
        a1 -> a2
        b1 -> b2
      after
        a1 -> a2
        b1 -> b2
      end
      """
    end

    test "with no extra arguments" do
      assert_same """
      foo do
        :ok
      end
      """
    end

    test "with no extra arguments and line breaks" do
      assert_same """
                  foo do
                    a1 ->
                      really_long_line

                    b1 ->
                      b2
                  rescue
                    c1
                  catch
                    d1 -> d1
                    e1 -> e1
                  else
                    f2
                  after
                    g1 ->
                      really_long_line

                    h1 ->
                      h2
                  end
                  """,
                  @medium_length
    end

    test "with extra arguments" do
      assert_same """
      foo bar, baz do
        :ok
      end
      """
    end

    test "with extra arguments and line breaks" do
      assert_same """
                  foo bar do
                    a1 ->
                      really_long_line

                    b1 ->
                      b2
                  rescue
                    c1
                  catch
                    d1 -> d1
                    e1 -> e1
                  else
                    f2
                  after
                    g1 ->
                      really_long_line

                    h1 ->
                      h2
                  end
                  """,
                  @medium_length

      assert_same """
                  foo really,
                      long,
                      list,
                      of,
                      arguments do
                    a1 ->
                      really_long_line

                    b1 ->
                      b2
                  rescue
                    c1
                  catch
                    d1 -> d1
                    e1 -> e1
                  else
                    f2
                  after
                    g1 ->
                      really_long_line

                    h1 ->
                      h2
                  end
                  """,
                  @medium_length
    end

    test "when empty" do
      assert_same """
      foo do
      end
      """

      assert_same """
      foo do
      rescue
      catch
      else
      after
      end
      """
    end

    test "inside call" do
      bad = "foo (bar do :ok end)"

      good = """
      foo(
        bar do
          :ok
        end
      )
      """

      assert_format bad, good

      bad = "import (bar do :ok end)"

      good = """
      import (bar do
                :ok
              end)
      """

      assert_format bad, good
    end

    test "inside operator" do
      bad = "foo + bar do :ok end"

      good = """
      foo +
        bar do
          :ok
        end
      """

      assert_format bad, good
    end

    test "inside operator inside argument" do
      bad = "fun foo + (bar do :ok end)"

      good = """
      fun(
        foo +
          bar do
            :ok
          end
      )
      """

      assert_format bad, good

      bad = "if foo + (bar do :ok end) do :ok end"

      good = """
      if foo +
           (bar do
              :ok
            end) do
        :ok
      end
      """

      assert_format bad, good
    end

    test "inside operator inside argument with remote call" do
      bad = "if foo + (Bar.baz do :ok end) do :ok end"

      good = """
      if foo +
           (Bar.baz do
              :ok
            end) do
        :ok
      end
      """

      assert_format bad, good
    end

    test "keeps repeated keys" do
      assert_same """
      receive do
        :ok
      after
        0 -> 1
      after
        2 -> 3
      end
      """
    end

    test "preserves user choice even when it fits" do
      assert_same """
      case do
        1 ->
          :ok

        2 ->
          :ok
      end
      """

      assert_same """
      case do
        1 ->
          :ok

        2 ->
          :ok

        3 ->
          :ok
      end
      """
    end
  end

  describe "tuple calls" do
    test "without arguments" do
      assert_format "foo . {}", "foo.{}"
    end

    test "with arguments" do
      assert_format "foo.{bar,baz,bat,}", "foo.{bar, baz, bat}"
    end

    test "with arguments on line limit" do
      bad = "foo.{bar,baz,bat,}"

      good = """
      foo.{
        bar,
        baz,
        bat
      }
      """

      assert_format bad, good, @short_length

      bad = "really_long_expression.{bar,baz,bat,}"

      good = """
      really_long_expression.{
        bar,
        baz,
        bat
      }
      """

      assert_format bad, good, @short_length
    end

    test "with keywords" do
      assert_same "expr.{:hello, foo: bar, baz: bat}"
    end

    test "preserves user choice on parens even when it fits" do
      assert_same """
      call.{
        :hello,
        :foo,
        :bar
      }
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format "call.{\nfoo, bar, baz}", "call.{foo, bar, baz}"
    end
  end

  describe "access" do
    test "with one argument" do
      assert_format "foo[ bar ]", "foo[bar]"
    end

    test "with arguments on line limit" do
      bad = "foo[really_long_argument()]"

      good = """
      foo[
        really_long_argument()
      ]
      """

      assert_format bad, good, @short_length

      bad = "really_long_expression[really_long_argument()]"

      good = """
      really_long_expression[
        really_long_argument()
      ]
      """

      assert_format bad, good, @short_length
    end

    test "with do-end blocks" do
      assert_same """
      (if true do
         false
       end)[key]
      """
    end

    test "with keywords" do
      assert_format "expr[foo: bar, baz: bat]", "expr[[foo: bar, baz: bat]]"
    end
  end
end
