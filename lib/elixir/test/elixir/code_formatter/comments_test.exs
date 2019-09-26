Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Formatter.CommentsTest do
  use ExUnit.Case, async: true

  import CodeFormatterHelpers

  @short_length [line_length: 10]

  describe "at the root" do
    test "for empty documents" do
      assert_same "# hello world"
    end

    test "are reformatted" do
      assert_format "#oops", "# oops"
      assert_format "##oops", "## oops"
      assert_same "# ## oops"
    end

    test "recognizes hashbangs" do
      assert_format "#!/usr/bin/env elixir", "#! /usr/bin/env elixir"
      assert_same "#!"
    end

    test "before and after expressions" do
      assert_same """
      # before comment
      :hello
      """

      assert_same """
      :hello
      # after comment
      """

      assert_same """
      # before comment
      :hello
      # after comment
      """
    end

    test "on expressions" do
      bad = """
      :hello # this is hello
      :world # this is world
      """

      good = """
      # this is hello
      :hello
      # this is world
      :world
      """

      assert_format bad, good

      bad = """
      foo   # this is foo
      ++ bar # this is bar
      ++ baz # this is baz
      """

      good = """
      # this is foo
      # this is bar
      # this is baz
      foo ++
        bar ++
        baz
      """

      assert_format bad, good, @short_length
    end

    test "empty comment" do
      assert_same """
      #
      :foo
      """
    end

    test "before and after expressions with newlines" do
      assert_same """
      # before comment
      # second line

      :hello

      # middle comment 1

      #

      # middle comment 2

      :world

      # after comment
      # second line
      """
    end
  end

  describe "interpolation" do
    test "with comment outside before, during and after" do
      assert_same ~S"""
      # comment
      IO.puts("Hello #{world}")
      """

      assert_same ~S"""
      IO.puts("Hello #{world}")
      # comment
      """
    end

    test "with trailing comments" do
      # This is trailing so we move the comment out
      trailing = ~S"""
      IO.puts("Hello #{world}") # comment
      """

      assert_format trailing, ~S"""
      # comment
      IO.puts("Hello #{world}")
      """

      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      IO.puts("Hello #{world # comment
      }")
      """

      assert_format ambiguous, ~S"""
      # comment
      IO.puts("Hello #{world}")
      """
    end

    test "with comment inside before and after" do
      assert_same ~S"""
      IO.puts(
        "Hello #{
          # comment
          world
        }"
      )
      """

      assert_same ~S"""
      IO.puts(
        "Hello #{
          world
          # comment
        }"
      )
      """
    end
  end

  describe "parens blocks" do
    test "with comment outside before and after" do
      assert_same ~S"""
      # comment
      assert (
               hello
               world
             )
      """

      assert_same ~S"""
      assert (
               hello
               world
             )

      # comment
      """
    end

    test "with trailing comments" do
      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      assert ( # comment
               hello
               world
             )
      """

      assert_format ambiguous, ~S"""
      # comment
      assert (
               hello
               world
             )
      """

      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      assert (
               hello
               world
             ) # comment
      """

      assert_format ambiguous, ~S"""
      assert (
               hello
               world
             )

      # comment
      """
    end

    test "with comment inside before and after" do
      assert_same ~S"""
      assert (
               # comment
               hello
               world
             )
      """

      assert_same ~S"""
      assert (
               hello
               world
               # comment
             )
      """
    end
  end

  describe "calls" do
    test "local with parens inside before and after" do
      assert_same ~S"""
      call(
        # before
        hello,
        # middle
        world
        # after
      )
      """

      assert_same ~S"""
      call(
        # command
      )
      """
    end

    test "remote with parens inside before and after" do
      assert_same ~S"""
      Remote.call(
        # before
        hello,
        # middle
        world
        # after
      )
      """

      assert_same ~S"""
      Remote.call(
        # command
      )
      """
    end

    test "local with parens and keywords inside before and after" do
      assert_same ~S"""
      call(
        # before
        hello,
        # middle
        world,
        # key before
        key: hello,
        # key middle
        key: world
        # key after
      )
      """
    end

    test "remote with parens and keywords inside before and after" do
      assert_same ~S"""
      call(
        # before
        hello,
        # middle
        world,
        # key before
        key: hello,
        # key middle
        key: world
        # key after
      )
      """
    end

    test "local with no parens inside before and after" do
      bad = ~S"""
             # before
      assert hello,
             # middle
             world
             # after
      """

      assert_format bad, ~S"""
      # before
      assert hello,
             # middle
             world

      # after
      """
    end

    test "local with no parens and keywords inside before and after" do
      bad = ~S"""
      config hello, world,
        # key before
        key: hello,
        # key middle
        key: world
        # key after
      """

      assert_format bad, ~S"""
      config hello, world,
        # key before
        key: hello,
        # key middle
        key: world

      # key after
      """

      bad = ~S"""
             # before
      config hello,
             # middle
             world,
             # key before
             key: hello,
             # key middle
             key: world
             # key after
      """

      assert_format bad, ~S"""
      # before
      config hello,
             # middle
             world,
             # key before
             key: hello,
             # key middle
             key: world

      # key after
      """
    end
  end

  describe "anonymous functions" do
    test "with one clause and no args" do
      assert_same ~S"""
      fn ->
        # comment
        hello
        world
      end
      """

      assert_same ~S"""
      fn ->
        hello
        world
        # comment
      end
      """
    end

    test "with one clause and no args and trailing comments" do
      bad = ~S"""
      fn # comment
        ->
        hello
        world
      end
      """

      assert_format bad, ~S"""
      # comment
      fn ->
        hello
        world
      end
      """

      bad = ~S"""
      fn
        # comment
        ->
        hello
        world
      end
      """

      assert_format bad, ~S"""
      # comment
      fn ->
        hello
        world
      end
      """
    end

    test "with one clause and args" do
      assert_same ~S"""
      fn hello ->
        # before
        hello
        # middle
        world
        # after
      end
      """
    end

    test "with one clause and args and trailing comments" do
      bad = ~S"""
      fn # fn
        # before head
        hello # middle head
        # after head
        ->
          # before body
          world # middle body
          # after body
      end
      """

      assert_format bad, ~S"""
      # fn
      fn
        # before head
        # middle head
        hello ->
          # after head
          # before body
          # middle body
          world
          # after body
      end
      """
    end

    test "with multiple clauses and args" do
      bad = ~S"""
      fn # fn
        # before one
        one, # middle one
        # after one / before two
        two # middle two
        # after two
        ->
          # before hello
          hello # middle hello
          # after hello

        # before three
        three # middle three
        # after three
        ->
          # before world
          world # middle world
          # after world
      end
      """

      assert_format bad, ~S"""
      # fn
      fn
        # before one
        # middle one
        # after one / before two
        # middle two
        one, two ->
          # after two
          # before hello
          # middle hello
          hello

        # after hello

        # before three
        # middle three
        three ->
          # after three
          # before world
          # middle world
          world
          # after world
      end
      """
    end

    test "with commented out clause" do
      assert_same """
      fn
        arg1 ->
          body1

        # arg2 ->
        #   body 2

        arg3 ->
          body3
      end
      """
    end
  end

  describe "do-end blocks" do
    test "with comment outside before and after" do
      assert_same ~S"""
      # comment
      assert do
        hello
        world
      end
      """

      assert_same ~S"""
      assert do
        hello
        world
      end

      # comment
      """
    end

    test "with trailing comments" do
      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      assert do # comment
        hello
        world
      end
      """

      assert_format ambiguous, ~S"""
      # comment
      assert do
        hello
        world
      end
      """

      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      assert do
        hello
        world
      end # comment
      """

      assert_format ambiguous, ~S"""
      assert do
        hello
        world
      end

      # comment
      """
    end

    test "with comment inside before and after" do
      assert_same ~S"""
      assert do
        # comment
        hello
        world
      end
      """

      assert_same ~S"""
      assert do
        hello
        world
        # comment
      end
      """
    end

    test "with comment inside before and after and multiple keywords" do
      assert_same ~S"""
      assert do
        # before
        hello
        world
        # after
      rescue
        # before
        hello
        world
        # after
      after
        # before
        hello
        world
        # after
      catch
        # before
        hello
        world
        # after
      else
        # before
        hello
        world
        # after
      end
      """
    end

    test "when empty" do
      assert_same ~S"""
      assert do
        # comment
      end
      """

      assert_same ~S"""
      assert do
        # comment
      rescue
        # comment
      after
        # comment
      catch
        # comment
      else
        # comment
      end
      """
    end

    test "with one-line clauses" do
      bad = ~S"""
      assert do # do
        # before
        one -> two
      end
      """

      assert_format bad, ~S"""
      # do
      assert do
        # before
        one -> two
      end
      """

      bad = ~S"""
      assert do # do
        # before
        one -> two
        # after
        three -> four
      end
      """

      assert_format bad, ~S"""
      # do
      assert do
        # before
        one -> two
        # after
        three -> four
      end
      """
    end

    test "with multiple clauses and args" do
      bad = ~S"""
      assert do # do
        # before one
        one, # middle one
        # after one / before two
        two # middle two
        # after two
        ->
          # before hello
          hello # middle hello
          # after hello

        # before three
        three # middle three
        # after three
        ->
          # before world
          world # middle world
          # after world
      end
      """

      assert_format bad, ~S"""
      # do
      assert do
        # before one
        # middle one
        # after one / before two
        # middle two
        one, two ->
          # after two
          # before hello
          # middle hello
          hello

        # after hello

        # before three
        # middle three
        three ->
          # after three
          # before world
          # middle world
          world
          # after world
      end
      """
    end
  end

  describe "operators" do
    test "with comment before, during and after uniform pipelines" do
      assert_same """
      foo
      # |> bar
      # |> baz
      |> bat
      """

      bad = """
      # before
      foo    # this is foo
      |> bar # this is bar
      |> baz # this is baz
      # after
      """

      good = """
      # before
      # this is foo
      foo
      # this is bar
      |> bar
      # this is baz
      |> baz

      # after
      """

      assert_format bad, good, @short_length
    end

    test "with comment before, during and after mixed pipelines" do
      assert_same """
      foo
      # |> bar
      # |> baz
      ~> bat
      """

      bad = """
      # before
      foo    # this is foo
      ~> bar # this is bar
      <|> baz # this is baz
      # after
      """

      good = """
      # before
      # this is foo
      foo
      # this is bar
      ~> bar
      # this is baz
      <|> baz

      # after
      """

      assert_format bad, good, @short_length
    end

    test "with comment before, during and after uniform right" do
      assert_same """
      foo
      # | bar
      # | baz
      | bat
      """

      bad = """
      # before
      foo    # this is foo
      | bar # this is bar
      | baz # this is baz
      # after
      """

      good = """
      # before
      # this is foo
      foo
      # this is bar
      | bar
      # this is baz
      | baz

      # after
      """

      assert_format bad, good, @short_length
    end

    test "with comment before, during and after mixed right" do
      assert_same """
      one
      # when two
      # when three
      when four
           # | five
           | six
      """
    end

    test "handles nodes without meta info" do
      assert_same "(a -> b) |> (c -> d)"
      assert_same "(a -> b) when c: d"
      assert_same "(a -> b) when (c -> d)"
    end
  end

  describe "containers" do
    test "with comment outside before, during and after" do
      assert_same ~S"""
      # comment
      [one, two, three]
      """

      assert_same ~S"""
      [one, two, three]
      # comment
      """
    end

    test "with trailing comments" do
      # This is trailing so we move the comment out
      trailing = ~S"""
      [one, two, three] # comment
      """

      assert_format trailing, ~S"""
      # comment
      [one, two, three]
      """

      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      [# comment
       one, two, three]
      """

      assert_format ambiguous, ~S"""
      # comment
      [
        one,
        two,
        three
      ]
      """
    end

    test "when empty" do
      assert_same ~S"""
      [
        # comment
      ]
      """
    end

    test "with block" do
      assert_same ~S"""
      [
        (
          # before
          multi
          line
          # after
        )
      ]
      """
    end

    test "with comments inside lists before and after" do
      bad = ~S"""
      [
        # 1. one

        # 1. two
        # 1. three
        one,
        after_one,
        after_one do
          :ok
        end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        three # final

        # 4. one

        # 4. two
        # 4. three
        # four
      ]
      """

      good = ~S"""
      [
        # 1. one

        # 1. two
        # 1. three
        one,
        after_one,
        after_one do
          :ok
        end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        # final
        three

        # 4. one

        # 4. two
        # 4. three
        # four
      ]
      """

      assert_format bad, good
    end

    test "with comments inside tuples before and after" do
      bad = ~S"""
      {
        # 1. one

        # 1. two
        # 1. three
        one,
        after_one,
        after_one do
          :ok
        end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        three # final

        # 4. one

        # 4. two
        # 4. three
        # four
      }
      """

      good = ~S"""
      {
        # 1. one

        # 1. two
        # 1. three
        one,
        after_one,
        after_one do
          :ok
        end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        # final
        three

        # 4. one

        # 4. two
        # 4. three
        # four
      }
      """

      assert_format bad, good
    end

    test "with comments inside bitstrings before and after" do
      bad = ~S"""
      <<
        # 1. one

        # 1. two
        # 1. three
        one,
        after_one,
        after_one do
          :ok
        end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        three # final

        # 4. one

        # 4. two
        # 4. three
        # four
      >>
      """

      good = ~S"""
      <<
        # 1. one

        # 1. two
        # 1. three
        one,
        after_one,
        after_one do
          :ok
        end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        # final
        three

        # 4. one

        # 4. two
        # 4. three
        # four
      >>
      """

      assert_format bad, good
    end

    test "with comments inside maps before and after" do
      bad = ~S"""
      %{
        # 1. one

        # 1. two
        # 1. three
        one: one,
        one: after_one,
        one: after_one do
          :ok
        end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        three: three # final

        # 4. one

        # 4. two
        # 4. three
        # four
      }
      """

      good = ~S"""
      %{
        # 1. one

        # 1. two
        # 1. three
        one: one,
        one: after_one,
        one:
          after_one do
            :ok
          end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        # final
        three: three

        # 4. one

        # 4. two
        # 4. three
        # four
      }
      """

      assert_format bad, good
    end

    test "with comments inside structs before and after" do
      bad = ~S"""
      %Foo{bar |
        # 1. one

        # 1. two
        # 1. three
        one: one,
        one: after_one,
        one: after_one do
          :ok
        end,

        # 2. one

        # 2. two
        # 2. three
        # two,

        # 3. one

        # 3. two
        # 3. three
        three: three # final

        # 4. one

        # 4. two
        # 4. three
        # four
      }
      """

      good = ~S"""
      %Foo{
        bar
        | # 1. one

          # 1. two
          # 1. three
          one: one,
          one: after_one,
          one:
            after_one do
              :ok
            end,

          # 2. one

          # 2. two
          # 2. three
          # two,

          # 3. one

          # 3. two
          # 3. three
          # final
          three: three

          # 4. one

          # 4. two
          # 4. three
          # four
      }
      """

      assert_format bad, good
    end
  end

  describe "defstruct" do
    test "with first field comments" do
      bad = ~S"""
      defmodule Foo do
        # defstruct
        defstruct [ # foo
          # 1. one
          one: 1, # 2. one
          # 1. two
          # 2. two
          two: 2
        ]
      end
      """

      good = ~S"""
      defmodule Foo do
        # defstruct
        # foo
        defstruct [
          # 1. one
          # 2. one
          one: 1,
          # 1. two
          # 2. two
          two: 2
        ]
      end
      """

      assert_format bad, good
    end

    test "with first field comments and defstruct has the parens" do
      bad = ~S"""
      defmodule Foo do
        # defstruct
        defstruct([ # foo
          # 1. one
          one: 1, # 2. one
          # 1. two
          # 2. two
          two: 2
        ])
      end
      """

      good = ~S"""
      defmodule Foo do
        # defstruct
        # foo
        defstruct(
          # 1. one
          # 2. one
          one: 1,
          # 1. two
          # 2. two
          two: 2
        )
      end
      """

      assert_format bad, good
    end

    test "without first field comments" do
      bad = ~S"""
      defmodule Foo do
        # defstruct
        defstruct [
          one: 1,
          # 1. two
          two: 2 # 2. two
        ]
      end
      """

      good = ~S"""
      defmodule Foo do
        # defstruct
        defstruct one: 1,
                  # 1. two
                  # 2. two
                  two: 2
      end
      """

      assert_format bad, good
    end

    test "without field comments" do
      bad = ~S"""
      defmodule Foo do
        # defstruct
        defstruct [
          one: 1,
          two: 2
        ]
      end
      """

      good = ~S"""
      defmodule Foo do
        # defstruct
        defstruct one: 1,
                  two: 2
      end
      """

      assert_format bad, good
    end

    test "without square brackets" do
      assert_same ~S"""
      defmodule Foo do
        # defstruct
        defstruct one: 1,
                  # 1. two
                  # 2. two
                  two: 2
      end
      """
    end
  end
end
