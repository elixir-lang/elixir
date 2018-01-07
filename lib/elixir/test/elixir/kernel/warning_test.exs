Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.WarningTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  defp capture_err(fun) do
    capture_io(:stderr, fun)
  end

  test "outdented heredoc" do
    output =
      capture_err(fn ->
        Code.eval_string("""
          '''
        outdented
          '''
        """)
      end)

    assert output =~ "outdented heredoc line"
    assert output =~ "nofile:2"
  end

  test "unused variable" do
    output =
      capture_err(fn ->
        Code.eval_string("""
        defmodule Sample do
          def hello(arg), do: nil

          if true do
            user = :warning
          else
            :nothing
          end
        end
        """)
      end)

    assert output =~ "variable \"arg\" is unused"
    assert output =~ "variable \"user\" is unused"
  after
    purge(Sample)
  end

  test "unsafe variable" do
    message = "variable \"x\" is unsafe"

    capture_err(fn ->
      Code.eval_string("""
      case false do
        true -> x = 1
        _ -> 1
      end
      x
      """)
    end) =~ message

    capture_err(fn ->
      Code.eval_string("""
      false and (x = 1)
      x
      """)
    end) =~ message

    capture_err(fn ->
      Code.eval_string("""
      true or (x = 1)
      x
      """)
    end) =~ message

    capture_err(fn ->
      Code.eval_string("""
      if false do
        x = 1
      end
      x
      """)
    end) =~ message

    capture_err(fn ->
      Code.eval_string("""
      cond do
        false -> x = 1
        true -> 1
      end
      x
      """)
    end) =~ message

    capture_err(fn ->
      Code.eval_string("""
      receive do
        :foo -> x = 1
      after
        0 -> 1
      end
      x
      """)
    end) =~ message

    capture_err(fn ->
      Code.eval_string("""
      false && (x = 1)
      x
      """)
    end) =~ message

    capture_err(fn ->
      Code.eval_string("""
      true || (x = 1)
      x
      """)
    end) =~ message
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

    assert output =~ "redefine_sample.ex:3"
    assert output =~ "variable \"var123\" is unused"
  after
    purge(Sample)
    purge(RedefineSample)
  end

  test "useless literal" do
    message = "code block contains unused literal \"oops\""

    assert capture_err(fn ->
             Code.eval_string("""
             "oops"
             :ok
             """)
           end) =~ message

    assert capture_err(fn ->
             Code.eval_string("""
             fn ->
               "oops"
               :ok
             end
             """)
           end) =~ message

    assert capture_err(fn ->
             Code.eval_string("""
             try do
               "oops"
               :ok
             after
               :ok
             end
             """)
           end) =~ message
  end

  test "useless attr" do
    message =
      capture_err(fn ->
        Code.eval_string("""
        defmodule Sample do
          @foo 1
          @bar 1
          @foo

          def bar do
            @bar
            :ok
          end
        end
        """)
      end)

    assert message =~ "module attribute @foo in code block has no effect as it is never returned "
    assert message =~ "module attribute @bar in code block has no effect as it is never returned "
  after
    purge(Sample)
  end

  test "useless var" do
    message = "variable foo in code block has no effect as it is never returned "

    assert capture_err(fn ->
             Code.eval_string("""
             foo = 1
             foo
             :ok
             """)
           end) =~ message

    assert capture_err(fn ->
             Code.eval_string("""
             fn ->
               foo = 1
               foo
               :ok
             end
             """)
           end) =~ message

    assert capture_err(fn ->
             Code.eval_string("""
             try do
               foo = 1
               foo
               :ok
             after
               :ok
             end
             """)
           end) =~ message

    assert capture_err(fn ->
             Code.eval_string("""
             node()
             :ok
             """)
           end) == ""
  end

  test "underscored variable on match" do
    assert capture_err(fn ->
             Code.eval_string("""
             {_arg, _arg} = {1, 1}
             """)
           end) =~ "the underscored variable \"_arg\" appears more than once in a match"
  end

  test "underscored variable on assign" do
    assert capture_err(fn ->
             Code.eval_string("""
              defmodule Sample do
               def fun(_var) do
                 _var + 1
               end
             end
             """)
           end) =~ "the underscored variable \"_var\" is used after being set"
  after
    purge(Sample)
  end

  test "unused function" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample1 do
               defp hello, do: nil
             end
             """)
           end) =~ "function hello/0 is unused"

    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample2 do
               defp hello(0), do: hello(1)
               defp hello(1), do: :ok
             end
             """)
           end) =~ "function hello/1 is unused"

    assert capture_err(fn ->
             Code.eval_string(~S"""
             defmodule Sample3 do
               def a, do: nil
               def b, do: d(10)
               defp c(x, y \\ 1), do: [x, y]
               defp d(x), do: x
             end
             """)
           end) =~ "function c/2 is unused"
  after
    purge([Sample1, Sample2, Sample3])
  end

  test "unused cyclic functions" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               defp a, do: b()
               defp b, do: a()
             end
             """)
           end) =~ "function a/0 is unused"
  after
    purge(Sample)
  end

  test "unused macro" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               defmacrop hello, do: nil
             end
             """)
           end) =~ "macro hello/0 is unused"
  after
    purge(Sample)
  end

  test "shadowing" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def test(x) do
                 case x do
                   {:file, fid} -> fid
                   {:path, _}   -> fn(fid) -> fid end
                 end
               end
             end
             """)
           end) == ""
  after
    purge(Sample)
  end

  test "unused default args" do
    assert capture_err(fn ->
             Code.eval_string(~S"""
             defmodule Sample1 do
               def a, do: b(1, 2, 3)
               defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3), do: [arg1, arg2, arg3]
             end
             """)
           end) =~ "default arguments in b/3 are never used"

    assert capture_err(fn ->
             Code.eval_string(~S"""
             defmodule Sample2 do
               def a, do: b(1, 2)
               defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3), do: [arg1, arg2, arg3]
             end
             """)
           end) =~ "the first 2 default arguments in b/3 are never used"

    assert capture_err(fn ->
             Code.eval_string(~S"""
             defmodule Sample3 do
               def a, do: b(1)
               defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3), do: [arg1, arg2, arg3]
             end
             """)
           end) =~ "the first default argument in b/3 is never used"

    assert capture_err(fn ->
             Code.eval_string(~S"""
             defmodule Sample4 do
               def a, do: b(1)
               defp b(arg1 \\ 1, arg2, arg3 \\ 3), do: [arg1, arg2, arg3]
             end
             """)
           end) == ""
  after
    purge([Sample1, Sample2, Sample3, Sample4])
  end

  test "unused import" do
    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Sample do
               import :lists
               def a, do: nil
             end
             """)
           end) =~ "unused import :lists\n"

    assert capture_err(fn ->
             Code.compile_string("""
             import :lists
             """)
           end) =~ "unused import :lists\n"
  after
    purge(Sample)
  end

  test "unused import of one of the functions in :only" do
    output =
      capture_err(fn ->
        Code.compile_string("""
        defmodule Sample do
          import String, only: [upcase: 1, downcase: 1, trim: 1]
          def a, do: upcase("hello")
        end
        """)
      end)

    assert output =~ "unused import String.downcase/1"
    assert output =~ "unused import String.trim/1"
  after
    purge(Sample)
  end

  test "unused import of any of the functions in :only" do
    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Sample do
               import String, only: [upcase: 1, downcase: 1]
               def a, do: nil
             end
             """)
           end) =~ "unused import String\n"
  after
    purge(Sample)
  end

  test "unused alias" do
    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Sample do
               alias :lists, as: List
               def a, do: nil
             end
             """)
           end) =~ "unused alias List"
  after
    purge(Sample)
  end

  test "unused alias when also import" do
    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Sample do
               alias :lists, as: List
               import MapSet
               new()
             end
             """)
           end) =~ "unused alias List"
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

  # TODO: Remove this check once we depend only on 20
  if :erlang.system_info(:otp_release) >= '20' do
    test "duplicate map keys" do
      output =
        capture_err(fn ->
          defmodule DuplicateMapKeys do
            assert %{a: :b, a: :c} == %{a: :c}
            assert %{1 => 2, 1 => 3} == %{1 => 3}
          end
        end)

      assert output =~ "key :a will be overridden in map"
      assert output =~ "key 1 will be overridden in map"
    end
  end

  test "unused guard" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def atom_case do
                 v = "bc"
                 case v do
                   _ when is_atom(v) -> :ok
                   _ -> :fail
                 end
               end
             end
             """)
           end) =~ "this check/guard will always yield the same result"
  after
    purge(Sample)
  end

  test "length(list) == 0 in guard" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def list_case do
                 v = []
                 case v do
                   _ when length(v) == 0 -> :ok
                   _ -> :fail
                 end
               end
             end
             """)
           end) =~
             "\"length(v) == 0\" is discouraged since it has to traverse the whole list to check if it is empty or not"
  after
    purge(Sample)
  end

  test "previous clause always matches" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def binary_cond do
                 v = "bc"
                 cond do
                   is_binary(v) -> :bin
                   true -> :ok
                 end
               end
             end
             """)
           end) =~ "this clause cannot match because a previous clause at line 5 always matches"
  after
    purge(Sample)
  end

  test "empty clause" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample1 do
               def hello
             end
             """)
           end) =~ "implementation not provided for predefined def hello/0"
  after
    purge(Sample1)
  end

  test "used import via alias" do
    assert capture_err(fn ->
             Code.eval_string("""
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
             """)
           end) == ""
  after
    purge([Sample1, Sample2])
  end

  test "clause not match" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def hello, do: nil
               def hello, do: nil
             end
             """)
           end) =~ "this clause cannot match because a previous clause at line 2 always matches"
  after
    purge(Sample)
  end

  test "generated clause not match" do
    assert capture_err(fn ->
             Code.eval_string("""
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
             """)
           end) =~ "this clause cannot match because a previous clause at line 10 always matches"
  after
    purge(Sample)
    purge(UseSample)
  end

  test "deprecated not left in right" do
    assert capture_err(fn ->
             Code.eval_string("not 1 in [1, 2, 3]")
           end) =~ "deprecated"
  end

  test "clause with defaults should be first" do
    message = "definitions with multiple clauses and default values require a header"

    assert capture_err(fn ->
             Code.eval_string(~S"""
             defmodule Sample1 do
               def hello(arg), do: arg
               def hello(arg \\ 0), do: arg
             end
             """)
           end) =~ message

    assert capture_err(fn ->
             Code.eval_string(~S"""
             defmodule Sample2 do
               def hello(_arg)
               def hello(arg \\ 0), do: arg
             end
             """)
           end) =~ message
  after
    purge([Sample1, Sample2])
  end

  test "clauses with default should use fun head" do
    message = "definitions with multiple clauses and default values require a header"

    assert capture_err(fn ->
             Code.eval_string(~S"""
             defmodule Sample1 do
               def hello(arg \\ 0), do: arg
               def hello(arg), do: arg
             end
             """)
           end) =~ message

    assert capture_err(fn ->
             Code.eval_string(~S"""
               defmodule Sample2 do
                 def hello(arg \\ 0), do: arg
                 def hello(_arg)
               end
             """)
           end) == ""
  after
    purge([Sample1, Sample2])
  end

  test "unused with local with overridable" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def hello, do: world()
               defp world, do: :ok
               defoverridable [hello: 0]
               def hello, do: :ok
             end
             """)
           end) =~ "function world/0 is unused"
  after
    purge(Sample)
  end

  test "undefined module attribute" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               @foo
             end
             """)
           end) =~
             "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
  after
    purge(Sample)
  end

  test "undefined module attribute in function" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def hello do
                 @foo
               end
             end
             """)
           end) =~
             "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
  after
    purge(Sample)
  end

  test "undefined module attribute with file" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               @foo
             end
             """)
           end) =~
             "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
  after
    purge(Sample)
  end

  test "in guard empty list" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def a(x) when x in [], do: x
             end
             """)
           end) =~ "this check/guard will always yield the same result"
  after
    purge(Sample)
  end

  test "no effect operator" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def a(x) do
                 x != :foo
                 :ok
               end
             end
             """)
           end) =~ "use of operator != has no effect"
  after
    purge(Sample)
  end

  test "badarg warning" do
    assert capture_err(fn ->
             assert_raise ArgumentError, fn ->
               Code.eval_string("""
               defmodule Sample do
                 Atom.to_string "abc"
               end
               """)
             end
           end) =~ "this expression will fail with ArgumentError"
  after
    purge([Sample])
  end

  test "undefined function for behaviour" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample1 do
               @callback foo :: term
             end

             defmodule Sample2 do
               @behaviour Sample1
             end
             """)
           end) =~
             "function foo/0 required by behaviour Sample1 is not implemented (in module Sample2)"
  after
    purge([Sample1, Sample2])
  end

  test "undefined macro for behaviour" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample1 do
               @macrocallback foo :: Macro.t
             end

             defmodule Sample2 do
               @behaviour Sample1
             end
             """)
           end) =~
             "macro foo/0 required by behaviour Sample1 is not implemented (in module Sample2)"
  after
    purge([Sample1, Sample2])
  end

  test "wrong kind for behaviour" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample1 do
               @callback foo :: term
             end

             defmodule Sample2 do
               @behaviour Sample1
               defmacro foo, do: :ok
             end
             """)
           end) =~
             "function foo/0 required by behaviour Sample1 was implemented as \"defmacro\" but should have been \"def\""
  after
    purge([Sample1, Sample2])
  end

  test "conflicting behaviour" do
    assert capture_err(fn ->
             Code.eval_string("""
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
             """)
           end) =~
             "conflicting behaviours found. function foo/0 is required by Sample1 and Sample2 (in module Sample3)"
  after
    purge([Sample1, Sample2, Sample3])
  end

  test "undefined behaviour" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               @behaviour UndefinedBehaviour
             end
             """)
           end) =~ "module UndefinedBehaviour does not exist (in module Sample)"
  after
    purge(Sample)
  end

  test "empty behaviours" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule EmptyBehaviour do
             end
             defmodule Sample do
               @behaviour EmptyBehaviour
             end
             """)
           end) =~ "module EmptyBehaviour is not a behaviour (in module Sample)"
  after
    purge(Sample)
    purge(EmptyBehaviour)
  end

  test "undefined behavior" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               @behavior Hello
             end
             """)
           end) =~ "@behavior attribute is not supported, please use @behaviour instead"
  after
    purge(Sample)
  end

  test "undefined function for protocol" do
    assert capture_err(fn ->
             Code.eval_string("""
             defprotocol Sample1 do
               def foo(subject)
             end

             defimpl Sample1, for: Atom do
             end
             """)
           end) =~
             "function foo/1 required by protocol Sample1 is not implemented (in module Sample1.Atom)"
  after
    purge([Sample1, Sample1.Atom])
  end

  test "overridden def" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               def foo(x, 1), do: x + 1
               def bar(), do: nil
               def foo(x, 2), do: x * 2
             end
             """)
           end) =~
             "clauses for the same def should be grouped together, def foo/2 was previously defined (nofile:2)"
  after
    purge(Sample)
  end

  test "warning with overridden file" do
    output =
      capture_err(fn ->
        Code.eval_string("""
        defmodule Sample do
          @file "sample"
          def foo(x), do: :ok
        end
        """)
      end)

    assert output =~ "variable \"x\" is unused"
    assert output =~ "sample:3"
  after
    purge(Sample)
  end

  test "with and do clauses emit errors, else clauses do not" do
    assert capture_err(fn ->
             Code.compile_string("""
             with {:first, int} when is_integer(int) <- {:second, Integer.gcd(2, 4)} do
               int
             end
             """)
           end) =~ "this clause cannot match"

    assert capture_err(fn ->
             Code.compile_string("""
             with {:first, int1} when is_integer(int1) <- {:first, Integer.gcd(2, 4)},
                  {:second, int2} when is_integer(int2) <- {:second, Integer.gcd(2, 4)} do
               {:ok, int1 + int2}
             else
               {:first, nil} -> {:error, "first number is not integer"}
               {:second, nil} -> {:error, "second number is not integer"}
             end
             """)
           end) == ""
  after
    purge(Sample1)
    purge(Sample2)
  end

  test "warning on codepoint escape" do
    assert capture_err(fn ->
             Code.eval_string("? ")
           end) =~ "found ? followed by codepoint 0x20 (space), please use \\s instead"
  end

  test "duplicated docs" do
    output =
      capture_err(fn ->
        Code.eval_string("""
        defmodule Sample do
          @doc "Something"
          @doc "Another"
          def foo, do: :ok

          @doc false
          @doc "Doc"
          def bar, do: :ok
        end
        """)
      end)

    assert output =~ "redefining @doc attribute previously set at line 2"
    assert output =~ "nofile:3: Sample (module)"
    refute output =~ "nofile:7"
  after
    purge(Sample)
  end

  test "typedoc on typep" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               @typedoc "Something"
               @typep priv :: any
               @spec foo() :: priv
               def foo(), do: nil
             end
             """)
           end) =~ "type priv/0 is private, @typedoc's are always discarded for private types"
  after
    purge(Sample)
  end

  test "attribute with no use" do
    content =
      capture_err(fn ->
        Code.eval_string("""
        defmodule Sample do
          @at "Something"
        end
        """)
      end)

    assert content =~ "module attribute @at was set but never used"
    assert content =~ "nofile:2"
  after
    purge(Sample)
  end

  test "typedoc with no type" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               @typedoc "Something"
             end
             """)
           end) =~ "module attribute @typedoc was set but no type follows it"
  after
    purge(Sample)
  end

  test "doc with no function" do
    assert capture_err(fn ->
             Code.eval_string("""
             defmodule Sample do
               @doc "Something"
             end
             """)
           end) =~ "module attribute @doc was set but no definition follows it"
  after
    purge(Sample)
  end

  test "pipe without explicit parentheses" do
    assert capture_err(fn ->
             Code.eval_string("""
             [5, 6, 7, 3]
             |> Enum.map_join "", &(Integer.to_string(&1))
             |> String.to_integer
             """)
           end) =~ "parentheses are required when piping into a function call"
  end

  test "variable is being expanded to function call" do
    output =
      capture_err(fn ->
        Code.eval_string("""
        self
        defmodule Sample do
          def my_node(), do: node
        end
        """)
      end)

    assert output =~ "variable \"self\" does not exist and is being expanded to \"self()\""
    assert output =~ "variable \"node\" does not exist and is being expanded to \"node()\""
  after
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
    output =
      capture_err(fn ->
        Code.eval_string("""
        try do
          :trying
        catch
          _ -> :caught
        rescue
          _ -> :error
        end
        """)
      end)

    assert output =~ ~s("catch" should always come after "rescue" in try)
  end

  test "unused variable in defguard" do
    capture_err(fn ->
      Code.eval_string("""
      defmodule Sample do
        defguard foo(bar, baz) when bar
      end
      """)
    end) =~ "variable \"baz\" is unused"
  after
    purge(Sample)
  end

  test "unused import in defguard" do
    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Sample do
               import Record
               defguard is_record(baz) when baz
             end
             """)
           end) =~ "unused import Record\n"
  after
    purge(Sample)
  end

  test "unused private guard" do
    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Sample do
               defguardp foo(bar, baz) when bar + baz
             end
             """)
           end) =~ "macro foo/2 is unused\n"
  after
    purge(Sample)
  end

  test "defguard overriding defmacro" do
    capture_err(fn ->
      Code.eval_string("""
      defmodule Sample do
        defmacro foo(bar), do: bar == :bar
        defguard foo(baz) when baz == :baz
      end
      """)
    end) =~ "this clause cannot match because a previous clause at line 2 always matches"
  after
    purge(Sample)
  end

  test "defmacro overriding defguard" do
    capture_err(fn ->
      Code.eval_string("""
      defmodule Sample do
        defguard foo(baz) when baz == :baz
        defmacro foo(bar), do: bar == :bar
      end
      """)
    end) =~ "this clause cannot match because a previous clause at line 2 always matches"
  after
    purge(Sample)
  end

  test "defguardp overriding defmacrop" do
    capture_err(fn ->
      Code.eval_string("""
      defmodule Sample do
        defmacrop foo(bar), do: bar == :bar
        defguardp foo(baz) when baz == :baz
      end
      """)
    end) =~ "this clause cannot match because a previous clause at line 2 always matches"
  after
    purge(Sample)
  end

  test "defmacrop overriding defguardp" do
    capture_err(fn ->
      Code.eval_string("""
      defmodule Sample do
      defguardp foo(baz) when baz == :baz
        defmacrop foo(bar), do: bar == :bar
      end
      """)
    end) =~ "this clause cannot match because a previous clause at line 2 always matches"
  after
    purge(Sample)
  end

  test "defguard needs an implementation" do
    capture_err(fn ->
      Code.eval_string("""
      defmodule Sample do
        defguard foo(bar)
      end
      """)
    end) =~ "implementation not provided for predefined defguard"
  after
    purge(Sample)
  end

  defp purge(list) when is_list(list) do
    Enum.each(list, &purge/1)
  end

  defp purge(module) when is_atom(module) do
    :code.delete(module)
    :code.purge(module)
  end
end
