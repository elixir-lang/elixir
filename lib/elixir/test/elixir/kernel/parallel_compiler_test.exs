Code.require_file("../test_helper.exs", __DIR__)

import PathHelpers

defmodule Kernel.ParallelCompilerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  defp purge(modules) do
    Enum.map(modules, fn mod ->
      :code.purge(mod)
      :code.delete(mod)
    end)
  end

  defp write_tmp(context, kv) do
    dir = tmp_path(context)
    File.rm_rf!(dir)
    File.mkdir_p!(dir)

    for {key, contents} <- kv do
      path = Path.join(dir, "#{key}.ex")
      File.write!(path, contents)
      path
    end
  end

  describe "compile" do
    test "solves dependencies between modules" do
      fixtures =
        write_tmp(
          "parallel_compiler",
          bar: """
          defmodule BarParallel do
          end

          require FooParallel
          IO.puts(FooParallel.message())
          """,
          foo: """
          defmodule FooParallel do
            # We use this ensure_compiled? clause so both Foo and
            # Bar block. Foo depends on Unknown and Bar depends on
            # Foo. The compiler will see this dependency and first
            # release Foo and then Bar, compiling with success.
            false = Code.ensure_compiled?(Unknown)
            def message, do: "message_from_foo"
          end
          """
        )

      assert capture_io(fn ->
               assert {:ok, [BarParallel, FooParallel], []} =
                        Kernel.ParallelCompiler.compile(fixtures)
             end) =~ "message_from_foo"
    after
      purge([FooParallel, BarParallel])
    end

    test "solves dependencies between structs" do
      fixtures =
        write_tmp(
          "parallel_struct",
          bar: """
          defmodule BarStruct do
            defstruct name: "", foo: %FooStruct{}
          end
          """,
          foo: """
          defmodule FooStruct do
            defstruct name: ""
            def bar?(%BarStruct{}), do: true
          end
          """
        )

      assert {:ok, modules, []} = Kernel.ParallelCompiler.compile(fixtures)
      assert [BarStruct, FooStruct] = Enum.sort(modules)
    after
      purge([FooStruct, BarStruct])
    end

    test "solves dependencies between structs in typespecs" do
      fixtures =
        write_tmp(
          "parallel_typespec_struct",
          bar: """
          defmodule BarStruct do
            defstruct name: ""
            @type t :: %FooStruct{}
          end
          """,
          foo: """
          defmodule FooStruct do
            defstruct name: ""
            @type t :: %BarStruct{}
          end
          """
        )

      assert {:ok, modules, []} = Kernel.ParallelCompiler.compile(fixtures)
      assert [BarStruct, FooStruct] = Enum.sort(modules)
    after
      purge([FooStruct, BarStruct])
    end

    test "returns struct undefined error when local struct is undefined" do
      [fixture] =
        write_tmp(
          "compile_struct",
          undef: """
          defmodule Undef do
            def undef() do
              %__MODULE__{}
            end
          end
          """
        )

      expected_msg = "Undef.__struct__/1 is undefined, cannot expand struct Undef"

      assert capture_io(fn ->
               assert {:error, [{^fixture, 3, msg}], []} =
                        Kernel.ParallelCompiler.compile([fixture])

               assert msg =~ expected_msg
             end) =~ expected_msg
    end

    test "does not hang on missing dependencies" do
      [fixture] =
        write_tmp(
          "compile_does_not_hang",
          with_behaviour_and_struct: """
          # We need to ensure it won't block even after multiple calls.
          # So we use both behaviour and struct expansion below.
          defmodule WithBehaviourAndStruct do
            # @behaviour will call ensure_compiled().
            @behaviour :unknown
            # Struct expansion calls it as well.
            %ThisModuleWillNeverBeAvailable{}
          end
          """
        )

      expected_msg =
        "ThisModuleWillNeverBeAvailable.__struct__/1 is undefined, cannot expand struct ThisModuleWillNeverBeAvailable"

      assert capture_io(fn ->
               assert {:error, [{^fixture, 7, msg}], []} =
                        Kernel.ParallelCompiler.compile([fixture])

               assert msg =~ expected_msg
             end) =~ "== Compilation error"
    end

    test "does not deadlock on missing dependencies" do
      [missing_struct, depends_on] =
        write_tmp(
          "does_not_deadlock",
          missing_struct: """
          defmodule MissingStruct do
            %ThisModuleWillNeverBeAvailable{}
            def hello, do: :ok
          end
          """,
          depends_on_missing_struct: """
          MissingStruct.hello()
          """
        )

      expected_msg =
        "ThisModuleWillNeverBeAvailable.__struct__/1 is undefined, cannot expand struct ThisModuleWillNeverBeAvailable"

      assert capture_io(fn ->
               assert {:error, [{^missing_struct, 2, msg}], []} =
                        Kernel.ParallelCompiler.compile([missing_struct, depends_on])

               assert msg =~ expected_msg
             end) =~ "== Compilation error"
    end

    test "handles deadlocks" do
      [foo, bar] =
        write_tmp(
          "parallel_deadlock",
          foo: """
          defmodule FooDeadlock do
            BarDeadlock.__info__(:module)
          end
          """,
          bar: """
          defmodule BarDeadlock do
            FooDeadlock.__info__(:module)
          end
          """
        )

      msg =
        capture_io(fn ->
          fixtures = [foo, bar]
          assert {:error, [bar_error, foo_error], []} = Kernel.ParallelCompiler.compile(fixtures)
          assert bar_error == {bar, nil, "deadlocked waiting on module FooDeadlock"}
          assert foo_error == {foo, nil, "deadlocked waiting on module BarDeadlock"}
        end)

      assert msg =~ "Compilation failed because of a deadlock between files."
      assert msg =~ "parallel_deadlock/foo.ex => BarDeadlock"
      assert msg =~ "parallel_deadlock/bar.ex => FooDeadlock"
      assert msg =~ ~r"== Compilation error in file .+parallel_deadlock/foo\.ex =="
      assert msg =~ "** (CompileError)  deadlocked waiting on module BarDeadlock"
      assert msg =~ ~r"== Compilation error in file .+parallel_deadlock/bar\.ex =="
      assert msg =~ "** (CompileError)  deadlocked waiting on module FooDeadlock"
    end

    test "handles deadlocks from Code.ensure_compiled" do
      [foo, bar] =
        write_tmp(
          "parallel_ensure_deadlock",
          foo: """
          defmodule FooDeadlock do
            Code.ensure_compiled(BarDeadlock)
          end
          """,
          bar: """
          defmodule BarDeadlock do
            Code.ensure_compiled(FooDeadlock)
          end
          """
        )

      msg =
        capture_io(fn ->
          fixtures = [foo, bar]
          assert {:error, [bar_error, foo_error], []} = Kernel.ParallelCompiler.compile(fixtures)
          assert bar_error == {bar, nil, "deadlocked waiting on module FooDeadlock"}
          assert foo_error == {foo, nil, "deadlocked waiting on module BarDeadlock"}
        end)

      assert msg =~ "Compilation failed because of a deadlock between files."
      assert msg =~ "parallel_ensure_deadlock/foo.ex => BarDeadlock"
      assert msg =~ "parallel_ensure_deadlock/bar.ex => FooDeadlock"
      assert msg =~ ~r"== Compilation error in file .+parallel_ensure_deadlock/foo\.ex =="
      assert msg =~ "** (CompileError)  deadlocked waiting on module BarDeadlock"
      assert msg =~ ~r"== Compilation error in file .+parallel_ensure_deadlock/bar\.ex =="
      assert msg =~ "** (CompileError)  deadlocked waiting on module FooDeadlock"
    end

    test "does not deadlock from soft Code.ensure_compiled" do
      [foo, bar] =
        write_tmp(
          "parallel_ensure_nodeadlock",
          foo: """
          defmodule FooCircular do
            {:error, :deadlock} = Code.ensure_compiled(BarCircular, raise_on_deadlock: false)
          end
          """,
          bar: """
          defmodule BarCircular do
            {:error, :deadlock} = Code.ensure_compiled(FooCircular, raise_on_deadlock: false)
          end
          """
        )

      assert {:ok, modules, []} = Kernel.ParallelCompiler.compile([foo, bar])
      assert Enum.sort([FooCircular, BarCircular]) == [BarCircular, FooCircular]
    after
      purge([FooCircular, BarCircular])
    end

    test "handles async deadlocks" do
      [foo, bar] =
        write_tmp(
          "async_deadlock",
          foo: """
          defmodule FooAsyncDeadlock do
            Kernel.ParallelCompiler.async(fn ->
              BarAsyncDeadlock.__info__(:module)
            end)

            BarAsyncDeadlock.__info__(:module)
          end
          """,
          bar: """
          defmodule BarAsyncDeadlock do
            FooAsyncDeadlock.__info__(:module)
          end
          """
        )

      capture_io(fn ->
        fixtures = [foo, bar]
        assert {:error, [bar_error, foo_error], []} = Kernel.ParallelCompiler.compile(fixtures)
        assert bar_error == {bar, nil, "deadlocked waiting on module FooAsyncDeadlock"}
        assert foo_error == {foo, nil, "deadlocked waiting on module BarAsyncDeadlock"}
      end)
    end

    test "supports warnings as errors" do
      warnings_as_errors = Code.compiler_options()[:warnings_as_errors]

      [fixture] =
        write_tmp(
          "warnings_as_errors",
          warnings_as_errors: """
          defmodule WarningsSample do
            def hello(a), do: a
            def hello(b), do: b
          end
          """
        )

      try do
        Code.compiler_options(warnings_as_errors: true)

        msg =
          capture_io(:stderr, fn ->
            assert {:error, [error], []} = Kernel.ParallelCompiler.compile([fixture])
            msg = "this clause cannot match because a previous clause at line 2 always matches"
            assert error == {fixture, 3, msg}
          end)

        assert msg =~
                 "Compilation failed due to warnings while using the --warnings-as-errors option\n"
      after
        Code.compiler_options(warnings_as_errors: warnings_as_errors)
        purge([WarningsSample])
      end
    end

    test "does not use incorrect line number when error originates in another file" do
      File.mkdir_p!(tmp_path())

      [a, b] =
        write_tmp(
          "error_line",
          a: """
          defmodule A do
            def fun(arg), do: arg / 2
          end
          """,
          b: """
          defmodule B do
            def fun(arg) do
              A.fun(arg)
              :ok
            end
          end
          B.fun(:not_a_number)
          """
        )

      capture_io(fn ->
        assert {:error, [{^b, nil, _}], _} = Kernel.ParallelCompiler.compile([a, b])
      end)
    end

    test "gets correct line number for UndefinedFunctionError" do
      File.mkdir_p!(tmp_path())

      [fixture] =
        write_tmp("undef",
          undef: """
          defmodule UndefErrorLine do
            Bogus.fun()
          end
          """
        )

      capture_io(fn ->
        assert {:error, [{^fixture, 2, _}], _} = Kernel.ParallelCompiler.compile([fixture])
      end)
    end

    test "gets proper beam destinations from dynamic modules" do
      fixtures =
        write_tmp(
          "dynamic",
          dynamic: """
          Module.create(Dynamic, quote(do: :ok), file: "dynamic.ex")
          [_ | _] = :code.which(Dynamic)
          """
        )

      assert {:ok, [Dynamic], []} = Kernel.ParallelCompiler.compile(fixtures, dest: "sample")
    after
      purge([Dynamic])
    end
  end

  describe "require" do
    test "returns struct undefined error when local struct is undefined" do
      [fixture] =
        write_tmp(
          "require_struct",
          undef: """
          defmodule Undef do
            def undef() do
              %__MODULE__{}
            end
          end
          """
        )

      expected_msg = "Undef.__struct__/1 is undefined, cannot expand struct Undef"

      assert capture_io(fn ->
               assert {:error, [{^fixture, 3, msg}], []} =
                        Kernel.ParallelCompiler.require([fixture])

               assert msg =~ expected_msg
             end) =~ expected_msg
    end

    test "does not hang on missing dependencies" do
      [fixture] =
        write_tmp(
          "require_does_not_hang",
          with_behaviour_and_struct: """
          # We need to ensure it won't block even after multiple calls.
          # So we use both behaviour and struct expansion below.
          defmodule WithBehaviourAndStruct do
            # @behaviour will call ensure_compiled().
            @behaviour :unknown
            # Struct expansion calls it as well.
            %ThisModuleWillNeverBeAvailable{}
          end
          """
        )

      expected_msg =
        "ThisModuleWillNeverBeAvailable.__struct__/1 is undefined, cannot expand struct ThisModuleWillNeverBeAvailable"

      assert capture_io(fn ->
               assert {:error, [{^fixture, 7, msg}], []} =
                        Kernel.ParallelCompiler.require([fixture])

               assert msg =~ expected_msg
             end) =~ "== Compilation error"
    end

    test "supports warnings as errors" do
      warnings_as_errors = Code.compiler_options()[:warnings_as_errors]

      [fixture] =
        write_tmp(
          "warnings_as_errors",
          warnings_as_errors: """
          defmodule WarningsSample do
            def hello(a), do: a
            def hello(b), do: b
          end
          """
        )

      try do
        Code.compiler_options(warnings_as_errors: true)

        msg =
          capture_io(:stderr, fn ->
            assert {:error, [error], []} = Kernel.ParallelCompiler.require([fixture])

            message =
              "this clause cannot match because a previous clause at line 2 always matches"

            assert error == {fixture, 3, message}
          end)

        assert msg =~
                 "Compilation failed due to warnings while using the --warnings-as-errors option\n"
      after
        Code.compiler_options(warnings_as_errors: warnings_as_errors)
        purge([WarningsSample])
      end
    end
  end
end
