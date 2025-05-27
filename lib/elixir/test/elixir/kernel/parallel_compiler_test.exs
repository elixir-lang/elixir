# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

import PathHelpers

defmodule Kernel.ParallelCompilerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  @no_warnings %{compile_warnings: [], runtime_warnings: []}

  defp compile(files, opts \\ []) do
    Kernel.ParallelCompiler.compile(files, [return_diagnostics: true] ++ opts)
  end

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
    test "with profiling" do
      fixtures =
        write_tmp(
          "profile_time",
          bar: """
          defmodule HelloWorld do
          end
          """
        )

      profile =
        capture_io(:stderr, fn ->
          assert {:ok, modules, @no_warnings} = compile(fixtures, profile: :time)

          assert HelloWorld in modules
        end)

      assert profile =~
               ~r"\[profile\] [\s\d]{6}ms compiling \+      0ms waiting while compiling .*tmp/profile_time/bar.ex"

      assert profile =~ ~r"\[profile\] Finished compilation cycle of 1 modules in \d+ms"
      assert profile =~ ~r"\[profile\] Finished group pass check of 1 modules in \d+ms"
    after
      purge([HelloWorld])
    end

    test "immediately loads modules when not writing them to disk" do
      fixtures =
        write_tmp(
          "compile_loads",
          will_be_loaded: """
          defmodule WillBeLoaded do
          end
          true = Code.loaded?(WillBeLoaded)
          """
        )

      assert {:ok, _modules, @no_warnings} = compile(fixtures)
    end

    test "lazily loads modules when writing them to disk" do
      fixtures =
        write_tmp(
          "compile_lazy_loads",
          will_be_lazy_loaded: """
          defmodule WillBeLazyLoaded do
          end
          false = Code.loaded?(WillBeLazyLoaded)
          {:error, _} = Code.ensure_loaded(WillBeLazyLoaded)
          {:module, _} = Code.ensure_compiled(WillBeLazyLoaded)
          """,
          is_autoloaded: """
          defmodule WillBeAutoLoaded do
            @compile {:autoload, true}
          end
          true = Code.loaded?(WillBeAutoLoaded)
          """
        )

      assert {:ok, _modules, @no_warnings} =
               Kernel.ParallelCompiler.compile_to_path(fixtures, tmp_path("pcload"),
                 return_diagnostics: true
               )
    end

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
            # We use this ensure_compiled clause so both Foo and
            # Bar block. Foo depends on Unknown and Bar depends on
            # Foo. The compiler will see this dependency and first
            # release Foo and then Bar, compiling with success.
            {:error, _} = Code.ensure_compiled(Unknown)
            def message, do: "message_from_foo"
          end
          """
        )

      assert capture_io(fn ->
               assert {:ok, modules, @no_warnings} = compile(fixtures)
               assert BarParallel in modules
               assert FooParallel in modules
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

      assert {:ok, modules, @no_warnings} = compile(fixtures)
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

      assert {:ok, modules, @no_warnings} = compile(fixtures)
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

      assert capture_io(:stderr, fn ->
               assert {:error,
                       [
                         %{file: ^fixture, position: {3, 5}, message: msg},
                         %{file: ^fixture, position: 0, message: compile_msg}
                       ], @no_warnings} =
                        compile([fixture])

               assert msg =~ expected_msg
               assert compile_msg =~ "cannot compile module Undef (errors have been logged)"
             end) =~ expected_msg
    end

    test "returns error when fails to expand struct" do
      [fixture] =
        write_tmp(
          "compile_struct_invalid_key",
          undef: """
          defmodule InvalidStructKey do
            def invalid_struct_key() do
              %Date{invalid_key: 2020}
            end
          end
          """
        )

      expected_msg = "** (KeyError) key :invalid_key not found"

      assert capture_io(:stderr, fn ->
               assert {:error, [%{file: ^fixture, position: 3, message: msg}], @no_warnings} =
                        compile([fixture])

               assert msg =~ expected_msg
             end) =~ expected_msg
    end

    test "does not crash with pending monitor message" do
      {pid, ref} = spawn_monitor(fn -> :ok end)

      [fixture] =
        write_tmp(
          "quick_example",
          quick_example: """
          defmodule QuickExample do
          end
          """
        )

      assert {:ok, [QuickExample], @no_warnings} = compile([fixture])
      assert_received {:DOWN, ^ref, _, ^pid, :normal}
    after
      purge([QuickExample])
    end

    test "does not crash on external reports" do
      [fixture] =
        write_tmp(
          "compile_quoted",
          quick_example: """
          defmodule CompileQuoted do
            try do
              Code.compile_quoted({:fn, [], [{:->, [], [[], quote(do: unknown_var)]}]})
            rescue
              _ -> :ok
            end
          end
          """
        )

      assert capture_io(:stderr, fn ->
               assert {:ok, [CompileQuoted], @no_warnings} = compile([fixture])
             end) =~ "undefined variable \"unknown_var\""
    after
      purge([CompileQuoted])
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

      assert capture_io(:stderr, fn ->
               assert {:error,
                       [
                         %{file: ^fixture, position: {7, 3}, message: msg},
                         %{file: ^fixture, position: 0, message: compile_msg}
                       ], @no_warnings} =
                        compile([fixture])

               assert msg =~ expected_msg

               assert compile_msg =~
                        "cannot compile module WithBehaviourAndStruct (errors have been logged)"
             end) =~ expected_msg
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

      assert capture_io(:stderr, fn ->
               assert {:error,
                       [
                         %{file: ^missing_struct, position: {2, 3}, message: msg},
                         %{file: ^missing_struct, position: 0, message: compile_msg}
                       ], @no_warnings} =
                        compile([missing_struct, depends_on])

               assert msg =~ expected_msg

               assert compile_msg =~
                        "cannot compile module MissingStruct (errors have been logged)"
             end) =~ expected_msg
    end

    test "does not deadlock on missing import/struct dependencies" do
      [missing_import, depends_on] =
        write_tmp(
          "import_and_structs",
          missing_import: """
          defmodule MissingStruct do
            import Unknown.Module
          end
          """,
          depends_on_missing_struct: """
          %MissingStruct{}
          """
        )

      expected_msg = "module Unknown.Module is not loaded and could not be found"

      assert capture_io(:stderr, fn ->
               assert {:error,
                       [
                         %{file: ^missing_import, position: {2, 3}, message: msg},
                         %{file: ^missing_import, position: 0, message: compile_msg}
                       ], @no_warnings} =
                        compile([missing_import, depends_on])

               assert msg =~ expected_msg

               assert compile_msg =~
                        "cannot compile module MissingStruct (errors have been logged)"
             end) =~ expected_msg
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
            %FooDeadlock{}
          end
          """
        )

      msg =
        capture_io(:stderr, fn ->
          fixtures = [foo, bar]
          assert {:error, [bar_error, foo_error], @no_warnings} = compile(fixtures)

          assert %{file: ^bar, position: 2, message: "deadlocked waiting on struct FooDeadlock"} =
                   bar_error

          assert %{file: ^foo, position: nil, message: "deadlocked waiting on module BarDeadlock"} =
                   foo_error
        end)

      assert msg =~ "Compilation failed because of a deadlock between files."
      assert msg =~ "parallel_deadlock/foo.ex => BarDeadlock"
      assert msg =~ "parallel_deadlock/bar.ex => FooDeadlock"
      assert msg =~ ~r"== Compilation error in file .+parallel_deadlock/foo\.ex =="
      assert msg =~ "** (CompileError) deadlocked waiting on module BarDeadlock"
      assert msg =~ ~r"== Compilation error in file .+parallel_deadlock/bar\.ex:2 =="
      assert msg =~ "** (CompileError) deadlocked waiting on struct FooDeadlock"
    end

    test "does not deadlock from Code.ensure_compiled" do
      [foo, bar] =
        write_tmp(
          "parallel_ensure_nodeadlock",
          foo: """
          defmodule FooCircular do
            {:error, :unavailable} = Code.ensure_compiled(BarCircular)
          end
          """,
          bar: """
          defmodule BarCircular do
            {:error, :unavailable} = Code.ensure_compiled(FooCircular)
          end
          """
        )

      assert {:ok, _modules, @no_warnings} = compile([foo, bar])
      assert Enum.sort([FooCircular, BarCircular]) == [BarCircular, FooCircular]
    after
      purge([FooCircular, BarCircular])
    end

    test "handles pmap compilation" do
      [foo, bar] =
        write_tmp(
          "async_compile",
          foo: """
          defmodule FooAsync do
            true = Code.can_await_module_compilation?()

            [BarAsync] =
              Kernel.ParallelCompiler.pmap([:ok], fn :ok ->
                true = Code.can_await_module_compilation?()
                BarAsync.__info__(:module)
              end)
          end
          """,
          bar: """
          defmodule BarAsync do
            true = Code.can_await_module_compilation?()
          end
          """
        )

      capture_io(:stderr, fn ->
        fixtures = [foo, bar]
        assert {:ok, modules, @no_warnings} = compile(fixtures)
        assert FooAsync in modules
        assert BarAsync in modules
      end)
    after
      purge([FooAsync, BarAsync])
    end

    test "handles pmap deadlocks" do
      [foo, bar] =
        write_tmp(
          "async_deadlock",
          foo: """
          defmodule FooAsyncDeadlock do
            Kernel.ParallelCompiler.pmap([:ok], fn :ok ->
              BarAsyncDeadlock.__info__(:module)
            end)
          end
          """,
          bar: """
          defmodule BarAsyncDeadlock do
            FooAsyncDeadlock.__info__(:module)
          end
          """
        )

      capture_io(:stderr, fn ->
        fixtures = [foo, bar]
        assert {:error, [bar_error, foo_error], @no_warnings} = compile(fixtures)

        assert %{
                 file: ^bar,
                 position: nil,
                 message: "deadlocked waiting on module FooAsyncDeadlock"
               } = bar_error

        assert %{file: ^foo, position: nil, message: "deadlocked waiting on pmap [#PID<" <> _} =
                 foo_error
      end)
    end

    test "does not use incorrect line number when error originates in another file" do
      File.mkdir_p!(tmp_path())

      [a, b] =
        write_tmp(
          "error_line",
          a: """
          defmodule PCA do
            def fun(arg), do: arg / 2
          end
          """,
          b: """
          defmodule PCB do
            def fun(arg) do
              PCA.fun(arg)
              :ok
            end
          end
          PCB.fun(:not_a_number)
          """
        )

      capture_io(:stderr, fn ->
        assert {:error, [%{file: ^b, position: 0, message: _}], _} = compile([a, b])
      end)
    end

    test "gets both source and file on @file annotations" do
      File.mkdir_p!(tmp_path())

      [a] =
        write_tmp(
          "file_source",
          a: """
          defmodule FileAttr do
            @file "unknown.foo.bar"
            def fun, do: (unused = :ok)
          end
          """
        )

      capture_io(:stderr, fn ->
        assert {:ok, [FileAttr], %{compile_warnings: [%{source: ^a, file: file, message: _}]}} =
                 compile([a])

        assert String.ends_with?(file, "unknown.foo.bar")
        assert Path.type(file) == :absolute
      end)
    after
      purge([FileAttr])
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

      capture_io(:stderr, fn ->
        assert {:error, [%{file: ^fixture, position: 2, message: _}], _} = compile([fixture])
      end)
    end

    test "gets correct file+line+column number for SyntaxError" do
      File.mkdir_p!(tmp_path())

      [fixture] =
        write_tmp("error",
          error: """
          raise SyntaxError, file: "foo/bar.ex", line: 3, column: 10
          """
        )

      file = Path.absname("foo/bar.ex")

      capture_io(:stderr, fn ->
        assert {:error, [%{file: ^file, source: ^fixture, position: {3, 10}}], _} =
                 compile([fixture])
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

      assert {:ok, [Dynamic], @no_warnings} = compile(fixtures, dest: "sample")
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

      assert capture_io(:stderr, fn ->
               assert {:error,
                       [
                         %{file: ^fixture, position: {3, 5}, message: msg},
                         %{file: ^fixture, position: 0, message: compile_msg}
                       ], @no_warnings} =
                        Kernel.ParallelCompiler.require([fixture], return_diagnostics: true)

               assert msg =~ expected_msg
               assert compile_msg =~ "cannot compile module Undef (errors have been logged)"
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

      assert capture_io(:stderr, fn ->
               assert {:error,
                       [
                         %{file: ^fixture, position: {7, 3}, message: msg},
                         %{file: ^fixture, position: 0, message: compile_msg}
                       ], @no_warnings} =
                        Kernel.ParallelCompiler.require([fixture], return_diagnostics: true)

               assert msg =~ expected_msg

               assert compile_msg =~
                        "cannot compile module WithBehaviourAndStruct (errors have been logged)"
             end) =~ expected_msg
    end
  end
end
