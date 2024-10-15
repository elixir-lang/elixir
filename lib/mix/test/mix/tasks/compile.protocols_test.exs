Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Compile.ProtocolsTest do
  use MixTest.Case

  @old {{2010, 1, 1}, {0, 0, 0}}

  test "compiles and consolidates local protocols", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.mkdir_p!("lib")
      assert Mix.Task.run("compile")

      # Define a local protocol
      File.write!("lib/protocol.ex", """
      defprotocol Compile.Protocol do
        def foo(a, b)
      end
      """)

      assert compile_elixir_and_protocols() == :ok
      assert apply(Compile.Protocol, :__protocol__, [:impls]) == {:consolidated, []}
      mark_as_old!("_build/dev/lib/sample/consolidated/Elixir.Compile.Protocol.beam")

      # Implement a local protocol
      File.write!("lib/impl.ex", """
      defimpl Compile.Protocol, for: Integer do
        def foo(a, b), do: a + b
      end
      """)

      assert compile_elixir_and_protocols() == :ok
      assert apply(Compile.Protocol, :__protocol__, [:impls]) == {:consolidated, [Integer]}

      assert mark_as_old!("_build/dev/lib/sample/consolidated/Elixir.Compile.Protocol.beam") !=
               @old

      # Delete a local implementation
      File.rm!("lib/impl.ex")
      assert compile_elixir_and_protocols() == :ok
      assert apply(Compile.Protocol, :__protocol__, [:impls]) == {:consolidated, []}

      assert mark_as_old!("_build/dev/lib/sample/consolidated/Elixir.Compile.Protocol.beam") !=
               @old

      # Delete a local protocol
      File.rm!("lib/protocol.ex")
      assert compile_elixir_and_protocols() == :ok
      refute File.regular?("_build/dev/lib/sample/consolidated/Elixir.Compile.Protocol.beam")
    end)
  end

  test "compiles after converting a protocol into a standard module", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.mkdir_p!("lib")
      Mix.Task.run("compile")
      purge_protocol(Compile.Protocol)

      # Define a local protocol
      File.write!("lib/protocol.ex", """
      defprotocol Compile.Protocol do
        def foo(a)
      end

      defimpl Compile.Protocol, for: Integer do
        def foo(a), do: a
      end
      """)

      assert compile_elixir_and_protocols() == :ok
      assert apply(Compile.Protocol, :__protocol__, [:impls]) == {:consolidated, [Integer]}
      mark_as_old!("_build/dev/lib/sample/consolidated/Elixir.Compile.Protocol.beam")
      File.rm!("lib/protocol.ex")

      # Define a standard module
      File.write!("lib/protocol.ex", """
      defmodule Compile.Protocol do
      end
      """)

      assert compile_elixir_and_protocols() == :ok
      refute function_exported?(Compile.Protocol, :__protocol__, 1)

      # Delete a local protocol
      File.rm!("lib/protocol.ex")
      assert compile_elixir_and_protocols() == :ok
      refute function_exported?(Compile.Protocol, :__protocol__, 1)
      refute File.regular?("_build/dev/lib/sample/consolidated/Elixir.Compile.Protocol.beam")
    end)
  end

  test "compiles and consolidates deps protocols", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.mkdir_p!("lib")
      Mix.Task.run("compile")
      purge_protocol(String.Chars)
      mark_as_old!("_build/dev/lib/sample/consolidated/Elixir.String.Chars.beam")

      assert compile_elixir_and_protocols() == :ok
      assert {:consolidated, [Atom | _]} = apply(String.Chars, :__protocol__, [:impls])
      assert mtime("_build/dev/lib/sample/consolidated/Elixir.String.Chars.beam") == @old

      # Implement a deps protocol
      File.write!("lib/struct.ex", """
      defmodule Aaaaa do
        defstruct a: nil
        defimpl String.Chars do
          def to_string(_), do: "ok"
        end
      end
      """)

      assert compile_elixir_and_protocols() == :ok
      assert {:consolidated, [Aaaaa, Atom | _]} = apply(String.Chars, :__protocol__, [:impls])
      assert mark_as_old!("_build/dev/lib/sample/consolidated/Elixir.String.Chars.beam") != @old

      # Delete the local implementation
      File.rm!("lib/struct.ex")
      assert compile_elixir_and_protocols() == :ok
      assert {:consolidated, [Atom | _]} = apply(String.Chars, :__protocol__, [:impls])
      assert mark_as_old!("_build/dev/lib/sample/consolidated/Elixir.String.Chars.beam") != @old
    end)
  end

  test "consolidated protocols keep relative path to their source" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      compile_elixir_and_protocols()

      # Load consolidated
      :code.add_patha(~c"_build/dev/lib/sample/consolidated")
      :code.purge(Enumerable)
      :code.delete(Enumerable)

      try do
        Enumerable.impl_for!(:oops)
      rescue
        Protocol.UndefinedError ->
          assert [{_, _, _, [file: ~c"lib/enum.ex"] ++ _} | _] = __STACKTRACE__
      else
        _ ->
          flunk("Enumerable.impl_for!/1 should have failed")
      after
        purge_protocol(Enumerable)
      end
    end)
  end

  defp compile_elixir_and_protocols do
    Mix.Tasks.Compile.reenable(protocols: :consolidate)
    with {result, _} <- Mix.Task.run("compile"), do: result
  end

  defp mtime(path) do
    File.stat!(path).mtime
  end

  defp mark_as_old!(path) do
    mtime = mtime(path)
    File.touch!(path, @old)
    mtime
  end

  defp purge_protocol(module) do
    :code.del_path(:filename.absname(~c"_build/dev/lib/sample/consolidated"))
    :code.purge(module)
    :code.delete(module)
  end
end
