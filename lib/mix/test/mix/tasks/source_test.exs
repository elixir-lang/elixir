# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.SourceTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  @moduletag :requires_source
  @editor System.get_env("ELIXIR_EDITOR")

  test "source MODULE", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Source.run(["Enum"])
      assert_received {:mix_shell, :info, [location]}
      assert location =~ ~r"lib/elixir/lib/enum\.ex:\d+"
    end)
  end

  @tag :require_ast
  test "source MODULE.FUN", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Source.run(["Enum.map"])
      assert_received {:mix_shell, :info, [location]}
      assert location =~ ~r"lib/elixir/lib/enum\.ex:\d+"
    end)
  end

  @tag :require_ast
  test "source MODULE.FUN/ARITY", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Source.run(["Enum.map/2"])
      assert_received {:mix_shell, :info, [location]}
      assert location =~ ~r"lib/elixir/lib/enum\.ex:\d+"
    end)
  end

  test "source NESTED MODULE", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Source.run(["IO.ANSI"])
      assert_received {:mix_shell, :info, [location]}
      assert location =~ ~r"lib/elixir/lib/io/ansi\.ex:\d+"
    end)
  end

  test "source Erlang MODULE", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Source.run([":math"])
      assert_received {:mix_shell, :info, [location]}
      assert location =~ ~r"math\.erl:\d+"
    end)
  end

  test "source ERROR" do
    assert_raise Mix.Error, "Invalid expression: Foo.bar(~s[baz])", fn ->
      Mix.Tasks.Source.run(["Foo.bar(~s[baz])"])
    end
  end

  test "source unavailable module" do
    assert_raise Mix.Error, ~r/Could not find source/, fn ->
      Mix.Tasks.Source.run(["DoesNotExist"])
    end
  end

  test "source --open opens __FILE__ and __LINE__", context do
    System.put_env("ELIXIR_EDITOR", "echo __LINE__:__FILE__")

    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Source.run(["--open", "Enum"])
        end)

      assert output =~ ~r"\d+:.*lib/elixir/lib/enum\.ex"
    end)
  after
    if @editor,
      do: System.put_env("ELIXIR_EDITOR", @editor),
      else: System.delete_env("ELIXIR_EDITOR")
  end

  test "source --open without editor" do
    System.delete_env("ELIXIR_EDITOR")
    System.delete_env("EDITOR")

    assert_raise Mix.Error, ~r/ELIXIR_EDITOR/, fn ->
      Mix.Tasks.Source.run(["--open", "Enum"])
    end
  after
    if @editor,
      do: System.put_env("ELIXIR_EDITOR", @editor),
      else: System.delete_env("ELIXIR_EDITOR")
  end

  test "bad arguments" do
    message = ~r/Unexpected arguments/

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Source.run(["foo", "bar"])
    end

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Source.run([])
    end
  end
end
