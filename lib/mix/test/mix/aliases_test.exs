Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.AliasesTest do
  use MixTest.Case

  defmodule Aliases do
    def project do
      [
        aliases: [
          h: "hello",
          p: &inspect/1,
          compile: "hello",
          help: ["help", "hello"],
          "nested.h": [&Mix.shell().info(inspect(&1)), "h foo bar"]
        ]
      ]
    end
  end

  setup do
    Mix.Project.push(Aliases)
    :ok
  end

  test "runs string aliases" do
    assert Mix.Task.run("h", []) == "Hello, World!"
    assert Mix.Task.run("h", []) == :noop
    assert Mix.Task.run("hello", []) == :noop

    Mix.Task.reenable("h")
    Mix.Task.reenable("hello")
    assert Mix.Task.run("h", ["foo", "bar"]) == "Hello, foo bar!"
  end

  test "runs function aliases" do
    assert Mix.Task.run("p", []) == "[]"
    assert Mix.Task.run("p", []) == :noop

    Mix.Task.reenable("p")
    assert Mix.Task.run("p", ["foo", "bar"]) == "[\"foo\", \"bar\"]"
  end

  test "runs list aliases" do
    assert Mix.Task.run("nested.h", ["baz"]) == "Hello, foo bar baz!"
    assert_received {:mix_shell, :info, ["[]"]}
  end

  test "run alias override" do
    assert Mix.Task.run("compile", []) == "Hello, World!"
    assert Mix.Task.run("compile", []) == :noop
  end

  test "run alias override with recursion" do
    assert Mix.Task.run("help", []) == "Hello, World!"
    assert_received {:mix_shell, :info, ["mix test" <> _]}
  end
end
