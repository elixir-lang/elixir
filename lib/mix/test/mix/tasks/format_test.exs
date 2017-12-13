Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.FormatTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  test "formats the given files", context do
    in_tmp context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      Mix.Tasks.Format.run(["a.ex"])

      assert File.read!("a.ex") == """
             foo(bar)
             """
    end
  end

  test "formats the given pattern", context do
    in_tmp context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      Mix.Tasks.Format.run(["*.ex", "a.ex"])

      assert File.read!("a.ex") == """
             foo(bar)
             """
    end
  end

  test "is a no-op if the file is already formatted", context do
    in_tmp context.test, fn ->
      File.write!("a.ex", """
      foo(bar)
      """)

      File.touch!("a.ex", {{2000, 1, 1}, {0, 0, 0}})
      Mix.Tasks.Format.run(["a.ex"])
      assert File.stat!("a.ex").mtime == {{2000, 1, 1}, {0, 0, 0}}
    end
  end

  test "does not write file to disk on dry-run", context do
    in_tmp context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      Mix.Tasks.Format.run(["a.ex", "--dry-run"])

      assert File.read!("a.ex") == """
             foo bar
             """
    end
  end

  test "reads file from stdin and prints to stdout", context do
    in_tmp context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      output =
        capture_io("foo( )", fn ->
          Mix.Tasks.Format.run(["a.ex", "-"])
        end)

      assert output == """
             foo()
             """

      assert File.read!("a.ex") == """
             foo(bar)
             """
    end
  end

  test "checks if file is formatted with --check-formatted", context do
    in_tmp context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      assert_raise Mix.Error, ~r"mix format failed due to --check-formatted", fn ->
        Mix.Tasks.Format.run(["a.ex", "--check-formatted"])
      end

      assert File.read!("a.ex") == """
             foo bar
             """

      assert Mix.Tasks.Format.run(["a.ex"]) == :ok
      assert Mix.Tasks.Format.run(["a.ex", "--check-formatted"]) == :ok

      assert File.read!("a.ex") == """
             foo(bar)
             """
    end
  end

  test "checks if stdin is formatted with --check-formatted" do
    assert_raise Mix.Error, ~r"mix format failed due to --check-formatted", fn ->
      capture_io("foo( )", fn ->
        Mix.Tasks.Format.run(["--check-formatted", "-"])
      end)
    end

    output =
      capture_io("foo()\n", fn ->
        Mix.Tasks.Format.run(["--check-formatted", "-"])
      end)

    assert output == ""
  end

  test "checks if file is equivalent with --check-equivalent", context do
    in_tmp context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      Mix.Tasks.Format.run(["a.ex", "--check-equivalent"])

      assert File.read!("a.ex") == """
             foo(bar)
             """
    end
  end

  test "uses inputs and configuration from .formatter.exs", context do
    in_tmp context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.ex"],
        locals_without_parens: [foo: 1]
      ]
      """)

      File.write!("a.ex", """
      foo bar baz
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("a.ex") == """
             foo bar(baz)
             """
    end
  end

  test "uses inputs and configuration from --dot-formatter", context do
    in_tmp context.test, fn ->
      File.write!("custom_formatter.exs", """
      [
        inputs: ["a.ex"],
        locals_without_parens: [foo: 1]
      ]
      """)

      File.write!("a.ex", """
      foo bar baz
      """)

      Mix.Tasks.Format.run(["--dot-formatter", "custom_formatter.exs"])

      assert File.read!("a.ex") == """
             foo bar(baz)
             """
    end
  end

  defmodule FormatWithDepsApp do
    def project do
      [
        app: :format_with_deps,
        version: "0.1.0",
        deps: [{:my_dep, "0.1.0", path: "deps/my_dep"}]
      ]
    end
  end

  test "can read exported configuration from dependencies" do
    Mix.Project.push(__MODULE__.FormatWithDepsApp)

    in_fixture "format_with_deps", fn ->
      File.write!(".formatter.exs", """
      [import_deps: [:my_dep]]
      """)

      File.write!("a.ex", """
      my_fun :foo, :bar
      """)

      File.mkdir_p!("deps/my_dep/")

      File.write!("deps/my_dep/.formatter.exs", """
      [export_locals_without_parens: [my_fun: 2]]
      """)

      Mix.Tasks.Format.run(["a.ex"])

      assert File.read!("a.ex") == """
             my_fun :foo, :bar
             """

      manifest_path = "_build/dev/.cached_deps_formatter.exs"
      assert File.regular?(manifest_path)

      # Let's check that the manifest gets updated if .formatter.exs changes.
      File.touch!(manifest_path, {{1970, 1, 1}, {0, 0, 0}})
      %File.Stat{mtime: mtime} = File.stat!(manifest_path)

      File.touch!(".formatter.exs")
      Mix.Tasks.Format.run(["a.ex"])
      %File.Stat{mtime: new_mtime} = File.stat!(manifest_path)

      assert new_mtime > mtime
    end
  end

  test "raises on invalid arguments", context do
    in_tmp context.test, fn ->
      assert_raise Mix.Error, ~r"Expected one or more files\/patterns to be given", fn ->
        Mix.Tasks.Format.run([])
      end

      assert_raise Mix.Error, ~r"Could not find a file to format", fn ->
        Mix.Tasks.Format.run(["unknown.whatever"])
      end
    end
  end

  test "raises SyntaxError when parsing invalid source file", context do
    in_tmp context.test, fn ->
      File.write!("a.ex", """
      defmodule <%= module %>.Bar do end
      """)

      assert_raise SyntaxError, ~r"a.ex:1: syntax error before: '='", fn ->
        Mix.Tasks.Format.run(["a.ex"])
      end

      assert_received {:mix_shell, :error, ["mix format failed for file: a.ex"]}
    end
  end
end
