Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.FormatTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  defmodule FormatWithDepsApp do
    def project do
      [
        app: :format_with_deps,
        version: "0.1.0",
        deps: [{:my_dep, "0.1.0", path: "deps/my_dep"}]
      ]
    end
  end

  test "formats the given files", context do
    in_tmp(context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      Mix.Tasks.Format.run(["a.ex"])

      assert File.read!("a.ex") == """
             foo(bar)
             """
    end)
  end

  test "formats the given pattern", context do
    in_tmp(context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      Mix.Tasks.Format.run(["*.ex", "a.ex"])

      assert File.read!("a.ex") == """
             foo(bar)
             """
    end)
  end

  test "is a no-op if the file is already formatted", context do
    in_tmp(context.test, fn ->
      File.write!("a.ex", """
      foo(bar)
      """)

      File.touch!("a.ex", {{2010, 1, 1}, {0, 0, 0}})
      Mix.Tasks.Format.run(["a.ex"])
      assert File.stat!("a.ex").mtime == {{2010, 1, 1}, {0, 0, 0}}
    end)
  end

  test "does not write file to disk on dry-run", context do
    in_tmp(context.test, fn ->
      File.write!("a.ex", """
      foo bar
      """)

      Mix.Tasks.Format.run(["a.ex", "--dry-run"])

      assert File.read!("a.ex") == """
             foo bar
             """
    end)
  end

  test "reads file from stdin and prints to stdout", context do
    in_tmp(context.test, fn ->
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
    end)
  end

  test "reads file from stdin and prints to stdout with formatter", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [locals_without_parens: [foo: 1]]
      """)

      output =
        capture_io("foo :bar", fn ->
          Mix.Tasks.Format.run(["-"])
        end)

      assert output == """
             foo :bar
             """
    end)
  end

  test "checks if file is formatted with --check-formatted", context do
    in_tmp(context.test, fn ->
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
    end)
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

  test "uses inputs and configuration from .formatter.exs", context do
    in_tmp(context.test, fn ->
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
    end)
  end

  test "expands patterns in inputs from .formatter.exs", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["{a,.b}.ex"]
      ]
      """)

      File.write!("a.ex", """
      foo bar
      """)

      File.write!(".b.ex", """
      foo bar
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("a.ex") == """
             foo(bar)
             """

      assert File.read!(".b.ex") == """
             foo(bar)
             """
    end)
  end

  defmodule Elixir.SigilWPlugin do
    @behaviour Mix.Tasks.Format

    def features(opts) do
      assert opts[:from_formatter_exs] == :yes
      [sigils: [:W], extensions: ~w(.w)]
    end

    def format(contents, opts) do
      assert opts[:from_formatter_exs] == :yes
      contents |> String.split(~r/\s/) |> Enum.join("\n")
    end
  end

  test "uses sigil plugins from .formatter.exs", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.ex"],
        plugins: [SigilWPlugin],
        from_formatter_exs: :yes
      ]
      """)

      File.write!("a.ex", """
      if true do
        ~W'''
        foo bar baz
        '''abc
      end
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("a.ex") == """
             if true do
               ~W'''
               foo
               bar
               baz
               '''abc
             end
             """
    end)
  end

  test "uses extension plugins from .formatter.exs", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.w"],
        plugins: [SigilWPlugin],
        from_formatter_exs: :yes
      ]
      """)

      File.write!("a.w", """
      foo bar baz
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("a.w") == """
             foo
             bar
             baz
             """
    end)
  end

  test "uses inputs and configuration from --dot-formatter", context do
    in_tmp(context.test, fn ->
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
    end)
  end

  test "can read exported configuration from subdirectories", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [subdirectories: ["lib"]]
      """)

      File.mkdir_p!("lib")

      File.write!("lib/.formatter.exs", """
      [inputs: "a.ex", locals_without_parens: [my_fun: 2]]
      """)

      {formatter, formatter_opts} = Mix.Tasks.Format.formatter_for_file("lib/extra/a.ex")
      assert Keyword.get(formatter_opts, :locals_without_parens) == [my_fun: 2]
      assert formatter.("my_fun 1, 2") == "my_fun 1, 2\n"

      File.write!("lib/a.ex", """
      my_fun :foo, :bar
      other_fun :baz
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("lib/a.ex") == """
             my_fun :foo, :bar
             other_fun(:baz)
             """

      Mix.Tasks.Format.run(["lib/a.ex"])

      assert File.read!("lib/a.ex") == """
             my_fun :foo, :bar
             other_fun(:baz)
             """

      # No caching without a project
      manifest_path = Path.join(Mix.Project.manifest_path(), "cached_dot_formatter")
      refute File.regular?(manifest_path)

      # Caching with a project
      Mix.Project.push(__MODULE__.FormatWithDepsApp)
      Mix.Tasks.Format.run(["lib/a.ex"])
      manifest_path = Path.join(Mix.Project.manifest_path(), "cached_dot_formatter")
      assert File.regular?(manifest_path)

      # Let's check that the manifest gets updated if it's stale.
      File.touch!(manifest_path, {{2010, 1, 1}, {0, 0, 0}})

      Mix.Tasks.Format.run(["lib/a.ex"])
      assert File.stat!(manifest_path).mtime > {{2010, 1, 1}, {0, 0, 0}}
    end)
  end

  test "can read exported configuration from dependencies", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(__MODULE__.FormatWithDepsApp)

      File.write!(".formatter.exs", """
      [import_deps: [:my_dep]]
      """)

      File.write!("a.ex", """
      my_fun :foo, :bar
      """)

      File.mkdir_p!("deps/my_dep/")

      File.write!("deps/my_dep/.formatter.exs", """
      [export: [locals_without_parens: [my_fun: 2]]]
      """)

      Mix.Tasks.Format.run(["a.ex"])

      assert File.read!("a.ex") == """
             my_fun :foo, :bar
             """

      manifest_path = Path.join(Mix.Project.manifest_path(), "cached_dot_formatter")
      assert File.regular?(manifest_path)

      # Let's check that the manifest gets updated if it's stale.
      File.touch!(manifest_path, {{2010, 1, 1}, {0, 0, 0}})

      {_, formatter_opts} = Mix.Tasks.Format.formatter_for_file("a.ex")
      assert [my_fun: 2] = Keyword.get(formatter_opts, :locals_without_parens)

      Mix.Tasks.Format.run(["a.ex"])
      assert File.stat!(manifest_path).mtime > {{2010, 1, 1}, {0, 0, 0}}
    end)
  end

  test "can read exported configuration from dependencies and subdirectories", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(__MODULE__.FormatWithDepsApp)

      File.mkdir_p!("deps/my_dep/")

      File.write!("deps/my_dep/.formatter.exs", """
      [export: [locals_without_parens: [my_fun: 2]]]
      """)

      File.mkdir_p!("lib/sub")
      File.mkdir_p!("lib/not_used_and_wont_raise")

      File.write!(".formatter.exs", """
      [subdirectories: ["lib"]]
      """)

      File.write!("lib/.formatter.exs", """
      [subdirectories: ["*"]]
      """)

      File.write!("lib/sub/.formatter.exs", """
      [inputs: "a.ex", import_deps: [:my_dep]]
      """)

      File.write!("lib/sub/a.ex", """
      my_fun :foo, :bar
      other_fun :baz
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("lib/sub/a.ex") == """
             my_fun :foo, :bar
             other_fun(:baz)
             """

      Mix.Tasks.Format.run(["lib/sub/a.ex"])

      assert File.read!("lib/sub/a.ex") == """
             my_fun :foo, :bar
             other_fun(:baz)
             """

      # Update .formatter.exs, check that file is updated
      File.write!("lib/sub/.formatter.exs", """
      [inputs: "a.ex"]
      """)

      File.touch!("lib/sub/.formatter.exs", {{2038, 1, 1}, {0, 0, 0}})
      Mix.Tasks.Format.run([])

      assert File.read!("lib/sub/a.ex") == """
             my_fun(:foo, :bar)
             other_fun(:baz)
             """

      # Add a new entry to "lib" and it also gets picked.
      File.mkdir_p!("lib/extra")

      File.write!("lib/extra/.formatter.exs", """
      [inputs: "a.ex", locals_without_parens: [other_fun: 1]]
      """)

      File.write!("lib/extra/a.ex", """
      my_fun :foo, :bar
      other_fun :baz
      """)

      File.touch!("lib/extra/.formatter.exs", {{2038, 1, 1}, {0, 0, 0}})
      Mix.Tasks.Format.run([])

      {_, formatter_opts} = Mix.Tasks.Format.formatter_for_file("lib/extra/a.ex")
      assert [other_fun: 1] = Keyword.get(formatter_opts, :locals_without_parens)

      assert File.read!("lib/extra/a.ex") == """
             my_fun(:foo, :bar)
             other_fun :baz
             """
    end)
  end

  test "validates subdirectories in :subdirectories", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [subdirectories: "oops"]
      """)

      message = "Expected :subdirectories to return a list of directories, got: \"oops\""
      assert_raise Mix.Error, message, fn -> Mix.Tasks.Format.run([]) end

      File.write!(".formatter.exs", """
      [subdirectories: ["lib"]]
      """)

      File.mkdir_p!("lib")

      File.write!("lib/.formatter.exs", """
      []
      """)

      message = "Expected :inputs or :subdirectories key in lib/.formatter.exs"
      assert_raise Mix.Error, message, fn -> Mix.Tasks.Format.run([]) end
    end)
  end

  test "validates dependencies in :import_deps", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(__MODULE__.FormatWithDepsApp)

      File.write!(".formatter.exs", """
      [import_deps: [:my_dep]]
      """)

      message =
        "Unavailable dependency :my_dep given to :import_deps in the formatter configuration. " <>
          "The dependency cannot be found in the file system, please run \"mix deps.get\" and try again"

      assert_raise Mix.Error, message, fn -> Mix.Tasks.Format.run([]) end

      File.write!(".formatter.exs", """
      [import_deps: [:nonexistent_dep]]
      """)

      message =
        "Unknown dependency :nonexistent_dep given to :import_deps in the formatter configuration. " <>
          "The dependency is not listed in your mix.exs for environment :dev"

      assert_raise Mix.Error, message, fn -> Mix.Tasks.Format.run([]) end
    end)
  end

  test "prints an error on conflicting .formatter.exs files", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [inputs: "lib/**/*.{ex,exs}", subdirectories: ["lib", "foo"]]
      """)

      File.mkdir_p!("lib")

      File.write!("lib/.formatter.exs", """
      [inputs: "a.ex", locals_without_parens: [my_fun: 2]]
      """)

      File.mkdir_p!("foo")

      File.write!("foo/.formatter.exs", """
      [inputs: "../lib/a.ex", locals_without_parens: [my_fun: 2]]
      """)

      File.write!("lib/a.ex", """
      my_fun :foo, :bar
      other_fun :baz
      """)

      Mix.Tasks.Format.run([])

      message1 =
        "Both .formatter.exs and lib/.formatter.exs specify the file lib/a.ex in their " <>
          ":inputs option. To resolve the conflict, the configuration in .formatter.exs " <>
          "will be ignored. Please change the list of :inputs in one of the formatter files " <>
          "so only one of them matches lib/a.ex"

      message2 =
        "Both lib/.formatter.exs and foo/.formatter.exs specify the file lib/a.ex in their " <>
          ":inputs option. To resolve the conflict, the configuration in lib/.formatter.exs " <>
          "will be ignored. Please change the list of :inputs in one of the formatter files " <>
          "so only one of them matches lib/a.ex"

      assert_received {:mix_shell, :error, [^message1]}
      assert_received {:mix_shell, :error, [^message2]}
    end)
  end

  test "raises on invalid arguments", context do
    in_tmp(context.test, fn ->
      assert_raise Mix.Error, ~r"Expected one or more files\/patterns to be given", fn ->
        Mix.Tasks.Format.run([])
      end

      assert_raise Mix.Error, ~r"Could not find a file to format", fn ->
        Mix.Tasks.Format.run(["unknown.whatever"])
      end
    end)
  end

  test "raises SyntaxError when parsing invalid source file", context do
    in_tmp(context.test, fn ->
      File.write!("a.ex", """
      defmodule <%= module %>.Bar do end
      """)

      assert_raise SyntaxError, ~r"a.ex:1:13: syntax error before: '='", fn ->
        Mix.Tasks.Format.run(["a.ex"])
      end

      assert_received {:mix_shell, :error, ["mix format failed for file: a.ex"]}
    end)
  end

  test "raises SyntaxError when parsing invalid stdin", context do
    in_tmp(context.test, fn ->
      assert_raise SyntaxError, ~r"stdin:1:13: syntax error before: '='", fn ->
        capture_io("defmodule <%= module %>.Bar do end", fn ->
          Mix.Tasks.Format.run(["-"])
        end)
      end

      assert_received {:mix_shell, :error, ["mix format failed for stdin"]}
    end)
  end
end
