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

  test "doesn't format empty files into line breaks", context do
    in_tmp(context.test, fn ->
      File.write!("a.exs", "")
      Mix.Tasks.Format.run(["a.exs"])
      assert File.read!("a.exs") == ""

      File.write!("b.exs", "  \n  \n")
      Mix.Tasks.Format.run(["b.exs"])
      assert File.read!("b.exs") == ""
    end)
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

  test "does not try to format a directory that matches a given pattern", context do
    in_tmp(context.test, fn ->
      File.mkdir_p!("a.ex")

      assert_raise Mix.Error, ~r"Could not find a file to format", fn ->
        Mix.Tasks.Format.run(["*.ex"])
      end
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

  test "checks that no error is raised with --check-formatted and --no-exit" do
    capture_io("foo( )", fn ->
      Mix.Tasks.Format.run(["--check-formatted", "--no-exit", "-"])
    end)

    assert_received {:mix_shell, :info, ["The following files are not formatted" <> _]}
  end

  test "raises an error if --no-exit is passed without --check-formatted" do
    assert_raise Mix.Error, ~r"--no-exit can only be used together", fn ->
      Mix.Tasks.Format.run(["--no-exit", "-"])
    end
  end

  test "uses inputs and configuration from .formatter.exs", context do
    in_tmp(context.test, fn ->
      # We need a project in order to enable caching
      Mix.Project.push(__MODULE__.FormatWithDepsApp)

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

      File.write!(".formatter.exs", """
      [inputs: ["a.ex"]]
      """)

      ensure_touched(".formatter.exs", "_build/dev/lib/format_with_deps/.mix/format_timestamp")
      Mix.Tasks.Format.run([])

      assert File.read!("a.ex") == """
             foo(bar(baz))
             """
    end)
  end

  test "does not cache inputs from .formatter.exs", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: Path.wildcard("{a,b}.ex"),
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

      File.write!("b.ex", """
      bar baz bat
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("b.ex") == """
             bar(baz(bat))
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
      [sigils: [:W]]
    end

    def format(contents, opts) do
      assert opts[:from_formatter_exs] == :yes
      assert opts[:sigil] == :W
      assert opts[:modifiers] == ~c"abc"
      assert opts[:line] == 2
      assert opts[:file] =~ ~r/\/a\.ex$/
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

      {formatter_function, _options} = Mix.Tasks.Format.formatter_for_file("a.ex")

      assert formatter_function.("""
             if true do
               ~W'''
               foo bar baz
               '''abc
             end
             """) == """
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

  defmodule Elixir.ExtensionWPlugin do
    @behaviour Mix.Tasks.Format

    def features(opts) do
      assert opts[:from_formatter_exs] == :yes
      [extensions: ~w(.w), sigils: [:W]]
    end

    def format(contents, opts) do
      assert opts[:from_formatter_exs] == :yes
      assert opts[:extension] == ".w"
      assert opts[:file] =~ ~r/\/a\.w$/
      assert [W: sigil_fun] = opts[:sigils]
      assert is_function(sigil_fun, 2)
      contents |> String.split(~r/\s/) |> Enum.join("\n")
    end
  end

  defmodule Elixir.NewlineToDotPlugin do
    @behaviour Mix.Tasks.Format

    def features(opts) do
      assert opts[:from_formatter_exs] == :yes
      [extensions: ~w(.w), sigils: [:W]]
    end

    def format(contents, opts) do
      assert opts[:from_formatter_exs] == :yes

      cond do
        opts[:extension] ->
          assert opts[:extension] == ".w"
          assert opts[:file] =~ ~r/\/a\.w$/
          assert [W: sigil_fun] = opts[:sigils]
          assert is_function(sigil_fun, 2)

        opts[:sigil] ->
          assert opts[:sigil] == :W
          assert opts[:inputs] == ["a.ex"]
          assert opts[:modifiers] == ~c"abc"

        true ->
          flunk("Plugin not loading in correctly.")
      end

      contents |> String.replace("\n", ".")
    end
  end

  test "uses extension plugins from .formatter.exs", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.w"],
        plugins: [ExtensionWPlugin],
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

  test "uses multiple plugins from .formatter.exs targeting the same file extension", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.w"],
        plugins: [ExtensionWPlugin, NewlineToDotPlugin],
        from_formatter_exs: :yes
      ]
      """)

      File.write!("a.w", """
      foo bar baz
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("a.w") == "foo.bar.baz."
    end)
  end

  test "uses multiple plugins from .formatter.exs with the same file extension in declared order",
       context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.w"],
        plugins: [NewlineToDotPlugin, ExtensionWPlugin],
        from_formatter_exs: :yes
      ]
      """)

      File.write!("a.w", """
      foo bar baz
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("a.w") == "foo\nbar\nbaz."
    end)
  end

  test "uses multiple plugins from .formatter.exs targeting the same sigil", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.ex"],
        plugins: [NewlineToDotPlugin, SigilWPlugin],
        from_formatter_exs: :yes
      ]
      """)

      File.write!("a.ex", """
      def sigil_test(assigns) do
        ~W"foo bar baz\n"abc
      end
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("a.ex") == """
             def sigil_test(assigns) do
               ~W"foo\nbar\nbaz."abc
             end
             """
    end)
  end

  test "uses multiple plugins from .formatter.exs with the same sigil in declared order",
       context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.ex"],
        plugins: [SigilWPlugin, NewlineToDotPlugin],
        from_formatter_exs: :yes
      ]
      """)

      File.write!("a.ex", """
      def sigil_test(assigns) do
        ~W"foo bar baz"abc
      end
      """)

      Mix.Tasks.Format.run([])

      assert File.read!("a.ex") == """
             def sigil_test(assigns) do
               ~W"foo.bar.baz"abc
             end
             """
    end)
  end

  test "uses extension plugins with --stdin-filename", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        inputs: ["a.w"],
        plugins: [ExtensionWPlugin],
        from_formatter_exs: :yes
      ]
      """)

      output =
        capture_io("foo bar baz", fn ->
          Mix.Tasks.Format.run(["--stdin-filename", Path.join(File.cwd!(), "a.w"), "-"])
        end)

      assert output ==
               String.trim("""
               foo
               bar
               baz
               """)
    end)
  end

  test "respects the --migrate flag", context do
    in_tmp(context.test, fn ->
      File.write!("a.ex", "unless foo, do: 'bar'\n")

      Mix.Tasks.Format.run(["a.ex"])
      assert File.read!("a.ex") == "unless foo, do: 'bar'\n"

      Mix.Tasks.Format.run(["a.ex", "--migrate"])
      assert File.read!("a.ex") == "if !foo, do: ~c\"bar\"\n"
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

  test "uses inputs and configuration from :root path", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [
        locals_without_parens: [foo: 1]
      ]
      """)

      File.mkdir_p!("lib")

      File.write!("lib/a.ex", """
      foo bar baz
      """)

      root = File.cwd!()

      {formatter_function, _options} =
        File.cd!("lib", fn ->
          Mix.Tasks.Format.formatter_for_file("lib/a.ex", root: root)
        end)

      assert formatter_function.("""
             foo bar baz
             """) == """
             foo bar(baz)
             """
    end)
  end

  test "reads exported configuration from subdirectories", context do
    in_tmp(context.test, fn ->
      File.write!(".formatter.exs", """
      [subdirectories: ["li", "lib"]]
      """)

      # We also create a directory called li to ensure files
      # from lib won't accidentally match on li.
      File.mkdir_p!("li")
      File.mkdir_p!("lib")

      File.write!("li/.formatter.exs", """
      [inputs: "**/*", locals_without_parens: [other_fun: 2]]
      """)

      File.write!("lib/.formatter.exs", """
      [inputs: "a.ex", locals_without_parens: [my_fun: 2]]
      """)

      # Should hit the formatter
      {formatter, formatter_opts} = Mix.Tasks.Format.formatter_for_file("lib/extra/a.ex")
      assert Keyword.get(formatter_opts, :locals_without_parens) == [my_fun: 2]
      assert formatter.("my_fun 1, 2") == "my_fun 1, 2\n"

      # Another directory should not hit the formatter
      {formatter, formatter_opts} = Mix.Tasks.Format.formatter_for_file("test/a.ex")
      assert Keyword.get(formatter_opts, :locals_without_parens) == []
      assert formatter.("my_fun 1, 2") == "my_fun(1, 2)\n"

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

  test "reads exported configuration from dependencies", context do
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

  test "reads exported configuration from dependencies and subdirectories", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(__MODULE__.FormatWithDepsApp)
      format_timestamp = "_build/dev/lib/format_with_deps/.mix/format_timestamp"

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
      File.touch!(format_timestamp, {{2010, 1, 1}, {0, 0, 0}})

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
      File.touch!(format_timestamp, {{2010, 1, 1}, {0, 0, 0}})

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

      message = "Expected :inputs or :subdirectories key in #{Path.expand("lib/.formatter.exs")}"
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
        ~r"Unknown dependency :my_dep given to :import_deps in the formatter configuration"

      assert_raise Mix.Error, message, fn -> Mix.Tasks.Format.run([]) end

      File.write!(".formatter.exs", """
      [import_deps: [:nonexistent_dep]]
      """)

      message =
        ~r"Unknown dependency :nonexistent_dep given to :import_deps in the formatter configuration"

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
        "Both .formatter.exs and #{Path.expand("lib/.formatter.exs")} specify the file " <>
          "#{Path.expand("lib/a.ex")} in their :inputs option. To resolve the conflict, " <>
          "the configuration in .formatter.exs will be ignored. Please change the list of " <>
          ":inputs in one of the formatter files so only one of them matches #{Path.expand("lib/a.ex")}"

      message2 =
        "Both #{Path.expand("lib/.formatter.exs")} and #{Path.expand("foo/.formatter.exs")} " <>
          "specify the file #{Path.expand("lib/a.ex")} in their :inputs option. To resolve " <>
          "the conflict, the configuration in #{Path.expand("lib/.formatter.exs")} " <>
          "will be ignored. Please change the list of :inputs in one of the formatter files " <>
          "so only one of them matches #{Path.expand("lib/a.ex")}"

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

      messages = [
        "invalid syntax found on a.ex:1:13:",
        "defmodule <%= module %>.Bar do end",
        "^",
        "syntax error before: '='"
      ]

      e =
        assert_raise SyntaxError, fn ->
          Mix.Tasks.Format.run(["a.ex"])
        end

      output = Exception.format(:error, e, [])

      for msg <- messages do
        assert output =~ msg
      end

      assert_received {:mix_shell, :error, ["mix format failed for file: a.ex"]}
    end)
  end

  test "raises SyntaxError when parsing invalid stdin", context do
    in_tmp(context.test, fn ->
      e =
        assert_raise SyntaxError, fn ->
          capture_io("defmodule <%= module %>.Bar do end", fn ->
            Mix.Tasks.Format.run(["-"])
          end)
        end

      messages = [
        "invalid syntax found on stdin.exs:1:13:",
        "defmodule <%= module %>.Bar do end",
        "^",
        "syntax error before: '='"
      ]

      output = Exception.format(:error, e, [])

      for msg <- messages do
        assert output =~ msg
      end

      assert_received {:mix_shell, :error, ["mix format failed for stdin"]}
    end)
  end

  describe "text_diff_format/3" do
    defp text_diff_format(old, new, opts \\ []) do
      Mix.Tasks.Format.text_diff_format(old, new, opts) |> IO.iodata_to_binary()
    end

    test "with unchanged texts" do
      assert text_diff_format("abc", "abc") == ""
    end

    test "with one deleted line" do
      old = "del"
      new = ""

      assert output = text_diff_format(old, new)

      if IO.ANSI.enabled?() do
        assert output == "1  \e[31m -\e[0m|\e[31mdel\e[0m\n  1\e[32m +\e[0m|\n"
      end

      assert text_diff_format(old, new, color: false) == """
             1   -|del
               1 +|
             """
    end

    test "with one changed line" do
      old = "one three two"
      new = "one two three"

      assert output = text_diff_format(old, new)

      if IO.ANSI.enabled?() do
        assert output ==
                 """
                 1  \e[31m -\e[0m|one three\e[31m\e[0m\e[41m \e[0m\e[31mtwo\e[0m
                   1\e[32m +\e[0m|one t\e[32mwo\e[0m\e[42m \e[0m\e[32mt\e[0mhree
                 """
      end

      assert text_diff_format(old, new, color: false) == """
             1   -|one three two
               1 +|one two three
             """
    end

    test "with one deleted line in the middle" do
      old = """
      aaa
      bbb
      ccc
      ddd
      eee
      fff
      ggg
      """

      new = """
      aaa
      bbb
      ccc
      eee
      fff
      ggg
      """

      exp = """
           |
      2 2  |bbb
      3 3  |ccc
      4   -|ddd
      5 4  |eee
      6 5  |fff
           |
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false) == exp
    end

    test "with multiple deleted lines" do
      old = """
      aaa
      bbb
      ccc
      ddd
      eee
      fff
      ggg\
      """

      new = """
      aaa
      ggg\
      """

      exp = """
      1 1  |aaa
      2   -|bbb
      3   -|ccc
      4   -|ddd
      5   -|eee
      6   -|fff
      7 2  |ggg
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false) == exp
    end

    test "with one added line in the middle" do
      old = """
      aaa
      bbb
      ccc
      eee
      fff
      ggg
      """

      new = """
      aaa
      bbb
      ccc
      ddd
      eee
      fff
      ggg
      """

      exp = """
           |
      2 2  |bbb
      3 3  |ccc
        4 +|ddd
      4 5  |eee
      5 6  |fff
           |
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false) == exp
    end

    test "with changed first line" do
      old = """
      aaa
      bbb
      ccc
      ddd
      """

      new = """
      axa
      bbb
      ccc
      ddd
      """

      exp = """
      1   -|aaa
        1 +|axa
      2 2  |bbb
      3 3  |ccc
           |
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false) == exp
    end

    test "with changed last line" do
      old = """
      aaa
      bbb
      ccc
      ddd
      """

      new = """
      aaa
      bbb
      ccc
      dxd
      """

      exp = """
           |
      2 2  |bbb
      3 3  |ccc
      4   -|ddd
        4 +|dxd
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false) == exp
    end

    test "with changed first and last line" do
      old = """
      aaa
      bbb
      ccc
      ddd
      eee
      """

      new = """
      axa
      bbb
      ccc
      ddd
      exe
      """

      exp = """
      1   -|aaa
        1 +|axa
      2 2  |bbb
           |
      4 4  |ddd
      5   -|eee
        5 +|exe
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false, before: 1, after: 1) == exp
    end

    test "with changed second and second last line" do
      old = """
      aaa
      bbb
      ccc
      ddd
      eee
      fff
      ggg
      hhh
      iii\
      """

      new = """
      aaa
      bXb
      ccc
      ddd
      eee
      fff
      ggg
      hXh
      iii\
      """

      exp = """
      1 1  |aaa
      2   -|bbb
        2 +|bXb
      3 3  |ccc
           |
      7 7  |ggg
      8   -|hhh
        8 +|hXh
      9 9  |iii
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false, before: 1, after: 1) == exp
    end

    test "colorized added tab" do
      assert output = text_diff_format("ab", "a\tb")

      if IO.ANSI.enabled?() do
        assert output =~ "\e[42m\t"
      end
    end

    test "colorized deleted tab" do
      assert output = text_diff_format("a\tb", "ab")

      if IO.ANSI.enabled?() do
        assert output =~ "\e[41m\t"
      end
    end

    test "shows added CR" do
      old = """
      aaa
      bbb
      """

      new = """
      aaa\r
      bbb
      """

      exp = """
      1   -|aaa
        1 +|aaa↵
      2 2  |bbb
           |
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false, before: 1, after: 1) == exp
    end

    test "shows multiple added CRs" do
      old = """
      aaa
      bbb
      """

      new = """
      aaa\r
      bbb\r
      ccc\r
      """

      exp = """
      1   -|aaa
      2   -|bbb
        1 +|aaa↵
        2 +|bbb↵
        3 +|ccc\r
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false, before: 1, after: 1) == exp
    end

    test "shows deleted CR" do
      old = """
      aaa\r
      bbb
      """

      new = """
      aaa
      bbb
      """

      exp = """
      1   -|aaa↵
        1 +|aaa
      2 2  |bbb
           |
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false, before: 1, after: 1) == exp
    end

    test "shows multiple deleted CRs" do
      old = """
      aaa\r
      bbb\r
      """

      new = """
      aaa
      bbb
      ccc
      """

      exp = """
      1   -|aaa↵
      2   -|bbb↵
        1 +|aaa
        2 +|bbb
        3 +|ccc
      """

      assert text_diff_format(old, new)
      assert text_diff_format(old, new, color: false) == exp
    end
  end
end
