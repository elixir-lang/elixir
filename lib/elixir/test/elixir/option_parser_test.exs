Code.require_file("test_helper.exs", __DIR__)

defmodule OptionParserTest do
  use ExUnit.Case, async: true

  doctest OptionParser

  test "parses --key value option" do
    assert OptionParser.parse(["--source", "form_docs/", "other"], switches: [source: :string]) ==
             {[source: "form_docs/"], ["other"], []}
  end

  test "parses --key=value option" do
    assert OptionParser.parse(["--source=form_docs/", "other"], switches: [source: :string]) ==
             {[source: "form_docs/"], ["other"], []}
  end

  test "parses overrides options by default" do
    assert OptionParser.parse(
             ["--require", "foo", "--require", "bar", "baz"],
             switches: [require: :string]
           ) == {[require: "bar"], ["baz"], []}
  end

  test "parses multi-word option" do
    config = [switches: [hello_world: :boolean]]
    assert OptionParser.next(["--hello-world"], config) == {:ok, :hello_world, true, []}
    assert OptionParser.next(["--no-hello-world"], config) == {:ok, :hello_world, false, []}

    assert OptionParser.next(["--no-hello-world"], strict: []) ==
             {:undefined, "--no-hello-world", nil, []}

    assert OptionParser.next(["--no-hello_world"], strict: []) ==
             {:undefined, "--no-hello_world", nil, []}

    config = [strict: [hello_world: :boolean]]
    assert OptionParser.next(["--hello-world"], config) == {:ok, :hello_world, true, []}
    assert OptionParser.next(["--no-hello-world"], config) == {:ok, :hello_world, false, []}
    assert OptionParser.next(["--hello_world"], config) == {:undefined, "--hello_world", nil, []}

    assert OptionParser.next(["--no-hello_world"], config) ==
             {:undefined, "--no-hello_world", nil, []}
  end

  test "parses more than one key-value pair options using switches" do
    opts = [switches: [source: :string, docs: :string]]

    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"], opts) ==
             {[source: "from_docs/", docs: "show"], [], []}

    assert OptionParser.parse(["--source", "from_docs/", "--doc", "show"], opts) ==
             {[source: "from_docs/", doc: "show"], [], []}

    assert OptionParser.parse(["--source", "from_docs/", "--doc=show"], opts) ==
             {[source: "from_docs/", doc: "show"], [], []}

    assert OptionParser.parse(["--no-bool"], strict: []) == {[], [], [{"--no-bool", nil}]}
  end

  test "parses more than one key-value pair options using strict" do
    opts = [strict: [source: :string, docs: :string]]

    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"], opts) ==
             {[source: "from_docs/", docs: "show"], [], []}

    assert OptionParser.parse(["--source", "from_docs/", "--doc", "show"], opts) ==
             {[source: "from_docs/"], ["show"], [{"--doc", nil}]}

    assert OptionParser.parse(["--source", "from_docs/", "--doc=show"], opts) ==
             {[source: "from_docs/"], [], [{"--doc", nil}]}

    assert OptionParser.parse(["--no-bool"], strict: []) == {[], [], [{"--no-bool", nil}]}
  end

  test "collects multiple invalid options" do
    argv = ["--bad", "opt", "foo", "-o", "bad", "bar"]

    assert OptionParser.parse(argv, switches: [bad: :integer]) ==
             {[], ["foo", "bar"], [{"--bad", "opt"}]}
  end

  test "parse/2 raises when using both options: switches and strict" do
    assert_raise ArgumentError, ":switches and :strict cannot be given together", fn ->
      OptionParser.parse(["--elixir"], switches: [ex: :string], strict: [elixir: :string])
    end
  end

  test "parse!/2 raises an exception for an unknown option using strict" do
    assert_raise OptionParser.ParseError, "1 error found!\n--doc : Unknown option", fn ->
      argv = ["--source", "from_docs/", "--doc", "show"]
      OptionParser.parse!(argv, strict: [source: :string, docs: :string])
    end
  end

  test "parse!/2 raises an exception when an option is of the wrong type" do
    assert_raise OptionParser.ParseError, fn ->
      argv = ["--bad", "opt", "foo", "-o", "bad", "bar"]
      OptionParser.parse!(argv, switches: [bad: :integer])
    end
  end

  test "parse_head!/2 raises an exception when an option is of the wrong type" do
    message = "1 error found!\n--number : Expected type integer, got \"lib\""

    assert_raise OptionParser.ParseError, message, fn ->
      argv = ["--number", "lib", "test/enum_test.exs"]
      OptionParser.parse_head!(argv, strict: [number: :integer])
    end
  end

  describe "arguments" do
    test "parses until --" do
      assert OptionParser.parse(
               ["--source", "foo", "--", "1", "2", "3"],
               switches: [source: :string]
             ) == {[source: "foo"], ["1", "2", "3"], []}

      assert OptionParser.parse_head(
               ["--source", "foo", "--", "1", "2", "3"],
               switches: [source: :string]
             ) == {[source: "foo"], ["1", "2", "3"], []}

      assert OptionParser.parse(
               ["--source", "foo", "bar", "--", "-x"],
               switches: [source: :string]
             ) == {[source: "foo"], ["bar", "-x"], []}

      assert OptionParser.parse_head(
               ["--source", "foo", "bar", "--", "-x"],
               switches: [source: :string]
             ) == {[source: "foo"], ["bar", "--", "-x"], []}
    end

    test "parses - as argument" do
      argv = ["--foo", "-", "-b", "-"]
      opts = [strict: [foo: :boolean, boo: :string], aliases: [b: :boo]]
      assert OptionParser.parse(argv, opts) == {[foo: true, boo: "-"], ["-"], []}
    end

    test "parses until first non-option arguments" do
      argv = ["--source", "from_docs/", "test/enum_test.exs", "--verbose"]

      assert OptionParser.parse_head(argv, switches: [source: :string]) ==
               {[source: "from_docs/"], ["test/enum_test.exs", "--verbose"], []}
    end
  end

  describe "aliases" do
    test "supports boolean aliases" do
      assert OptionParser.parse(["-d"], aliases: [d: :docs], switches: [docs: :boolean]) ==
               {[docs: true], [], []}
    end

    test "supports non-boolean aliases" do
      assert OptionParser.parse(
               ["-s", "from_docs/"],
               aliases: [s: :source],
               switches: [source: :string]
             ) == {[source: "from_docs/"], [], []}
    end

    test "supports --key=value aliases" do
      assert OptionParser.parse(
               ["-s=from_docs/", "other"],
               aliases: [s: :source],
               switches: [source: :string]
             ) == {[source: "from_docs/"], ["other"], []}
    end

    test "parses -ab as -a -b" do
      opts = [aliases: [a: :first, b: :second], switches: [second: :integer]]
      assert OptionParser.parse(["-ab=1"], opts) == {[first: true, second: 1], [], []}
      assert OptionParser.parse(["-ab", "1"], opts) == {[first: true, second: 1], [], []}

      opts = [aliases: [a: :first, b: :second], switches: [first: :boolean, second: :boolean]]
      assert OptionParser.parse(["-ab"], opts) == {[first: true, second: true], [], []}
      assert OptionParser.parse(["-ab3"], opts) == {[first: true], [], [{"-b", "3"}]}
      assert OptionParser.parse(["-ab=bar"], opts) == {[first: true], [], [{"-b", "bar"}]}
      assert OptionParser.parse(["-ab3=bar"], opts) == {[first: true], [], [{"-b", "3=bar"}]}
      assert OptionParser.parse(["-3ab"], opts) == {[], ["-3ab"], []}
    end
  end

  describe "types" do
    test "parses configured booleans" do
      assert OptionParser.parse(["--docs=false"], switches: [docs: :boolean]) ==
               {[docs: false], [], []}

      assert OptionParser.parse(["--docs=true"], switches: [docs: :boolean]) ==
               {[docs: true], [], []}

      assert OptionParser.parse(["--docs=other"], switches: [docs: :boolean]) ==
               {[], [], [{"--docs", "other"}]}

      assert OptionParser.parse(["--docs="], switches: [docs: :boolean]) ==
               {[], [], [{"--docs", ""}]}

      assert OptionParser.parse(["--docs", "foo"], switches: [docs: :boolean]) ==
               {[docs: true], ["foo"], []}

      assert OptionParser.parse(["--no-docs", "foo"], switches: [docs: :boolean]) ==
               {[docs: false], ["foo"], []}

      assert OptionParser.parse(["--no-docs=foo", "bar"], switches: [docs: :boolean]) ==
               {[], ["bar"], [{"--no-docs", "foo"}]}

      assert OptionParser.parse(["--no-docs=", "bar"], switches: [docs: :boolean]) ==
               {[], ["bar"], [{"--no-docs", ""}]}
    end

    test "does not set unparsed booleans" do
      assert OptionParser.parse(["foo"], switches: [docs: :boolean]) == {[], ["foo"], []}
    end

    test "keeps options on configured keep" do
      argv = ["--require", "foo", "--require", "bar", "baz"]

      assert OptionParser.parse(argv, switches: [require: :keep]) ==
               {[require: "foo", require: "bar"], ["baz"], []}

      assert OptionParser.parse(["--require"], switches: [require: :keep]) ==
               {[], [], [{"--require", nil}]}
    end

    test "parses configured strings" do
      assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :string]) ==
               {[value: "1"], ["foo"], []}

      assert OptionParser.parse(["--value=1", "foo"], switches: [value: :string]) ==
               {[value: "1"], ["foo"], []}

      assert OptionParser.parse(["--value"], switches: [value: :string]) ==
               {[], [], [{"--value", nil}]}

      assert OptionParser.parse(["--no-value"], switches: [value: :string]) ==
               {[no_value: true], [], []}
    end

    test "parses configured counters" do
      assert OptionParser.parse(["--verbose"], switches: [verbose: :count]) ==
               {[verbose: 1], [], []}

      assert OptionParser.parse(["--verbose", "--verbose"], switches: [verbose: :count]) ==
               {[verbose: 2], [], []}

      argv = ["--verbose", "-v", "-v", "--", "bar"]
      opts = [aliases: [v: :verbose], strict: [verbose: :count]]
      assert OptionParser.parse(argv, opts) == {[verbose: 3], ["bar"], []}
    end

    test "parses configured integers" do
      assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :integer]) ==
               {[value: 1], ["foo"], []}

      assert OptionParser.parse(["--value=1", "foo"], switches: [value: :integer]) ==
               {[value: 1], ["foo"], []}

      assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :integer]) ==
               {[], ["foo"], [{"--value", "WAT"}]}
    end

    test "parses configured integers with keep" do
      argv = ["--value", "1", "--value", "2", "foo"]

      assert OptionParser.parse(argv, switches: [value: [:integer, :keep]]) ==
               {[value: 1, value: 2], ["foo"], []}

      argv = ["--value=1", "foo", "--value=2", "bar"]

      assert OptionParser.parse(argv, switches: [value: [:integer, :keep]]) ==
               {[value: 1, value: 2], ["foo", "bar"], []}
    end

    test "parses configured floats" do
      assert OptionParser.parse(["--value", "1.0", "foo"], switches: [value: :float]) ==
               {[value: 1.0], ["foo"], []}

      assert OptionParser.parse(["--value=1.0", "foo"], switches: [value: :float]) ==
               {[value: 1.0], ["foo"], []}

      assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :float]) ==
               {[], ["foo"], [{"--value", "WAT"}]}
    end

    test "correctly handles negative integers" do
      opts = [switches: [option: :integer], aliases: [o: :option]]
      assert OptionParser.parse(["arg1", "-o43"], opts) == {[option: 43], ["arg1"], []}
      assert OptionParser.parse(["arg1", "-o", "-43"], opts) == {[option: -43], ["arg1"], []}
      assert OptionParser.parse(["arg1", "--option=-43"], opts) == {[option: -43], ["arg1"], []}

      assert OptionParser.parse(["arg1", "--option", "-43"], opts) ==
               {[option: -43], ["arg1"], []}
    end

    test "correctly handles negative floating-point numbers" do
      opts = [switches: [option: :float], aliases: [o: :option]]
      assert OptionParser.parse(["arg1", "-o43.2"], opts) == {[option: 43.2], ["arg1"], []}
      assert OptionParser.parse(["arg1", "-o", "-43.2"], opts) == {[option: -43.2], ["arg1"], []}

      assert OptionParser.parse(["arg1", "--option=-43.2"], switches: [option: :float]) ==
               {[option: -43.2], ["arg1"], []}

      assert OptionParser.parse(["arg1", "--option", "-43.2"], opts) ==
               {[option: -43.2], ["arg1"], []}
    end
  end

  describe "next" do
    test "with strict good options" do
      config = [strict: [str: :string, int: :integer, bool: :boolean]]
      assert OptionParser.next(["--str", "hello", "..."], config) == {:ok, :str, "hello", ["..."]}
      assert OptionParser.next(["--int=13", "..."], config) == {:ok, :int, 13, ["..."]}
      assert OptionParser.next(["--bool=false", "..."], config) == {:ok, :bool, false, ["..."]}
      assert OptionParser.next(["--no-bool", "..."], config) == {:ok, :bool, false, ["..."]}
      assert OptionParser.next(["--bool", "..."], config) == {:ok, :bool, true, ["..."]}
      assert OptionParser.next(["..."], config) == {:error, ["..."]}
    end

    test "with strict unknown options" do
      config = [strict: [bool: :boolean]]

      assert OptionParser.next(["--str", "13", "..."], config) ==
               {:undefined, "--str", nil, ["13", "..."]}

      assert OptionParser.next(["--int=hello", "..."], config) ==
               {:undefined, "--int", "hello", ["..."]}

      assert OptionParser.next(["-no-bool=other", "..."], config) ==
               {:undefined, "-no-bool", "other", ["..."]}
    end

    test "with strict bad type" do
      config = [strict: [str: :string, int: :integer, bool: :boolean]]
      assert OptionParser.next(["--str", "13", "..."], config) == {:ok, :str, "13", ["..."]}

      assert OptionParser.next(["--int=hello", "..."], config) ==
               {:invalid, "--int", "hello", ["..."]}

      assert OptionParser.next(["--int", "hello", "..."], config) ==
               {:invalid, "--int", "hello", ["..."]}

      assert OptionParser.next(["--bool=other", "..."], config) ==
               {:invalid, "--bool", "other", ["..."]}
    end

    test "with strict missing value" do
      config = [strict: [str: :string, int: :integer, bool: :boolean]]
      assert OptionParser.next(["--str"], config) == {:invalid, "--str", nil, []}
      assert OptionParser.next(["--int"], config) == {:invalid, "--int", nil, []}
      assert OptionParser.next(["--bool=", "..."], config) == {:invalid, "--bool", "", ["..."]}

      assert OptionParser.next(["--no-bool=", "..."], config) ==
               {:invalid, "--no-bool", "", ["..."]}
    end
  end

  test "split" do
    assert OptionParser.split(~S[]) == []
    assert OptionParser.split(~S[foo]) == ["foo"]
    assert OptionParser.split(~S[foo bar]) == ["foo", "bar"]
    assert OptionParser.split(~S[  foo  bar  ]) == ["foo", "bar"]
    assert OptionParser.split(~S[foo\ bar]) == ["foo bar"]
    assert OptionParser.split(~S[foo" bar"]) == ["foo bar"]
    assert OptionParser.split(~S[foo\" bar\"]) == ["foo\"", "bar\""]
    assert OptionParser.split(~S[foo "\ bar\""]) == ["foo", "\\ bar\""]
    assert OptionParser.split(~S[foo '\"bar"\'\ ']) == ["foo", "\\\"bar\"'\\ "]
  end

  describe "to_argv" do
    test "converts options back to switches" do
      assert OptionParser.to_argv(foo_bar: "baz") == ["--foo-bar", "baz"]

      assert OptionParser.to_argv(bool: true, bool: false, discarded: nil) ==
               ["--bool", "--no-bool"]
    end

    test "handles :count switch type" do
      original = ["--counter", "--counter"]
      {opts, [], []} = OptionParser.parse(original, switches: [counter: :count])
      assert original == OptionParser.to_argv(opts, switches: [counter: :count])
    end
  end
end

defmodule OptionsParserDeprecationsTest do
  use ExUnit.Case, async: true

  @warning ~r[not passing the :switches or :strict option to OptionParser is deprecated]

  def assert_deprecated(fun) do
    assert ExUnit.CaptureIO.capture_io(:stderr, fun) =~ @warning
  end

  test "parses boolean option" do
    assert_deprecated(fn ->
      assert OptionParser.parse(["--docs"]) == {[docs: true], [], []}
    end)
  end

  test "parses more than one boolean option" do
    assert_deprecated(fn ->
      assert OptionParser.parse(["--docs", "--compile"]) == {[docs: true, compile: true], [], []}
    end)
  end

  test "parses more than one boolean options as the alias" do
    assert_deprecated(fn ->
      assert OptionParser.parse(["-d", "--compile"], aliases: [d: :docs]) ==
               {[docs: true, compile: true], [], []}
    end)
  end

  test "parses --key value option" do
    assert_deprecated(fn ->
      assert OptionParser.parse(["--source", "form_docs/"]) == {[source: "form_docs/"], [], []}
    end)
  end

  test "does not interpret undefined options with value as boolean" do
    assert_deprecated(fn ->
      assert OptionParser.parse(["--no-bool"]) == {[no_bool: true], [], []}
    end)

    assert_deprecated(fn ->
      assert OptionParser.parse(["--no-bool=...", "other"]) == {[no_bool: "..."], ["other"], []}
    end)
  end

  test "parses -ab as -a -b" do
    assert_deprecated(fn ->
      assert OptionParser.parse(["-ab"], aliases: [a: :first, b: :second]) ==
               {[first: true, second: true], [], []}
    end)
  end

  test "parses mixed options" do
    argv = ["--source", "from_docs/", "--compile", "-x"]

    assert_deprecated(fn ->
      assert OptionParser.parse(argv, aliases: [x: :x]) ==
               {[source: "from_docs/", compile: true, x: true], [], []}
    end)
  end

  test "parses more than one key-value pair options" do
    assert_deprecated(fn ->
      assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"]) ==
               {[source: "from_docs/", docs: "show"], [], []}
    end)
  end

  test "multi-word option" do
    assert_deprecated(fn ->
      assert OptionParser.next(["--hello-world"], []) == {:ok, :hello_world, true, []}
    end)

    assert_deprecated(fn ->
      assert OptionParser.next(["--no-hello-world"], []) == {:ok, :no_hello_world, true, []}
    end)

    assert_deprecated(fn ->
      assert OptionParser.next(["--hello_world"], []) == {:undefined, "--hello_world", nil, []}
    end)

    assert_deprecated(fn ->
      assert OptionParser.next(["--no-hello_world"], []) ==
               {:undefined, "--no-hello_world", nil, []}
    end)
  end
end
