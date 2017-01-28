Code.require_file "test_helper.exs", __DIR__

defmodule OptionParserTest do
  use ExUnit.Case, async: true

  doctest OptionParser

  test "parses boolean option" do
    assert OptionParser.parse(["--docs"]) == {[docs: true], [], []}
  end

  test "parses alias boolean option as the alias key" do
    assert OptionParser.parse(["-d"], aliases: [d: :docs])
           == {[docs: true], [], []}
  end

  test "parses more than one boolean option" do
    assert OptionParser.parse(["--docs", "--compile"])
           == {[docs: true, compile: true], [], []}
  end

  test "parses more than one boolean options as the alias" do
    assert OptionParser.parse(["-d", "--compile"], aliases: [d: :docs])
           == {[docs: true, compile: true], [], []}
  end

  test "parses --key value option" do
    assert OptionParser.parse(["--source", "form_docs/"])
           == {[source: "form_docs/"], [], []}
  end

  test "parses only to existing atoms" do
    assert OptionParser.parse(["--option-key-does-not-exist"]) ==
           {[], [], [{"--option-key-does-not-exist", nil}]}
  end

  test "parses --key=value option" do
    assert OptionParser.parse(["--source=form_docs/", "other"])
           == {[source: "form_docs/"], ["other"], []}
  end

  test "parses alias --key value option as the alias" do
    assert OptionParser.parse(["-s", "from_docs/"], aliases: [s: :source])
           == {[source: "from_docs/"], [], []}
  end

  test "parses alias --key=value option as the alias" do
    assert OptionParser.parse(["-s=from_docs/", "other"], aliases: [s: :source])
           == {[source: "from_docs/"], ["other"], []}
  end

  test "does not interpret undefined options with value as boolean" do
    assert OptionParser.parse(["--no-bool"])
           == {[no_bool: true], [], []}
    assert OptionParser.parse(["--no-bool"], strict: [])
           == {[], [], [{"--no-bool", nil}]}
    assert OptionParser.parse(["--no-bool=...", "other"])
           == {[no_bool: "..."], ["other"], []}
  end

  test "does not parse -- as an alias" do
    assert OptionParser.parse(["--s=from_docs/"], aliases: [s: :source])
           == {[s: "from_docs/"], [], []}
  end

  test "parses -ab as -a -b" do
    aliases = [a: :first, b: :second]

    assert OptionParser.parse(["-ab"], aliases: aliases)
           == {[first: true, second: true], [], []}

    assert OptionParser.parse(["-ab=1"], aliases: aliases, switches: [second: :integer])
           == {[first: true, second: 1], [], []}

    assert OptionParser.parse(["-ab", "1"], aliases: aliases, switches: [second: :integer])
           == {[first: true, second: 1], [], []}
  end

  test "parses configured booleans" do
    assert OptionParser.parse(["--docs=false"], switches: [docs: :boolean])
           == {[docs: false], [], []}
    assert OptionParser.parse(["--docs=true"], switches: [docs: :boolean])
           == {[docs: true], [], []}
    assert OptionParser.parse(["--docs=other"], switches: [docs: :boolean])
           == {[], [], [{"--docs", "other"}]}
    assert OptionParser.parse(["--docs="], switches: [docs: :boolean])
           == {[], [], [{"--docs", ""}]}

    assert OptionParser.parse(["--docs", "foo"], switches: [docs: :boolean])
           == {[docs: true], ["foo"], []}
    assert OptionParser.parse(["--no-docs", "foo"], switches: [docs: :boolean])
           == {[docs: false], ["foo"], []}
    assert OptionParser.parse(["--no-docs=foo", "bar"], switches: [docs: :boolean])
           == {[], ["bar"], [{"--no-docs", "foo"}]}
    assert OptionParser.parse(["--no-docs=", "bar"], switches: [docs: :boolean])
           == {[], ["bar"], [{"--no-docs", ""}]}
  end

  test "does not set unparsed booleans" do
    assert OptionParser.parse(["foo"], switches: [docs: :boolean])
           == {[], ["foo"], []}
  end

  test "keeps options on configured keep" do
    argv = ["--require", "foo", "--require", "bar", "baz"]
    assert OptionParser.parse(argv, switches: [require: :keep])
           == {[require: "foo", require: "bar"], ["baz"], []}

    assert OptionParser.parse(["--require"], switches: [require: :keep])
           == {[], [], [{"--require", nil}]}
  end

  test "parses configured strings" do
    assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :string])
           == {[value: "1"], ["foo"], []}
    assert OptionParser.parse(["--value=1", "foo"], switches: [value: :string])
           == {[value: "1"], ["foo"], []}
    assert OptionParser.parse(["--value"], switches: [value: :string])
           == {[], [], [{"--value", nil}]}
    assert OptionParser.parse(["--no-value"], switches: [value: :string])
           == {[no_value: true], [], []}
  end

  test "parses configured counters" do
    assert OptionParser.parse(["--verbose"], switches: [verbose: :count])
           == {[verbose: 1], [], []}
    assert OptionParser.parse(["--verbose", "--verbose"], switches: [verbose: :count])
           == {[verbose: 2], [], []}
    assert OptionParser.parse(["--verbose", "-v", "-v", "--", "bar"],
                              aliases: [v: :verbose], strict: [verbose: :count])
           == {[verbose: 3], ["bar"], []}
  end

  test "parses configured integers" do
    assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :integer])
           == {[value: 1], ["foo"], []}
    assert OptionParser.parse(["--value=1", "foo"], switches: [value: :integer])
           == {[value: 1], ["foo"], []}
    assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :integer])
           == {[], ["foo"], [{"--value", "WAT"}]}
  end

  test "parses configured integers with keep" do
    argv = ["--value", "1", "--value", "2", "foo"]
    assert OptionParser.parse(argv, switches: [value: [:integer, :keep]])
           == {[value: 1, value: 2], ["foo"], []}

    argv = ["--value=1", "foo", "--value=2", "bar"]
    assert OptionParser.parse(argv, switches: [value: [:integer, :keep]])
           == {[value: 1, value: 2], ["foo", "bar"], []}
  end

  test "parses configured floats" do
    assert OptionParser.parse(["--value", "1.0", "foo"], switches: [value: :float])
           == {[value: 1.0], ["foo"], []}
    assert OptionParser.parse(["--value=1.0", "foo"], switches: [value: :float])
           == {[value: 1.0], ["foo"], []}
    assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :float])
           == {[], ["foo"], [{"--value", "WAT"}]}
  end

  test "overrides options by default" do
    assert OptionParser.parse(["--require", "foo", "--require", "bar", "baz"])
           == {[require: "bar"], ["baz"], []}
  end

  test "parses mixed options" do
    argv = ["--source", "from_docs/", "--compile", "-x"]
    assert OptionParser.parse(argv, aliases: [x: :x])
           == {[source: "from_docs/", compile: true, x: true], [], []}
  end

  test "stops on first non-option arguments" do
    argv = ["--source", "from_docs/", "test/enum_test.exs", "--verbose"]
    assert OptionParser.parse_head(argv)
           == {[source: "from_docs/"], ["test/enum_test.exs", "--verbose"], []}
  end

  test "stops on --" do
    options = OptionParser.parse(["--source", "foo", "--", "1", "2", "3"])
    assert options == {[source: "foo"], ["1", "2", "3"], []}

    options = OptionParser.parse_head(["--source", "foo", "--", "1", "2", "3"])
    assert options == {[source: "foo"], ["1", "2", "3"], []}

    options = OptionParser.parse(["--source", "foo", "bar", "--", "-x"])
    assert options == {[source: "foo"], ["bar", "-x"], []}

    options = OptionParser.parse_head(["--source", "foo", "bar", "--", "-x"])
    assert options == {[source: "foo"], ["bar", "--", "-x"], []}
  end

  test "goes beyond the first non-option arguments" do
    argv = ["--source", "from_docs/", "test/enum_test.exs", "--verbose"]
    assert OptionParser.parse(argv)
           == {[source: "from_docs/", verbose: true], ["test/enum_test.exs"], []}
  end

  test "parses more than one key/value options" do
    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"])
           == {[source: "from_docs/", docs: "show"], [], []}
  end

  test "collects multiple invalid options" do
    argv = ["--bad", "opt", "foo", "-o", "bad", "bar"]
    assert OptionParser.parse(argv, switches: [bad: :integer])
           == {[], ["foo", "bar"], [{"--bad", "opt"}, {"-o", "bad"}]}
  end

  test "parses more than one key/value options using strict" do
    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"],
                              strict: [source: :string, docs: :string])
           == {[source: "from_docs/", docs: "show"], [], []}

    assert OptionParser.parse(["--source", "from_docs/", "--doc", "show"],
                              strict: [source: :string, docs: :string])
           == {[source: "from_docs/"], ["show"], [{"--doc", nil}]}

    assert OptionParser.parse(["--source", "from_docs/", "--doc=show"],
                              strict: [source: :string, docs: :string])
           == {[source: "from_docs/"], [], [{"--doc", nil}]}
  end

  test "parse!/2 raise an exception for an unknown option using strict" do
    assert_raise OptionParser.ParseError, "1 error found!\n--doc : Unknown option", fn ->
      argv = ["--source", "from_docs/", "--doc", "show"]
      OptionParser.parse!(argv, strict: [source: :string, docs: :string])
    end
  end

  test "parse!/2 raise an exception when an option is of the wrong type" do
    assert_raise OptionParser.ParseError, fn ->
      argv = ["--bad", "opt", "foo", "-o", "bad", "bar"]
      OptionParser.parse!(argv, switches: [bad: :integer])
    end
  end

  test "parse_head!/2 raise an exception when an option is of the wrong type" do
    assert_raise OptionParser.ParseError, "1 error found!\n--number : Expected type integer, got \"lib\"", fn ->
      argv = ["--number", "lib", "test/enum_test.exs"]
      OptionParser.parse_head!(argv, strict: [number: :integer])
    end
  end

  test ":switches with :strict raises" do
    assert_raise ArgumentError, ":switches and :strict cannot be given together", fn ->
      OptionParser.parse([], strict: [], switches: [])
    end
  end

  test "parses - as argument" do
    assert OptionParser.parse(["-a", "-", "-", "-b", "-"], aliases: [b: :boo])
           == {[boo: "-"], ["-"], [{"-a", "-"}]}

    assert OptionParser.parse(["--foo", "-", "-b", "-"], strict: [foo: :boolean, boo: :string], aliases: [b: :boo])
           == {[foo: true, boo: "-"], ["-"], []}
  end

  test "allow nonexistent atoms" do
    opts = [switches: [a: :string], allow_nonexistent_atoms: true]
    assert OptionParser.parse(["--a", "b", "--c", "d", "--ee", "f"], opts)
           == {[a: "b", c: "d", ee: "f"], [], []}
  end

  test "correctly handles negative integers" do
    assert OptionParser.parse(["arg1", "-43"])
      == {[], ["arg1", "-43"], []}

    assert OptionParser.parse(["arg1", "-o", "-43"], switches: [option: :integer], aliases: [o: :option])
      == {[option: -43], ["arg1"], []}

    assert OptionParser.parse(["arg1", "--option=-43"], switches: [option: :integer])
      == {[option: -43], ["arg1"], []}
  end

  test "correctly handles negative floating point numbers" do
    assert OptionParser.parse(["arg1", "-43.2"])
      == {[], ["arg1", "-43.2"], []}

    assert OptionParser.parse(["arg1", "-o", "-43.2"], switches: [option: :float], aliases: [o: :option])
      == {[option: -43.2], ["arg1"], []}

    assert OptionParser.parse(["arg1", "--option=-43.2"], switches: [option: :float])
      == {[option: -43.2], ["arg1"], []}
  end

  test "multi-word option" do
    config = [switches: [hello_world: :boolean]]
    assert OptionParser.next(["--hello-world"], config)
           == {:ok, :hello_world, true, []}
    assert OptionParser.next(["--no-hello-world"], config)
           == {:ok, :hello_world, false, []}

    assert OptionParser.next(["--hello-world"], [])
           == {:ok, :hello_world, true, []}
    assert OptionParser.next(["--no-hello-world"], [])
           == {:ok, :no_hello_world, true, []}
    assert OptionParser.next(["--hello_world"], [])
           == {:invalid, "--hello_world", nil, []}
    assert OptionParser.next(["--no-hello_world"], [])
           == {:invalid, "--no-hello_world", nil, []}

    assert OptionParser.next(["--no-hello-world"], strict: [])
           == {:undefined, "--no-hello-world", nil, []}
    assert OptionParser.next(["--no-hello_world"], strict: [])
           == {:undefined, "--no-hello_world", nil, []}

    config = [strict: [hello_world: :boolean]]
    assert OptionParser.next(["--hello-world"], config)
           == {:ok, :hello_world, true, []}
    assert OptionParser.next(["--no-hello-world"], config)
           == {:ok, :hello_world, false, []}
    assert OptionParser.next(["--hello_world"], config)
           == {:undefined, "--hello_world", nil, []}
    assert OptionParser.next(["--no-hello_world"], config)
           == {:undefined, "--no-hello_world", nil, []}
  end

  test "next strict: good options" do
    config = [strict: [str: :string, int: :integer, bool: :boolean]]
    assert OptionParser.next(["--str", "hello", "..."], config)
           == {:ok, :str, "hello", ["..."]}
    assert OptionParser.next(["--int=13", "..."], config)
           == {:ok, :int, 13, ["..."]}
    assert OptionParser.next(["--bool=false", "..."], config)
           == {:ok, :bool, false, ["..."]}
    assert OptionParser.next(["--no-bool", "..."], config)
           == {:ok, :bool, false, ["..."]}
    assert OptionParser.next(["--bool", "..."], config)
           == {:ok, :bool, true, ["..."]}
    assert OptionParser.next(["..."], config)
           == {:error, ["..."]}
  end

  test "next strict: unknown options" do
    config = [strict: [bool: :boolean]]
    assert OptionParser.next(["--str", "13", "..."], config)
           == {:undefined, "--str", nil, ["13", "..."]}
    assert OptionParser.next(["--int=hello", "..."], config)
           == {:undefined, "--int", "hello", ["..."]}
    assert OptionParser.next(["-no-bool=other", "..."], config)
           == {:undefined, "-no-bool", "other", ["..."]}
  end

  test "next strict: bad type" do
    config = [strict: [str: :string, int: :integer, bool: :boolean]]
    assert OptionParser.next(["--str", "13", "..."], config)
           == {:ok, :str, "13", ["..."]}
    assert OptionParser.next(["--int=hello", "..."], config)
           == {:invalid, "--int", "hello", ["..."]}
    assert OptionParser.next(["--int", "hello", "..."], config)
           == {:invalid, "--int", "hello", ["..."]}
    assert OptionParser.next(["--bool=other", "..."], config)
           == {:invalid, "--bool", "other", ["..."]}
  end

  test "next strict: missing value" do
    config = [strict: [str: :string, int: :integer, bool: :boolean]]
    assert OptionParser.next(["--str"], config)
           == {:invalid, "--str", nil, []}
    assert OptionParser.next(["--int"], config)
           == {:invalid, "--int", nil, []}
    assert OptionParser.next(["--bool=", "..."], config)
           == {:invalid, "--bool", "", ["..."]}
    assert OptionParser.next(["--no-bool=", "..."], config)
           == {:invalid, "--no-bool", "", ["..."]}
  end

  test "split" do
    assert OptionParser.split(~S[])
           == []

    assert OptionParser.split(~S[foo])
           == ["foo"]

    assert OptionParser.split(~S[foo bar])
           == ["foo", "bar"]

    assert OptionParser.split(~S[  foo  bar  ])
           == ["foo", "bar"]

    assert OptionParser.split(~S[foo\ bar])
           == ["foo bar"]

    assert OptionParser.split(~S[foo" bar"])
           == ["foo bar"]

    assert OptionParser.split(~S[foo\" bar\"])
           == ["foo\"", "bar\""]

    assert OptionParser.split(~S[foo "\ bar\""])
           == ["foo", "\\ bar\""]

    assert OptionParser.split(~S[foo '\"bar"\'\ '])
           == ["foo", "\\\"bar\"'\\ "]
  end

  test "to_argv" do
    assert OptionParser.to_argv([foo_bar: "baz"]) ==
           ["--foo-bar", "baz"]

    assert OptionParser.to_argv([bool: true, bool: false, discarded: nil]) ==
           ["--bool", "--no-bool"]
  end

  test ":count switch type can be translated back" do
    original = ["--counter", "--counter"]
    {opts, [], []} = OptionParser.parse(original,  [switches: [counter: :count]])
    assert original == OptionParser.to_argv(opts, [switches: [counter: :count]])
  end
end
