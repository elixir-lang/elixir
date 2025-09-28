# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.InteractionTest do
  use IEx.Case

  test "whole output" do
    assert capture_io("IO.puts \"Hello world\"", fn ->
             IEx.Server.run(dot_iex: "")
           end) =~
             "Interactive Elixir (#{System.version()}) - press Ctrl+C to exit (type h() ENTER for help)" <>
               "\niex(1)> Hello world\n:ok\niex(2)>"
  end

  test "empty input" do
    assert capture_iex("\n") == "nil"
  end

  test "normal input" do
    assert capture_iex("1 + 2") == "3"
  end

  test "omits error color if exception has ansi reset character" do
    enabled? = IO.ANSI.enabled?()
    Application.put_env(:elixir, :ansi_enabled, true)

    expected = """
    ** (SyntaxError) invalid syntax found on iex:1:4:
        \e[31merror:\e[0m syntax error before: '='
        │
      1 │ a += 2
        │ \e[31m   ^\e[0m
        │
        └─ iex:1:4
    """

    opts = [colors: [enabled: true]]
    output = capture_iex("a += 2", opts)

    refute String.starts_with?(output, IO.ANSI.red())
    assert output =~ expected

    Application.put_env(:elixir, :ansi_enabled, enabled?)
  end

  test "invalid input" do
    output = capture_iex("if true do ) false end")

    assert output =~ "** (MismatchedDelimiterError) mismatched delimiter found on iex:1:12:"
    assert output =~ "unexpected token: )"
    assert output =~ "iex:1:12"
    assert output =~ "if true do ) false end"
    assert output =~ "└ unclosed delimiter"
    assert output =~ "└ mismatched closing delimiter"
  end

  test "multiple vars" do
    code = """
    <<a, b, c, d, e, f, g, h, i, j :: binary>> = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>
    <<a, b, c, d, e, f, g, h, i, x :: binary>> = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>
    x
    """

    assert capture_iex(code) =~ "10"
  end

  test "code escape" do
    code = """
    1 \\
    + 2
    """

    assert capture_iex(code) =~ "3"
  end

  test "no break" do
    input = """
      ["a
      b
      c
    """

    assert capture_iex(input) == ""
  end

  test "break" do
    input = """
      ["a
      b
      c
    #iex:break
    """

    output = capture_iex(input)

    assert output =~ "** (TokenMissingError) token missing on iex:1:"
    assert output =~ "error:"
    assert output =~ "incomplete expression\n"
    assert output =~ "iex:1"
  end

  test "module definition" do
    input = """
    defmodule Sample do
      def foo, do: bar()
      def bar, do: 13
    end && Sample.foo()
    """

    assert capture_iex(input) =~ "13"
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "blocks" do
    input = """
    (
      defmodule Sample do
        def foo, do: bar()
        def bar, do: 13
      end
      import Sample
      foo()
    )
    """

    assert capture_iex(input) =~ "13"
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "ExUnit.Assertions" do
    capture = capture_iex("import ExUnit.Assertions; assert 1 == 2")
    assert capture =~ "** (ExUnit.AssertionError)"
    assert capture =~ "assert 1 == 2"
  end

  test "prompt" do
    opts = [default_prompt: "prompt(%counter)>"]
    assert capture_iex("1\n", opts, [], true) == "prompt(1)> 1\nprompt(2)>"
  end

  if IO.ANSI.enabled?() do
    test "color" do
      opts = [colors: [enabled: true, eval_result: [:red]]]
      assert capture_iex("1 + 2", opts) == "\e[31m\e[33m3\e[0m\e[31m\e[0m"
      assert capture_iex("IO.ANSI.blue()", opts) == "\e[31m\e[32m\"\\e[34m\"\e[0m\e[31m\e[0m"

      assert capture_iex("{:ok}", opts) ==
               "\e[31m\e[39m{\e[0m\e[31m\e[36m:ok\e[0m\e[31m\e[39m}\e[0m\e[31m\e[0m"
    end
  end

  test "inspect opts" do
    opts = [
      inspect: [
        binaries: :as_binaries,
        charlists: :as_lists,
        structs: false,
        limit: 4,
        custom_options: [sort_maps: true]
      ]
    ]

    assert capture_iex("<<45, 46, 47>>\n[45, 46, 47]\n%IO.Stream{}", opts) ==
             "<<45, 46, 47>>\n[45, 46, 47]\n%{__struct__: IO.Stream, device: nil, line_or_bytes: :line, raw: true}"
  end

  test "exception" do
    exception = Regex.escape("** (ArithmeticError) bad argument in arithmetic expression")

    assert capture_iex("1 + :atom\n:this_is_still_working") =~
             ~r/^#{exception}.+\n:this_is_still_working$/s

    refute capture_iex("1 + :atom\n:this_is_still_working") =~ ~r/erl_eval/s
  end

  test "exception while invoking conflicting helpers" do
    import File, only: [open: 1], warn: false

    assert capture_io(:stderr, fn ->
             capture_iex("open('README.md')", [], env: __ENV__)
           end) =~
             ~r"function open/1 imported from both File and IEx.Helpers"
  end

  test "receive exit" do
    assert capture_iex("spawn_link(fn -> exit(:bye) end); Process.sleep(1000)") =~
             ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) shell process exited with reason: :bye"

    assert capture_iex("spawn_link(fn -> exit({:bye, [:world]}) end); Process.sleep(1000)") =~
             ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) shell process exited with reason: {:bye, \[:world\]}"
  end

  test "receive normal exits" do
    assert capture_iex("spawn_link(fn -> exit(:normal) end); Process.sleep(1000)") =~ ":ok"

    assert capture_iex("spawn_link(fn -> exit(:shutdown) end); Process.sleep(1000)") =~
             ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) shell process exited with reason: shutdown"

    assert capture_iex("spawn_link(fn -> exit({:shutdown, :bye}) end); Process.sleep(1000)") =~
             ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) shell process exited with reason: shutdown: :bye"
  end

  test "receive exit from exception" do
    # use exit/1 to fake an error so that an error message
    # is not sent to the error logger.
    content = capture_iex("spawn_link(fn -> exit({%ArgumentError{},
                           [{:not_a_real_module, :function, 0, []}]}) end);
                           Process.sleep(1000)")

    assert content =~
             ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) shell process exited with reason: an exception was raised:\n"

    assert content =~ ~r"\s{4}\*\* \(ArgumentError\) argument error\n"
    assert content =~ ~r"\s{8}:not_a_real_module\.function/0"
  end

  test "receive exit due to failed call" do
    assert capture_iex("exit({:bye, {:gen_server, :call, [self(), :hello]}})") =~
             ~r"\*\* \(exit\) exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hello\)\n\s{4}\*\* \(EXIT\) :bye"
  end

  test "blames function clause error" do
    import PathHelpers

    write_beam(
      defmodule ExampleModule do
        def fun(:valid), do: :ok
      end
    )

    content = capture_iex("IEx.InteractionTest.ExampleModule.fun(:invalid)")

    assert content =~
             "** (FunctionClauseError) no function clause matching in IEx.InteractionTest.ExampleModule.fun/1"

    assert content =~
             "The following arguments were given to IEx.InteractionTest.ExampleModule.fun/1"

    assert content =~ ":invalid"
    assert content =~ "def fun(-:valid-)"

    assert content =~
             ~r"test/iex/interaction_test.exs:\d+: IEx\.InteractionTest\.ExampleModule\.fun/1"

    content = capture_iex("Access.fetch(:foo, :bar)")

    assert content =~ ~r"\(elixir #{System.version()}\) lib/access\.ex:\d+: Access\.fetch/2"
  end

  test "parser" do
    defmodule EchoParser do
      def parse(input, _opts, buffer) do
        {:ok, input, buffer}
      end
    end

    assert capture_iex("foo", parser: {EchoParser, :parse, []}) == "~c\"foo\""
  after
    IEx.configure(parser: {IEx.Evaluator, :parse, []})
  end

  ## .iex file loading

  describe ".iex" do
    test "no .iex" do
      assert capture_io(:stderr, fn -> capture_iex("my_variable") end) =~
               "undefined variable \"my_variable\""
    end

    @tag :tmp_dir
    test "single .iex", %{tmp_dir: tmp_dir} do
      path =
        write_dot_iex!(tmp_dir, "dot-iex", """
        defmodule DotIEx do
          def my_fun_single, do: :single
        end
        import DotIEx
        my_variable = 42
        """)

      assert capture_iex("{my_fun_single(), my_variable}", [], dot_iex: path) ==
               "{:single, 42}"
    end

    @tag :tmp_dir
    test "nested .iex", %{tmp_dir: tmp_dir} do
      write_dot_iex!(tmp_dir, "dot-iex-1", """
      defmodule DotIExNested do
        def my_fun_nested, do: :nested
      end
      import DotIExNested
      nested_var = 42
      """)

      path =
        write_dot_iex!(tmp_dir, "dot-iex", "import_file \"#{tmp_dir}/dot-iex-1\"\nmy_variable=13")

      input = "nested_var\nmy_variable\nmy_fun_nested()"
      assert capture_iex(input, [], dot_iex: path) == "42\n13\n:nested"
    end

    @tag :tmp_dir
    test "configured .iex", %{tmp_dir: tmp_dir} do
      path =
        write_dot_iex!(tmp_dir, "configured-dot-iex", """
        defmodule ConfiguredDotIEx do
          def my_fun_single, do: :single
        end
        import ConfiguredDotIEx
        my_variable = 42
        """)

      assert capture_iex("{my_fun_single(), my_variable}", [dot_iex: path], dot_iex: nil) ==
               "{:single, 42}"
    end
  end

  defp write_dot_iex!(tmp_dir, name, contents) do
    path = Path.join(tmp_dir, name)
    File.write!(path, contents)
    path
  end

  describe "pipeline error safety" do
    test "syntax error in pipeline prevents subsequent operations" do
      input = """
      1, 2, 3]
      |> Enum.map(&(&1 * 2))
      """

      output = capture_iex(input)
      assert output =~ "skipping evaluation of expression because pipeline has failed"
    end

    test "runtime error in first pipeline step prevents subsequent operations" do
      input = """
      :not_a_list
      |> Enum.map(&(&1 * 2))
      |> Enum.sum()
      """

      output = capture_iex(input)
      assert output =~ "skipping evaluation of expression because pipeline has failed"
    end

    test "error state is cleared after successful evaluation" do
      input = """
      :not_a_list
      |> Enum.map(&(&1 * 2))
      |> Enum.sum()
      [1, 2, 3]
      |> Enum.sum()
      """

      output = capture_iex(input)
      assert output =~ "skipping evaluation of expression because pipeline has failed"
      assert String.ends_with?(output, "6")
    end
  end
end
