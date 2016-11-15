Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.InteractionTest do
  use IEx.Case

  ## Basic interaction

  test "whole output" do
    assert capture_io("IO.puts \"Hello world\"", fn ->
      IEx.Server.start([dot_iex_path: ""], {IEx, :dont_display_result, []})
    end) =~ "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)" <>
            "\niex(1)> Hello world\n:ok\niex(2)>"
  end

  test "empty input" do
    assert capture_iex("\n") == "nil"
  end

  test "normal input" do
    assert capture_iex("1 + 2") == "3"
  end

  test "multiple vars" do
    code = """
    << a, b, c, d, e, f, g, h, i, j :: binary >> = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>
    << a, b, c, d, e, f, g, h, i, x :: binary >> = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>
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

  test "exception" do
    exception = Regex.escape("** (ArithmeticError) bad argument in arithmetic expression")
    assert capture_iex("1 + :atom\n:this_is_still_working")
           =~ ~r/^#{exception}.+\n:this_is_still_working$/s
    refute capture_iex("1 + :atom\n:this_is_still_working")
           =~ ~r/erl_eval/s
  end

  test "empty history at the start" do
    assert capture_iex("v(-1)") =~ "** (RuntimeError) v(-1) is out of bounds"
  end

  test "empty history at the start redux" do
    assert capture_iex("v(1)") =~ "** (RuntimeError) v(1) is out of bounds"
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
    assert capture_iex(input) =~ "** (TokenMissingError) iex:1: incomplete expression"
  end

  test "invalid input" do
    assert capture_iex("if true do ) false end") =~ "** (SyntaxError) iex:1: \"do\" is missing terminator \"end\". unexpected token: \")\" at line 1"
  end

  test "module definition" do
    input = """
    defmodule Sample do
      def foo, do: bar()
      def bar, do: 13
    end && Sample.foo
    """
    assert capture_iex(input) =~ "13"
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "prompt" do
    opts = [default_prompt: "prompt(%counter)>"]
    assert capture_iex("1\n", opts, [], true) == "prompt(1)> 1\nprompt(2)>"
  end

  unless match?({:win32, _}, :os.type) do
    test "color" do
      opts = [colors: [enabled: true, eval_result: [:red]]]
      assert capture_iex("1 + 2", opts) ==
        "\e[31m\e[35m3\e[0m\e[31m\e[0m"
      assert capture_iex("IO.ANSI.blue", opts) ==
        "\e[31m\e[32m\"\\e[34m\"\e[0m\e[31m\e[0m"
      assert capture_iex("{:ok}", opts) ==
        "\e[31m{\e[36m:ok\e[0m\e[31m}\e[0m"
    end
  end
  test "inspect opts" do
    opts = [inspect: [binaries: :as_binaries, charlists: :as_lists, structs: false, limit: 4]]
    assert capture_iex("<<45, 46, 47>>\n[45, 46, 47]\n%IO.Stream{}", opts) ==
              "<<45, 46, 47>>\n[45, 46, 47]\n%{__struct__: IO.Stream, device: nil, line_or_bytes: :line, raw: true}"
  end

  test "history size" do
    opts = [history_size: 3]
    assert capture_iex("1\n2\n3\nv(1)", opts) == "1\n2\n3\n1"
    assert "1\n2\n3\n4\n** (RuntimeError) v(1) is out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(1)", opts)
    assert "1\n2\n3\n4\n** (RuntimeError) v(-4) is out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(-4)", opts)
    assert "1\n2\n3\n4\n2\n** (RuntimeError) v(2) is out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(2)\nv(2)", opts)
  end

  ## .iex file loading

  test "no .iex" do
    capture_io(:stderr, fn ->
      assert "** (CompileError) iex:1: undefined function my_variable/0" <> _ = capture_iex("my_variable")
    end)
  end

  test ".iex" do
    File.write!("dot-iex", "my_variable = 144")
    assert capture_iex("my_variable", [], [dot_iex_path: "dot-iex"]) == "144"
  after
    File.rm("dot-iex")
  end

  test "nested .iex" do
    File.write!("dot-iex-1", "nested_var = 13\nimport IO")
    File.write!("dot-iex", "import_file \"dot-iex-1\"\nmy_variable=14")

    input = "nested_var\nmy_variable\nputs \"hello\""
    assert capture_iex(input, [], [dot_iex_path: "dot-iex"]) == "13\n14\nhello\n:ok"
  after
    File.rm("dot-iex-1")
    File.rm("dot-iex")
  end

  test "receive exit" do
    assert capture_iex("spawn_link(fn -> exit(:bye) end); Process.sleep(1000)") =~
           ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) :bye"
    assert capture_iex("spawn_link(fn -> exit({:bye, [:world]}) end); Process.sleep(1000)") =~
           ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) {:bye, \[:world\]}"
  end

  test "receive exit from exception" do
    # use exit/1 to fake an error so that an error message
    # is not sent to the error logger.
    content = capture_iex("spawn_link(fn -> exit({%ArgumentError{},
                           [{:not_a_real_module, :function, 0, []}]}) end);
                           Process.sleep(1000)")
    assert content =~ ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) an exception was raised:\n"
    assert content =~ ~r"\s{4}\*\* \(ArgumentError\) argument error\n"
    assert content =~ ~r"\s{8}:not_a_real_module\.function/0"
  end

  test "exit due to failed call" do
    assert capture_iex("exit({:bye, {:gen_server, :call, [self(), :hello]}})") =~
           ~r"\*\* \(exit\) exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hello\)\n\s{4}\*\* \(EXIT\) :bye"
  end
end
