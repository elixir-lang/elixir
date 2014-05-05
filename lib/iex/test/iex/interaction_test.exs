Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.InteractionTest do
  use IEx.Case

  ## Basic interaction

  test "whole output" do
    IEx.Options.set :colors, enabled: false

    assert capture_io("IO.puts \"Hello world\"", fn ->
      IEx.Server.start([dot_iex_path: ""], fn -> end)
    end) =~ "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)\niex(1)> Hello world\n:ok\niex(2)>"
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
    assert capture_iex("if true do ) false end") =~ "** (SyntaxError) iex:1: \"do\" starting at"
  end

  test "undefined function" do
    assert "** (RuntimeError) undefined function: format/0"   <> _ = capture_iex("format")
    assert "** (RuntimeError) undefined function: with_one/1" <> _ = capture_iex("with_one(22)")
    assert "** (RuntimeError) undefined function: many/3"     <> _ = capture_iex("many(:ok, 22, \"hi\")")
  end

  test "module definition" do
    input = """
    defmodule Sample do
      def foo, do: bar
      def bar, do: 13
    end && Sample.foo
    """
    assert capture_iex(input) =~ "13"
  after
    :code.purge(Sample)
    :code.delete(Sample)
  end

  ## .iex file loading

  test "no .iex" do
    assert "** (RuntimeError) undefined function: my_variable/0" <> _ = capture_iex("my_variable")
  end

  test ".iex" do
    File.write!("dot-iex", "my_variable = 144")
    assert capture_iex("my_variable", [], [dot_iex_path: "dot-iex"]) == "144"
  after
    File.rm!("dot-iex")
  end

  test "nested .iex" do
    File.write!("dot-iex-1", "nested_var = 13\nimport IO")
    File.write!("dot-iex", "import_file \"dot-iex-1\"\nmy_variable=14")

    input = "nested_var\nmy_variable\nputs \"hello\""
    assert capture_iex(input, [], [dot_iex_path: "dot-iex"]) == "13\n14\nhello\n:ok"
  after
    File.rm("dot-iex-1")
    File.rm!("dot-iex")
  end

  test "receive exit" do
    assert capture_iex("spawn_link(fn -> exit(:bye) end)") =~ ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) :bye"
    assert capture_iex("spawn_link(fn -> exit({:bye, [:world]}) end)") =~ ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) {:bye, \[:world\]}"
  end

  test "receive exit from exception" do
    # use exit/1 to fake an error so that an error message is not sent to the
    # error logger.
    assert capture_iex("spawn_link(fn -> exit({ArgumentError[],
      [{:not_a_real_module, :function, 0, []}]}) end)") =~ ~r"\*\* \(EXIT from #PID<\d+\.\d+\.\d+>\) an exception was raised:\n\s{4}\*\* \(ArgumentError\) argument error\n\s{8}:not_a_real_module\.function/0"
  end

  test "exit due to failed call" do
    assert capture_iex("exit({:bye, {:gen_server, :call, [self(), :hello]}})") =~ ~r"\*\* \(exit\) exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hello\)\n\s{4}\*\* \(EXIT\) :bye"
  end

end
