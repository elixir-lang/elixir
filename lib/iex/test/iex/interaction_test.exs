Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.InteractionTest do
  use IEx.Case

  ### basic interaction ###

  test "whole output" do
    IEx.Options.set :colors, enabled: false

    # We're using bare-bones capture_io instead of capture_iex only once here
    assert capture_io("IO.puts \"Hello world\"", fn ->
      IEx.Server.start(IEx.boot_config(dot_iex_path: ""))
    end) =~ %r"^Interactive Elixir \(.+?\) - press Ctrl\+C to exit \(type h\(\) ENTER for help\)\niex\(1\)> Hello world\n:ok\niex\(2\)> $"
  end

  test "empty input" do
    assert capture_iex("\n") == "nil"
  end

  test "normal input" do
    assert capture_iex("1 + 2") == "3"
  end

  test "exception" do
    exception = Regex.escape("** (ArithmeticError) bad argument in arithmetic expression")
    assert capture_iex("1 + :atom\n:this_is_still_working")
           =~ %r/^#{exception}.+\n:this_is_still_working$/s
  end

  test "empty history at the start" do
    assert "** (RuntimeError) Out of bounds" <> _ = capture_iex("v(-1)")
  end

  test "empty history at the start redux" do
    assert "** (RuntimeError) Out of bounds" <> _ = capture_iex("v(1)")
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
    assert "** (TokenMissingError) iex:1: incomplete expression" <> _ = capture_iex(input)
  end

  test "invalid input" do
    assert "** (SyntaxError) iex:1: \"do\" starting at" <> _ = capture_iex("if true do ) false end")
  end

  ### .iex file loading ###

  test "no .iex" do
    assert "** (UndefinedFunctionError) undefined function: IEx.Helpers.my_variable/0" <> _ = capture_iex("my_variable")
  end

  test ".iex" do
    File.write!("dot-iex", "my_variable = 144")
    assert capture_iex("my_variable", [], "dot-iex") == "144"
  after
    File.rm!("dot-iex")
  end

  test "nested .iex" do
    File.write!("dot-iex-1", "nested_var = 13\nimport IO")
    File.write!("dot-iex", "import_file \"dot-iex-1\"\nmy_variable=14")

    input = "nested_var\nmy_variable\nputs \"hello\""
    assert capture_iex(input, [], "dot-iex") == "13\n14\nhello\n:ok"
  after
    File.rm("dot-iex-1")
    File.rm!("dot-iex")
  end

  test "receive exit" do
    assert capture_iex("spawn_link(fn -> exit(:bye) end)") =~ %r"EXIT from #PID"
  end
end
