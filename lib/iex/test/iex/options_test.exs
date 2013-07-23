Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.OptionsTest do
  use IEx.Case

  @doc """
  Hello, I have %{red}ANSI%{reset} escapes.
  """
  def ansi_escapes, do: :ok

  unless match?({:win32,_}, :os.type) do
    test "color" do
      opts = [colors: [enabled: true, eval_result: "red"]]
      assert capture_iex("1 + 2", opts) == "\e[31m3\e[0m"

      # Sanity checks
      assert capture_iex("IO.ANSI.escape(\"%{blue}hello\", true)", opts)
             == "\e[31m\"\\e[34mhello\\e[0m\"\e[0m"
      assert capture_iex("IO.puts IO.ANSI.escape(\"%{blue}hello\", true)", opts)
             == "\e[34mhello\e[0m\n\e[31m:ok\e[0m"
      assert capture_iex("IO.puts IO.ANSI.escape(\"%{blue}hello\", true)", [colors: [enabled: false]])
             == "\e[34mhello\e[0m\n:ok"

      # Test that ANSI escapes in the docs are left alone
      opts = [colors: [enabled: true, info: "red", eval_result: "red"]]
      assert capture_iex("h IEx.OptionsTest.ansi_escapes", opts)
             == "\e[31m* def ansi_escapes()\n\e[0m\n\e[31mHello, I have %{red}ANSI%{reset} escapes.\n\e[0m"

      # Test that ANSI escapes in iex output are left alone
      assert capture_iex("\"%{red} %{blue}\"", opts) == "\e[31m\"%{red} %{blue}\"\e[0m"
      assert capture_iex("IO.puts IEx.color(:info, \"%{red} %{blue}\")", opts)
             == "\e[31m%{red} %{blue}\e[0m\n\e[31m:ok\e[0m"
    end
  end

  test "inspect opts" do
    opts = [inspect: [limit: 3, raw: true]]
    assert capture_iex("[1,2,3,4,5]\nArgumentError[]", opts) ==
              "[1, 2, 3, ...]\n{ArgumentError, :__exception__, \"argument error\"}"

    opts = [inspect: [raw: false]]
    assert capture_iex("ArgumentError[]", opts) == "ArgumentError[message: \"argument error\"]"
  end

  test "history size" do
    opts = [history_size: 3]
    assert capture_iex("1\n2\n3\nv(1)", opts) == "1\n2\n3\n1"
    assert "1\n2\n3\n4\n** (RuntimeError) Out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(1)", opts)
    assert "1\n2\n3\n4\n** (RuntimeError) Out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(-4)", opts)
    assert "1\n2\n3\n4\n2\n** (RuntimeError) Out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(2)\nv(2)", opts)
  end

  test "bad option" do
    assert_raise ArgumentError, fn ->
      IEx.Options.set :nonexistent_option, nil
    end
  end

  test "bad key" do
    assert_raise ArgumentError, fn ->
      IEx.Options.set :colors, nonexistent_color_name: "red"
    end
  end
end
