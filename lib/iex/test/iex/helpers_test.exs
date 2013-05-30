Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.HelpersTest do
  use IEx.Case

  @doc """
  Test function 1
  """
  def test_fun_1

  @doc """
  Test function 2
  """
  def test_fun_1(arg)

  test "h helper" do
    assert "# IEx.Helpers\n\nWelcome to Interactive Elixir" <> _
           = capture_iex("h")
  end

  test "h helper module" do
    assert "# IEx.Helpers\n\nWelcome to Interactive Elixir" <> _
           = capture_iex("h IEx.Helpers")
    assert capture_iex("h :whatever") == "Could not load module :whatever: nofile\n:ok"
  end

  test "h helper function" do
    doc_1 = "* def test_fun_1()\n\nTest function 1\n:ok"
    doc_2 = "* def test_fun_1(arg)\n\nTest function 2\n:ok"

    assert capture_iex("h IEx.HelpersTest.test_fun_1/0") == doc_1
    assert capture_iex("h IEx.HelpersTest.test_fun_1/1") == doc_2

    output = capture_iex("h IEx.HelpersTest.test_fun_1")
    assert :binary.match(output, doc_1)
    assert :binary.match(output, doc_2)

    assert capture_iex("h pwd") == "* def pwd()\n\nPrints the current working directory.\n\n:ok"
  end

  test "t helper" do
    assert "** (UndefinedFunctionError) undefined function: IEx.Helpers.t/0" <> _
           = capture_iex("t")

    assert capture_iex("t ExUnit") == "No types for ExUnit have been found\n:ok"

    # Test that it shows at least two types
    assert Enum.count(capture_iex("t Enum") |> String.split("\n"), fn line ->
      String.starts_with? line, "@type"
    end) >= 2

    assert "@type t() :: " <> _
           = capture_iex("t Enum.t")
    assert capture_iex("t Enum.t") == capture_iex("t Enum.t/0")
  end

  test "s helper" do
    assert "** (UndefinedFunctionError) undefined function: IEx.Helpers.s/0" <> _
           = capture_iex("s")

    assert capture_iex("s ExUnit") == "No specs for ExUnit have been found\n:ok"

    # Test that it shows at least two specs
    assert Enum.count(capture_iex("s Enum") |> String.split("\n"), fn line ->
      String.starts_with? line, "@spec"
    end) >= 2

    assert Enum.count(capture_iex("s Enum.all?") |> String.split("\n"), fn line ->
      String.starts_with? line, "@spec"
    end) >= 2

    assert capture_iex("s Enum.all?/1") == "@spec all?(t()) :: boolean()\n:ok"
    assert capture_iex("s list_to_binary") == "@spec list_to_binary(iolist()) :: binary()\n:ok"
  end

  test "v helper" do
    assert capture_iex("v") == ":ok"
    assert capture_iex("1\n2\nv") == String.rstrip """
      1
      2
      1: 1
      #=> 1

      2: 2
      #=> 2

      :ok
      """

    assert "** (RuntimeError) Out of bounds" <> _
           = capture_iex("v(0)")
    assert capture_iex("1\n2\nv(2)") == "1\n2\n2"
    assert capture_iex("1\n2\nv(2)") == capture_iex("1\n2\nv(-1)")
  end

  test "flush helper" do
    assert capture_iex("self() <- :hello\nflush") == ":hello\n:hello\n:ok"
  end

  test "pwd helper" do
    assert capture_iex("pwd") =~ %r"lib[\\/]iex\n:ok$"
  end

  test "ls helper" do
    assert [":ok", "ebin", "lib", "mix.exs", "test"]
           = capture_iex("ls")
             |> String.split
             |> Enum.map(String.strip(&1))
             |> Enum.sort
    assert capture_iex("ls \"~\"") == capture_iex("ls System.user_home")
  end

  test "import_file helper" do
    File.write! "dot-iex", "variable = :hello\nimport IO"

    assert "** (UndefinedFunctionError) undefined function: IEx.Helpers.variable/0" <> _
           = capture_iex("variable")
    assert "** (UndefinedFunctionError) undefined function: IEx.Helpers.puts/1" <> _
           = capture_iex("puts \"hi\"")

    assert capture_iex("import_file \"dot-iex\"\nvariable\nputs \"hi\"")
           == "nil\n:hello\nhi\n:ok"
  after
    File.rm! "dot-iex"
  end

  test "import_file nested" do
    File.write! "dot-iex-1", "variable = :hello\nimport IO"
    File.write! "dot-iex", "parent = true\nimport_file \"dot-iex-1\""

    assert "** (UndefinedFunctionError) undefined function: IEx.Helpers.parent/0" <> _
           = capture_iex("parent")
    assert "** (UndefinedFunctionError) undefined function: IEx.Helpers.variable/0" <> _
           = capture_iex("variable")
    assert "** (UndefinedFunctionError) undefined function: IEx.Helpers.puts/1" <> _
           = capture_iex("puts \"hi\"")

    assert capture_iex("import_file \"dot-iex\"\nvariable\nputs \"hi\"\nparent")
           == "nil\n:hello\nhi\n:ok\ntrue"
  after
    File.rm "dot-iex-1"
    File.rm! "dot-iex"
  end

  test "m helper" do
    regexes = [
      %r/^:application\s+.+application\.beam$/,
      %r/^:code\s+.+code\.beam$/,
      %r/^Kernel\s+.+Elixir\.Kernel\.beam$/,
    ]

    assert Enum.count(capture_iex("m") |> String.split("\n"), fn line ->
      Enum.any? regexes, fn re ->
        Regex.match? re, line
      end
    end) >= 2
  end

  test "c helper" do
    assert "** (UndefinedFunctionError) undefined function: Helpers_test_module.run/0" <> _
           = capture_iex("Helpers_test_module.run")

    File.write! "test-module-code.ex", test_module_code
    assert capture_iex("c \"test-module-code.ex\"\nHelpers_test_module.run") == "[Helpers_test_module]\nrun!\n:ok"
  after
    File.rm "test-module-code.ex"
    File.rm! "Elixir.Helpers_test_module.beam"
    true = :code.delete Helpers_test_module
    :code.purge Helpers_test_module
  end

  test "c helper multiple modules" do
    File.write! "test-module-code.ex", test_module_code <> "\n" <> another_test_module
    assert capture_iex("c(\"test-module-code.ex\") |> Enum.sort\nHelpers_test_module.run\nAnother_test_module.hello")
           == "[Another_test_module,Helpers_test_module]\nrun!\n:ok\nworld\n:ok"
  after
    File.rm "test-module-code.ex"
    File.rm "Elixir.Helpers_test_module.beam"
    true = :code.delete Helpers_test_module
    :code.purge Helpers_test_module

    File.rm! "Elixir.Another_test_module.beam"
    true = :code.delete Another_test_module
    :code.purge Another_test_module
  end

  test "c helper list" do
    File.write! "test-module-code-1.ex", test_module_code
    File.write! "test-module-code-2.ex", another_test_module
    assert capture_iex("c([\"test-module-code-1.ex\", \"test-module-code-2.ex\"]) |> Enum.sort\nHelpers_test_module.run\nAnother_test_module.hello")
           == "[Another_test_module,Helpers_test_module]\nrun!\n:ok\nworld\n:ok"
  after
    File.rm "test-module-code-1.ex"
    File.rm "test-module-code-2.ex"

    File.rm "Elixir.Helpers_test_module.beam"
    true = :code.delete Helpers_test_module
    :code.purge Helpers_test_module

    File.rm! "Elixir.Another_test_module.beam"
    true = :code.delete Another_test_module
    :code.purge Another_test_module
  end

  test "l helper" do
    File.write! "test-module-code.ex", test_module_code
    input = """
    c "test-module-code.ex"
    File.write! "test-module-code.ex", "defmodule Helpers_test_module do end"
    l Helpers_test_module
    Helpers_test_module.run
    """
    assert capture_iex(input) == "[Helpers_test_module]\n:ok\n{:module,Helpers_test_module}\nrun!\n:ok"

    assert capture_iex("l :non_existent_module") == "{:error,:nofile}"
  after
    File.rm "test-module-code.ex"
    File.rm! "Elixir.Helpers_test_module.beam"

    # FIXME: This errors out with "Module 'Elixir.Helpers_test_module' must be purged before loading"
    #true = :code.delete Helpers_test_module

    :code.purge Helpers_test_module
  end

  test "r helper" do
    assert capture_iex("r") == "[]"
    assert capture_iex("r Kernel") == ":nosource"
    assert "** (UndefinedFunctionError) undefined function: :non_existent_module.module_info/1" <> _
           = capture_iex("r :non_existent_module")

    File.write! "test-module-code.ex", test_module_code
    # FIXME: `r Helpers_test_module` returns :nosource
    assert capture_iex("c \"test-module-code.ex\"\nr Helpers_test_module") == "[Helpers_test_module]\n[Helpers_test_module]"

    assert capture_iex("r") == "[Helpers_test_module]"
  after
    File.rm "test-module-code.ex"
    File.rm! "Elixir.Helpers_test_module.beam"

    # FIXME: This errors out with "Module 'Elixir.Helpers_test_module' must be purged before loading"
    #true = :code.delete Helpers_test_module

    :code.purge Helpers_test_module
  end

  defp test_module_code do
    """
    defmodule Helpers_test_module do
      def run do
        IO.puts "run!"
      end
    end
    """
  end

  defp another_test_module do
    """
    defmodule Another_test_module do
      def hello do
        IO.puts "world"
      end
    end
    """
  end
end
