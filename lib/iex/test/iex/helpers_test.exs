Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.HelpersTest do
  use IEx.Case

  import IEx.Helpers

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
           = capture_io(fn -> h IEx.Helpers end)

    assert capture_io(fn -> h :whatever end)
           == "Could not load module :whatever: nofile\n"
  end

  test "h helper function" do
    doc_1 = "* def test_fun_1()\n\nTest function 1\n"
    doc_2 = "* def test_fun_1(arg)\n\nTest function 2\n"

    assert capture_io(fn -> h IEx.HelpersTest.test_fun_1/0 end) == doc_1
    assert capture_io(fn -> h IEx.HelpersTest.test_fun_1/1 end) == doc_2

    output = capture_io(fn -> h IEx.HelpersTest.test_fun_1 end)
    assert :binary.match(output, doc_1)
    assert :binary.match(output, doc_2)

    assert capture_io(fn -> h pwd end)
           == "* def pwd()\n\nPrints the current working directory.\n\n"
  end

  test "t helper" do
    assert capture_io(fn -> t ExUnit end) == "No types for ExUnit have been found\n"

    # Test that it shows at least two types
    assert Enum.count(capture_io(fn -> t Enum end) |> String.split("\n"), fn line ->
      String.starts_with? line, "@type"
    end) >= 2

    assert "@type t() :: " <> _
           = capture_io(fn -> t Enum.t end)
    assert capture_io(fn -> t Enum.t end) == capture_io(fn -> t Enum.t/0 end)
  end

  test "s helper" do
    assert capture_io(fn -> s ExUnit end) == "No specs for ExUnit have been found\n"

    # Test that it shows at least two specs
    assert Enum.count(capture_io(fn -> s Enum end) |> String.split("\n"), fn line ->
      String.starts_with? line, "@spec"
    end) >= 2

    assert Enum.count(capture_io(fn -> s Enum.all? end) |> String.split("\n"), fn line ->
      String.starts_with? line, "@spec"
    end) >= 2

    assert capture_io(fn -> s Enum.all?/1 end) == "@spec all?(t()) :: boolean()\n"
    assert capture_io(fn -> s list_to_binary end) == "@spec list_to_binary(iolist()) :: binary()\n"
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
    assert capture_io(fn -> self() <- :hello; flush end) == ":hello\n"
  end

  test "pwd helper" do
    assert capture_io(fn -> pwd end) =~ %r"lib[\\/]iex\n$"
  end

  test "ls helper" do
    assert ["ebin", "lib", "mix.exs", "test"]
           = capture_io(fn -> ls end)
             |> String.split
             |> Enum.map(String.strip(&1))
             |> Enum.sort
    assert capture_io(fn -> ls "~" end) == capture_io(fn -> ls System.user_home end)
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

    assert Enum.count(capture_io(fn -> m end) |> String.split("\n"), fn line ->
      Enum.any? regexes, fn re ->
        Regex.match? re, line
      end
    end) >= 2
  end

  test "c helper" do
    assert_raise UndefinedFunctionError, "undefined function: Helpers_test_module.run/0", fn ->
      Helpers_test_module.run
    end

    File.write! "test-module-code.ex", test_module_code
    assert c("test-module-code.ex") == [Helpers_test_module]
    assert Helpers_test_module.run == :run
  after
    File.rm "test-module-code.ex"
    File.rm! "Elixir.Helpers_test_module.beam"
    true = :code.delete Helpers_test_module
    :code.purge Helpers_test_module
  end

  test "c helper multiple modules" do
    assert_raise UndefinedFunctionError, "undefined function: Helpers_test_module.run/0", fn ->
      Helpers_test_module.run
    end

    File.write! "test-module-code.ex", test_module_code <> "\n" <> another_test_module
    assert c("test-module-code.ex") |> Enum.sort == [Another_test_module,Helpers_test_module]
    assert Helpers_test_module.run == :run
    assert Another_test_module.hello == :world
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
    assert_raise UndefinedFunctionError, "undefined function: Helpers_test_module.run/0", fn ->
      Helpers_test_module.run
    end

    File.write! "test-module-code-1.ex", test_module_code
    File.write! "test-module-code-2.ex", another_test_module
    assert c(["test-module-code-1.ex", "test-module-code-2.ex"]) |> Enum.sort
           == [Another_test_module,Helpers_test_module]
    assert Helpers_test_module.run == :run
    assert Another_test_module.hello == :world
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
    assert_raise UndefinedFunctionError, "undefined function: Helpers_test_module.run/0", fn ->
      Helpers_test_module.run
    end

    assert l(:non_existent_module) == {:error,:nofile}

    File.write! "test-module-code.ex", test_module_code
    assert c("test-module-code.ex") == [Helpers_test_module]
    assert Helpers_test_module.run == :run

    File.write! "test-module-code.ex", "defmodule Helpers_test_module do end"
    # FIXME: is there another way to compile a file without loading its module?
    System.cmd "elixirc test-module-code.ex"

    assert l(Helpers_test_module) == {:module, Helpers_test_module}
    assert_raise UndefinedFunctionError, fn ->
      Helpers_test_module.run
    end
  after
    File.rm "test-module-code.ex"
    File.rm! "Elixir.Helpers_test_module.beam"

    # FIXME: This errors out with "Module 'Elixir.Helpers_test_module' must be purged before loading"
    #true = :code.delete Helpers_test_module

    :code.purge Helpers_test_module
  end

  test "r helper" do
    assert_raise UndefinedFunctionError, "undefined function: Helpers_test_module.run/0", fn ->
      Helpers_test_module.run
    end

    assert r == []
    assert r(Kernel) == :nosource
    assert_raise UndefinedFunctionError, "undefined function: :non_existent_module.module_info/1", fn ->
      r :non_existent_module
    end

    File.write! "test-module-code.ex", test_module_code
    assert c("test-module-code.ex") == [Helpers_test_module]
    assert Helpers_test_module.run == :run
    # FIXME: `r Helpers_test_module` returns :nosource
    assert r(Helpers_test_module) == [Helpers_test_module]

    assert r == [Helpers_test_module]
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
        :run
      end
    end
    """
  end

  defp another_test_module do
    """
    defmodule Another_test_module do
      def hello do
        :world
      end
    end
    """
  end
end
