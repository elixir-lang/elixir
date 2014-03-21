Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.HelpersTest do
  use IEx.Case

  import IEx.Helpers

  @doc """
  Test function 1
  """
  def test_fun_1, do: :ok

  @doc """
  Test function 2
  """
  def test_fun_2(arg \\ 99), do: arg

  test "clear helper" do
    assert "\e[H\e[2J" == capture_iex("clear")
  end

  test "h helper" do
    assert "* IEx.Helpers\n\nWelcome to Interactive Elixir" <> _
           = capture_iex("h")
  end

  test "h helper module" do
    assert "* IEx.Helpers\n\nWelcome to Interactive Elixir" <> _ =
           capture_io(fn -> h IEx.Helpers end)

    assert capture_io(fn -> h :whatever end) ==
           "Could not load module :whatever, got: nofile\n"

    assert capture_io(fn -> h :lists end) ==
           ":lists is an Erlang module and, as such, it does not have Elixir-style docs\n"
  end

  test "h helper function" do
    doc_1 = "* def test_fun_1()\n\nTest function 1\n\n"
    doc_2 = "* def test_fun_2(arg \\\\ 99)\n\nTest function 2\n\n"

    assert capture_io(fn -> h IEx.HelpersTest.test_fun_1/0 end) == doc_1
    assert capture_io(fn -> h IEx.HelpersTest.test_fun_2/1 end) == doc_2

    output = capture_io(fn -> h IEx.HelpersTest.test_fun_1 end)
    assert :binary.match(output, doc_1)
    assert :binary.match(output, doc_2)

    assert capture_io(fn -> h pwd end)
           == "* def pwd()\n\nPrints the current working directory.\n\n\n"
  end

  test "h helper __info__" do
    h_output_module = capture_io(fn -> h Module.__info__ end)
    assert capture_io(fn -> h Module.UnlikelyTo.Exist.__info__ end) == h_output_module
    assert capture_io(fn -> h Module.UnlikelyTo.Exist.__info__/1 end) == h_output_module
    assert capture_io(fn -> h __info__ end) == "No documentation for __info__ was found\n"
  end

  test "t helper" do
    assert capture_io(fn -> t IEx end) == "No type information for IEx was found\n"

    # Test that it shows at least two types
    assert Enum.count(capture_io(fn -> t Enum end) |> String.split("\n"), fn line ->
      String.starts_with? line, "@type"
    end) >= 2

    assert "@type t() :: " <> _
           = capture_io(fn -> t Enum.t end)
    assert capture_io(fn -> t Enum.t end) == capture_io(fn -> t Enum.t/0 end)
  end

  test "s helper" do
    assert capture_io(fn -> s ExUnit end) == "No specification for ExUnit was found\n"

    # Test that it shows at least two specs
    assert Enum.count(capture_io(fn -> s Enum end) |> String.split("\n"), fn line ->
      String.starts_with? line, "@spec"
    end) >= 2

    assert Enum.count(capture_io(fn -> s Enum.all? end) |> String.split("\n"), fn line ->
      String.starts_with? line, "@spec"
    end) >= 2

    assert capture_io(fn -> s Enum.all?/1 end) == "@spec all?(t()) :: boolean()\n"
    assert capture_io(fn -> s iolist_to_binary end) == "@spec iolist_to_binary(iolist() | binary()) :: binary()\n"
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

    assert "** (RuntimeError) v(0) is out of bounds" <> _
           = capture_iex("v(0)")
    assert capture_iex("1\n2\nv(2)") == "1\n2\n2"
    assert capture_iex("1\n2\nv(2)") == capture_iex("1\n2\nv(-1)")

    assert capture_iex("1\n2\nIEx.History.reset\nv")
           == String.rstrip """
           1
           2
           true
           3: IEx.History.reset
           #=> true

           :ok
           """
  end

  test "flush helper" do
    assert capture_io(fn -> send self(), :hello; flush end) == ":hello\n"
  end

  test "pwd helper" do
    File.cd! iex_path, fn ->
      assert capture_io(fn -> pwd end) =~ ~r"lib[\\/]iex\n$"
    end
  end

  test "ls helper" do
    File.cd! iex_path, fn ->
      paths = capture_io(fn -> ls end)
              |> String.split
              |> Enum.map(&String.strip(&1))

      assert "ebin" in paths
      assert "mix.exs" in paths

      assert capture_io(fn -> ls "~" end) ==
             capture_io(fn -> ls System.user_home end)
    end
  end

  test "import_file helper" do
    with_file "dot-iex", "variable = :hello\nimport IO", fn ->
      assert "** (RuntimeError) undefined function: variable/0" <> _
             = capture_iex("variable")
      assert "** (RuntimeError) undefined function: puts/1" <> _
             = capture_iex("puts \"hi\"")

      assert capture_iex("import_file \"dot-iex\"\nvariable\nputs \"hi\"")
             == "nil\n:hello\nhi\n:ok"
    end
  end

  test "import_file nested" do
    dot   = "parent = true\nimport_file \"dot-iex-1\""
    dot_1 = "variable = :hello\nimport IO"

    with_file ["dot-iex", "dot-iex-1"], [dot, dot_1], fn ->
      assert "** (RuntimeError) undefined function: parent/0" <> _
             = capture_iex("parent")
      assert "** (RuntimeError) undefined function: variable/0" <> _
             = capture_iex("variable")
      assert "** (RuntimeError) undefined function: puts/1" <> _
             = capture_iex("puts \"hi\"")

      assert capture_iex("import_file \"dot-iex\"\nvariable\nputs \"hi\"\nparent")
             == "nil\n:hello\nhi\n:ok\ntrue"
    end
  end

  test "c helper" do
    assert_raise UndefinedFunctionError, "undefined function: Sample.run/0", fn ->
      Sample.run
    end

    filename = "sample.ex"
    with_file filename, test_module_code, fn ->
      assert c(filename) == [Sample]
      assert Sample.run == :run
    end
  after
    cleanup_modules([Sample])
  end

  test "c helper with full path" do
    filename = "sample.ex"
    with_file filename, test_module_code, fn ->
      assert c(Path.expand(filename)) == [Sample]
      assert Sample.run == :run
    end
  after
    cleanup_modules([Sample])
  end

  test "c helper multiple modules" do
    assert_raise UndefinedFunctionError, "undefined function: Sample.run/0", fn ->
      Sample.run
    end

    filename = "sample.ex"
    with_file filename, test_module_code <> "\n" <> another_test_module, fn ->
      assert c(filename) |> Enum.sort == [Sample, Sample2]
      assert Sample.run == :run
      assert Sample2.hello == :world
    end
  after
    cleanup_modules([Sample, Sample2])
  end

  test "c helper list" do
    assert_raise UndefinedFunctionError, "undefined function: Sample.run/0", fn ->
      Sample.run
    end

    filenames = ["sample1.ex", "sample2.ex"]
    with_file filenames, [test_module_code, another_test_module], fn ->
      assert c(filenames) |> Enum.sort == [Sample, Sample2]
      assert Sample.run == :run
      assert Sample2.hello == :world
    end
  after
    cleanup_modules([Sample, Sample2])
  end

  test "c helper erlang" do
    assert_raise UndefinedFunctionError, "undefined function: :sample.hello/0", fn ->
      :sample.hello
    end

    filename = "sample.erl"
    with_file filename, erlang_module_code, fn ->
      assert c(filename) == [:sample]
      assert :sample.hello == :world
    end
  after
    cleanup_modules([:sample])
  end


  test "c helper skips unknown files" do
    assert_raise UndefinedFunctionError, "undefined function: :sample.hello/0", fn ->
      :sample.hello
    end

   filenames = ["sample.erl", "not_found.ex", "sample2.ex"]
   with_file filenames, [erlang_module_code, "", another_test_module], fn ->
      assert c(filenames) |> Enum.sort == [Sample2, :sample]
      assert :sample.hello == :world
      assert Sample2.hello == :world
    end
  after
    cleanup_modules([:sample, Sample2])
  end


  test "l helper" do
    assert_raise UndefinedFunctionError, "undefined function: Sample.run/0", fn ->
      Sample.run
    end

    assert l(:non_existent_module) == {:error, :nofile}

    filename = "sample.ex"
    with_file filename, test_module_code, fn ->
      assert c(filename) == [Sample]
      assert Sample.run == :run

      File.write! filename, "defmodule Sample do end"
      elixirc("sample.ex")

      assert l(Sample) == {:module, Sample}
      assert_raise UndefinedFunctionError, "undefined function: Sample.run/0", fn ->
        Sample.run
      end
    end
  after
    # Clean up the old version left over after l()
    cleanup_modules([Sample])
  end

  test "r helper unavailable" do
    assert_raise ArgumentError, "could not load nor find module: :non_existent_module", fn ->
      r :non_existent_module
    end
  end

  test "r helper elixir" do
    assert_raise UndefinedFunctionError, "undefined function: Sample.run/0", fn ->
      Sample.run
    end

    filename = "sample.ex"
    with_file filename, test_module_code, fn ->
      assert capture_io(:stderr, fn ->
        assert c(filename) == [Sample]
        assert Sample.run == :run

        File.write! filename, "defmodule Sample do end"
        assert { :reloaded, Sample, [Sample] } = r(Sample)
        assert_raise UndefinedFunctionError, "undefined function: Sample.run/0", fn ->
          Sample.run
        end
      end) =~ ~r"^.*?sample\.ex:1: warning: redefining module Sample\n$"
    end
  after
    # Clean up old version produced by the r helper
    cleanup_modules([Sample])
  end

  test "r helper erlang" do
    assert_raise UndefinedFunctionError, "undefined function: :sample.hello/0", fn ->
      :sample.hello
    end

    filename = "sample.erl"
    with_file filename, erlang_module_code, fn ->
      assert c(filename) == [:sample]
      assert :sample.hello == :world

      File.write!(filename, other_erlang_module_code)
      assert { :reloaded, :sample, [:sample] } = r(:sample)
      assert :sample.hello == :bye
    end
  after
    cleanup_modules([:sample])
  end

  defp test_module_code do
    """
    defmodule Sample do
      def run do
        :run
      end
    end
    """
  end

  defp another_test_module do
    """
    defmodule Sample2 do
      def hello do
        :world
      end
    end
    """
  end

  defp erlang_module_code do
    """
    -module(sample).
    -export([hello/0]).
    hello() -> world.
    """
  end

  defp other_erlang_module_code do
    """
    -module(sample).
    -export([hello/0]).
    hello() -> bye.
    """
  end

  defp cleanup_modules(mods) do
    Enum.each mods, fn mod ->
      File.rm("#{mod}.beam")
      :code.purge(mod)
      true = :code.delete(mod)
    end
  end

  defp with_file(names, codes, fun) when is_list(names) and is_list(codes) do
    Enum.each Enum.zip(names, codes), fn { name, code } ->
      File.write! name, code
    end

    try do
      fun.()
    after
      Enum.each names, &File.rm/1
    end
  end

  defp with_file(name, code, fun) do
    with_file(List.wrap(name), List.wrap(code), fun)
  end

  defp elixirc(args) do
    executable = Path.expand("../../../../bin/elixirc", __DIR__)
    System.cmd("#{executable}#{executable_extension} #{args}")
  end

  defp iex_path do
    Path.expand "../..", __DIR__
  end

  if match? { :win32, _ }, :os.type do
    defp executable_extension, do: ".bat"
  else
    defp executable_extension, do: ""
  end
end
