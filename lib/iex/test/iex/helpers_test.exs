Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.HelpersTest do
  use IEx.Case

  import IEx.Helpers

  describe "whereami" do
    test "is disabled by default" do
      assert capture_iex("whereami()") =~ "Pry session is not currently enabled"
    end

    test "shows current location for custom envs" do
      whereami = capture_iex("whereami()", [], env: %{__ENV__ | line: 3})
      assert whereami =~ "test/iex/helpers_test.exs:3"
      assert whereami =~ "3: defmodule IEx.HelpersTest do"
    end

    test "prints message when location is not available" do
      whereami = capture_iex("whereami()", [], env: %{__ENV__ | line: 30000})
      assert whereami =~ "test/iex/helpers_test.exs:30000"
      assert whereami =~ "Could not extract source snippet. Location is not available."

      whereami = capture_iex("whereami()", [], env: %{__ENV__ | file: "nofile", line: 1})
      assert whereami =~ "nofile:1"
      assert whereami =~ "Could not extract source snippet. Location is not available."
    end
  end

  if :erlang.system_info(:otp_release) >= '20' do
    describe "breakpoints" do
      setup do
        on_exit(fn -> IEx.Pry.remove_breaks() end)
      end

      test "sets up a breakpoint with macro syntax" do
        assert break!(URI.decode_query() / 2) == 1
        assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
      end

      test "sets up a breakpoint on the given module" do
        assert break!(URI, :decode_query, 2) == 1
        assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
      end

      test "resets breaks on the given id" do
        assert break!(URI, :decode_query, 2) == 1
        assert reset_break(1) == :ok
        assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 0}]
      end

      test "resets breaks on the given module" do
        assert break!(URI, :decode_query, 2) == 1
        assert reset_break(URI, :decode_query, 2) == :ok
        assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 0}]
      end

      test "removes breaks in the given module" do
        assert break!(URI.decode_query() / 2) == 1
        assert remove_breaks(URI) == :ok
        assert IEx.Pry.breaks() == []
      end

      test "removes breaks on all modules" do
        assert break!(URI.decode_query() / 2) == 1
        assert remove_breaks() == :ok
        assert IEx.Pry.breaks() == []
      end

      test "errors when setting up a break with no beam" do
        assert_raise RuntimeError,
                     "could not set breakpoint, could not find .beam file for IEx.HelpersTest",
                     fn -> break!(__MODULE__, :setup, 1) end
      end

      test "errors when setting up a break for unknown function" do
        assert_raise RuntimeError,
                     "could not set breakpoint, unknown function/macro URI.unknown/2",
                     fn -> break!(URI, :unknown, 2) end
      end

      test "errors for non-Elixir modules" do
        assert_raise RuntimeError,
                     "could not set breakpoint, module :elixir was not written in Elixir",
                     fn -> break!(:elixir, :unknown, 2) end
      end

      test "prints table with breaks" do
        break!(URI, :decode_query, 2)

        assert capture_io(fn -> breaks() end) == """

                ID   Module.function/arity   Pending stops
               ---- ----------------------- ---------------
                1    URI.decode_query/2      1

               """

        assert capture_io(fn -> URI.decode_query("foo=bar", %{}) end) != ""

        assert capture_io(fn -> breaks() end) == """

                ID   Module.function/arity   Pending stops
               ---- ----------------------- ---------------
                1    URI.decode_query/2      0

               """

        assert capture_io(fn -> URI.decode_query("foo=bar", %{}) end) == ""

        assert capture_io(fn -> breaks() end) == """

                ID   Module.function/arity   Pending stops
               ---- ----------------------- ---------------
                1    URI.decode_query/2      0

               """
      end

      test "does not print table when there are no breaks" do
        assert capture_io(fn -> breaks() end) == "No breakpoints set\n"
      end
    end
  end

  describe "open" do
    @iex_helpers "iex/lib/iex/helpers.ex"
    @elixir_erl "elixir/src/elixir.erl"
    {:ok, vsn} = :application.get_key(:stdlib, :vsn)
    @lists_erl "lib/stdlib-#{vsn}/src/lists.erl"
    @httpc_erl "src/http_client/httpc.erl"
    @editor System.get_env("ELIXIR_EDITOR")

    test "opens __FILE__ and __LINE__" do
      System.put_env("ELIXIR_EDITOR", "echo __LINE__:__FILE__")

      assert capture_iex("open({#{inspect(__ENV__.file)}, 3})") |> maybe_trim_quotes() ==
               "3:#{__ENV__.file}"
    after
      System.put_env("ELIXIR_EDITOR", @editor)
    end

    test "opens Elixir module" do
      assert capture_iex("open(IEx.Helpers)") |> maybe_trim_quotes() =~ ~r/#{@iex_helpers}:1$/
    end

    test "opens function" do
      assert capture_iex("open(h)") |> maybe_trim_quotes() =~ ~r/#{@iex_helpers}:\d+$/
    end

    test "opens function/arity" do
      assert capture_iex("open(b/1)") |> maybe_trim_quotes() =~ ~r/#{@iex_helpers}:\d+$/
      assert capture_iex("open(h/0)") |> maybe_trim_quotes() =~ ~r/#{@iex_helpers}:\d+$/
    end

    test "opens module.function" do
      assert capture_iex("open(IEx.Helpers.b)") |> maybe_trim_quotes() =~ ~r/#{@iex_helpers}:\d+$/
      assert capture_iex("open(IEx.Helpers.h)") |> maybe_trim_quotes() =~ ~r/#{@iex_helpers}:\d+$/
    end

    test "opens module.function/arity" do
      assert capture_iex("open(IEx.Helpers.b/1)") |> maybe_trim_quotes() =~
               ~r/#{@iex_helpers}:\d+$/

      assert capture_iex("open(IEx.Helpers.h/0)") |> maybe_trim_quotes() =~
               ~r/#{@iex_helpers}:\d+$/
    end

    test "opens Erlang module" do
      assert capture_iex("open(:elixir)") |> maybe_trim_quotes() =~ ~r/#{@elixir_erl}:\d+$/
    end

    test "opens Erlang module.function" do
      assert capture_iex("open(:elixir.start)") |> maybe_trim_quotes() =~ ~r/#{@elixir_erl}:\d+$/
    end

    test "opens Erlang module.function/arity" do
      assert capture_iex("open(:elixir.start/2)") |> maybe_trim_quotes() =~
               ~r/#{@elixir_erl}:\d+$/
    end

    test "opens OTP lists module" do
      assert capture_iex("open(:lists)") |> maybe_trim_quotes() =~ ~r/#{@lists_erl}:\d+$/
    end

    test "opens OTP lists module.function" do
      assert capture_iex("open(:lists.reverse)") |> maybe_trim_quotes() =~ ~r/#{@lists_erl}:\d+$/
    end

    test "opens OTP lists module.function/arity" do
      assert capture_iex("open(:lists.reverse/1)") |> maybe_trim_quotes() =~
               ~r/#{@lists_erl}:\d+$/
    end

    test "opens OTP httpc module" do
      assert capture_iex("open(:httpc)") |> maybe_trim_quotes() =~ ~r/#{@httpc_erl}:\d+$/
    end

    test "opens OTP httpc module.function" do
      assert capture_iex("open(:httpc.request)") |> maybe_trim_quotes() =~ ~r/#{@httpc_erl}:\d+$/
    end

    test "opens OTP httpc module.function/arity" do
      assert capture_iex("open(:httpc.request/1)") |> maybe_trim_quotes() =~
               ~r/#{@httpc_erl}:\d+$/
    end

    test "errors OTP preloaded module" do
      assert capture_iex("open(:init)") == "Could not open: :init. Module is not available."
    end

    test "errors if module is not available" do
      assert capture_iex("open(:unknown)") == "Could not open: :unknown. Module is not available."
    end

    test "errors if module.function is not available" do
      assert capture_iex("open(:unknown.unknown)") ==
               "Could not open: :unknown.unknown. Module is not available."

      assert capture_iex("open(:elixir.unknown)") ==
               "Could not open: :elixir.unknown. Function/macro is not available."

      assert capture_iex("open(:lists.unknown)") ==
               "Could not open: :lists.unknown. Function/macro is not available."

      assert capture_iex("open(:httpc.unknown)") ==
               "Could not open: :httpc.unknown. Function/macro is not available."
    end

    test "errors if module.function/arity is not available" do
      assert capture_iex("open(:unknown.start/10)") ==
               "Could not open: :unknown.start/10. Module is not available."

      assert capture_iex("open(:elixir.start/10)") ==
               "Could not open: :elixir.start/10. Function/macro is not available."

      assert capture_iex("open(:lists.reverse/10)") ==
               "Could not open: :lists.reverse/10. Function/macro is not available."

      assert capture_iex("open(:httpc.request/10)") ==
               "Could not open: :httpc.request/10. Function/macro is not available."
    end

    test "errors if module is in-memory" do
      assert capture_iex("defmodule Foo, do: nil ; open(Foo)") =~
               ~r"Invalid arguments for open helper:"
    after
      cleanup_modules([Foo])
    end

    test "opens the current pry location" do
      assert capture_iex("open()", [], env: %{__ENV__ | line: 3}) |> maybe_trim_quotes() ==
               "#{__ENV__.file}:3"
    end

    test "errors if prying is not available" do
      assert capture_iex("open()") == "Pry session is not currently enabled"
    end

    test "opens given {file, line}" do
      assert capture_iex("open({#{inspect(__ENV__.file)}, 3})") |> maybe_trim_quotes() ==
               "#{__ENV__.file}:3"
    end

    test "errors when given {file, line} is not available" do
      assert capture_iex("open({~s[foo], 3})") ==
               "Could not open: \"foo\". File is not available."
    end

    defp maybe_trim_quotes(string) do
      case :os.type() do
        {:win32, _} -> String.replace(string, "\"", "")
        _ -> string
      end
    end
  end

  describe "clear" do
    test "clear the screen with ansi" do
      Application.put_env(:elixir, :ansi_enabled, true)
      assert capture_iex("clear()") == "\e[H\e[2J"

      Application.put_env(:elixir, :ansi_enabled, false)

      assert capture_iex("clear()") =~
               "Cannot clear the screen because ANSI escape codes are not enabled on this shell"
    after
      Application.delete_env(:elixir, :ansi_enabled)
    end
  end

  describe "runtime_info" do
    test "shows vm information" do
      assert "\n## System and architecture" <> _ = capture_io(fn -> runtime_info() end)
    end
  end

  describe "h" do
    test "shows help" do
      assert "* IEx.Helpers\n\nWelcome to Interactive Elixir" <> _ = capture_iex("h()")
    end

    test "prints module documentation" do
      assert "* IEx.Helpers\n\nWelcome to Interactive Elixir" <> _ =
               capture_io(fn -> h(IEx.Helpers) end)

      assert capture_io(fn -> h(:whatever) end) ==
               "Could not load module :whatever, got: nofile\n"

      assert capture_io(fn -> h(:lists) end) ==
               ":lists is an Erlang module and, as such, it does not have Elixir-style docs\n"
    end

    test "prints function documentation" do
      pwd_h = "* def pwd()\n\nPrints the current working directory.\n\n"
      c_h = "* def c(files, path \\\\ :in_memory)\n\nCompiles the given files."
      eq_h = "* def ==(left, right)\n\nReturns `true` if the two items are equal.\n\n"

      assert capture_io(fn -> h(IEx.Helpers.pwd() / 0) end) =~ pwd_h
      assert capture_io(fn -> h(IEx.Helpers.c() / 2) end) =~ c_h
      assert capture_io(fn -> h(== / 2) end) =~ eq_h

      assert capture_io(fn -> h(IEx.Helpers.c() / 1) end) =~ c_h
      assert capture_io(fn -> h(pwd) end) =~ pwd_h
    end

    test "prints __info__ documentation" do
      h_output_module = capture_io(fn -> h(Module.__info__()) end)
      assert capture_io(fn -> h(Module.UnlikelyTo.Exist.__info__()) end) == h_output_module
      assert capture_io(fn -> h(Module.UnlikelyTo.Exist.__info__() / 1) end) == h_output_module

      assert capture_io(fn -> h(__info__) end) ==
               "No documentation for Kernel.__info__ was found\n"
    end

    test "considers underscored functions without docs by default" do
      content = """
      defmodule Sample do
        def __foo__(), do: 0
        @doc "Bar doc"
        def __bar__(), do: 1
      end
      """

      filename = "sample.ex"

      with_file(filename, content, fn ->
        assert c(filename, ".") == [Sample]

        assert capture_io(fn -> h(Sample.__foo__()) end) ==
                 "No documentation for Sample.__foo__ was found\n"

        assert capture_io(fn -> h(Sample.__bar__()) end) == "* def __bar__()\n\nBar doc\n"

        assert capture_io(fn -> h(Sample.__foo__() / 0) end) ==
                 "No documentation for Sample.__foo__/0 was found\n"

        assert capture_io(fn -> h(Sample.__bar__() / 0) end) == "* def __bar__()\n\nBar doc\n"
      end)
    after
      cleanup_modules([Sample])
    end

    test "prints callback documentation when function docs are not available" do
      behaviour = """
      defmodule MyBehaviour do
        @doc "Docs for MyBehaviour.first"
        @callback first(integer) :: integer
        @callback second(integer) :: integer
        @callback second(integer, integer) :: integer
      end
      """

      impl = """
      defmodule Impl do
        @behaviour MyBehaviour
        def first(0), do: 0
        @doc "Docs for Impl.second/1"
        def second(0), do: 0
        @doc "Docs for Impl.second/2"
        def second(0, 0), do: 0
      end
      """

      files = ["my_behaviour.ex", "impl.ex"]

      with_file(files, [behaviour, impl], fn ->
        assert c(files, ".") |> Enum.sort() == [Impl, MyBehaviour]

        assert capture_io(fn -> h(Impl.first() / 1) end) ==
                 "* @callback first(integer()) :: integer()\n\nDocs for MyBehaviour.first\n"

        assert capture_io(fn -> h(Impl.second() / 1) end) ==
                 "* def second(int)\n\nDocs for Impl.second/1\n"

        assert capture_io(fn -> h(Impl.second() / 2) end) ==
                 "* def second(int1, int2)\n\nDocs for Impl.second/2\n"

        assert capture_io(fn -> h(Impl.first()) end) ==
                 "* @callback first(integer()) :: integer()\n\nDocs for MyBehaviour.first\n"

        assert capture_io(fn -> h(Impl.second()) end) ==
                 "* def second(int)\n\nDocs for Impl.second/1\n* def second(int1, int2)\n\nDocs for Impl.second/2\n"

        assert capture_io(fn -> h(MyBehaviour.first()) end) == """
               No documentation for function MyBehaviour.first was found, but there is a callback with the same name.
               You can view callback documentations with the b/1 helper.\n
               """

        assert capture_io(fn -> h(MyBehaviour.second() / 2) end) == """
               No documentation for function MyBehaviour.second/2 was found, but there is a callback with the same name.
               You can view callback documentations with the b/1 helper.\n
               """

        assert capture_io(fn -> h(MyBehaviour.second() / 3) end) ==
                 "No documentation for MyBehaviour.second/3 was found\n"
      end)
    after
      cleanup_modules([Impl, MyBehaviour])
    end

    test "prints documentation for delegates" do
      filename = "delegate.ex"

      content = """
      defmodule Delegator do
        defdelegate func1, to: Delegated
        @doc "Delegator func2 doc"
        defdelegate func2, to: Delegated
      end
      defmodule Delegated do
        def func1, do: 1
        def func2, do: 2
      end
      """

      with_file(filename, content, fn ->
        assert c(filename, ".") |> Enum.sort() == [Delegated, Delegator]

        assert capture_io(fn -> h(Delegator.func1()) end) ==
                 "* def func1()\n\nSee `Delegated.func1/0`.\n"

        assert capture_io(fn -> h(Delegator.func2()) end) ==
                 "* def func2()\n\nDelegator func2 doc\n"
      end)
    after
      cleanup_modules([Delegated, Delegator])
    end
  end

  describe "b" do
    test "lists all callbacks for a module" do
      assert capture_io(fn -> b(Mix) end) == "No callbacks for Mix were found\n"
      assert capture_io(fn -> b(NoMix) end) == "Could not load module NoMix, got: nofile\n"

      assert capture_io(fn -> b(Mix.SCM) end) =~ """
             @callback accepts_options(app :: atom(), opts()) :: opts() | nil
             @callback checked_out?(opts()) :: boolean()
             """
    end

    test "lists callback with multiple clauses" do
      filename = "multiple_clauses_callback.ex"

      content = """
      defmodule MultipleClauseCallback do
        @callback test(:foo) :: integer
        @callback test(:bar) :: [integer]
      end
      """

      with_file(filename, content, fn ->
        assert c(filename, ".") |> Enum.sort() == [MultipleClauseCallback]

        assert capture_io(fn -> b(MultipleClauseCallback) end) =~ """
               @callback test(:foo) :: integer()
               @callback test(:bar) :: [integer()]
               """
      end)
    after
      cleanup_modules([MultipleClauseCallback])
    end

    test "prints callback documentation" do
      assert capture_io(fn -> b(Mix.Task.stop()) end) ==
               "No documentation for Mix.Task.stop was found\n"

      assert capture_io(fn -> b(Mix.Task.run()) end) =~
               "* @callback run(command_line_args :: [binary()]) :: any()\n\nA task needs to implement `run`"

      assert capture_io(fn -> b(NoMix.run()) end) == "Could not load module NoMix, got: nofile\n"

      assert capture_io(fn -> b(Exception.message() / 1) end) ==
               "* @callback message(t()) :: String.t()\n\n\n"
    end
  end

  describe "t" do
    test "prints when there is no type information" do
      assert capture_io(fn -> t(IEx) end) == "No type information for IEx was found\n"
    end

    test "prints all types in module" do
      # Test that it shows at least two types
      assert Enum.count(capture_io(fn -> t(Enum) end) |> String.split("\n"), fn line ->
               String.starts_with?(line, "@type")
             end) >= 2
    end

    test "prints type information" do
      assert "@type t() :: " <> _ = capture_io(fn -> t(Enum.t()) end)
      assert capture_io(fn -> t(Enum.t()) end) == capture_io(fn -> t(Enum.t() / 0) end)

      assert "@opaque t(value)\n@type t() :: t(term())\n" = capture_io(fn -> t(MapSet.t()) end)
      assert capture_io(fn -> t(URI.t()) end) == capture_io(fn -> t(URI.t() / 0) end)
    end

    test "prints type documentation" do
      content = """
      defmodule TypeSample do
        @typedoc "An id with description."
        @type id_with_desc :: {number, String.t}
      end
      """

      filename = "typesample.ex"

      with_file(filename, content, fn ->
        assert c(filename, ".") == [TypeSample]

        assert capture_io(fn -> t(TypeSample.id_with_desc() / 0) end) == """
               An id with description.
               @type id_with_desc() :: {number(), String.t()}
               """

        assert capture_io(fn -> t(TypeSample.id_with_desc()) end) == """
               An id with description.
               @type id_with_desc() :: {number(), String.t()}
               """
      end)
    after
      cleanup_modules([TypeSample])
    end
  end

  describe "s" do
    test "prints when there is no spec information" do
      assert capture_io(fn -> s(IEx.Remsh) end) == "No specification for IEx.Remsh was found\n"
    end

    test "prints all specs in module" do
      # Test that it shows at least two specs
      assert Enum.count(capture_io(fn -> s(Process) end) |> String.split("\n"), fn line ->
               String.starts_with?(line, "@spec")
             end) >= 2
    end

    test "prints specs" do
      assert Enum.count(capture_io(fn -> s(Process.flag()) end) |> String.split("\n"), fn line ->
               String.starts_with?(line, "@spec")
             end) >= 2

      assert capture_io(fn -> s(Process.register() / 2) end) ==
               "@spec register(pid() | port(), atom()) :: true\n"

      assert capture_io(fn -> s(struct) end) ==
               "@spec struct(module() | struct(), Enum.t()) :: struct()\n"
    end
  end

  describe "v" do
    test "returns history" do
      assert "** (RuntimeError) v(0) is out of bounds" <> _ = capture_iex("v(0)")
      assert "** (RuntimeError) v(1) is out of bounds" <> _ = capture_iex("v(1)")
      assert "** (RuntimeError) v(-1) is out of bounds" <> _ = capture_iex("v(-1)")
      assert capture_iex("1\n2\nv(2)") == "1\n2\n2"
      assert capture_iex("1\n2\nv(2)") == capture_iex("1\n2\nv(-1)")
      assert capture_iex("1\n2\nv(2)") == capture_iex("1\n2\nv()")
    end
  end

  describe "flush" do
    test "flushes messages" do
      assert capture_io(fn ->
               send(self(), :hello)
               flush()
             end) == ":hello\n"
    end
  end

  describe "pwd" do
    test "prints the working directory" do
      File.cd!(iex_path(), fn ->
        assert capture_io(fn -> pwd() end) =~ ~r"lib[\\/]iex\n$"
      end)
    end
  end

  describe "ls" do
    test "lists the current directory" do
      File.cd!(iex_path(), fn ->
        paths =
          capture_io(fn -> ls() end)
          |> String.split()
          |> Enum.map(&String.trim/1)

        assert "ebin" in paths
        assert "mix.exs" in paths
      end)
    end

    test "lists the given directory" do
      assert capture_io(fn -> ls("~") end) == capture_io(fn -> ls(System.user_home()) end)
    end
  end

  describe "exports" do
    test "prints module exports" do
      exports = capture_io(fn -> exports(IEx.Autocomplete) end)
      assert exports == "expand/1      expand/2      exports/1     \n"
    end
  end

  describe "import_file" do
    test "imports a file" do
      with_file("dot-iex", "variable = :hello\nimport IO", fn ->
        capture_io(:stderr, fn ->
          assert "** (CompileError) iex:1: undefined function variable/0" <> _ =
                   capture_iex("variable")
        end)

        assert "** (CompileError) iex:1: undefined function puts/1" <> _ =
                 capture_iex("puts \"hi\"")

        assert capture_iex("import_file \"dot-iex\"\nvariable\nputs \"hi\"") ==
                 "IO\n:hello\nhi\n:ok"
      end)
    end

    test "imports a file that imports another file" do
      dot = "parent = true\nimport_file \"dot-iex-1\""
      dot_1 = "variable = :hello\nimport IO"

      with_file(["dot-iex", "dot-iex-1"], [dot, dot_1], fn ->
        capture_io(:stderr, fn ->
          assert "** (CompileError) iex:1: undefined function parent/0" <> _ =
                   capture_iex("parent")
        end)

        assert "** (CompileError) iex:1: undefined function puts/1" <> _ =
                 capture_iex("puts \"hi\"")

        assert capture_iex("import_file \"dot-iex\"\nvariable\nputs \"hi\"\nparent") ==
                 "IO\n:hello\nhi\n:ok\ntrue"
      end)
    end

    test "raises if file is missing" do
      failing = capture_iex("import_file \"nonexistent\"")
      assert "** (File.Error) could not read file" <> _ = failing
      assert failing =~ "no such file or directory"
    end

    test "does not raise if file is missing and using import_file_if_available" do
      assert "nil" == capture_iex("import_file_if_available \"nonexistent\"")
    end
  end

  describe "import_if_available" do
    test "imports a module only if available" do
      assert "nil" == capture_iex("import_if_available NoSuchModule")
      assert "[1, 2, 3]" == capture_iex("import_if_available Integer; digits 123")

      assert "[1, 2, 3]" ==
               capture_iex("import_if_available Integer, only: [digits: 1]; digits 123")
    end
  end

  describe "c" do
    test "compiles a file" do
      assert_raise UndefinedFunctionError, ~r"function Sample\.run/0 is undefined", fn ->
        Sample.run()
      end

      filename = "sample.ex"

      with_file(filename, test_module_code(), fn ->
        assert c(Path.expand(filename)) == [Sample]
        refute File.exists?("Elixir.Sample.beam")
        assert Sample.run() == :run
      end)
    after
      cleanup_modules([Sample])
    end

    test "handles errors" do
      ExUnit.CaptureIO.capture_io(fn ->
        with_file("sample.ex", "raise \"oops\"", fn ->
          assert_raise CompileError, fn -> c("sample.ex") end
        end)
      end)
    end

    test "compiles a file with multiple modules " do
      assert_raise UndefinedFunctionError, ~r"function Sample.run/0 is undefined", fn ->
        Sample.run()
      end

      filename = "sample.ex"

      with_file(filename, test_module_code() <> "\n" <> another_test_module(), fn ->
        assert c(filename) |> Enum.sort() == [Sample, Sample2]
        assert Sample.run() == :run
        assert Sample2.hello() == :world
      end)
    after
      cleanup_modules([Sample, Sample2])
    end

    test "compiles multiple modules" do
      assert_raise UndefinedFunctionError, ~r"function Sample.run/0 is undefined", fn ->
        Sample.run()
      end

      filenames = ["sample1.ex", "sample2.ex"]

      with_file(filenames, [test_module_code(), another_test_module()], fn ->
        assert c(filenames) |> Enum.sort() == [Sample, Sample2]
        assert Sample.run() == :run
        assert Sample2.hello() == :world
      end)
    after
      cleanup_modules([Sample, Sample2])
    end

    test "compiles Erlang modules" do
      assert_raise UndefinedFunctionError, ~r"function :sample.hello/0 is undefined", fn ->
        :sample.hello()
      end

      filename = "sample.erl"

      with_file(filename, erlang_module_code(), fn ->
        assert c(filename) == [:sample]
        assert :sample.hello() == :world
        refute File.exists?("sample.beam")
      end)
    after
      cleanup_modules([:sample])
    end

    test "skips unknown files" do
      assert_raise UndefinedFunctionError, ~r"function :sample.hello/0 is undefined", fn ->
        :sample.hello()
      end

      filenames = ["sample.erl", "not_found.ex", "sample2.ex"]

      with_file(filenames, [erlang_module_code(), "", another_test_module()], fn ->
        assert c(filenames) |> Enum.sort() == [Sample2, :sample]
        assert :sample.hello() == :world
        assert Sample2.hello() == :world
      end)
    after
      cleanup_modules([:sample, Sample2])
    end

    test "compiles file in path" do
      assert_raise UndefinedFunctionError, ~r"function Sample\.run/0 is undefined", fn ->
        Sample.run()
      end

      filename = "sample.ex"

      with_file(filename, test_module_code(), fn ->
        assert c(filename, ".") == [Sample]
        assert File.exists?("Elixir.Sample.beam")
        assert Sample.run() == :run
      end)
    after
      cleanup_modules([Sample])
    end
  end

  describe "l" do
    test "loads a given module" do
      assert_raise UndefinedFunctionError, ~r"function Sample.run/0 is undefined", fn ->
        Sample.run()
      end

      assert l(:nonexistent_module) == {:error, :nofile}

      filename = "sample.ex"

      with_file(filename, test_module_code(), fn ->
        assert c(filename, ".") == [Sample]
        assert Sample.run() == :run

        File.write!(filename, "defmodule Sample do end")
        elixirc(["sample.ex"])

        assert l(Sample) == {:module, Sample}

        assert_raise UndefinedFunctionError,
                     "function Sample.run/0 is undefined or private",
                     fn ->
                       Sample.run()
                     end
      end)
    after
      # Clean up the old version left over after l()
      cleanup_modules([Sample])
    end
  end

  describe "nl" do
    test "loads a given module on the given nodes" do
      assert nl(:nonexistent_module) == {:error, :nofile}
      assert nl([node()], Enum) == {:ok, [{:nonode@nohost, :loaded, Enum}]}
      assert nl([:nosuchnode@badhost], Enum) == {:ok, [{:nosuchnode@badhost, :badrpc, :nodedown}]}

      capture_log(fn ->
        assert nl([node()], :lists) == {:ok, [{:nonode@nohost, :error, :sticky_directory}]}
      end)
    end
  end

  describe "r" do
    test "raises when reloading a nonexistent module" do
      assert_raise ArgumentError, "could not load nor find module: :nonexistent_module", fn ->
        r(:nonexistent_module)
      end
    end

    test "reloads elixir modules" do
      assert_raise UndefinedFunctionError,
                   ~r"function Sample.run/0 is undefined \(module Sample is not available\)",
                   fn ->
                     Sample.run()
                   end

      filename = "sample.ex"

      with_file(filename, test_module_code(), fn ->
        assert capture_io(:stderr, fn ->
                 assert c(filename, ".") == [Sample]
                 assert Sample.run() == :run

                 File.write!(filename, "defmodule Sample do end")
                 assert {:reloaded, Sample, [Sample]} = r(Sample)

                 assert_raise UndefinedFunctionError,
                              "function Sample.run/0 is undefined or private",
                              fn ->
                                Sample.run()
                              end
               end) =~ "redefining module Sample (current version loaded from Elixir.Sample.beam)"
      end)
    after
      # Clean up old version produced by the r helper
      cleanup_modules([Sample])
    end

    test "reloads Erlang modules" do
      assert_raise UndefinedFunctionError, ~r"function :sample.hello/0 is undefined", fn ->
        :sample.hello()
      end

      filename = "sample.erl"

      with_file(filename, erlang_module_code(), fn ->
        assert c(filename, ".") == [:sample]
        assert :sample.hello() == :world

        File.write!(filename, other_erlang_module_code())
        assert {:reloaded, :sample, [:sample]} = r(:sample)
        assert :sample.hello() == :bye
      end)
    after
      cleanup_modules([:sample])
    end
  end

  describe "pid" do
    test "builds a pid from string" do
      assert inspect(pid("0.32767.3276")) == "#PID<0.32767.3276>"
      assert inspect(pid("0.5.6")) == "#PID<0.5.6>"

      assert_raise ArgumentError, fn ->
        pid("0.6.-6")
      end
    end

    test "builds a pid from integers" do
      assert inspect(pid(0, 32767, 3276)) == "#PID<0.32767.3276>"
      assert inspect(pid(0, 5, 6)) == "#PID<0.5.6>"

      assert_raise FunctionClauseError, fn ->
        pid(0, 6, -6)
      end
    end
  end

  describe "i" do
    test "prints information about the data type" do
      output = capture_io(fn -> i(:ok) end)

      assert output =~
               String.trim_trailing("""
               Term
                 :ok
               Data type
                 Atom
               Reference modules
                 Atom
               """)
    end

    test "handles functions that don't display result" do
      output = capture_io(fn -> i(IEx.dont_display_result()) end)

      assert output =~
               String.trim_trailing("""
               Term
                 :"do not show this result in output"
               Data type
                 Atom
               Description
                 This atom is returned by IEx when a function that should not print its
                 return value on screen is executed.
               """)
    end
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
    Enum.each(mods, fn mod ->
      File.rm("#{mod}.beam")
      :code.purge(mod)
      true = :code.delete(mod)
    end)
  end

  defp with_file(names, codes, fun) when is_list(names) and is_list(codes) do
    Enum.each(Enum.zip(names, codes), fn {name, code} ->
      File.write!(name, code)
    end)

    try do
      fun.()
    after
      Enum.each(names, &File.rm/1)
    end
  end

  defp with_file(name, code, fun) do
    with_file(List.wrap(name), List.wrap(code), fun)
  end

  defp elixirc(args) do
    executable = Path.expand("../../../../bin/elixirc", __DIR__)
    System.cmd("#{executable}#{executable_extension()}", args, stderr_to_stdout: true)
  end

  defp iex_path do
    Path.expand("../..", __DIR__)
  end

  if match?({:win32, _}, :os.type()) do
    defp executable_extension, do: ".bat"
  else
    defp executable_extension, do: ""
  end
end
