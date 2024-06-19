Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.HelpersTest do
  use IEx.Case

  import IEx.Helpers

  @compile {:no_warn_undefined, [:sample, Sample, Sample2]}

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

  describe "breakpoints" do
    setup do
      on_exit(fn -> IEx.Pry.remove_breaks() end)
    end

    test "sets up a breakpoint with capture syntax" do
      assert break!(URI.decode_query() / 2) == 1
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "sets up a breakpoint with call syntax" do
      assert break!(URI.decode_query(_, %{})) == 1
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "sets up a breakpoint with guards syntax" do
      assert break!(URI.decode_query(_, map) when is_map(map)) == 1
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "sets up a breakpoint on the given module" do
      assert break!(URI, :decode_query, 2) == 1
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "resets breaks on the given ID" do
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

    test "errors when setting up a breakpoint with invalid guard" do
      assert capture_io(:stderr, fn ->
               assert_raise CompileError, fn ->
                 break!(URI.decode_query(_, map) when is_whatever(map))
               end
             end) =~ "cannot find or invoke local is_whatever/1"
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

  describe "open" do
    @iex_helpers "iex/lib/iex/helpers.ex"
    @elixir_erl "elixir/src/elixir.erl"
    @lists_erl Application.app_dir(:stdlib, "src/lists.erl")
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

    # Some installations remove the source file once Erlang is compiled. See #7348.
    if File.regular?(@lists_erl) do
      test "opens OTP lists module" do
        assert capture_iex("open(:lists)") |> maybe_trim_quotes() =~ ~r/#{@lists_erl}:\d+$/
      end

      test "opens OTP lists module.function" do
        assert capture_iex("open(:lists.reverse)") |> maybe_trim_quotes() =~
                 ~r/#{@lists_erl}:\d+$/
      end

      test "opens OTP lists module.function/arity" do
        assert capture_iex("open(:lists.reverse/1)") |> maybe_trim_quotes() =~
                 ~r/#{@lists_erl}:\d+$/
      end
    end

    # Some installations remove the source file once Erlang is compiled. See #7348.
    if File.regular?(@httpc_erl) do
      test "opens OTP httpc module" do
        assert capture_iex("open(:httpc)") |> maybe_trim_quotes() =~ ~r/#{@httpc_erl}:\d+$/
      end

      test "opens OTP httpc module.function" do
        assert capture_iex("open(:httpc.request)") |> maybe_trim_quotes() =~
                 ~r/#{@httpc_erl}:\d+$/
      end

      test "opens OTP httpc module.function/arity" do
        assert capture_iex("open(:httpc.request/1)") |> maybe_trim_quotes() =~
                 ~r/#{@httpc_erl}:\d+$/
      end
    end

    test "errors OTP preloaded module" do
      assert capture_iex("open(:init)") =~ ~r"(Could not open)|(Invalid arguments)"
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
      assert capture_iex("open({~s[foo], 3})") =~
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
    test "shows VM information" do
      assert "\n## System and architecture" <> _ = capture_io(fn -> runtime_info() end)

      assert "\n## Loaded OTP applications" <> _ =
               capture_io(fn -> runtime_info([:applications]) end)

      assert "\n## Memory allocators" <> _ = capture_io(fn -> runtime_info([:allocators]) end)
    end
  end

  describe "h" do
    test "shows help" do
      help = capture_iex("h()")
      assert help =~ "IEx.Helpers"
      assert help =~ "Welcome to Interactive Elixir"
    end

    @tag :erlang_doc
    test "prints Erlang module documentation" do
      captured = capture_io(fn -> h(:timer) end)
      assert captured =~ "This module provides useful functions related to time."
    end

    @tag :erlang_doc
    test "prints Erlang module function specs" do
      captured = capture_io(fn -> h(:timer.sleep() / 1) end)

      # TODO remove once we require Erlang/OTP 27+
      if System.otp_release() < "27" do
        assert captured =~ ":timer.sleep/1"
        assert captured =~ "-spec sleep(Time) -> ok when Time :: timeout()."
      else
        assert captured =~ "sleep(Time)"
        assert captured =~ "@spec sleep(time) :: :ok when time: timeout()"
      end
    end

    @tag :erlang_doc
    test "handles non-existing Erlang module function" do
      captured = capture_io(fn -> h(:timer.baz() / 1) end)

      # TODO remove once we require Erlang/OTP 27+
      if System.otp_release() < "27" do
        assert captured =~ "No documentation for :timer.baz was found"
      else
        assert captured =~ "No documentation for :timer.baz/1 was found"
      end
    end

    @tag :erlang_doc
    # TODO remove once we require Erlang/OTP 27+
    @tag skip: System.otp_release() < "27"
    test "erlang -moduledoc" do
      filename = "sample.erl"

      code = ~s'''
      -module(sample).
      -moduledoc "Module documentation.".
      '''

      with_file(filename, code, fn ->
        assert c(filename, ".") == [:sample]

        help = capture_iex("h(:sample)")
        assert help =~ "Module documentation."
      end)
    after
      cleanup_modules([:sample])
    end

    @tag :erlang_doc
    # TODO remove once we require Erlang/OTP 27+
    @tag skip: System.otp_release() < "27"
    test "erlang -doc" do
      filename = "sample.erl"

      code = ~s'''
      -module(sample).
      -export([hello/0]).

      -doc "Function documentation.".
      hello() -> world.
      '''

      with_file(filename, code, fn ->
        assert c(filename, ".") == [:sample]

        help = capture_iex("h(:sample.hello)")
        assert help =~ "Function documentation."
      end)
    after
      cleanup_modules([:sample])
    end

    test "prints module documentation" do
      assert "\n                                  IEx.Helpers\n\nWelcome to Interactive Elixir" <>
               _ = capture_io(fn -> h(IEx.Helpers) end)

      assert capture_io(fn -> h(:whatever) end) ==
               "Could not load module :whatever, got: nofile\n"
    end

    test "prints function/macro documentation" do
      pwd_h = """

                                         def pwd()

      Prints the current working directory.

      """

      c_h = """

                              def c(files, path \\\\ :in_memory)

      Compiles the given files.
      """

      eq_h = """

                                     def left == right

        @spec term() == term() :: boolean()

      guard: true

      Equal to operator. Returns `true` if the two terms are equal.
      """

      def_h = """

                              defmacro def(call, expr \\\\ nil)

      Defines a public function with the given name and body.
      """

      assert capture_io(fn -> h(IEx.Helpers.pwd() / 0) end) =~ pwd_h
      assert capture_io(fn -> h(IEx.Helpers.c() / 2) end) =~ c_h
      assert capture_io(fn -> h(== / 2) end) =~ eq_h
      assert capture_io(fn -> h(def / 2) end) =~ def_h

      assert capture_io(fn -> h(IEx.Helpers.c() / 1) end) =~ c_h
      assert capture_io(fn -> h(pwd) end) =~ pwd_h
      assert capture_io(fn -> h(def) end) =~ def_h
    end

    test "prints sigil documentation" do
      assert capture_io(fn -> h(~w//) end) =~ "Handles the sigil `~w` for list of words"
    end

    test "prints __info__ documentation" do
      h_output_module = capture_io(fn -> h(Module.__info__()) end)
      assert capture_io(fn -> h(Module.UnlikelyTo.Exist.__info__()) end) == h_output_module
      assert capture_io(fn -> h(Module.UnlikelyTo.Exist.__info__() / 1) end) == h_output_module

      assert capture_io(fn -> h(__info__) end) ==
               "No documentation for Kernel.__info__ was found\n"
    end

    test "prints documentation metadata" do
      content = """
      defmodule Sample do
        @moduledoc "Sample module"
        @moduledoc deprecated: "Use OtherSample", since: "1.2.3", authors: ["Alice", "Bob"]
        @doc "With metadata"
        @doc since: "1.2.3", author: "Alice"
        @deprecated "Use OtherSample.with_metadata/0"
        def with_metadata(), do: 0
        @doc "Without metadata"
        def without_metadata(), do: 1
      end
      """

      filename = "sample.ex"

      with_file(filename, content, fn ->
        assert c(filename, ".") == [Sample]

        assert capture_io(fn -> h(Sample) end) == """

                                                    Sample

               deprecated: Use OtherSample
               since: 1.2.3

               Sample module

               """

        assert capture_io(fn -> h(Sample.with_metadata()) end) == """

                                             def with_metadata()

               deprecated: Use OtherSample.with_metadata/0
               since: 1.2.3

               With metadata

               """

        assert capture_io(fn -> h(Sample.without_metadata()) end) == """

                                            def without_metadata()

               Without metadata

               """
      end)
    after
      cleanup_modules([Sample])
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

        assert capture_io(fn -> h(Sample.__bar__()) end) == """

                                                def __bar__()

               Bar doc

               """

        assert capture_io(fn -> h(Sample.__foo__() / 0) end) ==
                 "No documentation for Sample.__foo__/0 was found\n"

        assert capture_io(fn -> h(Sample.__bar__() / 0) end) == """

                                                def __bar__()

               Bar doc

               """
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
        @doc delegate_to: {Foo, :bar, 3}
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

        assert capture_io(fn -> h(Impl.first() / 1) end) == """

                                                def first(int)

               delegate_to: Foo.bar/3

               Impl.first/1 has no docs but is a callback for behaviour MyBehaviour. Showing
               callback docs instead.

               @callback first(integer()) :: integer()

               Docs for MyBehaviour.first

               """

        assert capture_io(fn -> h(Impl.second() / 1) end) == """

                                               def second(int)

               Docs for Impl.second/1

               """

        assert capture_io(fn -> h(Impl.second() / 2) end) == """

                                            def second(int1, int2)

               Docs for Impl.second/2

               """

        assert capture_io(fn -> h(Impl.first()) end) == capture_io(fn -> h(Impl.first() / 1) end)

        assert capture_io(fn -> h(Impl.second()) end) == """

                                               def second(int)

               Docs for Impl.second/1


                                            def second(int1, int2)

               Docs for Impl.second/2

               """

        assert capture_io(fn -> h(MyBehaviour.first()) end) == """
               No documentation for function MyBehaviour.first was found, but there is a callback with the same name.
               You can view callback documentation with the b/1 helper.\n
               """

        assert capture_io(fn -> h(MyBehaviour.second() / 2) end) == """
               No documentation for function MyBehaviour.second/2 was found, but there is a callback with the same name.
               You can view callback documentation with the b/1 helper.\n
               """

        assert capture_io(fn -> h(MyBehaviour.second() / 3) end) ==
                 "No documentation for MyBehaviour.second/3 was found\n"
      end)
    after
      cleanup_modules([Impl, MyBehaviour])
    end

    test "prints protocol function docs" do
      output = capture_io(fn -> h(Enumerable.reduce()) end)
      assert output =~ "@spec reduce(t(), acc(), reducer()) :: result()"
      assert output =~ "Reduces the `enumerable`"
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

        assert capture_io(fn -> h(Delegator.func1()) end) == """

                                                 def func1()

               delegate_to: Delegated.func1/0

               """

        assert capture_io(fn -> h(Delegator.func2()) end) == """

                                                 def func2()

               delegate_to: Delegated.func2/0

               Delegator func2 doc

               """
      end)
    after
      cleanup_modules([Delegated, Delegator])
    end

    test "prints type documentation when function docs are not available" do
      content = """
      defmodule MyTypes do
        @type first() :: any()
        @type second(a, b) :: {a, b}
      end
      """

      filename = "my_types.ex"

      with_file(filename, content, fn ->
        assert c(filename, ".") == [MyTypes]

        assert capture_io(fn -> h(MyTypes.first()) end) == """
               No documentation for function MyTypes.first was found, but there is a type with the same name.
               You can view type documentation with the t/1 helper.\n
               """

        assert capture_io(fn -> h(MyTypes.second() / 2) end) == """
               No documentation for function MyTypes.second/2 was found, but there is a type with the same name.
               You can view type documentation with the t/1 helper.\n
               """
      end)
    after
      cleanup_modules([MyTypes])
    end

    test "prints modules compiled without docs" do
      Code.compiler_options(docs: false)

      content = """
      defmodule Sample do
        @spec foo(any()) :: any()
        def foo(arg), do: arg
      end
      """

      filename = "sample.ex"

      with_file(filename, content, fn ->
        assert c(filename, ".") == [Sample]

        assert capture_io(fn -> h(Sample.foo() / 1) end) == """

                                                 Sample.foo/1

                 @spec foo(any()) :: any()

               Module was compiled without docs. Showing only specs.

               """
      end)
    after
      Code.compiler_options(docs: true)
      cleanup_modules([Sample])
    end

    test "does not print docs for @doc false functions" do
      # Here we assert that @doc false works and that we are not leaking
      # IEx.Pry internal functions.
      assert capture_io(fn -> h(IEx.Pry.child_spec()) end) ==
               "No documentation for IEx.Pry.child_spec was found\n"
    end
  end

  describe "b" do
    test "lists all callbacks for an Elixir module" do
      assert capture_io(fn -> b(Mix) end) == "No callbacks for Mix were found\n"
      assert capture_io(fn -> b(NoMix) end) == "Could not load module NoMix, got: nofile\n"

      assert capture_io(fn -> b(Mix.SCM) end) =~ """
             @callback accepts_options(app :: atom(), opts()) :: opts() | nil

             @callback checked_out?(opts()) :: boolean()
             """
    end

    test "lists all callbacks for an Erlang module" do
      output = capture_io(fn -> b(:gen_server) end)

      assert output =~ "@callback init(args :: term()) ::"
      assert output =~ "@callback handle_cast("
      assert output =~ "@callback handle_info("
    end

    test "lists all macrocallbacks for a module" do
      filename = "macrocallbacks.ex"

      content = """
      defmodule Macrocallbacks do
        @macrocallback test(:foo) :: integer
        @macrocallback test(:bar) :: var when var: integer
      end
      """

      with_file(filename, content, fn ->
        assert c(filename, ".") == [Macrocallbacks]
        callbacks = capture_io(fn -> b(Macrocallbacks) end)
        assert callbacks =~ "@macrocallback test(:foo) :: integer()\n"
        assert callbacks =~ "@macrocallback test(:bar) :: var when var: integer()\n"
      end)
    after
      cleanup_modules([Macrocallbacks])
    end

    test "lists all callbacks for a protocol" do
      assert capture_io(fn -> b(Enumerable) end) =~ """
             @callback count(t()) :: {:ok, non_neg_integer()} | {:error, module()}

             @callback member?(t(), term()) :: {:ok, boolean()} | {:error, module()}

             @callback reduce(t(), acc(), reducer()) :: result()
             """
    end

    test "prints protocol function docs" do
      output = capture_io(fn -> b(Enumerable.reduce()) end)
      assert output =~ "@callback reduce(t(), acc(), reducer()) :: result()"
      assert output =~ "Reduces the `enumerable`"
    end

    test "lists callback with multiple clauses" do
      filename = "multiple_clauses_callback.ex"

      content = """
      defmodule MultipleClauseCallback do
        @doc "callback"
        @callback test(:foo) :: integer
        @callback test(:bar) :: [integer]
      end
      """

      with_file(filename, content, fn ->
        assert c(filename, ".") == [MultipleClauseCallback]

        assert capture_io(fn -> b(MultipleClauseCallback) end) =~ """
               @callback test(:foo) :: integer()
               @callback test(:bar) :: [integer()]
               """

        assert capture_io(fn -> b(MultipleClauseCallback.test()) end) =~ """
               @callback test(:foo) :: integer()
               @callback test(:bar) :: [integer()]

               callback
               """
      end)
    after
      cleanup_modules([MultipleClauseCallback])
    end

    test "prints callback documentation" do
      assert capture_io(fn -> b(Mix.Task.stop()) end) ==
               "No documentation for Mix.Task.stop was found\n"

      assert capture_io(fn -> b(Mix.Task.run()) end) =~
               "@callback run(command_line_args :: [binary()]) :: any()\n\nA task needs to implement `run`"

      assert capture_io(fn -> b(NoMix.run()) end) == "Could not load module NoMix, got: nofile\n"

      assert capture_io(fn -> b(Exception.message() / 1) end) =~
               "@callback message(t()) :: String.t()\n\n"

      assert capture_io(fn -> b(:gen_server.handle_cast() / 2) end) =~
               "@callback handle_cast("
    end

    test "prints callback documentation metadata" do
      filename = "callback_with_metadata.ex"

      content = """
      defmodule CallbackWithMetadata do
        @doc "callback"
        @doc since: "1.2.3", deprecated: "Use handle_test/1", purpose: :test
        @callback test(:foo) :: integer
      end
      """

      with_file(filename, content, fn ->
        assert c(filename, ".") == [CallbackWithMetadata]

        assert capture_io(fn -> b(CallbackWithMetadata.test()) end) == """
               @callback test(:foo) :: integer()

               deprecated: Use handle_test/1
               since: 1.2.3

               callback

               """
      end)
    after
      cleanup_modules([CallbackWithMetadata])
    end

    test "prints optional callback" do
      filename = "optional_callbacks.ex"

      content = """
      defmodule OptionalCallbacks do
        @doc "callback"
        @callback optional_callback(:foo) :: integer
        @macrocallback optional_macrocallback(:bar) :: atom
        @optional_callbacks optional_callback: 1, optional_macrocallback: 1
      end
      """

      with_file(filename, content, fn ->
        assert c(filename, ".") == [OptionalCallbacks]

        assert capture_io(fn -> b(OptionalCallbacks) end) =~ """
               @callback optional_callback(:foo) :: integer()

               @macrocallback optional_macrocallback(:bar) :: atom()

               @optional_callbacks optional_callback: 1, optional_macrocallback: 1

               """
      end)
    after
      cleanup_modules([OptionalCallbacks])
    end

    test "does not print docs for @doc false callbacks" do
      filename = "hidden_callbacks.ex"

      content = """
      defmodule HiddenCallbacks do
        @doc false
        @callback hidden_callback() :: integer

        @doc false
        @macrocallback hidden_macrocallback() :: integer
      end
      """

      with_file(filename, content, fn ->
        assert c(filename, ".") == [HiddenCallbacks]

        assert capture_io(fn -> b(HiddenCallbacks) end) =~
                 "No callbacks for HiddenCallbacks were found\n"
      end)
    after
      cleanup_modules([HiddenCallbacks])
    end
  end

  describe "t" do
    test "prints when there is no type information or the type is private" do
      assert capture_io(fn -> t(IEx) end) == "No type information for IEx was found\n"

      assert capture_io(fn -> t(Enum.doesnt_exist()) end) ==
               "No type information for Enum.doesnt_exist was found or " <>
                 "Enum.doesnt_exist is private\n"

      contents = """
      defmodule TypeSample do
        @type public_so_t_doesnt_warn() :: t()
        @typep t() :: term()
      end
      """

      filename = "typesample.ex"

      with_file(filename, contents, fn ->
        assert c(filename, ".") == [TypeSample]

        assert capture_io(fn -> t(TypeSample.t() / 0) end) ==
                 "No type information for TypeSample.t was found or TypeSample.t is private\n"
      end)
    after
      cleanup_modules([TypeSample])
    end

    test "prints all types in module" do
      # Test that it shows at least two types
      assert Enum.count(capture_io(fn -> t(Enum) end) |> String.split("\n"), fn line ->
               String.starts_with?(line, "@type")
             end) >= 2
    end

    test "prints private types" do
      assert capture_io(fn -> t(Date.Range) end) =~ "@typep days"
    end

    test "prints type information" do
      assert "@type t() ::" <> _ = capture_io(fn -> t(Enum.t()) end)
      assert capture_io(fn -> t(Enum.t()) end) == capture_io(fn -> t(Enum.t() / 0) end)
      assert capture_io(fn -> t(URI.t()) end) == capture_io(fn -> t(URI.t() / 0) end)
    end

    test "sorts types alphabetically" do
      unsorted =
        capture_io(fn -> t(Enum) end)
        |> String.split("\n")
        |> Enum.reject(&(&1 == ""))
        |> Enum.map(&String.replace(&1, ~r/@(type|opaque) /, ""))

      assert unsorted == Enum.sort(unsorted)
    end

    test "prints type documentation" do
      content = """
      defmodule TypeSample do
        @typedoc "An ID with description."
        @type id_with_desc :: {number, String.t}
        @type unquote(:"?")() :: :question_mark
      end
      """

      filename = "typesample.ex"

      with_file(filename, content, fn ->
        assert c(filename, ".") == [TypeSample]

        assert capture_io(fn -> t(TypeSample.id_with_desc() / 0) end) == """
               @type id_with_desc() :: {number(), String.t()}

               An ID with description.

               """

        assert capture_io(fn -> t(TypeSample.id_with_desc()) end) == """
               @type id_with_desc() :: {number(), String.t()}

               An ID with description.

               """

        assert capture_io(fn -> t(TypeSample."?"()) end) == """
               @type ?() :: :question_mark

               """
      end)
    after
      cleanup_modules([TypeSample])
    end

    test "prints type documentation metadata" do
      content = """
      defmodule TypeSample do
        @typedoc "An ID with description."
        @typedoc since: "1.2.3", deprecated: "Use t/0", purpose: :test
        @type id_with_desc :: {number, String.t}
      end
      """

      filename = "typesample.ex"

      with_file(filename, content, fn ->
        assert c(filename, ".") == [TypeSample]

        assert capture_io(fn -> t(TypeSample.id_with_desc()) end) == """
               @type id_with_desc() :: {number(), String.t()}

               deprecated: Use t/0
               since: 1.2.3

               An ID with description.

               """
      end)
    after
      cleanup_modules([TypeSample])
    end

    @tag :erlang_doc
    test "prints all types in Erlang module" do
      captured = capture_io(fn -> t(:queue) end)

      # TODO remove once we require Erlang/OTP 27+
      if System.otp_release() < "27" do
        assert captured =~ "-type queue() :: queue(_)"
        assert captured =~ "-opaque queue(Item)"
      else
        assert captured =~ "@type queue() :: queue(_)"
        assert captured =~ "@opaque queue(item)"
      end
    end

    @tag :erlang_doc
    test "prints single type from Erlang module" do
      captured = capture_io(fn -> t(:erlang.iovec()) end)

      # TODO remove once we require Erlang/OTP 27+
      if System.otp_release() < "27" do
        assert captured =~ "-type iovec() :: [binary()]"
      else
        assert captured =~ "@type iovec() :: [binary()]"
      end

      assert captured =~ "A list of binaries."

      captured = capture_io(fn -> t(:erlang.iovec() / 0) end)

      # TODO remove once we require Erlang/OTP 27+
      if System.otp_release() < "27" do
        assert captured =~ "-type iovec() :: [binary()]"
      else
        assert captured =~ "@type iovec() :: [binary()]"
      end

      assert captured =~ "A list of binaries."
    end

    @tag :erlang_doc
    test "handles non-existing types from Erlang module" do
      captured = capture_io(fn -> t(:erlang.foo()) end)
      assert captured =~ "No type information for :erlang.foo was found or :erlang.foo is private"

      captured = capture_io(fn -> t(:erlang.foo() / 1) end)
      assert captured =~ "No type information for :erlang.foo was found or :erlang.foo is private"
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

    test "returns proper error when trying to access history out of bounds" do
      # We evaluate 22 statements as 20 is the current limit for iex history
      assert capture_iex("""
             \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
             v(1)
             """) =~
               "(RuntimeError) v(1) is out of bounds, the currently preserved history ranges from 3 to 22"
    end

    test "negative lookup works properly after crashes" do
      capture =
        capture_iex("""
            \n
            \n
            \n
            Process.exit(self(), :some_reason)\n
            :target_value
            v(-1)
        """)

      assert capture |> String.split("\n") |> List.last() == ":target_value"
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
      assert exports == "expand/1      expand/2      exports/1     remsh/1       \n"
    end

    test "handles long function names" do
      exports = capture_io(fn -> exports(Calendar.UTCOnlyTimeZoneDatabase) end)

      assert exports ==
               "time_zone_period_from_utc_iso_days/2 time_zone_periods_from_wall_datetime/2 \n"
    end
  end

  describe "import_file" do
    test "imports a file" do
      with_file("dot-iex", "variable = :hello\nimport IO", fn ->
        assert capture_io(:stderr, fn -> capture_iex("variable") end) =~
                 "undefined variable \"variable\""

        assert capture_io(:stderr, fn -> capture_iex("puts \"hi\"") end) =~
                 "undefined function puts/1"

        assert capture_iex("import_file \"dot-iex\"\nvariable\nputs \"hi\"") ==
                 "IO\n:hello\nhi\n:ok"
      end)
    end

    test "imports a file that imports another file" do
      dot = "parent = true\nimport_file \"dot-iex-1\""
      dot_1 = "variable = :hello\nimport IO"

      with_file(["dot-iex", "dot-iex-1"], [dot, dot_1], fn ->
        assert capture_io(:stderr, fn -> capture_iex("parent") end) =~
                 "undefined variable \"parent\""

        assert capture_io(:stderr, fn -> capture_iex("puts \"hi\"") end) =~
                 "undefined function puts/1"

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

    test "circular imports" do
      dot_1 = "import_file \"dot-iex-2\""
      dot_2 = "import_file \"dot-iex-1\""

      with_file(["dot-iex-1", "dot-iex-2"], [dot_1, dot_2], fn ->
        assert capture_io(:stderr, fn ->
                 assert capture_iex(":ok", [], dot_iex: "dot-iex-1") == ":ok"
               end) =~ "dot-iex-2 was already imported, skipping circular file imports"
      end)
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

  describe "use_if_available" do
    test "uses a module only if available" do
      assert "nil" == capture_iex("use_if_available NoSuchModule")
    end
  end

  describe "iex> |> (and other binary operators)" do
    test "passes previous result to the pipe" do
      Enum.each([:~>>, :<<~, :~>, :<~, :<~>], fn op ->
        assert capture_io(:stderr, fn -> capture_iex("42\n  #{op} IO.puts()") end) =~
                 "undefined function #{op}/2"
      end)

      assert capture_iex("+42") =~ "42"
      assert capture_iex("-42") =~ "-42"
      assert capture_iex("42\n |> IO.inspect(label: \"foo\")") =~ "foo: 42"
      assert capture_iex("[42]\n++ [24]\n|> IO.inspect(label: \"foo\")") =~ "foo: [42, 24]"
      assert capture_iex("|> IO.puts()") =~ "(RuntimeError) v(-1) is out of bounds"
    end

    test "raises if previous expression was a match" do
      assert capture_iex("x = 42\n|> IO.puts()") =~
               "surround the whole pipeline with parentheses '|>'"

      assert capture_iex("%{x: x} = %{x: 42}\n|> IO.puts()") =~
               "surround the whole pipeline with parentheses '|>'"

      assert capture_iex("%{x: x} = map = %{x: 42}\n|> IO.puts()") =~
               "surround the whole pipeline with parentheses '|>'"
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
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
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
    test "returns error tuple for nonexistent modules" do
      assert l(:nonexistent_module) == {:error, :nofile}
    end

    test "loads a given module" do
      assert_raise UndefinedFunctionError,
                   ~r"function Sample.run/0 is undefined",
                   fn -> Sample.run() end

      filename = "sample.ex"

      with_file(filename, test_module_code(), fn ->
        assert c(filename, ".") == [Sample]
        assert Sample.run() == :run

        File.write!(filename, "defmodule Sample do end")
        elixirc(["sample.ex"])
        assert l(Sample) == {:module, Sample}

        assert_raise UndefinedFunctionError,
                     ~r"function Sample.run/0 is undefined",
                     fn -> Sample.run() end
      end)
    after
      # Clean up the old version left over after l()
      cleanup_modules([Sample])
    end
  end

  describe "nl" do
    @tag :capture_log
    test "loads a given module on the given nodes" do
      assert nl([node()], :lists) == {:ok, [{:nonode@nohost, :error, :sticky_directory}]}
      assert nl([node()], Enum) == {:ok, [{:nonode@nohost, :loaded, Enum}]}

      assert nl(:nonexistent_module) == {:error, :nofile}

      assert nl([:nosuchnode@badhost], Enum) ==
               {:ok, [{:nosuchnode@badhost, :badrpc, :noconnection}]}
    end
  end

  describe "r" do
    test "raises when reloading a nonexistent module" do
      assert_raise ArgumentError, "could not load nor find module: :nonexistent_module", fn ->
        r(:nonexistent_module)
      end
    end

    test "reloads Elixir modules" do
      message = ~r"function Sample.run/0 is undefined \(module Sample is not available\)"

      assert_raise UndefinedFunctionError, message, fn ->
        Sample.run()
      end

      filename = "sample.ex"

      with_file(filename, test_module_code(), fn ->
        assert capture_io(:stderr, fn ->
                 assert c(filename, ".") == [Sample]
                 assert Sample.run() == :run

                 File.write!(filename, "defmodule Sample do end")
                 assert {:reloaded, [Sample]} = r(Sample)
                 assert {:reloaded, [Sample]} = r([Sample])

                 message = "function Sample.run/0 is undefined or private"

                 assert_raise UndefinedFunctionError, message, fn ->
                   Sample.run()
                 end
               end) =~
                 "redefining module Sample (current version loaded from Elixir.Sample.beam)"
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
        assert {:reloaded, [:sample]} = r(:sample)
        assert {:reloaded, [:sample]} = r([:sample])
        assert :sample.hello() == :bye
      end)
    after
      cleanup_modules([:sample])
    end
  end

  describe "pid/1,3" do
    test "builds a PID from string" do
      assert inspect(pid("0.32767.0")) == "#PID<0.32767.0>"
      assert inspect(pid("0.5.0")) == "#PID<0.5.0>"

      assert_raise ArgumentError, fn ->
        pid("0.6.-6")
      end
    end

    test "builds a PID from atom" do
      assert inspect(pid(:init)) == "#PID<0.0.0>"

      assert_raise ArgumentError, fn ->
        pid(:random_atom_ZM6pX6VwQx)
      end
    end

    test "builds a PID from integers" do
      assert inspect(pid(0, 32767, 0)) == "#PID<0.32767.0>"
      assert inspect(pid(0, 5, 0)) == "#PID<0.5.0>"

      assert_raise FunctionClauseError, fn ->
        pid(0, 6, -6)
      end
    end
  end

  describe "port" do
    test "builds a port from string" do
      assert inspect(port("0.8080")) == "#Port<0.8080>"
      assert inspect(port("0.0")) == "#Port<0.0>"

      assert_raise ArgumentError, fn ->
        port("0.-6")
      end
    end

    test "builds a port from integers" do
      assert inspect(port(0, 8080)) == "#Port<0.8080>"
      assert inspect(port(0, 0)) == "#Port<0.0>"

      assert_raise FunctionClauseError, fn ->
        port(-1, -6)
      end
    end
  end

  describe "ref" do
    test "builds a ref from string" do
      ref = make_ref()
      [_, inner, _] = String.split(inspect(ref), ["<", ">"])
      assert ref(inner) == ref

      assert_raise ArgumentError, fn ->
        ref("0.6.6.-6")
      end
    end

    test "builds a ref from integers" do
      ref = make_ref()
      [_, inner, _] = String.split(inspect(ref), ["<", ">"])
      [p1, p2, p3, p4] = inner |> String.split(".") |> Enum.map(&String.to_integer/1)
      assert ref(p1, p2, p3, p4) == ref

      assert_raise FunctionClauseError, fn ->
        ref(0, 6, 6, -6)
      end
    end
  end

  describe "i" do
    test "prints information about the data type" do
      assert capture_io(fn -> i(:ok) end) =~ """
             Term
               :ok
             Data type
               Atom
             Reference modules
               Atom\
             """
    end

    test "handles functions that don't display result" do
      assert capture_io(fn -> i(IEx.dont_display_result()) end) =~ """
             Term
               :"do not show this result in output"
             Data type
               Atom
             Description
               This atom is returned by IEx when a function that should not print its
               return value on screen is executed.\
             """
    end

    defmodule MyIExInfoModule do
      defstruct []

      defimpl IEx.Info do
        def info(_), do: [{"A", "it's A"}, {:b, "it's :b"}, {~c"c", "it's 'c'"}]
      end
    end

    test "uses the IEx.Info protocol" do
      assert capture_io(fn -> i(%MyIExInfoModule{}) end) =~ """
             Term
               %IEx.HelpersTest.MyIExInfoModule{}
             A
               it's A
             b
               it's :b
             c
               it's 'c'
             """
    after
      cleanup_modules([MyIExInfoModule])
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
