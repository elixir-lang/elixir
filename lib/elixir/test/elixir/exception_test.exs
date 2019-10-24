Code.require_file("test_helper.exs", __DIR__)

defmodule ExceptionTest do
  use ExUnit.Case, async: true

  doctest Exception

  test "raising preserves the stacktrace" do
    stacktrace =
      try do
        raise "a"
      rescue
        _ -> hd(__STACKTRACE__)
      end

    file = __ENV__.file |> Path.relative_to_cwd() |> String.to_charlist()

    assert {__MODULE__, :"test raising preserves the stacktrace", _, [file: ^file, line: 11]} =
             stacktrace
  end

  test "exception?/1" do
    assert Exception.exception?(%RuntimeError{})
    refute Exception.exception?(%Regex{})
    refute Exception.exception?({})
  end

  test "message/1" do
    defmodule BadException do
      def message(exception) do
        if exception.raise do
          raise "oops"
        end
      end
    end

    assert Exception.message(%{__struct__: BadException, __exception__: true, raise: true}) =~
             "got RuntimeError with message \"oops\" while retrieving Exception.message/1 " <>
               "for %{__exception__: true, __struct__: ExceptionTest.BadException, raise: true}"

    assert Exception.message(%{__struct__: BadException, __exception__: true, raise: false}) =~
             "got nil while retrieving Exception.message/1 " <>
               "for %{__exception__: true, __struct__: ExceptionTest.BadException, raise: false}"
  end

  test "normalize/2" do
    assert Exception.normalize(:throw, :badarg, []) == :badarg
    assert Exception.normalize(:exit, :badarg, []) == :badarg
    assert Exception.normalize({:EXIT, self()}, :badarg, []) == :badarg
    assert Exception.normalize(:error, :badarg, []).__struct__ == ArgumentError
    assert Exception.normalize(:error, %ArgumentError{}, []).__struct__ == ArgumentError
  end

  test "format/2 without stacktrace" do
    stacktrace =
      try do
        throw(:stack)
      catch
        :stack -> __STACKTRACE__
      end

    assert Exception.format(:error, :badarg, stacktrace) ==
             "** (ArgumentError) argument error\n" <> Exception.format_stacktrace(stacktrace)
  end

  test "format/2 with empty stacktrace" do
    assert Exception.format(:error, :badarg, []) == "** (ArgumentError) argument error"
  end

  test "format/2 with EXIT (has no stacktrace)" do
    assert Exception.format({:EXIT, self()}, :badarg, []) ==
             "** (EXIT from #{inspect(self())}) :badarg"
  end

  test "format_banner/2" do
    assert Exception.format_banner(:error, :badarg) == "** (ArgumentError) argument error"
    assert Exception.format_banner(:throw, :badarg) == "** (throw) :badarg"
    assert Exception.format_banner(:exit, :badarg) == "** (exit) :badarg"

    assert Exception.format_banner({:EXIT, self()}, :badarg) ==
             "** (EXIT from #{inspect(self())}) :badarg"
  end

  test "format_stacktrace/1 from file" do
    try do
      Code.eval_string("def foo do end", [], file: "my_file")
    rescue
      ArgumentError ->
        assert Exception.format_stacktrace(__STACKTRACE__) =~ "my_file:1: (file)"
    else
      _ -> flunk("expected failure")
    end
  end

  test "format_stacktrace/1 from module" do
    try do
      Code.eval_string(
        "defmodule FmtStack do raise ArgumentError, ~s(oops) end",
        [],
        file: "my_file"
      )
    rescue
      ArgumentError ->
        assert Exception.format_stacktrace(__STACKTRACE__) =~ "my_file:1: (module)"
    else
      _ -> flunk("expected failure")
    end
  end

  test "format_stacktrace_entry/1 with no file or line" do
    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], []}) == "Foo.bar(1, 2, 3)"
    assert Exception.format_stacktrace_entry({Foo, :bar, [], []}) == "Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, 1, []}) == "Foo.bar/1"
  end

  test "format_stacktrace_entry/1 with file and line" do
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex', line: 10]}) ==
             "file.ex:10: Foo.bar()"

    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], [file: 'file.ex', line: 10]}) ==
             "file.ex:10: Foo.bar(1, 2, 3)"

    assert Exception.format_stacktrace_entry({Foo, :bar, 1, [file: 'file.ex', line: 10]}) ==
             "file.ex:10: Foo.bar/1"
  end

  test "format_stacktrace_entry/1 with file no line" do
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex']}) ==
             "file.ex: Foo.bar()"

    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex', line: 0]}) ==
             "file.ex: Foo.bar()"

    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], [file: 'file.ex']}) ==
             "file.ex: Foo.bar(1, 2, 3)"

    assert Exception.format_stacktrace_entry({Foo, :bar, 1, [file: 'file.ex']}) ==
             "file.ex: Foo.bar/1"
  end

  test "format_stacktrace_entry/1 with application" do
    assert Exception.format_stacktrace_entry({Exception, :bar, [], [file: 'file.ex']}) ==
             "(elixir #{System.version()}) file.ex: Exception.bar()"

    assert Exception.format_stacktrace_entry({Exception, :bar, [], [file: 'file.ex', line: 10]}) ==
             "(elixir #{System.version()}) file.ex:10: Exception.bar()"
  end

  test "format_stacktrace_entry/1 with fun" do
    assert Exception.format_stacktrace_entry({fn x -> x end, [1], []}) =~ ~r/#Function<.+>\(1\)/

    assert Exception.format_stacktrace_entry({fn x, y -> {x, y} end, 2, []}) =~
             ~r"#Function<.+>/2"
  end

  test "format_mfa/3" do
    # Let's create this atom so that String.to_existing_atom/1 inside
    # format_mfa/3 doesn't raise.
    _ = :"some function"

    assert Exception.format_mfa(Foo, nil, 1) == "Foo.nil/1"
    assert Exception.format_mfa(Foo, :bar, 1) == "Foo.bar/1"
    assert Exception.format_mfa(Foo, :bar, []) == "Foo.bar()"
    assert Exception.format_mfa(nil, :bar, []) == "nil.bar()"
    assert Exception.format_mfa(:foo, :bar, [1, 2]) == ":foo.bar(1, 2)"
    assert Exception.format_mfa(Foo, :b@r, 1) == "Foo.\"b@r\"/1"
    assert Exception.format_mfa(Foo, :"bar baz", 1) == "Foo.\"bar baz\"/1"
    assert Exception.format_mfa(Foo, :"-func/2-fun-0-", 4) == "anonymous fn/4 in Foo.func/2"

    assert Exception.format_mfa(Foo, :"-some function/2-fun-0-", 4) ==
             "anonymous fn/4 in Foo.\"some function\"/2"

    assert Exception.format_mfa(Foo, :"42", 1) == "Foo.\"42\"/1"
    assert Exception.format_mfa(Foo, :Bar, [1, 2]) == "Foo.\"Bar\"(1, 2)"
    assert Exception.format_mfa(Foo, :%{}, [1, 2]) == "Foo.\"%{}\"(1, 2)"
    assert Exception.format_mfa(Foo, :..., 1) == "Foo.\"...\"/1"
  end

  test "format_mfa/3 with unicode" do
    assert Exception.format_mfa(Foo, :olá, [1, 2]) == "Foo.olá(1, 2)"
    assert Exception.format_mfa(Foo, :Olá, [1, 2]) == "Foo.\"Olá\"(1, 2)"
    assert Exception.format_mfa(Foo, :Ólá, [1, 2]) == "Foo.\"Ólá\"(1, 2)"
    assert Exception.format_mfa(Foo, :こんにちは世界, [1, 2]) == "Foo.こんにちは世界(1, 2)"

    nfd = :unicode.characters_to_nfd_binary("olá")
    assert Exception.format_mfa(Foo, String.to_atom(nfd), [1, 2]) == "Foo.\"#{nfd}\"(1, 2)"
  end

  test "format_fa/2" do
    assert Exception.format_fa(fn -> nil end, 1) =~
             ~r"#Function<\d+\.\d+/0 in ExceptionTest\.\"test format_fa/2\"/1>/1"
  end

  ## Format exits

  test "format_exit/1" do
    assert Exception.format_exit(:bye) == ":bye"
    assert Exception.format_exit(:noconnection) == "no connection"
    assert Exception.format_exit({:nodedown, :node@host}) == "no connection to node@host"
    assert Exception.format_exit(:timeout) == "time out"
    assert Exception.format_exit(:noproc) |> String.starts_with?("no process:")
    assert Exception.format_exit(:killed) == "killed"
    assert Exception.format_exit(:normal) == "normal"
    assert Exception.format_exit(:shutdown) == "shutdown"
    assert Exception.format_exit(:calling_self) == "process attempted to call itself"
    assert Exception.format_exit({:shutdown, :bye}) == "shutdown: :bye"

    assert Exception.format_exit({:badarg, [{:not_a_real_module, :function, 0, []}]}) ==
             "an exception was raised:\n    ** (ArgumentError) argument error\n        :not_a_real_module.function/0"

    assert Exception.format_exit({:bad_call, :request}) == "bad call: :request"
    assert Exception.format_exit({:bad_cast, :request}) == "bad cast: :request"

    assert Exception.format_exit({:start_spec, :unexpected}) ==
             "bad child specification, got: :unexpected"

    assert Exception.format_exit({:supervisor_data, :unexpected}) ==
             "bad supervisor configuration, got: :unexpected"
  end

  defmodule Sup do
    def start_link(fun), do: :supervisor.start_link(__MODULE__, fun)

    def init(fun), do: fun.()
  end

  test "format_exit/1 with supervisor errors" do
    Process.flag(:trap_exit, true)

    {:error, reason} = __MODULE__.Sup.start_link(fn -> :foo end)

    assert Exception.format_exit(reason) ==
             "#{inspect(__MODULE__.Sup)}.init/1 returned a bad value: :foo"

    return = {:ok, {:foo, []}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) == "bad supervisor configuration, invalid type: :foo"

    return = {:ok, {{:foo, 1, 1}, []}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) == "bad supervisor configuration, invalid strategy: :foo"

    return = {:ok, {{:one_for_one, :foo, 1}, []}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)

    assert Exception.format_exit(reason) ==
             "bad supervisor configuration, invalid max_restarts (intensity): :foo"

    return = {:ok, {{:one_for_one, 1, :foo}, []}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)

    assert Exception.format_exit(reason) ==
             "bad supervisor configuration, invalid max_seconds (period): :foo"

    return = {:ok, {{:simple_one_for_one, 1, 1}, :foo}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) == "bad child specification, invalid children: :foo"

    return = {:ok, {{:one_for_one, 1, 1}, [:foo]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)

    assert Exception.format_exit(reason) ==
             "bad child specification, invalid child specification: :foo"

    return = {:ok, {{:one_for_one, 1, 1}, [{:child, :foo, :temporary, 1, :worker, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) == "bad child specification, invalid mfa: :foo"

    return = {:ok, {{:one_for_one, 1, 1}, [{:child, {:m, :f, []}, :foo, 1, :worker, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) =~ "bad child specification, invalid restart type: :foo"

    return = {
      :ok,
      {{:one_for_one, 1, 1}, [{:child, {:m, :f, []}, :temporary, :foo, :worker, []}]}
    }

    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) =~ "bad child specification, invalid shutdown: :foo"

    return = {:ok, {{:one_for_one, 1, 1}, [{:child, {:m, :f, []}, :temporary, 1, :foo, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) =~ "bad child specification, invalid child type: :foo"

    return = {:ok, {{:one_for_one, 1, 1}, [{:child, {:m, :f, []}, :temporary, 1, :worker, :foo}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) =~ "bad child specification, invalid modules: :foo"

    return = {
      :ok,
      {{:one_for_one, 1, 1}, [{:child, {:m, :f, []}, :temporary, 1, :worker, [{:foo}]}]}
    }

    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)
    assert Exception.format_exit(reason) =~ "bad child specification, invalid module: {:foo}"

    return = {
      :ok,
      {
        {:one_for_one, 1, 1},
        [
          {:child, {:m, :f, []}, :permanent, 1, :worker, []},
          {:child, {:m, :f, []}, :permanent, 1, :worker, []}
        ]
      }
    }

    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)

    assert Exception.format_exit(reason) =~
             "bad child specification, more than one child specification has the id: :child"

    return = {
      :ok,
      {{:one_for_one, 1, 1}, [{:child, {Kernel, :exit, [:foo]}, :temporary, 1, :worker, []}]}
    }

    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)

    assert Exception.format_exit(reason) ==
             "shutdown: failed to start child: :child\n    ** (EXIT) :foo"

    return = {
      :ok,
      {
        {:one_for_one, 1, 1},
        [{:child, {Kernel, :apply, [fn -> {:error, :foo} end, []]}, :temporary, 1, :worker, []}]
      }
    }

    {:error, reason} = __MODULE__.Sup.start_link(fn -> return end)

    assert Exception.format_exit(reason) ==
             "shutdown: failed to start child: :child\n    ** (EXIT) :foo"
  end

  test "format_exit/1 with call" do
    reason =
      try do
        :gen_server.call(:does_not_exist, :hello)
      catch
        :exit, reason -> reason
      end

    expected_to_start_with =
      "exited in: :gen_server.call(:does_not_exist, :hello)\n    ** (EXIT) no process:"

    assert Exception.format_exit(reason) |> String.starts_with?(expected_to_start_with)
  end

  test "format_exit/1 with nested calls" do
    Process.flag(:trap_exit, true)
    # Fake reason to prevent error_logger printing to stdout
    exit_fun = fn -> receive do: (_ -> exit(:normal)) end

    outer_pid =
      spawn_link(fn ->
        Process.flag(:trap_exit, true)

        receive do
          _ ->
            :gen_event.call(spawn_link(exit_fun), :handler, :hello)
        end
      end)

    reason =
      try do
        :gen_server.call(outer_pid, :hi)
      catch
        :exit, reason -> reason
      end

    formatted = Exception.format_exit(reason)
    assert formatted =~ ~r"exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hi\)\n"

    assert formatted =~
             ~r"\s{4}\*\* \(EXIT\) exited in: :gen_event\.call\(#PID<\d+\.\d+\.\d+>, :handler, :hello\)\n"

    assert formatted =~ ~r"\s{8}\*\* \(EXIT\) normal"
  end

  test "format_exit/1 with nested calls and exception" do
    Process.flag(:trap_exit, true)
    # Fake reason to prevent error_logger printing to stdout
    exit_reason = {%ArgumentError{}, [{:not_a_real_module, :function, 0, []}]}
    exit_fun = fn -> receive do: (_ -> exit(exit_reason)) end

    outer_pid =
      spawn_link(fn ->
        Process.flag(:trap_exit, true)
        :gen_event.call(spawn_link(exit_fun), :handler, :hello)
      end)

    reason =
      try do
        :gen_server.call(outer_pid, :hi)
      catch
        :exit, reason -> reason
      end

    formatted = Exception.format_exit(reason)
    assert formatted =~ ~r"exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hi\)\n"

    assert formatted =~
             ~r"\s{4}\*\* \(EXIT\) exited in: :gen_event\.call\(#PID<\d+\.\d+\.\d+>, :handler, :hello\)\n"

    assert formatted =~ ~r"\s{8}\*\* \(EXIT\) an exception was raised:\n"
    assert formatted =~ ~r"\s{12}\*\* \(ArgumentError\) argument error\n"
    assert formatted =~ ~r"\s{16}:not_a_real_module\.function/0"
  end

  describe "blaming" do
    test "does not annotate throws/exits" do
      stack = [{Keyword, :pop, [%{}, :key, nil], [line: 13]}]
      assert Exception.blame(:throw, :function_clause, stack) == {:function_clause, stack}
      assert Exception.blame(:exit, :function_clause, stack) == {:function_clause, stack}
    end

    test "annotates badarg on apply" do
      assert blame_message([], & &1.foo) ==
               "you attempted to apply :foo on []. If you are using apply/3, make sure " <>
                 "the module is an atom. If you are using the dot syntax, such as " <>
                 "map.field or module.function, make sure the left side of the dot is an atom or a map"

      assert blame_message([], &apply(&1, :foo, [])) ==
               "you attempted to apply :foo on []. If you are using apply/3, make sure " <>
                 "the module is an atom. If you are using the dot syntax, such as " <>
                 "map.field or module.function, make sure the left side of the dot is an atom or a map"

      assert blame_message([], &apply(Kernel, &1, [1, 2])) ==
               "you attempted to apply [] on module Kernel. Functions (the second argument of apply) must always be an atom"

      assert blame_message(123, &apply(Kernel, :+, &1)) ==
               "you attempted to apply :+ on module Kernel with arguments 123. " <>
                 "Arguments (the third argument of apply) must always be a list"
    end

    test "annotates undefined function error with suggestions" do
      assert blame_message(Enum, & &1.map(:ok)) == """
             function Enum.map/1 is undefined or private. Did you mean one of:

                   * map/2
             """

      assert blame_message(Enum, & &1.man(:ok)) == """
             function Enum.man/1 is undefined or private. Did you mean one of:

                   * map/2
                   * max/1
                   * max/2
                   * max/3
                   * min/1
             """

      assert blame_message(:erlang, & &1.gt_cookie()) == """
             function :erlang.gt_cookie/0 is undefined or private. Did you mean one of:

                   * get_cookie/0
                   * set_cookie/2
             """
    end

    test "annotates undefined function clause error with macro hints" do
      assert blame_message(Integer, & &1.is_odd(1)) ==
               "function Integer.is_odd/1 is undefined or private. However there is " <>
                 "a macro with the same name and arity. Be sure to require Integer if " <>
                 "you intend to invoke this macro"
    end

    test "annotates undefined function clause error with nil hints" do
      assert blame_message(nil, & &1.foo) ==
               "function nil.foo/0 is undefined. If you are using the dot syntax, " <>
                 "such as map.field or module.function, make sure the left side of the dot is an atom or a map"
    end

    test "annotates key error with suggestions if keys are atoms" do
      message = blame_message(%{first: nil, second: nil}, fn map -> map.firts end)

      assert message == """
             key :firts not found in: %{first: nil, second: nil}. Did you mean one of:

                   * :first
             """

      message = blame_message(%{"first" => nil, "second" => nil}, fn map -> map.firts end)

      assert message == "key :firts not found in: %{\"first\" => nil, \"second\" => nil}"

      message =
        blame_message(%{"first" => nil, "second" => nil}, fn map -> Map.fetch!(map, "firts") end)

      assert message == "key \"firts\" not found in: %{\"first\" => nil, \"second\" => nil}"

      message =
        blame_message([first: nil, second: nil], fn kwlist -> Keyword.fetch!(kwlist, :firts) end)

      assert message == """
             key :firts not found in: [first: nil, second: nil]. Did you mean one of:

                   * :first
             """
    end

    test "annotates key error with suggestions for structs" do
      message = blame_message(%URI{}, fn map -> map.schema end)
      assert message =~ "key :schema not found in: %URI{"
      assert message =~ "Did you mean one of:"
      assert message =~ "* :scheme"
    end

    if :erlang.system_info(:otp_release) >= '21' do
      test "annotates +/1 arithmetic errors" do
        assert blame_message(:foo, &(+&1)) == "bad argument in arithmetic expression: +(:foo)"
      end

      test "annotates -/1 arithmetic errors" do
        assert blame_message(:foo, &(-&1)) == "bad argument in arithmetic expression: -(:foo)"
      end

      test "annotates div arithmetic errors" do
        assert blame_message(0, &div(10, &1)) ==
                 "bad argument in arithmetic expression: div(10, 0)"
      end

      test "annotates rem arithmetic errors" do
        assert blame_message(0, &rem(10, &1)) ==
                 "bad argument in arithmetic expression: rem(10, 0)"
      end

      test "annotates band arithmetic errors" do
        use Bitwise

        assert blame_message(:foo, &band(&1, 10)) ==
                 "bad argument in arithmetic expression: Bitwise.band(:foo, 10)"

        assert blame_message(:foo, &(&1 &&& 10)) ==
                 "bad argument in arithmetic expression: Bitwise.band(:foo, 10)"
      end

      test "annotates bor arithmetic errors" do
        use Bitwise

        assert blame_message(:foo, &bor(&1, 10)) ==
                 "bad argument in arithmetic expression: Bitwise.bor(:foo, 10)"

        assert blame_message(:foo, &(&1 ||| 10)) ==
                 "bad argument in arithmetic expression: Bitwise.bor(:foo, 10)"
      end

      test "annotates bxor arithmetic errors" do
        use Bitwise

        assert blame_message(:foo, &bxor(&1, 10)) ==
                 "bad argument in arithmetic expression: Bitwise.bxor(:foo, 10)"

        assert blame_message(:foo, &(&1 ^^^ 10)) ==
                 "bad argument in arithmetic expression: Bitwise.bxor(:foo, 10)"
      end

      test "annotates bsl arithmetic errors" do
        use Bitwise

        assert blame_message(:foo, &bsl(10, &1)) ==
                 "bad argument in arithmetic expression: Bitwise.bsl(10, :foo)"

        assert blame_message(:foo, &(10 <<< &1)) ==
                 "bad argument in arithmetic expression: Bitwise.bsl(10, :foo)"
      end

      test "annotates bsr arithmetic errors" do
        use Bitwise

        assert blame_message(:foo, &bsr(10, &1)) ==
                 "bad argument in arithmetic expression: Bitwise.bsr(10, :foo)"

        assert blame_message(:foo, &(10 >>> &1)) ==
                 "bad argument in arithmetic expression: Bitwise.bsr(10, :foo)"
      end

      test "annotates bnot arithmetic errors" do
        use Bitwise

        assert blame_message(:foo, &bnot(&1)) ==
                 "bad argument in arithmetic expression: Bitwise.bnot(:foo)"

        assert blame_message(:foo, &(~~~&1)) ==
                 "bad argument in arithmetic expression: Bitwise.bnot(:foo)"
      end
    end

    defp blame_message(arg, fun) do
      try do
        fun.(arg)
      rescue
        e ->
          Exception.blame(:error, e, __STACKTRACE__) |> elem(0) |> Exception.message()
      end
    end

    test "annotates function clause errors" do
      args = [%{}, :key, nil]

      {exception, stack} =
        Exception.blame(:error, :function_clause, [{Keyword, :pop, args, [line: 13]}])

      assert %FunctionClauseError{kind: :def, args: ^args, clauses: [_]} = exception
      assert stack == [{Keyword, :pop, 3, [line: 13]}]
    end

    test "annotates args and clauses from mfa" do
      import PathHelpers

      write_beam(
        defmodule Blaming do
          def with_elem(x, y) when elem(x, 1) == 0 and elem(x, y) == 1 do
            {x, y}
          end

          def fetch(%module{} = container, key), do: {module, container, key}
          def fetch(map, key) when is_map(map), do: {map, key}
          def fetch(list, key) when is_list(list) and is_atom(key), do: {list, key}
          def fetch(nil, _key), do: nil

          require Integer
          def even_and_odd(foo, bar) when Integer.is_even(foo) and Integer.is_odd(bar), do: :ok
        end
      )

      :code.delete(Blaming)
      :code.purge(Blaming)

      {:ok, :def, clauses} = Exception.blame_mfa(Blaming, :with_elem, [1, 2])

      assert annotated_clauses_to_string(clauses) == [
               "{[+x+, +y+], [-elem(x, 1) == 0- and -elem(x, y) == 1-]}"
             ]

      {:ok, :def, clauses} = Exception.blame_mfa(Blaming, :fetch, [self(), "oops"])

      assert annotated_clauses_to_string(clauses) == [
               "{[-%module{} = container-, +key+], []}",
               "{[+map+, +key+], [-is_map(map)-]}",
               "{[+list+, +key+], [-is_list(list)- and -is_atom(key)-]}",
               "{[-nil-, +_key+], []}"
             ]

      {:ok, :def, clauses} = Exception.blame_mfa(Blaming, :even_and_odd, [1, 1])

      assert annotated_clauses_to_string(clauses) == [
               "{[+foo+, +bar+], [+is_integer(foo)+ and -Bitwise.band(foo, 1) == 0- and (+is_integer(bar)+ and +Bitwise.band(bar, 1) == 1+)]}"
             ]

      {:ok, :defmacro, clauses} = Exception.blame_mfa(Kernel, :!, [true])

      assert annotated_clauses_to_string(clauses) == [
               "{[-{:!, _, [value]}-], []}",
               "{[+value+], []}"
             ]
    end

    defp annotated_clauses_to_string(clauses) do
      Enum.map(clauses, fn args_and_clauses ->
        Macro.to_string(args_and_clauses, fn
          %{match?: true, node: node}, _string ->
            "+" <> Macro.to_string(node) <> "+"

          %{match?: false, node: node}, _string ->
            "-" <> Macro.to_string(node) <> "-"

          _node, string ->
            string
        end)
      end)
    end
  end

  ## Exception messages

  describe "exception messages" do
    import Exception, only: [message: 1]

    test "RuntimeError" do
      assert %RuntimeError{} |> message == "runtime error"
      assert %RuntimeError{message: "unexpected roquefort"} |> message == "unexpected roquefort"
    end

    test "ArithmeticError" do
      assert %ArithmeticError{} |> message == "bad argument in arithmetic expression"

      assert %ArithmeticError{message: "unexpected camembert"}
             |> message == "unexpected camembert"
    end

    test "ArgumentError" do
      assert %ArgumentError{} |> message == "argument error"
      assert %ArgumentError{message: "unexpected comté"} |> message == "unexpected comté"
    end

    test "Enum.OutOfBoundsError" do
      assert %Enum.OutOfBoundsError{} |> message == "out of bounds error"

      assert %Enum.OutOfBoundsError{message: "the brie is not on the table"}
             |> message == "the brie is not on the table"
    end

    test "Enum.EmptyError" do
      assert %Enum.EmptyError{} |> message == "empty error"

      assert %Enum.EmptyError{message: "there is no saint-nectaire left!"}
             |> message == "there is no saint-nectaire left!"
    end

    test "UndefinedFunctionError" do
      assert %UndefinedFunctionError{} |> message == "undefined function"

      assert %UndefinedFunctionError{module: Kernel, function: :bar, arity: 1}
             |> message == "function Kernel.bar/1 is undefined or private"

      assert %UndefinedFunctionError{module: Foo, function: :bar, arity: 1}
             |> message == "function Foo.bar/1 is undefined (module Foo is not available)"

      assert %UndefinedFunctionError{module: nil, function: :bar, arity: 3}
             |> message == "function nil.bar/3 is undefined"

      assert %UndefinedFunctionError{module: nil, function: :bar, arity: 0}
             |> message == "function nil.bar/0 is undefined"
    end

    test "UndefinedFunctionError for a callback" do
      defmodule Behaviour do
        @callback callback() :: :ok
        @optional_callbacks callback: 0
      end

      defmodule Implementation do
        @behaviour Behaviour
      end

      assert Exception.blame(:error, :undef, [{Implementation, :callback, 0, []}])
             |> elem(0)
             |> message ==
               "function ExceptionTest.Implementation.callback/0 is undefined or private" <>
                 ", but the behaviour ExceptionTest.Behaviour expects it to be present"
    end

    test "FunctionClauseError" do
      assert %FunctionClauseError{} |> message == "no function clause matches"

      assert %FunctionClauseError{module: Foo, function: :bar, arity: 1}
             |> message == "no function clause matching in Foo.bar/1"
    end

    test "FunctionClauseError with blame" do
      {exception, _} =
        Exception.blame(:error, :function_clause, [{Access, :fetch, [:foo, :bar], [line: 13]}])

      assert message(exception) =~ """
             no function clause matching in Access.fetch/2

             The following arguments were given to Access.fetch/2:

                 # 1
                 :foo

                 # 2
                 :bar

             Attempted function clauses (showing 5 out of 5):

                 def fetch(-%module{} = container-, key)
             """
    end

    test "ErlangError" do
      assert %ErlangError{original: :sample} |> message == "Erlang error: :sample"
    end
  end
end
