Code.require_file("test_helper.exs", __DIR__)

defmodule ExceptionTest do
  use ExUnit.Case, async: true

  doctest Exception

  test "raising preserves the stacktrace" do
    stacktrace =
      try do
        raise "a"
      rescue
        _ ->
          [top | _] = System.stacktrace()
          top
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
    assert Exception.normalize(:throw, :badarg) == :badarg
    assert Exception.normalize(:exit, :badarg) == :badarg
    assert Exception.normalize({:EXIT, self()}, :badarg) == :badarg
    assert Exception.normalize(:error, :badarg).__struct__ == ArgumentError
    assert Exception.normalize(:error, %ArgumentError{}).__struct__ == ArgumentError
  end

  test "format/2 without stacktrace" do
    stacktrace =
      try do
        throw(:stack)
      catch
        :stack -> System.stacktrace()
      end

    assert Exception.format(:error, :badarg) ==
             "** (ArgumentError) argument error\n" <> Exception.format_stacktrace(stacktrace)
  end

  test "format/2 with empty stacktrace" do
    assert Exception.format(:error, :badarg, []) == "** (ArgumentError) argument error"
  end

  test "format/2 with EXIT (has no stacktrace)" do
    assert Exception.format({:EXIT, self()}, :badarg) ==
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
    assert_raise ArgumentError, fn ->
      Code.eval_string("def foo do end", [], file: "my_file")
    end

    assert Exception.format_stacktrace(System.stacktrace()) =~ "my_file:1: (file)"
  end

  test "format_stacktrace/1 from module" do
    assert_raise ArgumentError, fn ->
      Code.eval_string(
        "defmodule FmtStack do raise ArgumentError, ~s(oops) end",
        [],
        file: "my_file"
      )
    end

    assert Exception.format_stacktrace(System.stacktrace()) =~ "my_file:1: (module)"
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
             "(elixir) file.ex: Exception.bar()"

    assert Exception.format_stacktrace_entry({Exception, :bar, [], [file: 'file.ex', line: 10]}) ==
             "(elixir) file.ex:10: Exception.bar()"

    assert Exception.format_stacktrace_entry({:lists, :bar, [1, 2, 3], []}) ==
             "(stdlib) :lists.bar(1, 2, 3)"
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

  if :erlang.system_info(:otp_release) >= '20' do
    test "format_mfa/3 with unicode" do
      assert Exception.format_mfa(Foo, :olá, [1, 2]) == "Foo.olá(1, 2)"
      assert Exception.format_mfa(Foo, :Olá, [1, 2]) == "Foo.\"Olá\"(1, 2)"
      assert Exception.format_mfa(Foo, :Ólá, [1, 2]) == "Foo.\"Ólá\"(1, 2)"

      hello_world = String.to_atom("こんにちは世界")
      assert Exception.format_mfa(Foo, hello_world, [1, 2]) == "Foo.こんにちは世界(1, 2)"

      nfd = :unicode.characters_to_nfd_binary("olá")
      assert Exception.format_mfa(Foo, String.to_atom(nfd), [1, 2]) == "Foo.\"#{nfd}\"(1, 2)"
    end
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

  # TODO: Remove this check once we depend only on 20
  if :erlang.system_info(:otp_release) >= '20' do
    describe "blaming" do
      test "annotates function clause errors" do
        args = [%{}, :key, nil]

        {exception, stack} =
          Exception.blame(:error, :function_clause, [{Keyword, :pop, args, [line: 13]}])

        assert %FunctionClauseError{kind: :def, args: ^args, clauses: [_]} = exception
        assert stack == [{Keyword, :pop, 3, [line: 13]}]
      end

      test "does not annotate throws/exits" do
        stack = [{Keyword, :pop, [%{}, :key, nil], [line: 13]}]
        assert Exception.blame(:throw, :function_clause, stack) == {:function_clause, stack}
        assert Exception.blame(:exit, :function_clause, stack) == {:function_clause, stack}
      end

      test "annotates args and clauses from mfa" do
        {:ok, :def, clauses} = Exception.blame_mfa(Keyword, :pop, [%{}, :key, nil])

        assert annotated_clauses_to_string(clauses) == [
                 "{[+keywords+, +key+, +default+], [-is_list(keywords)-]}"
               ]

        {:ok, :def, clauses} = Exception.blame_mfa(Keyword, :fetch, [[], "oops"])

        assert annotated_clauses_to_string(clauses) == [
                 "{[+keywords+, +key+], [+is_list(keywords)+ and -is_atom(key)-]}"
               ]

        {:ok, :def, clauses} = Exception.blame_mfa(Path, :type, [self()])

        assert annotated_clauses_to_string(clauses) == [
                 "{[+name+], [-is_list(name)-, -is_binary(name)-]}"
               ]

        {:ok, :def, clauses} = Exception.blame_mfa(Access, :fetch, [self(), "oops"])

        assert annotated_clauses_to_string(clauses) == [
                 "{[-%module{} = container-, +key+], []}",
                 "{[+map+, +key+], [-is_map(map)-]}",
                 "{[+list+, +key+], [-is_list(list)- and -is_atom(key)-]}",
                 "{[+list+, +key+], [-is_list(list)-]}",
                 "{[-nil-, +_key+], []}"
               ]

        {:ok, :defmacro, clauses} = Exception.blame_mfa(Kernel, :!, [true])

        assert annotated_clauses_to_string(clauses) == [
                 "{[-{:!, _, [value]}-], []}",
                 "{[+value+], []}"
               ]
      end
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

      assert %UndefinedFunctionError{module: nil, function: :bar, arity: 0}
             |> message == "function nil.bar/0 is undefined or private"
    end

    test "UndefinedFunctionError with suggestions" do
      assert %UndefinedFunctionError{module: Enum, function: :map, arity: 1}
             |> message == """
             function Enum.map/1 is undefined or private. Did you mean one of:

                   * map/2
             """

      assert %UndefinedFunctionError{module: Enum, function: :man, arity: 1}
             |> message == """
             function Enum.man/1 is undefined or private. Did you mean one of:

                   * map/2
                   * max/1
                   * max/2
                   * min/1
                   * min/2
             """

      assert %UndefinedFunctionError{module: :erlang, function: :gt_cookie, arity: 0}
             |> message == """
             function :erlang.gt_cookie/0 is undefined or private. Did you mean one of:

                   * get_cookie/0
                   * set_cookie/2
             """
    end

    test "UndefinedFunctionError when the mfa is a macro but require wasn't called" do
      _ = Code.ensure_loaded(Integer)

      assert %UndefinedFunctionError{module: Integer, function: :is_odd, arity: 1}
             |> message ==
               "function Integer.is_odd/1 is undefined or private. However there is " <>
                 "a macro with the same name and arity. Be sure to require Integer if " <>
                 "you intend to invoke this macro"
    end

    test "FunctionClauseError" do
      assert %FunctionClauseError{} |> message == "no function clause matches"

      assert %FunctionClauseError{module: Foo, function: :bar, arity: 1}
             |> message == "no function clause matching in Foo.bar/1"
    end

    # TODO: Remove this check once we depend only on 20
    if :erlang.system_info(:otp_release) >= '20' do
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
    end

    test "ErlangError" do
      assert %ErlangError{original: :sample} |> message == "Erlang error: :sample"
    end
  end
end
