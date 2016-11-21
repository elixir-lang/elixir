Code.require_file "test_helper.exs", __DIR__

defmodule ExceptionTest do
  use ExUnit.Case, async: true

  doctest Exception

  test "raise preserves the stacktrace" do
    stacktrace =
      try do
        raise "a"
      rescue _ ->
        [top | _] = System.stacktrace
        top
      end
    file = __ENV__.file |> Path.relative_to_cwd |> String.to_charlist
    assert {__MODULE__, :"test raise preserves the stacktrace", _,
           [file: ^file, line: 11]} = stacktrace
  end

  test "exception?" do
    assert Exception.exception?(%RuntimeError{})
    refute Exception.exception?(%Regex{})
    refute Exception.exception?({})
  end

  test "message" do
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

  test "normalize" do
    assert Exception.normalize(:throw, :badarg) == :badarg
    assert Exception.normalize(:exit, :badarg) == :badarg
    assert Exception.normalize({:EXIT, self()}, :badarg) == :badarg
    assert Exception.normalize(:error, :badarg).__struct__ == ArgumentError
    assert Exception.normalize(:error, %ArgumentError{}).__struct__ == ArgumentError
  end

  test "format without stacktrace" do
    stacktrace = try do throw(:stack) catch :stack -> System.stacktrace() end
    assert Exception.format(:error, :badarg) ==
           "** (ArgumentError) argument error\n" <> Exception.format_stacktrace(stacktrace)
  end

  test "format with empty stacktrace" do
    assert Exception.format(:error, :badarg, []) == "** (ArgumentError) argument error"
  end

  test "format with EXIT has no stacktrace" do
    assert Exception.format({:EXIT, self()}, :badarg) == "** (EXIT from #{inspect self()}) :badarg"
  end

  test "format_banner" do
    assert Exception.format_banner(:error, :badarg) == "** (ArgumentError) argument error"
    assert Exception.format_banner(:throw, :badarg) == "** (throw) :badarg"
    assert Exception.format_banner(:exit, :badarg) == "** (exit) :badarg"
    assert Exception.format_banner({:EXIT, self()}, :badarg) == "** (EXIT from #{inspect self()}) :badarg"
  end

  test "format_stacktrace from file" do
    assert_raise ArgumentError, fn ->
      Code.eval_string("def foo do end", [], file: "my_file")
    end

    assert Exception.format_stacktrace(System.stacktrace) =~ "my_file:1: (file)"
  end

  test "format_stacktrace from module" do
    assert_raise ArgumentError, fn ->
      Code.eval_string("defmodule FmtStack do raise ArgumentError, ~s(oops) end", [], file: "my_file")
    end

    assert Exception.format_stacktrace(System.stacktrace) =~ "my_file:1: (module)"
  end

  test "format_stacktrace_entry with no file or line" do
    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], []}) == "Foo.bar(1, 2, 3)"
    assert Exception.format_stacktrace_entry({Foo, :bar, [], []}) == "Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, 1, []}) == "Foo.bar/1"
  end

  test "format_stacktrace_entry with file and line" do
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex', line: 10]}) == "file.ex:10: Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], [file: 'file.ex', line: 10]}) == "file.ex:10: Foo.bar(1, 2, 3)"
    assert Exception.format_stacktrace_entry({Foo, :bar, 1, [file: 'file.ex', line: 10]}) == "file.ex:10: Foo.bar/1"
  end

  test "format_stacktrace_entry with file no line" do
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex']}) == "file.ex: Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex', line: 0]}) == "file.ex: Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], [file: 'file.ex']}) == "file.ex: Foo.bar(1, 2, 3)"
    assert Exception.format_stacktrace_entry({Foo, :bar, 1, [file: 'file.ex']}) == "file.ex: Foo.bar/1"
  end

  test "format_stacktrace_entry with application" do
    assert Exception.format_stacktrace_entry({Exception, :bar, [], [file: 'file.ex']}) ==
                       "(elixir) file.ex: Exception.bar()"
    assert Exception.format_stacktrace_entry({Exception, :bar, [], [file: 'file.ex', line: 10]}) ==
                       "(elixir) file.ex:10: Exception.bar()"
    assert Exception.format_stacktrace_entry({:lists, :bar, [1, 2, 3], []}) ==
                       "(stdlib) :lists.bar(1, 2, 3)"
  end

  test "format_stacktrace_entry with fun" do
    assert Exception.format_stacktrace_entry({fn(x) -> x end, [1], []}) =~ ~r/#Function<.+>\(1\)/
    assert Exception.format_stacktrace_entry({fn(x, y) -> {x, y} end, 2, []}) =~ ~r"#Function<.+>/2"
  end

  test "format_mfa" do
    assert Exception.format_mfa(Foo, nil, 1) == "Foo.nil/1"
    assert Exception.format_mfa(Foo, :bar, 1) == "Foo.bar/1"
    assert Exception.format_mfa(Foo, :bar, []) == "Foo.bar()"
    assert Exception.format_mfa(nil, :bar, []) == "nil.bar()"
    assert Exception.format_mfa(:foo, :bar, [1, 2]) == ":foo.bar(1, 2)"
    assert Exception.format_mfa(Foo, :"bar baz", 1) == "Foo.\"bar baz\"/1"
    assert Exception.format_mfa(Foo, :"-func/2-fun-0-", 4) == "anonymous fn/4 in Foo.func/2"
  end

  test "format_fa" do
    assert Exception.format_fa(fn -> nil end, 1) =~
           ~r"#Function<\d+\.\d+/0 in ExceptionTest\.test format_fa/1>/1"
  end

  ## Format exits

  test "format_exit" do
    assert Exception.format_exit(:bye) == ":bye"
    assert Exception.format_exit(:noconnection) == "no connection"
    assert Exception.format_exit({:nodedown, :"node@host"}) == "no connection to node@host"
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
           "bad start spec: :unexpected"
    assert Exception.format_exit({:supervisor_data, :unexpected}) ==
           "bad supervisor data: :unexpected"
  end

  defmodule Sup do
    def start_link(fun), do: :supervisor.start_link(__MODULE__, fun)

    def init(fun), do: fun.()
  end

  test "format_exit with supervisor errors" do
    trap = Process.flag(:trap_exit, true)

    {:error, reason} = __MODULE__.Sup.start_link(fn() -> :foo end)
    assert Exception.format_exit(reason) ==
           "#{inspect(__MODULE__.Sup)}.init/1 returned a bad value: :foo"

    return = {:ok, {:foo, []}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad supervisor data: invalid type: :foo"

    return = {:ok, {{:foo, 1, 1}, []}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad supervisor data: invalid strategy: :foo"

    return = {:ok, {{:one_for_one, :foo, 1}, []}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad supervisor data: invalid intensity: :foo"

    return = {:ok, {{:one_for_one, 1, :foo}, []}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad supervisor data: invalid period: :foo"

    return = {:ok, {{:simple_one_for_one, 1, 1}, :foo}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad start spec: invalid children: :foo"

    return = {:ok, {{:one_for_one, 1, 1}, [:foo]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad start spec: invalid child spec: :foo"

    return = {:ok, {{:one_for_one, 1, 1},
        [{:child, :foo, :temporary, 1, :worker, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad start spec: invalid mfa: :foo"

    return = {:ok, {{:one_for_one, 1, 1},
        [{:child, {:m, :f, []}, :foo, 1, :worker, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad start spec: invalid restart type: :foo"

    return = {:ok, {{:one_for_one, 1, 1},
        [{:child, {:m, :f, []}, :temporary, :foo, :worker, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad start spec: invalid shutdown: :foo"

    return = {:ok, {{:one_for_one, 1, 1},
        [{:child, {:m, :f, []}, :temporary, 1, :foo, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad start spec: invalid child type: :foo"

    return = {:ok, {{:one_for_one, 1, 1},
        [{:child, {:m, :f, []}, :temporary, 1, :worker, :foo}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad start spec: invalid modules: :foo"

    return = {:ok, {{:one_for_one, 1, 1},
        [{:child, {:m, :f, []}, :temporary, 1, :worker, [{:foo}]}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "bad start spec: invalid module: {:foo}"

    return = {:ok, {{:one_for_one, 1, 1},
        [{:child, {Kernel, :exit, [:foo]}, :temporary, 1, :worker, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "shutdown: failed to start child: :child\n    ** (EXIT) :foo"

    return = {:ok, {{:one_for_one, 1, 1},
        [{:child, {Kernel, :apply, [fn() -> {:error, :foo} end, []]}, :temporary, 1, :worker, []}]}}
    {:error, reason} = __MODULE__.Sup.start_link(fn() -> return end)
    assert Exception.format_exit(reason) ==
           "shutdown: failed to start child: :child\n    ** (EXIT) :foo"

    Process.flag(:trap_exit, trap)
  end

  test "format_exit with call" do
    reason = try do
      :gen_server.call(:does_not_exist, :hello)
    catch
      :exit, reason -> reason
    end

    expected_to_start_with = "exited in: :gen_server.call(:does_not_exist, :hello)\n    ** (EXIT) no process:"
    assert Exception.format_exit(reason) |> String.starts_with?(expected_to_start_with)
  end

  test "format_exit with call with exception" do
    # Fake reason to prevent error_logger printing to stdout
    fsm_reason = {%ArgumentError{}, [{:not_a_real_module, :function, 0, []}]}
    reason = try do
      :gen_fsm.sync_send_event(spawn(fn() ->
          :timer.sleep(200) ; exit(fsm_reason)
      end), :hello)
    catch
      :exit, reason -> reason
    end

    formatted = Exception.format_exit(reason)
    assert formatted =~ ~r"exited in: :gen_fsm\.sync_send_event\(#PID<\d+\.\d+\.\d+>, :hello\)"
    assert formatted =~ ~r"\s{4}\*\* \(EXIT\) an exception was raised:\n"
    assert formatted =~ ~r"\s{8}\*\* \(ArgumentError\) argument error\n"
    assert formatted =~ ~r"\s{12}:not_a_real_module\.function/0"
  end

  test "format_exit with nested calls" do
    # Fake reason to prevent error_logger printing to stdout
    event_fun = fn() -> :timer.sleep(200) ; exit(:normal) end
    server_pid = spawn(fn()-> :gen_event.call(spawn(event_fun), :handler, :hello) end)
    reason = try do
      :gen_server.call(server_pid, :hi)
    catch
      :exit, reason -> reason
    end

    formatted = Exception.format_exit(reason)
    assert formatted =~ ~r"exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hi\)\n"
    assert formatted =~ ~r"\s{4}\*\* \(EXIT\) exited in: :gen_event\.call\(#PID<\d+\.\d+\.\d+>, :handler, :hello\)\n"
    assert formatted =~ ~r"\s{8}\*\* \(EXIT\) normal"
  end

  test "format_exit with nested calls and exception" do
    # Fake reason to prevent error_logger printing to stdout
    event_reason = {%ArgumentError{}, [{:not_a_real_module, :function, 0, []}]}
    event_fun = fn() -> :timer.sleep(200) ; exit(event_reason) end
    server_pid = spawn(fn()-> :gen_event.call(spawn(event_fun), :handler, :hello) end)
    reason = try do
      :gen_server.call(server_pid, :hi)
    catch
      :exit, reason -> reason
    end

    formatted = Exception.format_exit(reason)
    assert formatted =~ ~r"exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hi\)\n"
    assert formatted =~ ~r"\s{4}\*\* \(EXIT\) exited in: :gen_event\.call\(#PID<\d+\.\d+\.\d+>, :handler, :hello\)\n"
    assert formatted =~ ~r"\s{8}\*\* \(EXIT\) an exception was raised:\n"
    assert formatted =~ ~r"\s{12}\*\* \(ArgumentError\) argument error\n"
    assert formatted =~ ~r"\s{16}:not_a_real_module\.function/0"
  end

  ## Exception messages

  import Exception, only: [message: 1]

  test "RuntimeError message" do
    assert %RuntimeError{} |> message == "runtime error"
    assert %RuntimeError{message: "unexpected roquefort"} |> message == "unexpected roquefort"
  end

  test "ArithmeticError message" do
    assert %ArithmeticError{} |> message == "bad argument in arithmetic expression"
    assert %ArithmeticError{message: "unexpected camembert"} |> message == "unexpected camembert"
  end

  test "ArgumentError message" do
    assert %ArgumentError{} |> message == "argument error"
    assert %ArgumentError{message: "unexpected comté"} |> message == "unexpected comté"
  end

  test "Enum.OutOfBoundsError message" do
    assert %Enum.OutOfBoundsError{} |> message == "out of bounds error"
    assert %Enum.OutOfBoundsError{message: "the brie is not on the table"} |> message == "the brie is not on the table"
  end

  test "Enum.EmptyError message" do
    assert %Enum.EmptyError{} |> message == "empty error"
    assert %Enum.EmptyError{message: "there is no saint-nectaire left!"} |> message == "there is no saint-nectaire left!"
  end

  test "UndefinedFunctionError message" do
    assert %UndefinedFunctionError{} |> message == "undefined function"
    assert %UndefinedFunctionError{module: Kernel, function: :bar, arity: 1} |> message ==
           "function Kernel.bar/1 is undefined or private"
    assert %UndefinedFunctionError{module: Foo, function: :bar, arity: 1} |> message ==
           "function Foo.bar/1 is undefined (module Foo is not available)"
    assert %UndefinedFunctionError{module: nil, function: :bar, arity: 0} |> message ==
           "function nil.bar/0 is undefined or private"
  end

  test "UndefinedFunctionError message suggestions" do
    assert %UndefinedFunctionError{module: Enum, function: :map, arity: 1} |> message == """
           function Enum.map/1 is undefined or private. Did you mean one of:

                 * map/2
           """
    assert %UndefinedFunctionError{module: Enum, function: :man, arity: 1} |> message == """
           function Enum.man/1 is undefined or private. Did you mean one of:

                 * map/2
                 * max/1
                 * max/2
                 * min/1
                 * min/2
           """
    assert %UndefinedFunctionError{module: :erlang, function: :gt_cookie, arity: 0} |> message == """
           function :erlang.gt_cookie/0 is undefined or private. Did you mean one of:

                 * get_cookie/0
                 * set_cookie/2
           """
  end

  test "UndefinedFunctionError when the mfa is a macro but require wasn't called" do
    assert %UndefinedFunctionError{module: Integer, function: :is_odd, arity: 1} |> message ==
           "function Integer.is_odd/1 is undefined or private but Integer defines " <>
           "a macro with the same name and arity. Be sure to require Integer if " <>
           "you intend to invoke this macro"
  end

  test "FunctionClauseError message" do
    assert %FunctionClauseError{} |> message ==
           "no function clause matches"
    assert %FunctionClauseError{module: Foo, function: :bar, arity: 1} |> message ==
           "no function clause matching in Foo.bar/1"
  end

  test "ErlangError message" do
    assert %ErlangError{original: :sample} |> message ==
           "erlang error: :sample"
  end
end
