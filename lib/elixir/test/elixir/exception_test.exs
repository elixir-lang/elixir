Code.require_file "test_helper.exs", __DIR__

defmodule Kernel.ExceptionTest do
  use ExUnit.Case, async: true

  # Ensure fields passed through an expression are valid
  defexception Custom, ~w(message)a

  test "is_exception" do
    assert is_exception(RuntimeError.new)
    refute is_exception(empty_tuple)
    refute is_exception(a_tuple)
    refute is_exception(a_list)
  end

  test "normalize" do
    assert Exception.normalize(:throw, :badarg) == :badarg
    assert is_record Exception.normalize(:error, :badarg), ArgumentError
    assert is_record Exception.normalize(:error, ArgumentError[]), ArgumentError
  end

  test "format_message" do
    assert Exception.format_message(:error, :badarg) == "** (ArgumentError) argument error"
    assert Exception.format_message(:throw, :badarg) == "** (throw) :badarg"
    assert Exception.format_message(:exit, :badarg) == "** (exit) :badarg"
  end

  test "format without stacktrace" do
    stacktrace = try do throw(:stack) catch :stack -> System.stacktrace() end
    assert Exception.format(:error, :badarg) == "** (ArgumentError) argument error" <>
      "\n" <> Exception.format_stacktrace(stacktrace)
  end

  test "format with empty stacktrace" do
    assert Exception.format(:error, :badarg, []) == "** (ArgumentError) argument error"
  end

  test "format_reason" do
    assert Exception.format_reason(:bye) == ":bye"
    assert Exception.format_reason({:badarg,[{:not_a_real_module,
          :function, 0, []}]}) == "an exception was raised:\n    ** (ArgumentError) argument error\n        :not_a_real_module\.function/0"
    assert Exception.format_reason(:noconnection) == "no connection"
    assert Exception.format_reason({:nodedown, :"node@host"}) == "no connection to node@host"
    assert Exception.format_reason(:timeout) == "time out"
    assert Exception.format_reason(:noproc) == "no process"
    assert Exception.format_reason(:killed) == "killed"
    assert Exception.format_reason(:normal) == "normal"
    assert Exception.format_reason(:shutdown) == "shutdown"
    assert Exception.format_reason({:shutdown, :bye}) == "shutdown: :bye"
  end

  test "format_reason with call" do
    reason = try do
      :gen_server.call(:does_not_exist, :hello)
    catch
      :exit, reason -> reason
    end
    assert Exception.format_reason(reason) == "exited in: :gen_server\.call\(:does_not_exist, :hello\)\n    ** (EXIT) no process"
  end

  test "format_reason with call with exception" do

    # Fake reason to prevent error_logger printing to stdout
    fsm_reason = {ArgumentError[], [{:not_a_real_module, :function, 0, []}]}
    reason = try do
      :gen_fsm.sync_send_event(spawn(fn() -> :timer.sleep(200) ; exit(fsm_reason) end), :hello)
    catch
      :exit, reason -> reason
    end
    assert Exception.format_reason(reason) =~ ~r"exited in: :gen_fsm\.sync_send_event\(#PID<\d+\.\d+\.\d+>, :hello\)\n\s{4}\*\* \(EXIT\) an exception was raised:\n\s{8}\*\* \(ArgumentError\) argument error\n\s{12}:not_a_real_module\.function/0"
  end

  test "format_reason with nested calls" do
    event_fun = fn() -> :timer.sleep(200) ; exit(:normal) end
    server_pid = spawn(fn()-> :gen_event.call(spawn(event_fun), :handler, :hello) end)
    reason = try do
      :gen_server.call(server_pid, :hi)
    catch
      :exit, reason -> reason
    end
    assert Exception.format_reason(reason) =~ ~r"exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hi\)\n\s{4}\*\* \(EXIT\) exited in: :gen_event\.call\(#PID<\d+\.\d+\.\d+>, :handler, :hello\)\n\s{8}\*\* \(EXIT\) normal"
  end

  test "format_reason with nested calls and exception" do
    # Fake reason to prevent error_logger printing to stdout
    event_reason = {ArgumentError[], [{:not_a_real_module, :function, 0, []}]}
    event_fun = fn() -> :timer.sleep(200) ; exit(event_reason) end
    server_pid = spawn(fn()-> :gen_event.call(spawn(event_fun), :handler, :hello) end)
    reason = try do
      :gen_server.call(server_pid, :hi)
    catch
      :exit, reason -> reason
    end
    assert Exception.format_reason(reason) =~ ~r"exited in: :gen_server\.call\(#PID<\d+\.\d+\.\d+>, :hi\)\n\s{4}\*\* \(EXIT\) exited in: :gen_event\.call\(#PID<\d+\.\d+\.\d+>, :handler, :hello\)\n\s{8}\*\* \(EXIT\) an exception was raised:\n\s{12}\*\* \(ArgumentError\) argument error\n\s{16}:not_a_real_module\.function/0"
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
    assert Exception.format_fa(fn -> end, 1) =~
           ~r"#Function<\d\.\d+/0 in Kernel\.ExceptionTest\.test format_fa/1>/1"
  end

  test "runtime error message" do
    assert RuntimeError.new.message == "runtime error"
    assert RuntimeError.new(message: "exception").message == "exception"
  end

  test "argument error message" do
    assert ArgumentError.new.message == "argument error"
    assert ArgumentError.new(message: "exception").message == "exception"
  end

  test "undefined function message" do
    assert UndefinedFunctionError.new.message == "undefined function"
    assert UndefinedFunctionError.new(module: Foo, function: :bar, arity: 1).message == "undefined function: Foo.bar/1"
    assert UndefinedFunctionError.new(module: nil, function: :bar, arity: 0).message == "undefined function: nil.bar/0"
  end

  test "function clause message" do
    assert FunctionClauseError.new.message == "no function clause matches"
    assert FunctionClauseError.new(module: Foo, function: :bar, arity: 1).message == "no function clause matching in Foo.bar/1"
  end

  test "erlang error message" do
    assert ErlangError.new(original: :sample).message == "erlang error: :sample"
  end

  test "raise preserves the stacktrace" do
    stacktrace =
      try do
        raise "a"
      rescue _ ->
        [top|_] = System.stacktrace
        top
      end
    file = __ENV__.file |> Path.relative_to_cwd |> List.from_char_data!
    assert {Kernel.ExceptionTest, :"test raise preserves the stacktrace", _,
           [file: ^file, line: 173]} = stacktrace
  end

  test "defexception" do
    defexception SampleError, message: nil do
      # Check do block is properly inline
      def exception(_), do: SampleError[message: "hello"]
    end
  end

  defp empty_tuple, do: {}
  defp a_tuple, do: {:foo, :bar, :baz}
  defp a_list,  do: [ :foo, :bar, :baz ]
end
