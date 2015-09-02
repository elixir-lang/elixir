Code.require_file "test_helper.exs", __DIR__

defmodule TestEmptyError do
  defexception [:message]
end

defmodule TestNonEmptyError do
  defexception [description: "compile test error", message: nil]

  def message(exception) do
    cond do
      exception.message ->
        exception.message
      true ->
        exception.description
    end
  end
end

defmodule ExceptionTest do
  use ExUnit.Case, async: true

  test "raise preserves the stacktrace" do
    stacktrace =
      try do
        raise "a"
      rescue _ ->
        [top|_] = System.stacktrace
        top
      end
    file = __ENV__.file |> Path.relative_to_cwd |> String.to_char_list
    assert {__MODULE__, :"test raise preserves the stacktrace", _,
           [file: ^file, line: 26]} = stacktrace
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
           "got RuntimeError with message `oops` while retrieving Exception.message/1 " <>
           "for %{__exception__: true, __struct__: ExceptionTest.BadException, raise: true}"

    assert Exception.message(%{__struct__: BadException, __exception__: true, raise: false}) =~
           "got nil while retrieving Exception.message/1 " <>
           "for %{__exception__: true, __struct__: ExceptionTest.BadException, raise: false}"
  end

  test "normalize" do
    assert Exception.normalize(:throw, :badarg) == :badarg
    assert Exception.normalize(:exit, :badarg) == :badarg
    assert Exception.normalize({:EXIT, self}, :badarg) == :badarg
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
    assert Exception.format({:EXIT, self}, :badarg) == "** (EXIT from #{inspect self}) :badarg"
  end

  test "format_banner" do
    assert Exception.format_banner(:error, :badarg) == "** (ArgumentError) argument error"
    assert Exception.format_banner(:throw, :badarg) == "** (throw) :badarg"
    assert Exception.format_banner(:exit, :badarg) == "** (exit) :badarg"
    assert Exception.format_banner({:EXIT, self}, :badarg) == "** (EXIT from #{inspect self}) :badarg"
  end

  test "format_stacktrace from file" do
    assert_raise ArgumentError, fn ->
      Code.eval_string("def foo do end", [], file: "myfile")
    end

    assert Exception.format_stacktrace(System.stacktrace) =~ "myfile:1: (file)"
  end

  test "format_stacktrace from module" do
    assert_raise ArgumentError, fn ->
      Code.eval_string("defmodule Foo do raise ArgumentError, ~s(oops) end", [], file: "myfile")
    end

    assert Exception.format_stacktrace(System.stacktrace) =~ "myfile:1: (module)"
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
           ~r"#Function<\d+\.\d+/0 in ExceptionTest\.test format_fa/1>/1"
  end

  ## Format exits

  test "format_exit" do
    assert Exception.format_exit(:bye) == ":bye"
    assert Exception.format_exit(:noconnection) == "no connection"
    assert Exception.format_exit({:nodedown, :"node@host"}) == "no connection to node@host"
    assert Exception.format_exit(:timeout) == "time out"
    assert Exception.format_exit(:noproc) == "no process"
    assert Exception.format_exit(:killed) == "killed"
    assert Exception.format_exit(:normal) == "normal"
    assert Exception.format_exit(:shutdown) == "shutdown"
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

    assert Exception.format_exit(reason) ==
           "exited in: :gen_server.call(:does_not_exist, :hello)\n    ** (EXIT) no process"
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

  test "sanitize_file_line" do
    assert Exception.sanitize_file_line("/usr/file.ex", 10) == "/usr/file.ex:10:"
    assert Exception.sanitize_file_line("exception_test.exs", 20, " **") == "exception_test.exs:20: **"
    assert Exception.sanitize_file_line("exception_test.exs", nil, " **") == "exception_test.exs: **"
    assert Exception.sanitize_file_line("exception_test.exs", "") == "exception_test.exs:"
    assert Exception.sanitize_file_line("", "", "SUFFIX") == ""
    assert Exception.sanitize_file_line("nofile", "100a") == "nofile:100a:"
    assert Exception.sanitize_file_line("nofile", :invalid_line) == "nofile:invalid_line:"
    assert Exception.sanitize_file_line(:invalid_file, :invalid_line) == ""
  end


  ####################################################
  # CompileError - Test case for an Error that involve :file and :line

  ## Raise exceptions with options
  test "raise CompileError with options" do
    assert_raise CompileError,
      "yace.ex: yet another compile error",
      fn -> raise CompileError, [file: "yace.ex", description: "yet another compile error"] end

    assert_raise CompileError,
      "/usr/local/lib/elixir/rocks.ex:2015: custom error message",
      fn -> raise CompileError, [
        file: "/usr/local/lib/elixir/rocks.ex",
        line: 2015,
        message: "custom error message",
        ]
      end

    assert_raise CompileError,
      "dir/file/exception_test.exs:42: compile error",
      fn -> raise CompileError, [file: "dir/file/exception_test.exs", line: 42] end

    assert_raise CompileError,
      "compile error",
      fn -> raise CompileError, [line: 84] end

    # ignore invalid file names
    assert_raise CompileError,
      "compile error",
      fn -> raise CompileError, [file: ""] end
    assert_raise CompileError,
      "compile error",
      fn -> raise CompileError, [file: :wrong_value] end
  end
  
  test "CompileError - message as second argument" do
    assert_raise CompileError,
      "custom message",
      fn -> raise CompileError, "custom message"
    end
  end

  test "CompileError - invalid options" do
    assert_raise CompileError,
      "file.ex:10: custom error",
      fn ->
        raise CompileError, [file: "file.ex", line: 10, message: "custom error", description: ""]
    end
  end

  test "CompileError - force all options to nil, except message" do
    assert_raise CompileError,
      "",
      fn ->
        raise CompileError, [file: nil, line: nil, description: nil, message: ""]
    end
  end

  test "CompileError - message: nil" do
    assert_raise CompileError,
      "",
      fn ->
        raise CompileError, [message: nil, description: nil]
    end
  end

  ####################################################
  # Errors using file and line number

  ## Raise exceptions with no options
  test "raise exceptions that require :file (no options)" do
    assert_raise CompileError,
      "compile error",
      fn -> raise CompileError end

    assert_raise SyntaxError,
      "syntax error",
      fn -> raise SyntaxError end

    assert_raise TokenMissingError,
      "expression is incomplete",
      fn -> raise TokenMissingError end

    assert_raise Code.LoadError,
      "could not load file",
      fn -> raise Code.LoadError end
  end

  test "raise exceptions that require :file (with options)" do
    assert_raise SyntaxError,
      "file.ex:1: syntax error",
      fn -> raise SyntaxError, [file: "file.ex", line: 1] end

    assert_raise TokenMissingError,
      "file.ex:2: custom error message",
      fn -> raise TokenMissingError, [file: "file.ex", line: "2", message: "custom error message"] end

    assert_raise Code.LoadError,
      "custom error message",
      fn -> raise Code.LoadError, [file: "", description: "desc", message: "custom error message"] end
  end

  ####################################################
  # TestEmptyError & TestNonEmptyError

  test "TestEmptyError" do
    undefined_error_msg = "got nil while retrieving Exception.message/1 for %TestEmptyError{message: nil} (expected a string)"

    assert_raise TestEmptyError,
      "got nil while retrieving Exception.message/1 for %TestEmptyError{message: nil} (expected a string)",
      fn -> raise TestEmptyError
    end

    assert_raise TestEmptyError,
      "yes",
      fn -> raise TestEmptyError, [message: "yes"]
    end

    assert_raise TestEmptyError,
      undefined_error_msg,
      fn -> raise TestEmptyError, [description: "desc"]
    end
  end

  test "TestNonEmptyError - options" do
    assert_raise TestNonEmptyError,
      "custom error",
      fn -> raise TestNonEmptyError, [file: "file.ex", line: 10, message: "custom error"]
    end

    assert_raise TestNonEmptyError,
      "compile test error",
      fn -> raise TestNonEmptyError, [unknown_option: ""] # ignore unknown options
    end

    assert_raise TestNonEmptyError,
      "yes",
      fn -> raise TestNonEmptyError, [message: "yes"]
    end

    assert_raise TestNonEmptyError,
      "desc",
      fn -> raise TestNonEmptyError, [description: "desc"]
    end
  end

  ####################################################
  ## Exception messages

  import Exception, only: [message: 1]

  test "runtime error message" do
    assert %RuntimeError{} |> message == "runtime error"
    assert %RuntimeError{message: "exception"} |> message == "exception"
  end

  test "argument error message" do
    assert %ArgumentError{} |> message == "argument error"
    assert %ArgumentError{message: "exception"} |> message == "exception"
  end

  test "undefined function message" do
    assert %UndefinedFunctionError{} |> message == "undefined function"
    assert %UndefinedFunctionError{module: Kernel, function: :bar, arity: 1} |> message ==
           "undefined function: Kernel.bar/1"
    assert %UndefinedFunctionError{module: Foo, function: :bar, arity: 1} |> message ==
           "undefined function: Foo.bar/1 (module Foo is not available)"
    assert %UndefinedFunctionError{module: nil, function: :bar, arity: 0} |> message ==
           "undefined function: nil.bar/0"
  end

  test "function clause message" do
    assert %FunctionClauseError{} |> message ==
           "no function clause matches"
    assert %FunctionClauseError{module: Foo, function: :bar, arity: 1} |> message ==
           "no function clause matching in Foo.bar/1"
  end

  test "erlang error message" do
    assert %ErlangError{original: :sample} |> message ==
           "erlang error: :sample"
  end
end
