Code.require_file "test_helper.exs", __DIR__

defmodule Kernel.ExceptionTest do
  use ExUnit.Case, async: true

  test :is_exception do
    assert is_exception(RuntimeError.new)
    refute is_exception(empty_tuple)
    refute is_exception(a_tuple)
    refute is_exception(a_list)
  end

  test :normalize do
    assert is_record Exception.normalize(:badarg), ArgumentError
    assert is_record Exception.normalize(ArgumentError[]), ArgumentError

    assert Exception.normalize(:throw, :badarg) == :badarg
    assert is_record Exception.normalize(:error, :badarg), ArgumentError
    assert is_record Exception.normalize(:error, ArgumentError[]), ArgumentError
  end

  test :format_stacktrace_entry_with_no_file_or_line do
    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], []}) == "Foo.bar(1, 2, 3)"
    assert Exception.format_stacktrace_entry({Foo, :bar, [], []}) == "Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, 1, []}) == "Foo.bar/1"
  end

  test :format_stacktrace_entry_with_file_and_line do
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex', line: 10]}) == "file.ex:10: Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], [file: 'file.ex', line: 10]}) == "file.ex:10: Foo.bar(1, 2, 3)"
    assert Exception.format_stacktrace_entry({Foo, :bar, 1, [file: 'file.ex', line: 10]}) == "file.ex:10: Foo.bar/1"
  end

  test :format_stacktrace_entry_with_file_and_line_and_cwd do
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: '/foo/file.ex', line: 10]}, "/foo") == "file.ex:10: Foo.bar()"
  end

  test :format_stacktrace_entry_with_file_no_line do
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex']}) == "file.ex: Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, [], [file: 'file.ex', line: 0]}) == "file.ex: Foo.bar()"
    assert Exception.format_stacktrace_entry({Foo, :bar, [1, 2, 3], [file: 'file.ex']}) == "file.ex: Foo.bar(1, 2, 3)"
    assert Exception.format_stacktrace_entry({Foo, :bar, 1, [file: 'file.ex']}) == "file.ex: Foo.bar/1"
  end

  test :format_stacktrace_entry_with_fun do
    assert Exception.format_stacktrace_entry({fn(x) -> x end, [1], []}) =~ %r/#Function<.+>\(1\)/
    assert Exception.format_stacktrace_entry({fn(x, y) -> { x, y } end, 2, []}) =~ %r"#Function<.+>/2"
  end

  test :format_mfa do
    assert Exception.format_mfa(Foo, nil, 1) == "Foo.nil/1"
    assert Exception.format_mfa(Foo, :bar, 1) == "Foo.bar/1"
    assert Exception.format_mfa(Foo, :bar, []) == "Foo.bar()"
    assert Exception.format_mfa(:foo, :bar, [1, 2]) == ":foo.bar(1, 2)"
    assert Exception.format_mfa(Foo, :"bar baz", 1) == "Foo.\"bar baz\"/1"
  end

  test :format_fa do
    assert Exception.format_fa(fn -> end, 1) =~
           %r"#Function<\d\.\d+ in Kernel\.ExceptionTest\.test_format_fa/1>/1"
  end

  test :runtime_error_message do
    assert RuntimeError.new.message == "runtime error"
    assert RuntimeError.new(message: "exception").message == "exception"
  end

  test :argument_error_message do
    assert ArgumentError.new.message == "argument error"
    assert ArgumentError.new(message: "exception").message == "exception"
  end

  test :undefined_function_message do
    assert UndefinedFunctionError.new.message == "undefined function"
    assert UndefinedFunctionError.new(module: Foo, function: :bar, arity: 1).message == "undefined function: Foo.bar/1"
  end

  test :function_clause_message do
    assert FunctionClauseError.new.message == "no function clause matches"
    assert FunctionClauseError.new(module: Foo, function: :bar, arity: 1).message == "no function clause matching in Foo.bar/1"
  end

  test :erlang_error_message do
    assert ErlangError.new(original: :sample).message == "erlang error: :sample"
  end

  test :raise_preserves_the_stacktrace do
    stacktrace =
    try do
      raise "a"
    rescue _ -> 
      [top|_] = System.stacktrace
      top
    end
    file = to_char_list(__FILE__)
    assert {Kernel.ExceptionTest, :test_raise_preserves_the_stacktrace, _,
           [file: ^file, line: 90]} = stacktrace # line #90 is sensitive
  end

  defp empty_tuple, do: {}
  defp a_tuple, do: { :foo, :bar, :baz }
  defp a_list,  do: [ :foo, :bar, :baz ]
end
