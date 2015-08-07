Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.RaiseTest do
  use ExUnit.Case, async: true

  # Silence warnings
  defp atom, do: RuntimeError
  defp binary, do: "message"
  defp opts, do: [message: "message"]
  defp struct, do: %RuntimeError{message: "message"}

  @trace [{:foo, :bar, 0, []}]

  test "raise message" do
    assert_raise RuntimeError, "message", fn ->
      raise "message"
    end

    assert_raise RuntimeError, "message", fn ->
      var = binary()
      raise var
    end
  end

  test "raise with no arguments" do
    assert_raise RuntimeError, fn ->
      raise RuntimeError
    end

    assert_raise RuntimeError, fn ->
      var = atom()
      raise var
    end
  end

  test "raise with arguments" do
    assert_raise RuntimeError, "message", fn ->
      raise RuntimeError, message: "message"
    end

    assert_raise RuntimeError, "message", fn ->
      atom = atom()
      opts = opts()
      raise atom, opts
    end
  end

  test "raise existing exception" do
    assert_raise RuntimeError, "message", fn ->
      raise %RuntimeError{message: "message"}
    end

    assert_raise RuntimeError, "message", fn ->
      var = struct()
      raise var
    end
  end

  test "reraise message" do
    try do
      reraise "message", @trace
      flunk "should not reach"
    rescue
      RuntimeError ->
        assert @trace == :erlang.get_stacktrace()
    end

    try do
      var = binary()
      reraise var, @trace
      flunk "should not reach"
    rescue
      RuntimeError ->
        assert @trace == :erlang.get_stacktrace()
    end
  end

  test "reraise with no arguments" do
    try do
      reraise RuntimeError, @trace
      flunk "should not reach"
    rescue
      RuntimeError ->
        assert @trace == :erlang.get_stacktrace()
    end

    try do
      var = atom()
      reraise var, @trace
      flunk "should not reach"
    rescue
      RuntimeError ->
        assert @trace == :erlang.get_stacktrace()
    end
  end

  test "reraise with arguments" do
    try do
      reraise RuntimeError, [message: "message"], @trace
      flunk "should not reach"
    rescue
      RuntimeError ->
        assert @trace == :erlang.get_stacktrace()
    end

    try do
      atom = atom()
      opts = opts()
      reraise atom, opts, @trace
      flunk "should not reach"
    rescue
      RuntimeError ->
        assert @trace == :erlang.get_stacktrace()
    end
  end

  test "reraise existing exception" do
    try do
      reraise %RuntimeError{message: "message"}, @trace
      flunk "should not reach"
    rescue
      RuntimeError ->
        assert @trace == :erlang.get_stacktrace()
    end

    try do
      var = struct()
      reraise var, @trace
      flunk "should not reach"
    rescue
      RuntimeError ->
        assert @trace == :erlang.get_stacktrace()
    end
  end

  test :rescue_with_underscore_no_exception do
    result = try do
      RescueUndefinedModule.go
    rescue
      _ -> true
    end

    assert result
  end

  test :rescue_with_higher_precedence_than_catch do
    result = try do
      RescueUndefinedModule.go
    catch
      _, _ -> false
    rescue
      _ -> true
    end

    assert result
  end

  test :rescue_runtime_error do
    result = try do
      raise "an exception"
    rescue
      RuntimeError -> true
    catch
      :error, _ -> false
    end

    assert result

    result = try do
      raise "an exception"
    rescue
      AnotherError -> true
    catch
      :error, _ -> false
    end

    refute result
  end

  test :rescue_named_runtime_error do
    result = try do
      raise "an exception"
    rescue
      x in [RuntimeError] -> Exception.message(x)
    catch
      :error, _ -> false
    end

    assert result == "an exception"
  end

  test :rescue_argument_error_from_elixir do
    result = try do
     raise ArgumentError, ""
    rescue
      ArgumentError -> true
    end

    assert result
  end

  test :rescue_named_with_underscore do
    result = try do
      raise "an exception"
    rescue
      x in _ -> Exception.message(x)
    end

    assert result == "an exception"
  end

  test :wrap_custom_erlang_error do
    result = try do
      :erlang.error(:sample)
    rescue
      x in [RuntimeError, ErlangError] -> Exception.message(x)
    end

    assert result == "erlang error: :sample"
  end

  test :undefined_function_error do
    result = try do
      DoNotExist.for_sure()
    rescue
      x in [UndefinedFunctionError] -> Exception.message(x)
    end

    assert result == "undefined function: DoNotExist.for_sure/0 (module DoNotExist is not available)"
  end

  test :function_clause_error do
    result = try do
      zero(1)
    rescue
      x in [FunctionClauseError] -> Exception.message(x)
    end

    assert result == "no function clause matching in Kernel.RaiseTest.zero/1"
  end

  test :badarg_error do
    result = try do
      :erlang.error(:badarg)
    rescue
      x in [ArgumentError] -> Exception.message(x)
    end

    assert result == "argument error"
  end

  test :tuple_badarg_error do
    result = try do
      :erlang.error({:badarg, [1, 2, 3]})
    rescue
      x in [ArgumentError] -> Exception.message(x)
    end

    assert result == "argument error: [1, 2, 3]"
  end

  test :badarith_error do
    result = try do
      :erlang.error(:badarith)
    rescue
      x in [ArithmeticError] -> Exception.message(x)
    end

    assert result == "bad argument in arithmetic expression"
  end

  test :badarity_error do
    fun    = fn(x) -> x end
    string = "#{inspect(fun)} with arity 1 called with 2 arguments (1, 2)"

    result = try do
      fun.(1, 2)
    rescue
      x in [BadArityError] -> Exception.message(x)
    end

    assert result == string
  end

  test :badfun_error do
    x = :example
    result = try do
      x.(2)
    rescue
      x in [BadFunctionError] -> Exception.message(x)
    end

    assert result == "expected a function, got: :example"
  end

  test :badmatch_error do
    x = :example
    result = try do
      ^x = zero(0)
    rescue
      x in [MatchError] -> Exception.message(x)
    end

    assert result == "no match of right hand side value: 0"
  end

  if :erlang.system_info(:otp_release) >= '18' do
    test :bad_key_error do
      result = try do
        %{%{} | foo: :bar}
      rescue
        x in [KeyError] -> Exception.message(x)
      end

      assert result == "key :foo not found"

      result = try do
        %{}.foo
      rescue
        x in [KeyError] -> Exception.message(x)
      end

      assert result == "key :foo not found in: %{}"
    end

    test :bad_map_error do
      result = try do
        %{zero(0) | foo: :bar}
      rescue
        x in [BadMapError] -> Exception.message(x)
      end

      assert result == "expected a map, got: 0"
    end
  end

  test :case_clause_error do
    x = :example
    result = try do
      case zero(0) do
        ^x -> nil
      end
    rescue
      x in [CaseClauseError] -> Exception.message(x)
    end

    assert result == "no case clause matching: 0"
  end

  test :cond_clause_error do
    result = try do
      cond do
        !zero(0) -> :ok
      end
    rescue
      x in [CondClauseError] -> Exception.message(x)
    end

    assert result == "no cond clause evaluated to a true value"
  end

  test :try_clause_error do
    f = fn() -> :example end
    result = try do
      try do
        f.()
      else
        :other ->
          :ok
      end
    rescue
      x in [TryClauseError] -> Exception.message(x)
    end

    assert result == "no try clause matching: :example"
  end

  test :undefined_function_error_as_erlang_error do
    result = try do
      DoNotExist.for_sure()
    rescue
      x in [ErlangError] -> Exception.message(x)
    end

    assert result == "undefined function: DoNotExist.for_sure/0 (module DoNotExist is not available)"
  end

  defmacrop exceptions do
    [ErlangError]
  end

  test :with_macros do
    result = try do
      DoNotExist.for_sure()
    rescue
      x in exceptions -> Exception.message(x)
    end

    assert result == "undefined function: DoNotExist.for_sure/0 (module DoNotExist is not available)"
  end

  defp zero(0), do: 0
end
