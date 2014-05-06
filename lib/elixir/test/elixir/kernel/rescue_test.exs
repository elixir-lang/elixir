Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.RescueTest do
  use ExUnit.Case, async: true

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
     raise ArgumentError, message: ""
    rescue
      ArgumentError -> true
    end

    assert result
  end

  test :rescue_argument_error_and_undefined_function_error do
    named = ArgumentError

    result = try do
      Certainly.Undefined.function(1, 2, 3)
    rescue
      x in [named] -> Exception.message(x)
    catch
      :error, _ -> "didn't catch it"
    end

    assert result == "didn't catch it"
  end

  test :rescue_named_with_underscore do
    result = try do
      raise "an exception"
    rescue
      x in _ -> Exception.message(x)
    end

    assert result == "an exception"
  end

  test :rescue_named_defined_variable do
    expected = RuntimeError

    result = try do
      raise RuntimeError, message: "an exception"
    rescue
      x in [expected, AnotherError] -> Exception.message(x)
    catch
      :error, _ -> false
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

    assert result == "undefined function: DoNotExist.for_sure/0"
  end

  test :function_clause_error do
    result = try do
      zero(1)
    rescue
      x in [FunctionClauseError] -> Exception.message(x)
    end

    assert result == "no function clause matching in Kernel.RescueTest.zero/1"
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

  test :undefined_function_error_from_expected_variable do
    expected = UndefinedFunctionError
    result = try do
      DoNotExist.for_sure()
    rescue
      x in [expected] -> Exception.message(x)
    end

    assert result == "undefined function: DoNotExist.for_sure/0"
  end

  test :undefined_function_error_as_erlang_error do
    result = try do
      DoNotExist.for_sure()
    rescue
      x in [ErlangError] -> Exception.message(x)
    end

    assert result == "undefined function: DoNotExist.for_sure/0"
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

    assert result == "undefined function: DoNotExist.for_sure/0"
  end

  defp zero(0), do: 0
end
