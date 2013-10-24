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
      x in [RuntimeError] -> x.message
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
      x in [named] -> x.message
    catch
      :error, _ -> "didn't catch it"
    end

    assert result == "didn't catch it"
  end

  test :rescue_named_with_underscore do
    result = try do
      raise "an exception"
    rescue
      x in _ -> x.message
    end

    assert result == "an exception"
  end

  test :rescue_named_defined_variable do
    expected = RuntimeError

    result = try do
      raise RuntimeError, message: "an exception"
    rescue
      x in [expected, AnotherError] -> x.message
    catch
      :error, _ -> false
    end

    assert result == "an exception"
  end

  test :wrap_custom_erlang_error do
    result = try do
      :erlang.error(:sample)
    rescue
      x in [RuntimeError, ErlangError] -> x.message
    end

    assert result == "erlang error: :sample"
  end

  test :undefined_function_error do
    result = try do
      DoNotExist.for_sure()
    rescue
      x in [UndefinedFunctionError] -> x.message
    end

    assert result == "undefined function: DoNotExist.for_sure/0"
  end

  test :function_clause_error do
    result = try do
      zero(1)
    rescue
      x in [FunctionClauseError] -> x.message
    end

    assert result == "no function clause matching in Kernel.RescueTest.zero/1"
  end

  test :badarg_error do
    result = try do
      :erlang.error(:badarg)
    rescue
      x in [ArgumentError] -> x.message
    end

    assert result == "argument error"
  end

  test :tuple_badarg_error do
    result = try do
      :erlang.error({ :badarg, [1, 2, 3] })
    rescue
      x in [ArgumentError] -> x.message
    end

    assert result == "argument error: [1, 2, 3]"
  end

  test :badarith_error do
    result = try do
      :erlang.error(:badarith)
    rescue
      x in [ArithmeticError] -> x.message
    end

    assert result == "bad argument in arithmetic expression"
  end

  test :badarity_error do
    fun    = fn(x) -> x end
    string = "bad arity error: #{inspect(fun)} called with [1, 2]"

    result = try do
      fun.(1, 2)
    rescue
      x in [BadArityError] -> x.message
    end

    assert result == string
  end

  test :badfun_error do
    x = :example
    result = try do
      x.(2)
    rescue
      x in [BadFunctionError] -> x.message
    end

    assert result == "expected a function, got: :example"
  end

  test :badmatch_error do
    x = :example
    result = try do
      ^x = :other
    rescue
      x in [MatchError] -> x.message
    end

    assert result == "no match of right hand side value: :other"
  end

  test :case_clause_error do
    x = :example
    result = try do
      case :other do
        ^x -> nil
      end
    rescue
      x in [CaseClauseError] -> x.message
    end

    assert result == "no case clause matching: :other"
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
      x in [TryClauseError] -> x.message
    end

    assert result == "no try clause matching: :example"
  end

  test :undefined_function_error_from_expected_variable do
    expected = UndefinedFunctionError
    result = try do
      DoNotExist.for_sure()
    rescue
      x in [expected] -> x.message
    end

    assert result == "undefined function: DoNotExist.for_sure/0"
  end

  test :undefined_function_error_as_erlang_error do
    result = try do
      DoNotExist.for_sure()
    rescue
      x in [ErlangError] -> x.message
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
      x in exceptions -> x.message
    end

    assert result == "undefined function: DoNotExist.for_sure/0"
  end

  defp zero(0), do: 0
end
