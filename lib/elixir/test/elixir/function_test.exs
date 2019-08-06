Code.require_file("test_helper.exs", __DIR__)

defmodule DummyFunction do
  def function_with_arity_0 do
    true
  end

  def zero?(0), do: true
  def zero?(_), do: false
end

defmodule FunctionTest do
  use ExUnit.Case, async: true

  doctest Function
  import Function

  @information_keys_for_named [:type, :module, :arity, :name, :env]
  @information_keys_for_anonymous @information_keys_for_named ++
                                    [:pid, :index, :new_index, :new_uniq, :uniq]

  describe "capture/3" do
    test "captures module functions with arity 0" do
      f = capture(DummyFunction, :function_with_arity_0, 0)

      assert is_function(f)
    end

    test "captures module functions with any arity" do
      f = capture(DummyFunction, :zero?, 1)

      assert is_function(f)
      assert f.(0)
    end
  end

  describe "info/1" do
    test "returns info for named captured functions" do
      f = &DummyFunction.zero?/1
      expected = [module: DummyFunction, name: :zero?, arity: 1, env: [], type: :external]

      result = info(f)

      assert expected == result
    end

    test "returns info for anonymous functions" do
      f = fn x -> x end

      result = info(f)

      for {key, _value} <- result do
        assert key in @information_keys_for_anonymous
      end
    end
  end

  describe "info/2" do
    test "returns info for every possible information key for named functions" do
      f = &DummyFunction.zero?/1

      for x <- @information_keys_for_named do
        assert {^x, _} = info(f, x)
      end
    end

    test "returns info for every possible information key for anonymous functions" do
      f = &DummyFunction.zero?/1

      for x <- @information_keys_for_anonymous do
        assert {^x, _} = info(f, x)
      end

      assert {:arity, 1} = info(f, :arity)
    end
  end

  describe "identity/1" do
    test "returns whatever it gets passed" do
      assert :hello = Function.identity(:hello)
    end
  end
end
