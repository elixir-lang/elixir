Code.require_file "../../test_helper", __FILE__

defmodule Elixir.Builtin.Test do
  use ExUnit.Case, async: true

  test :remote_fun do
    assert is_function(fn(:erlang, :atom_to_list, 1))
    assert :erlang.fun_info(fn(:erlang, :atom_to_list, 1), :arity) == {:arity, 1}
    assert fn(:erlang, :atom_to_list, 1).(:a) == 'a'
  end

  test :local_fun do
    assert is_function(fn(:atl, 1))
    assert :erlang.fun_info(fn(:atl, 1), :arity) == {:arity, 1}
    assert fn(:atl, 1).(:a) == 'a'
  end

  test :imported_fun do
    assert is_function(fn(:atom_to_list, 1))
    assert :erlang.fun_info(fn(:atom_to_list, 1), :arity) == {:arity, 1}
    assert fn(:atom_to_list, 1).(:a) == 'a'
  end

  test :dynamic_fun do
    a = :erlang
    b = :atom_to_list
    c = 1

    assert is_function(fn(a, b, c))
    assert :erlang.fun_info(fn(a, b, c), :arity) == {:arity, 1}
    assert fn(a, b, c).(:a) == 'a'
  end

  defp atl(arg) do
    :erlang.atom_to_list arg
  end
end