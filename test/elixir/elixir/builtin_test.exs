Code.require_file "../../test_helper", __FILE__

defmodule Elixir.Builtin.Test do
  use ExUnit.Case

  test :fun do
     assert is_function(fun(System, :argv, 0))
     assert :erlang.fun_info(fun(System, :argv, 0), :arity) == {:arity, 0}
     assert is_list(fun(System, :argv, 0).())

     assert is_function(fun(:erlang, :atom_to_list, 1))
     assert :erlang.fun_info(fun(:erlang, :atom_to_list, 1), :arity) == {:arity, 1}
     assert fun(:erlang, :atom_to_list, 1).(:a) == 'a'
  end
end

