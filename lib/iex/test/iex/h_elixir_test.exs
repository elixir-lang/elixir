Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.H_Elixir_Test do
  use IEx.Case

  @h_modules [IEx.Helpers, Kernel, Kernel.SpecialForms]

  test "documentation/1 for Elixir module" do
    assert {:not_found,_} = IEx.H_Elixir.documentation(IEx.Case)
    assert {:found,_} = IEx.H_Elixir.documentation(Tuple)
  end 

  test "documentation/1 for Erlang module" do
    assert {:unknown,_} = IEx.H_Elixir.documentation(:erlang)
  end 

  test "documentation/2 for Elixir module function" do
    assert {:found,_} = IEx.H_Elixir.documentation(Tuple,:to_list)
  end 

  test "documentation/3 for Elixir module function arity" do
    assert {:not_found,_} = IEx.H_Elixir.documentation(Tuple,:duplicate,1)
    assert {:found,_} = IEx.H_Elixir.documentation(Tuple,:duplicate,2)
  end

  test "documentation/3 for Elixir module function arity with default arg" do
    assert {:found,_} = IEx.H_Elixir.documentation(IEx.Helpers,:c,1)
    assert {:found,_} = IEx.H_Elixir.documentation(IEx.Helpers,:c,2)
  end

  test "documentation correctly identifies macros" do 
    assert [{:not_found,_},{:found,_},{:not_found,_}] = test_mod_lists(:defp)
    assert [{:not_found,_},{:not_found,_},{:found,_}] = test_mod_lists(:{})
  end

  defp test_mod_lists(function) do
    @h_modules |> 
    Enum.map(fn(mod) -> 
               IEx.H_Elixir.documentation(mod,function) end )   
  end
 
end
