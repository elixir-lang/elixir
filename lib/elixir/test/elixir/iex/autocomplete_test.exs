Code.require_file "../../test_helper", __FILE__

defmodule IEx.AutocompleteTest do
  use ExUnit.Case, async: true

  def expand(expr) do
    IEx.Autocomplete.expand(List.reverse expr)
  end

  test :erlang_module_simple_completion do
    assert expand(':z') == {:yes, 'lib.', []}
  end

  test :erlang_module_no_completion do
    assert expand(':x') == {:no, '', []}
  end

  test :erlang_alias_completion do
    assert expand('Erlang.z') == {:yes, 'lib.', []}
  end

  test :erlang_alias_call_completion do
    assert expand('Erlang.lists.flatt') == {:yes, 'en', []}
  end

  test :erlang_alias_function_list_completion do
    {:yes, '', list} = expand('Erlang.lists.')
    assert is_list(list)
  end

  test :erlang_module_common_prefix_completion do
    assert expand(':us') == {:yes, 'er', []}
  end

  test :erlang_module_multiple_values_completion do
    {:yes, '', list} = expand(':user') 
    assert length(list) > 1
  end

  test :elixir_simple_completion do
    assert expand('En') == {:yes, 'um.', []}
  end

  test :elixir_auto_completion_with_self do
    assert expand('Enum') == {:yes, '.', []}
  end

  test :elixir_no_completion do
    assert expand('Xyz') == {:no, '', []}
  end

  test :elixir_root_submodule_completion do
    _ = [foo: 1][:foo]
    assert expand('Elixir.Acce') == {:yes, 'ss.', []}
  end

  test :elixir_submodule_completion do
    assert expand('Binary.Cha') == {:yes, 'rs.', []}
  end

  test :elixir_submodule_no_completion do
    assert expand('IEx.Xyz') == {:no, '', []} 
  end

  test :elixir_function_completion do
    assert expand('System.ve') == {:yes, 'rsion', []}
  end

  test :elixir_macro_completion do
    {:yes, '', list} = expand('Kernel.is_')
    assert is_list(list)
  end

  test :elixir_root_completion do
    {:yes, '', list} = expand('')
    assert is_list(list)
    assert 'd/1' in list
    assert 'Elixir' in list
  end

  test :elixir_kernel_completion do
    assert expand('defreco') == {:yes, 'rd', []}
  end

  test :elixir_and_erlang_proxies do
    {:yes, '', list} = expand('E')
    assert 'Elixir' in list
    assert 'Erlang' in list
  end

  test :elixir_erlang_module_root_completion do
    {:yes, '', list} = expand(':')
    assert is_list(list)
    assert 'lists' in list
  end

  test :elixir_erlang_alias_root_completion do
    {:yes, '', list} = expand('Erlang.')
    assert is_list(list)
    assert 'lists' in list
  end

  test :completion_inside_expression do
    assert expand('1+En') == {:yes, 'um.', []}
    assert expand('Test(En') == {:yes, 'um.', []}
    assert expand('Test :z') == {:yes, 'lib.', []}
    assert expand('[:z') == {:yes, 'lib.', []}
  end
end
