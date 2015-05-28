Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.AutocompleteTest do
  use ExUnit.Case, async: true

  def expand(expr) do
    IEx.Autocomplete.expand(Enum.reverse expr)
  end

  test :erlang_module_completion do
    assert expand(':zl') == {:yes, 'ib.', []}
  end

  test :erlang_module_no_completion do
    assert expand(':unknown') == {:no, '', []}
  end

  test :erlang_module_multiple_values_completion do
    {:yes, '', list} = expand(':user')
    assert 'user' in list
    assert 'user_drv' in list
  end

  test :erlang_root_completion do
    {:yes, '', list} = expand(':')
    assert is_list(list)
    assert 'lists' in list
  end

  test :elixir_proxy do
    {:yes, '', list} = expand('E')
    assert 'Elixir' in list
  end

  test :elixir_completion do
    assert expand('En') == {:yes, 'um', []}
    assert expand('Enumera') == {:yes, 'ble.', []}
  end

  test :elixir_completion_with_self do
    assert expand('Enumerable') == {:yes, '.', []}
  end

  test :elixir_completion_on_modules_from_load_path do
    assert expand('Str') == {:yes, [], ['Stream', 'String', 'StringIO']}
    assert expand('Ma') == {:yes, '', ['Macro', 'Map', 'MapSet', 'MatchError']}
    assert expand('Dic') == {:yes, 't.', []}
    assert expand('Ex')  == {:yes, [], ['ExUnit', 'Exception']}
  end

  test :elixir_no_completion do
    assert expand('.')   == {:no, '', []}
    assert expand('Xyz') == {:no, '', []}
    assert expand('x.Foo') == {:no, '', []}
  end

  test :elixir_root_submodule_completion do
    assert expand('Elixir.Acce') == {:yes, 'ss.', []}
  end

  test :elixir_submodule_completion do
    assert expand('String.Cha') == {:yes, 'rs.', []}
  end

  test :elixir_submodule_no_completion do
    assert expand('IEx.Xyz') == {:no, '', []}
  end

  test :function_completion do
    assert expand('System.ve') == {:yes, 'rsion', []}
    assert expand(':ets.fun2') == {:yes, 'ms', []}
  end

  test :function_completion_with_arity do
    assert expand('String.printable?')  == {:yes, '', ['printable?/1']}
    assert expand('String.printable?/') == {:yes, '', ['printable?/1']}
  end

  test :macro_completion do
    {:yes, '', list} = expand('Kernel.is_')
    assert is_list(list)
  end

  test :imports_completion do
    {:yes, '', list} = expand('')
    assert is_list(list)
    assert 'h/1' in list
    assert 'unquote/1' in list
    assert 'pwd/0' in list
  end

  test :kernel_import_completion do
    assert expand('defstru') == {:yes, 'ct', []}
    assert expand('put_') == {:yes, '', ['put_elem/3', 'put_in/2', 'put_in/3']}
  end

  test :kernel_special_form_completion do
    assert expand('unquote_spl') == {:yes, 'icing', []}
  end

  test :completion_inside_expression do
    assert expand('1 En') == {:yes, 'um', []}
    assert expand('Test(En') == {:yes, 'um', []}
    assert expand('Test :zl') == {:yes, 'ib.', []}
    assert expand('[:zl') == {:yes, 'ib.', []}
    assert expand('{:zl') == {:yes, 'ib.', []}
  end

  defmodule SublevelTest.LevelA.LevelB do
  end

  test :elixir_completion_sublevel do
    assert expand('IEx.AutocompleteTest.SublevelTest.') == {:yes, 'LevelA.', []}
  end
end
