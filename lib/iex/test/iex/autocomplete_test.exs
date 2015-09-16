Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.AutocompleteTest do
  use ExUnit.Case, async: true

  def expand(expr) do
    IEx.Autocomplete.expand(Enum.reverse expr)
  end

  test "erlang module completion" do
    assert expand(':zl') == {:yes, 'ib.', []}
  end

  test "erlang module no completion" do
    assert expand(':unknown') == {:no, '', []}
  end

  test "erlang module multiple values completion" do
    {:yes, '', list} = expand(':user')
    assert 'user' in list
    assert 'user_drv' in list
  end

  test "erlang root completion" do
    {:yes, '', list} = expand(':')
    assert is_list(list)
    assert 'lists' in list
  end

  test "elixir proxy" do
    {:yes, '', list} = expand('E')
    assert 'Elixir' in list
  end

  test "elixir completion" do
    assert expand('En') == {:yes, 'um', []}
    assert expand('Enumera') == {:yes, 'ble.', []}
  end

  test "elixir completion with self" do
    assert expand('Enumerable') == {:yes, '.', []}
  end

  test "elixir completion on modules from load path" do
    assert expand('Str') == {:yes, [], ['Stream', 'String', 'StringIO']}
    assert expand('Ma') == {:yes, '', ['Macro', 'Map', 'MapSet', 'MatchError']}
    assert expand('Dic') == {:yes, 't.', []}
    assert expand('Ex')  == {:yes, [], ['ExUnit', 'Exception']}
  end

  test "elixir no completion" do
    assert expand('.')   == {:no, '', []}
    assert expand('Xyz') == {:no, '', []}
    assert expand('x.Foo') == {:no, '', []}
  end

  test "elixir root submodule completion" do
    assert expand('Elixir.Acce') == {:yes, 'ss.', []}
  end

  test "elixir submodule completion" do
    assert expand('String.Cha') == {:yes, 'rs.', []}
  end

  test "elixir submodule no completion" do
    assert expand('IEx.Xyz') == {:no, '', []}
  end

  test "function completion" do
    assert expand('System.ve') == {:yes, 'rsion', []}
    assert expand(':ets.fun2') == {:yes, 'ms', []}
  end

  test "function completion with arity" do
    assert expand('String.printable?')  == {:yes, '', ['printable?/1']}
    assert expand('String.printable?/') == {:yes, '', ['printable?/1']}
  end

  test "macro completion" do
    {:yes, '', list} = expand('Kernel.is_')
    assert is_list(list)
  end

  test "imports completion" do
    {:yes, '', list} = expand('')
    assert is_list(list)
    assert 'h/1' in list
    assert 'unquote/1' in list
    assert 'pwd/0' in list
  end

  test "kernel import completion" do
    assert expand('defstru') == {:yes, 'ct', []}
    assert expand('put_') == {:yes, '', ['put_elem/3', 'put_in/2', 'put_in/3']}
  end

  test "kernel special form completion" do
    assert expand('unquote_spl') == {:yes, 'icing', []}
  end

  test "completion inside expression" do
    assert expand('1 En') == {:yes, 'um', []}
    assert expand('Test(En') == {:yes, 'um', []}
    assert expand('Test :zl') == {:yes, 'ib.', []}
    assert expand('[:zl') == {:yes, 'ib.', []}
    assert expand('{:zl') == {:yes, 'ib.', []}
  end

  defmodule SublevelTest.LevelA.LevelB do
  end

  test "elixir completion sublevel" do
    assert expand('IEx.AutocompleteTest.SublevelTest.') == {:yes, 'LevelA.', []}
  end

  defmodule MyServer do
    def current_env do
      %Macro.Env{aliases: [{MyList, List}, {EList, :lists}]}
    end
  end

  test "complete aliases of elixir modules" do
    Application.put_env(:iex, :autocomplete_server, MyServer)

    assert expand('MyL') == {:yes, 'ist.', []}
    assert expand('MyList') == {:yes, '.', []}
    assert expand('MyList.to_integer') == {:yes, [], ['to_integer/1', 'to_integer/2']}
  end

  test "complete aliases of erlang modules" do
    Application.put_env(:iex, :autocomplete_server, MyServer)

    assert expand('EL') == {:yes, 'ist.', []}
    assert expand('EList') == {:yes, '.', []}
    assert expand('EList.map') == {:yes, [], ['map/2', 'mapfoldl/3', 'mapfoldr/3']}
  end

end
