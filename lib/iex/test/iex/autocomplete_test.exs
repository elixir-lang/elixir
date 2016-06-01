Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.AutocompleteTest do
  use ExUnit.Case, async: true

  def expand(expr) do
    IEx.Autocomplete.expand(Enum.reverse expr)
  end

  test "Erlang module completion" do
    assert expand(':zl') == {:yes, 'ib.', []}
  end

  test "Erlang module no completion" do
    assert expand(':unknown') == {:no, '', []}
    assert expand('Enum:') == {:no, '', []}
  end

  test "Erlang module multiple values completion" do
    {:yes, '', list} = expand(':user')
    assert 'user' in list
    assert 'user_drv' in list
  end

  test "Erlang root completion" do
    {:yes, '', list} = expand(':')
    assert is_list(list)
    assert 'lists' in list
  end

  test "Elixir proxy" do
    {:yes, '', list} = expand('E')
    assert 'Elixir' in list
  end

  test "Elixir completion" do
    assert expand('En') == {:yes, 'um', []}
    assert expand('Enumera') == {:yes, 'ble.', []}
  end

  test "Elixir completion with self" do
    assert expand('Enumerable') == {:yes, '.', []}
  end

  test "Elixir completion on modules from load path" do
    assert expand('Str') == {:yes, [], ['Stream', 'String', 'StringIO']}
    assert expand('Ma') == {:yes, '', ['Macro', 'Map', 'MapSet', 'MatchError']}
    assert expand('Dic') == {:yes, 't.', []}
    assert expand('Ex')  == {:yes, [], ['ExUnit', 'Exception']}
  end

  test "Elixir no completion for underscored functions with no doc" do
    {:module, _, bytecode, _} =
      defmodule Elixir.Sample do
        def __foo__(), do: 0
        @doc "Bar doc"
        def __bar__(), do: 1
      end
    File.write!("Elixir.Sample.beam", bytecode)
    assert Code.get_docs(Sample, :docs)
    assert expand('Sample._') == {:yes, '_bar__', []}
  after
    File.rm("Elixir.Sample.beam")
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "Elixir no completion" do
    assert expand('.')   == {:no, '', []}
    assert expand('Xyz') == {:no, '', []}
    assert expand('x.Foo') == {:no, '', []}
  end

  test "Elixir root submodule completion" do
    assert expand('Elixir.Acce') == {:yes, 'ss.', []}
  end

  test "Elixir submodule completion" do
    assert expand('String.Cha') == {:yes, 'rs.', []}
  end

  test "Elixir submodule no completion" do
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

  test "ampersand completion" do
    assert expand('&Enu') == {:yes, 'm', []}
    assert expand('&Enum.a') == {:yes, [], ['all?/1', 'all?/2', 'any?/1', 'any?/2', 'at/2', 'at/3']}
    assert expand('f = &Enum.a') == {:yes, [], ['all?/1', 'all?/2', 'any?/1', 'any?/2', 'at/2', 'at/3']}
  end

  defmodule SublevelTest.LevelA.LevelB do
  end

  test "Elixir completion sublevel" do
    assert expand('IEx.AutocompleteTest.SublevelTest.') == {:yes, 'LevelA.', []}
  end

  defmodule MyServer do
    def current_env do
      %Macro.Env{aliases: [{MyList, List}, {EList, :lists}]}
    end
  end

  test "complete aliases of Elixir modules" do
    Application.put_env(:iex, :autocomplete_server, MyServer)

    assert expand('MyL') == {:yes, 'ist.', []}
    assert expand('MyList') == {:yes, '.', []}
    assert expand('MyList.to_integer') == {:yes, [], ['to_integer/1', 'to_integer/2']}
  end

  test "complete aliases of Erlang modules" do
    Application.put_env(:iex, :autocomplete_server, MyServer)

    assert expand('EL') == {:yes, 'ist.', []}
    assert expand('EList') == {:yes, '.', []}
    assert expand('EList.map') == {:yes, [], ['map/2', 'mapfoldl/3', 'mapfoldr/3']}
  end

  test "completion for functions added when compiled module is reloaded" do
    {:module, _, bytecode, _} =
      defmodule Elixir.Sample do
        def foo(), do: 0
      end
    File.write!("Elixir.Sample.beam", bytecode)
    assert Code.get_docs(Sample, :docs)
    assert expand('Sample.foo') == {:yes, '', ['foo/0']}

    Code.compiler_options(ignore_module_conflict: true)
    defmodule Elixir.Sample do
      def foo(), do: 0
      def foobar(), do: 0
    end
    assert expand('Sample.foo') == {:yes, '', ['foo/0', 'foobar/0']}
  after
    File.rm("Elixir.Sample.beam")
    Code.compiler_options(ignore_module_conflict: false)
    :code.purge(Sample)
    :code.delete(Sample)
  end
end
