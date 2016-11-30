Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.AutocompleteTest do
  use ExUnit.Case, async: true

  setup context do
    ExUnit.CaptureIO.capture_io(fn ->
      evaluator = IEx.Server.start_evaluator([])
      Process.put(:evaluator, evaluator)

      previous_line = context[:previous_line]
      if previous_line do
        send evaluator, {:eval, self(), previous_line <> "\n", %IEx.State{}}
        assert_receive {:evaled, _, _}
      end
    end)

    :ok
  end

  defmodule MyServer do
    def evaluator do
      Process.get(:evaluator)
    end
  end

  def expand(expr) do
    IEx.Autocomplete.expand(Enum.reverse(expr), MyServer)
  end

  test "Erlang module completion" do
    assert expand(':zl') == {:yes, 'ib', []}
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
    assert expand('Enumera') == {:yes, 'ble', []}
  end

  test "Elixir completion with self" do
    assert expand('Enumerable') == {:yes, '.', []}
  end

  test "Elixir completion on modules from load path" do
    assert expand('Str') == {:yes, [], ['Stream', 'String', 'StringIO']}
    assert expand('Ma') == {:yes, '', ['Macro', 'Map', 'MapSet', 'MatchError']}
    assert expand('Dic') == {:yes, 't', []}
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
    assert expand('Elixir.Acce') == {:yes, 'ss', []}
  end

  test "Elixir submodule completion" do
    assert expand('String.Cha') == {:yes, 'rs', []}
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

  @tag previous_line: "mod = String"
  test "function completion using a variable bound to a module" do
    assert expand('mod.print') == {:yes, 'able?', []}
  end

  @tag previous_line: "map = %{foo: 1, bar_1: 23, bar_2: 14}"
  test "map atom key completion is supported" do
    assert expand('map.f') == {:yes, 'oo', []}
    assert expand('map.b') == {:yes, 'ar_', []}
    assert expand('map.bar_') == {:yes, '', ['bar_1', 'bar_2']}
    assert expand('map.c') == {:no, '', []}
    assert expand('map.') == {:yes, '', ['bar_1', 'bar_2', 'foo']}
    assert expand('map.foo') == {:no, '', []}
  end

  @tag previous_line: "map = %{nested: %{deeply: %{foo: 1, bar_1: 23, bar_2: 14, mod: String, num: 1}}}"
  test "nested map atom key completion is supported" do
    assert expand('map.nested.deeply.f') == {:yes, 'oo', []}
    assert expand('map.nested.deeply.b') == {:yes, 'ar_', []}
    assert expand('map.nested.deeply.bar_') == {:yes, '', ['bar_1', 'bar_2']}
    assert expand('map.nested.deeply.') == {:yes, '', ['bar_1', 'bar_2', 'foo', 'mod', 'num']}
    assert expand('map.nested.deeply.mod.print') == {:yes, 'able?', []}

    assert expand('map.nested') == {:yes, '.', []}
    assert expand('map.nested.deeply') == {:yes, '.', []}
    assert expand('map.nested.deeply.foo') == {:no, '', []}

    assert expand('map.nested.deeply.c') == {:no, '', []}
    assert expand('map.a.b.c.f') == {:no, '', []}
  end

  @tag previous_line: ~s(map = %{"foo" => 1})
  test "map string key completion is not supported" do
    assert expand('map.f') == {:no, '', []}
  end

  @tag previous_line: "num = 5; map = %{nested: %{num: 23}}"
  test "autocompletion off a bound variable only works for modules and maps" do
    assert expand('num.print') == {:no, '', []}
    assert expand('map.nested.num.f') == {:no, '', []}
    assert expand('map.nested.num.key.f') == {:no, '', []}
  end

  @tag previous_line: "map = %{nested: %{deeply: %{num: 23}}}"
  test "autocompletion using access syntax does is not supported" do
    assert expand('map[:nested][:deeply].n') == {:no, '', []}
    assert expand('map[:nested].deeply.n') == {:no, '', []}
    assert expand('map.nested.[:deeply].n') == {:no, '', []}
  end

  @tag previous_line: "num = 5"
  test "autocompletion off of unbound variables is not supported" do
    assert expand('other_var.f') == {:no, '', []}
    assert expand('a.b.c.d') == {:no, '', []}
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
    assert expand('put_') == {:yes, '', ['put_elem/3', 'put_in/3', 'put_in/2']}
  end

  @tag previous_line: "numeral = 3; number = 3; nothing = nil"
  test "variable name completion" do
    assert expand('numb') == {:yes, 'er', []}
    assert expand('num') == {:yes, '', ['number', 'numeral']}
    assert expand('no') == {:yes, '', ['nothing', 'node/0', 'node/1', 'not/1']}
  end

  @tag previous_line: "import Enum; import Supervisor, only: [count_children: 1]; import Protocol"
  test "completion of manually imported functions and macros" do
    assert expand('take') == {:yes, '', ['take/2', 'take_every/2', 'take_random/2', 'take_while/2']}
    assert expand('count') == {:yes, '', ['count_children/1', 'count/1', 'count/2']}
    assert expand('der') == {:yes, 'ive', []}
  end

  defmacro define_var do
    quote do: var!(my_var_1, Elixir) = 1
  end

  @tag previous_line: "require #{__MODULE__}; #{__MODULE__}.define_var(); my_var_2 = 2"
  test "ignores quoted variables when performing variable completion" do
    assert expand('my_var') == {:yes, '_2', []}
  end

  test "kernel special form completion" do
    assert expand('unquote_spl') == {:yes, 'icing', []}
  end

  test "completion inside expression" do
    assert expand('1 En') == {:yes, 'um', []}
    assert expand('Test(En') == {:yes, 'um', []}
    assert expand('Test :zl') == {:yes, 'ib', []}
    assert expand('[:zl') == {:yes, 'ib', []}
    assert expand('{:zl') == {:yes, 'ib', []}
  end

  test "ampersand completion" do
    assert expand('&Enu') == {:yes, 'm', []}
    assert expand('&Enum.a') == {:yes, [], ['all?/1', 'all?/2', 'any?/1', 'any?/2', 'at/2', 'at/3']}
    assert expand('f = &Enum.a') == {:yes, [], ['all?/1', 'all?/2', 'any?/1', 'any?/2', 'at/2', 'at/3']}
  end

  defmodule SublevelTest.LevelA.LevelB do
  end

  test "Elixir completion sublevel" do
    assert expand('IEx.AutocompleteTest.SublevelTest.') == {:yes, 'LevelA', []}
  end

  @tag previous_line: "alias List, as: MyList"
  test "complete aliases of Elixir modules" do
    assert expand('MyL') == {:yes, 'ist', []}
    assert expand('MyList') == {:yes, '.', []}
    assert expand('MyList.to_integer') == {:yes, [], ['to_integer/1', 'to_integer/2']}
  end

  @tag previous_line: "alias :lists, as: EList"
  test "complete aliases of Erlang modules" do
    assert expand('EL') == {:yes, 'ist', []}
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

  defmodule MyStruct do
    defstruct [:my_val]
  end

  test "completion for structs" do
    assert expand('%IEx.AutocompleteTest.MyStr') == {:yes, 'uct', []}
  end
end
