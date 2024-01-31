Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.AutocompleteTest do
  use ExUnit.Case, async: true

  setup do
    evaluator = IEx.Server.start_evaluator(1, [])
    Process.put(:evaluator, evaluator)
    :ok
  end

  defp eval(line) do
    ExUnit.CaptureIO.capture_io(fn ->
      evaluator = Process.get(:evaluator)
      Process.group_leader(evaluator, Process.group_leader())
      send(evaluator, {:eval, self(), line <> "\n", 1, ""})
      assert_receive {:evaled, _, _, _}
    end)
  end

  defp expand(expr) do
    IEx.Autocomplete.expand(Enum.reverse(expr), self())
  end

  test "Erlang module completion" do
    assert expand(~c":zl") == {:yes, ~c"ib", []}
  end

  test "Erlang module no completion" do
    assert expand(~c":unknown") == {:no, ~c"", []}
  end

  test "Erlang module multiple values completion" do
    {:yes, ~c"", list} = expand(~c":logger")
    assert ~c"logger" in list
    assert ~c"logger_proxy" in list
  end

  test "Erlang root completion" do
    {:yes, ~c"", list} = expand(~c":")
    assert is_list(list)
    assert ~c"lists" in list
    assert ~c"Elixir.List" not in list
  end

  test "Elixir proxy" do
    {:yes, ~c"", list} = expand(~c"E")
    assert ~c"Elixir" in list
  end

  test "Elixir completion" do
    assert expand(~c"En") == {:yes, ~c"um", []}
    assert expand(~c"Enumera") == {:yes, ~c"ble", []}
  end

  test "Elixir type completion" do
    assert expand(~c"t :gen_ser") == {:yes, ~c"ver", []}
    assert expand(~c"t String") == {:yes, ~c"", [~c"String", ~c"StringIO"]}

    assert expand(~c"t String.") ==
             {:yes, ~c"", [~c"codepoint/0", ~c"grapheme/0", ~c"pattern/0", ~c"t/0"]}

    assert expand(~c"t String.grap") == {:yes, ~c"heme", []}
    assert expand(~c"t  String.grap") == {:yes, ~c"heme", []}
    assert {:yes, ~c"", [~c"date_time/0" | _]} = expand(~c"t :file.")
    assert expand(~c"t :file.n") == {:yes, ~c"ame", []}
  end

  test "Elixir callback completion" do
    assert expand(~c"b :strin") == {:yes, ~c"g", []}
    assert expand(~c"b String") == {:yes, ~c"", [~c"String", ~c"StringIO"]}
    assert expand(~c"b String.") == {:no, ~c"", []}
    assert expand(~c"b Access.") == {:yes, ~c"", [~c"fetch/2", ~c"get_and_update/3", ~c"pop/2"]}
    assert expand(~c"b GenServer.term") == {:yes, ~c"inate", []}
    assert expand(~c"b   GenServer.term") == {:yes, ~c"inate", []}
    assert expand(~c"b :gen_server.handle_in") == {:yes, ~c"fo", []}
  end

  test "Elixir helper completion with parentheses" do
    assert expand(~c"t(:gen_ser") == {:yes, ~c"ver", []}
    assert expand(~c"t(String") == {:yes, ~c"", [~c"String", ~c"StringIO"]}

    assert expand(~c"t(String.") ==
             {:yes, ~c"", [~c"codepoint/0", ~c"grapheme/0", ~c"pattern/0", ~c"t/0"]}

    assert expand(~c"t(String.grap") == {:yes, ~c"heme", []}
  end

  test "Elixir completion with self" do
    assert expand(~c"Enumerable") == {:yes, ~c".", []}
  end

  test "Elixir completion on modules from load path" do
    assert expand(~c"Str") == {:yes, [], [~c"Stream", ~c"String", ~c"StringIO"]}
    assert expand(~c"Ma") == {:yes, ~c"", [~c"Macro", ~c"Map", ~c"MapSet", ~c"MatchError"]}
    assert expand(~c"Dic") == {:yes, ~c"t", []}
    assert expand(~c"Ex") == {:yes, [], [~c"ExUnit", ~c"Exception"]}
  end

  test "Elixir no completion for underscored functions with no doc" do
    {:module, _, bytecode, _} =
      defmodule Elixir.Sample do
        def __foo__(), do: 0
        @doc "Bar doc"
        def __bar__(), do: 1
      end

    File.write!("Elixir.Sample.beam", bytecode)
    assert {:docs_v1, _, _, _, _, _, _} = Code.fetch_docs(Sample)
    assert expand(~c"Sample._") == {:yes, ~c"_bar__", []}
  after
    File.rm("Elixir.Sample.beam")
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "Elixir no completion for default argument functions with doc set to false" do
    {:yes, ~c"", available} = expand(~c"String.")
    refute Enum.member?(available, ~c"rjust/2")
    assert Enum.member?(available, ~c"replace/3")

    assert expand(~c"String.r") == {:yes, ~c"e", []}

    {:module, _, bytecode, _} =
      defmodule Elixir.DefaultArgumentFunctions do
        def foo(a \\ :a, b, c \\ :c), do: {a, b, c}

        def _do_fizz(a \\ :a, b, c \\ :c), do: {a, b, c}

        @doc false
        def __fizz__(a \\ :a, b, c \\ :c), do: {a, b, c}

        @doc "bar/0 doc"
        def bar(), do: :bar
        @doc false
        def bar(a \\ :a, b, c \\ :c, d \\ :d), do: {a, b, c, d}
        @doc false
        def bar(a, b, c, d, e), do: {a, b, c, d, e}

        @doc false
        def baz(a \\ :a), do: {a}

        @doc "biz/3 doc"
        def biz(a, b, c \\ :c), do: {a, b, c}
      end

    File.write!("Elixir.DefaultArgumentFunctions.beam", bytecode)
    assert {:docs_v1, _, _, _, _, _, _} = Code.fetch_docs(DefaultArgumentFunctions)

    functions_list = [~c"bar/0", ~c"biz/2", ~c"biz/3", ~c"foo/1", ~c"foo/2", ~c"foo/3"]
    assert expand(~c"DefaultArgumentFunctions.") == {:yes, ~c"", functions_list}

    assert expand(~c"DefaultArgumentFunctions.bi") == {:yes, ~c"z", []}

    assert expand(~c"DefaultArgumentFunctions.foo") ==
             {:yes, ~c"", [~c"foo/1", ~c"foo/2", ~c"foo/3"]}
  after
    File.rm("Elixir.DefaultArgumentFunctions.beam")
    :code.purge(DefaultArgumentFunctions)
    :code.delete(DefaultArgumentFunctions)
  end

  test "Elixir no completion" do
    assert expand(~c".") == {:no, ~c"", []}
    assert expand(~c"Xyz") == {:no, ~c"", []}
    assert expand(~c"x.Foo") == {:no, ~c"", []}
    assert expand(~c"x.Foo.get_by") == {:no, ~c"", []}
    assert expand(~c"@foo.bar") == {:no, ~c"", []}
  end

  test "Elixir root submodule completion" do
    assert expand(~c"Elixir.Acce") == {:yes, ~c"ss", []}
  end

  test "Elixir submodule completion" do
    assert expand(~c"String.Cha") == {:yes, ~c"rs", []}
  end

  test "Elixir submodule no completion" do
    assert expand(~c"IEx.Xyz") == {:no, ~c"", []}
  end

  test "function completion" do
    assert expand(~c"System.ve") == {:yes, ~c"rsion", []}
    assert expand(~c":ets.fun2") == {:yes, ~c"ms", []}
  end

  test "function completion with arity" do
    assert expand(~c"String.printable?") == {:yes, ~c"", [~c"printable?/1", ~c"printable?/2"]}
    assert expand(~c"String.printable?/") == {:yes, ~c"", [~c"printable?/1", ~c"printable?/2"]}

    assert expand(~c"Enum.count") ==
             {:yes, ~c"", [~c"count/1", ~c"count/2", ~c"count_until/2", ~c"count_until/3"]}

    assert expand(~c"Enum.count/") == {:yes, ~c"", [~c"count/1", ~c"count/2"]}
  end

  test "operator completion" do
    assert expand(~c"+") == {:yes, ~c"", [~c"+/1", ~c"+/2", ~c"++/2"]}
    assert expand(~c"+/") == {:yes, ~c"", [~c"+/1", ~c"+/2"]}
    assert expand(~c"++/") == {:yes, ~c"", [~c"++/2"]}
  end

  test "sigil completion" do
    {:yes, ~c"", sigils} = expand(~c"~")
    assert ~c"~C (sigil_C)" in sigils
    {:yes, ~c"", sigils} = expand(~c"~r")
    assert ~c"\"" in sigils
    assert ~c"(" in sigils
  end

  test "function completion using a variable bound to a module" do
    eval("mod = String")
    assert expand(~c"mod.print") == {:yes, ~c"able?", []}
  end

  test "map atom key completion is supported" do
    eval("map = %{foo: 1, bar_1: 23, bar_2: 14}")
    assert expand(~c"map.f") == {:yes, ~c"oo", []}
    assert expand(~c"map.b") == {:yes, ~c"ar_", []}
    assert expand(~c"map.bar_") == {:yes, ~c"", [~c"bar_1", ~c"bar_2"]}
    assert expand(~c"map.c") == {:no, ~c"", []}
    assert expand(~c"map.") == {:yes, ~c"", [~c"bar_1", ~c"bar_2", ~c"foo"]}
    assert expand(~c"map.foo") == {:no, ~c"", []}
  end

  test "nested map atom key completion is supported" do
    eval("map = %{nested: %{deeply: %{foo: 1, bar_1: 23, bar_2: 14, mod: String, num: 1}}}")
    assert expand(~c"map.nested.deeply.f") == {:yes, ~c"oo", []}
    assert expand(~c"map.nested.deeply.b") == {:yes, ~c"ar_", []}
    assert expand(~c"map.nested.deeply.bar_") == {:yes, ~c"", [~c"bar_1", ~c"bar_2"]}

    assert expand(~c"map.nested.deeply.") ==
             {:yes, ~c"", [~c"bar_1", ~c"bar_2", ~c"foo", ~c"mod", ~c"num"]}

    assert expand(~c"map.nested.deeply.mod.print") == {:yes, ~c"able?", []}

    assert expand(~c"map.nested") == {:yes, ~c".", []}
    assert expand(~c"map.nested.deeply") == {:yes, ~c".", []}
    assert expand(~c"map.nested.deeply.foo") == {:no, ~c"", []}

    assert expand(~c"map.nested.deeply.c") == {:no, ~c"", []}
    assert expand(~c"map.a.b.c.f") == {:no, ~c"", []}
  end

  test "map string key completion is not supported" do
    eval(~S(map = %{"foo" => 1}))
    assert expand(~c"map.f") == {:no, ~c"", []}
  end

  test "bound variables for modules and maps" do
    eval("num = 5; map = %{nested: %{num: 23}}")
    assert expand(~c"num.print") == {:no, ~c"", []}
    assert expand(~c"map.nested.num.f") == {:no, ~c"", []}
    assert expand(~c"map.nested.num.key.f") == {:no, ~c"", []}
  end

  test "access syntax is not supported" do
    eval("map = %{nested: %{deeply: %{num: 23}}}")
    assert expand(~c"map[:nested][:deeply].n") == {:no, ~c"", []}
    assert expand(~c"map[:nested].deeply.n") == {:no, ~c"", []}
    assert expand(~c"map.nested.[:deeply].n") == {:no, ~c"", []}
  end

  test "unbound variables is not supported" do
    eval("num = 5")
    assert expand(~c"other_var.f") == {:no, ~c"", []}
    assert expand(~c"a.b.c.d") == {:no, ~c"", []}
  end

  test "macro completion" do
    {:yes, ~c"", list} = expand(~c"Kernel.is_")
    assert is_list(list)
  end

  test "imports completion" do
    {:yes, ~c"", list} = expand(~c"")
    assert is_list(list)
    assert ~c"h/1" in list
    assert ~c"unquote/1" in list
    assert ~c"pwd/0" in list
  end

  test "kernel import completion" do
    assert expand(~c"defstru") == {:yes, ~c"ct", []}
    assert expand(~c"put_") == {:yes, ~c"", [~c"put_elem/3", ~c"put_in/2", ~c"put_in/3"]}
  end

  test "variable name completion" do
    eval("numeral = 3; number = 3; nothing = nil")
    assert expand(~c"numb") == {:yes, ~c"er", []}
    assert expand(~c"num") == {:yes, ~c"", [~c"number", ~c"numeral"]}
    assert expand(~c"no") == {:yes, ~c"", [~c"nothing", ~c"node/0", ~c"node/1", ~c"not/1"]}
  end

  test "completion of manually imported functions and macros" do
    eval("import Enum; import Supervisor, only: [count_children: 1]; import Protocol")

    assert expand(~c"der") == {:yes, ~c"ive", []}

    assert expand(~c"take") ==
             {:yes, ~c"", [~c"take/2", ~c"take_every/2", ~c"take_random/2", ~c"take_while/2"]}

    assert expand(~c"take/") == {:yes, ~c"", [~c"take/2"]}

    assert expand(~c"count") ==
             {:yes, ~c"",
              [
                ~c"count/1",
                ~c"count/2",
                ~c"count_children/1",
                ~c"count_until/2",
                ~c"count_until/3"
              ]}

    assert expand(~c"count/") == {:yes, ~c"", [~c"count/1", ~c"count/2"]}
  end

  defmacro define_var do
    quote(do: var!(my_var_1, Elixir) = 1)
  end

  test "ignores quoted variables when performing variable completion" do
    eval("require #{__MODULE__}; #{__MODULE__}.define_var(); my_var_2 = 2")
    assert expand(~c"my_var") == {:yes, ~c"_2", []}
  end

  test "kernel special form completion" do
    assert expand(~c"unquote_spl") == {:yes, ~c"icing", []}
  end

  test "completion inside expression" do
    assert expand(~c"1 En") == {:yes, ~c"um", []}
    assert expand(~c"Test(En") == {:yes, ~c"um", []}
    assert expand(~c"Test :zl") == {:yes, ~c"ib", []}
    assert expand(~c"[:zl") == {:yes, ~c"ib", []}
    assert expand(~c"{:zl") == {:yes, ~c"ib", []}
  end

  defmodule SublevelTest.LevelA.LevelB do
  end

  test "Elixir completion sublevel" do
    assert expand(~c"IEx.AutocompleteTest.SublevelTest.") == {:yes, ~c"LevelA", []}
  end

  test "complete aliases of Elixir modules" do
    eval("alias List, as: MyList")
    assert expand(~c"MyL") == {:yes, ~c"ist", []}
    assert expand(~c"MyList") == {:yes, ~c".", []}
    assert expand(~c"MyList.to_integer") == {:yes, [], [~c"to_integer/1", ~c"to_integer/2"]}
  end

  test "complete aliases of Erlang modules" do
    eval("alias :lists, as: EList")
    assert expand(~c"EL") == {:yes, ~c"ist", []}
    assert expand(~c"EList") == {:yes, ~c".", []}
    assert expand(~c"EList.map") == {:yes, [], [~c"map/2", ~c"mapfoldl/3", ~c"mapfoldr/3"]}
  end

  test "completion for functions added when compiled module is reloaded" do
    {:module, _, bytecode, _} =
      defmodule Sample do
        def foo(), do: 0
      end

    File.write!("Elixir.IEx.AutocompleteTest.Sample.beam", bytecode)
    assert {:docs_v1, _, _, _, _, _, _} = Code.fetch_docs(Sample)
    assert expand(~c"IEx.AutocompleteTest.Sample.foo") == {:yes, ~c"", [~c"foo/0"]}

    Code.compiler_options(ignore_module_conflict: true)

    defmodule Sample do
      def foo(), do: 0
      def foobar(), do: 0
    end

    assert expand(~c"IEx.AutocompleteTest.Sample.foo") == {:yes, ~c"", [~c"foo/0", ~c"foobar/0"]}
  after
    File.rm("Elixir.IEx.AutocompleteTest.Sample.beam")
    Code.compiler_options(ignore_module_conflict: false)
    :code.purge(Sample)
    :code.delete(Sample)
  end

  defmodule MyStruct do
    defstruct [:my_val]
  end

  test "completion for struct names" do
    assert {:yes, ~c"", entries} = expand(~c"%")
    assert ~c"URI" in entries
    assert ~c"IEx.History" in entries
    assert ~c"IEx.Server" in entries

    assert {:yes, ~c"", entries} = expand(~c"%IEx.")
    assert ~c"IEx.History" in entries
    assert ~c"IEx.Server" in entries

    assert expand(~c"%IEx.AutocompleteTe") == {:yes, ~c"st.MyStruct{", []}
    assert expand(~c"%IEx.AutocompleteTest.MyStr") == {:yes, ~c"uct{", []}

    eval("alias IEx.AutocompleteTest.MyStruct")
    assert expand(~c"%MyStr") == {:yes, ~c"uct{", []}
  end

  test "completion for struct keys" do
    assert {:yes, ~c"", entries} = expand(~c"%URI{")
    assert ~c"path:" in entries
    assert ~c"query:" in entries

    assert {:yes, ~c"", entries} = expand(~c"%URI{path: \"foo\",")
    assert ~c"path:" not in entries
    assert ~c"query:" in entries

    assert {:yes, ~c"ry: ", []} = expand(~c"%URI{path: \"foo\", que")
    assert {:no, [], []} = expand(~c"%URI{path: \"foo\", unkno")
    assert {:no, [], []} = expand(~c"%Unknown{path: \"foo\", unkno")
  end

  test "completion for struct keys in update syntax" do
    assert {:yes, ~c"", entries} = expand(~c"%URI{var | ")
    assert ~c"path:" in entries
    assert ~c"query:" in entries

    assert {:yes, ~c"", entries} = expand(~c"%URI{var | path: \"foo\",")
    assert ~c"path:" not in entries
    assert ~c"query:" in entries

    assert {:yes, ~c"ry: ", []} = expand(~c"%URI{var | path: \"foo\", que")
    assert {:no, [], []} = expand(~c"%URI{var | path: \"foo\", unkno")
    assert {:no, [], []} = expand(~c"%Unknown{var | path: \"foo\", unkno")
  end

  test "completion for map keys in update syntax" do
    eval("map = %{some: 1, other: :ok, another: \"qwe\"}")
    assert {:yes, ~c"", entries} = expand(~c"%{map | ")
    assert ~c"some:" in entries
    assert ~c"other:" in entries

    assert {:yes, ~c"", entries} = expand(~c"%{map | some: \"foo\",")
    assert ~c"some:" not in entries
    assert ~c"other:" in entries

    assert {:yes, ~c"er: ", []} = expand(~c"%{map | some: \"foo\", oth")
    assert {:no, [], []} = expand(~c"%{map | some: \"foo\", unkno")
    assert {:no, [], []} = expand(~c"%{unknown | some: \"foo\", unkno")
  end

  test "completion for struct var keys" do
    eval("struct = %IEx.AutocompleteTest.MyStruct{}")
    assert expand(~c"struct.my") == {:yes, ~c"_val", []}
  end

  test "completion for bitstring modifiers" do
    assert {:yes, ~c"", entries} = expand(~c"<<foo::")
    assert ~c"integer" in entries
    assert ~c"size/1" in entries

    assert {:yes, ~c"eger", []} = expand(~c"<<foo::int")

    assert {:yes, ~c"", entries} = expand(~c"<<foo::integer-")
    refute ~c"integer" in entries
    assert ~c"little" in entries
    assert ~c"size/1" in entries

    assert {:yes, ~c"", entries} = expand(~c"<<foo::integer-little-")
    refute ~c"integer" in entries
    refute ~c"little" in entries
    assert ~c"size/1" in entries
  end

  test "completion for aliases in special forms" do
    assert {:yes, ~c"", entries} = expand(~c"alias ")
    assert ~c"Atom" in entries
    refute ~c"is_atom" in entries

    assert {:yes, ~c"Range", []} = expand(~c"alias Date.")
  end

  test "ignore invalid Elixir module literals" do
    defmodule(:"Elixir.IEx.AutocompleteTest.Unicodé", do: nil)
    assert expand(~c"IEx.AutocompleteTest.Unicod") == {:no, ~c"", []}
  after
    :code.purge(:"Elixir.IEx.AutocompleteTest.Unicodé")
    :code.delete(:"Elixir.IEx.AutocompleteTest.Unicodé")
  end

  test "signature help for functions and macros" do
    assert expand(~c"String.graphemes(") == {:yes, ~c"", [~c"graphemes(string)"]}
    assert expand(~c"def ") == {:yes, ~c"", [~c"def(call, expr \\\\ nil)"]}

    eval("import Enum; import Protocol")

    assert expand(~c"reduce(") ==
             {:yes, ~c"", [~c"reduce(enumerable, fun)", ~c"reduce(enumerable, acc, fun)"]}

    assert expand(~c"take(") == {:yes, ~c"", [~c"take(enumerable, amount)"]}
    assert expand(~c"derive(") == {:yes, ~c"", [~c"derive(protocol, module, options \\\\ [])"]}

    defmodule NoDocs do
      def sample(a), do: a
    end

    assert {:yes, [], [_ | _]} = expand(~c"NoDocs.sample(")
  end

  @tag :tmp_dir
  test "path completion inside strings", %{tmp_dir: dir} do
    dir |> Path.join("single1") |> File.touch()
    dir |> Path.join("file1") |> File.touch()
    dir |> Path.join("file2") |> File.touch()
    dir |> Path.join("dir") |> File.mkdir()
    dir |> Path.join("dir/file3") |> File.touch()
    dir |> Path.join("dir/file4") |> File.touch()

    assert expand(~c"\"./") == path_autocompletion(".")
    assert expand(~c"\"/") == path_autocompletion("/")
    assert expand(~c"\"./#\{") == expand(~c"{")
    assert expand(~c"\"./#\{Str") == expand(~c"{Str")
    assert expand(~c"Path.join(\"./\", is_") == expand(~c"is_")

    assert expand(~c"\"#{dir}/") == path_autocompletion(dir)
    assert expand(~c"\"#{dir}/sin") == {:yes, ~c"gle1", []}
    assert expand(~c"\"#{dir}/single1") == {:yes, ~c"\"", []}
    assert expand(~c"\"#{dir}/fi") == {:yes, ~c"le", []}
    assert expand(~c"\"#{dir}/file") == path_autocompletion(dir, "file")
    assert expand(~c"\"#{dir}/d") == {:yes, ~c"ir/", []}
    assert expand(~c"\"#{dir}/dir") == {:yes, ~c"/", []}
    assert expand(~c"\"#{dir}/dir/") == {:yes, ~c"file", []}
    assert expand(~c"\"#{dir}/dir/file") == dir |> Path.join("dir") |> path_autocompletion("file")
  end

  defp path_autocompletion(dir, hint \\ "") do
    dir
    |> File.ls!()
    |> Stream.filter(&String.starts_with?(&1, hint))
    |> Enum.map(&String.to_charlist/1)
    |> case do
      [] -> {:no, ~c"", []}
      list -> {:yes, ~c"", list}
    end
  end
end
