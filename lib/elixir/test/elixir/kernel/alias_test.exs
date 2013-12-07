Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.AliasTest.Nested do
  def value, do: 1
end

defmodule Kernel.AliasTest do
  alias Kernel.AliasTest.Nested, as: Nested

  use ExUnit.Case, async: true

  test :alias_erlang do
    alias :lists, as: MyList
    assert MyList.flatten([1, [2], 3]) == [1, 2, 3]
    assert Elixir.MyList.Bar == :"Elixir.MyList.Bar"
    assert MyList.Bar == :"Elixir.lists.Bar"
  end

  test :double_alias do
    alias Kernel.AliasTest.Nested, as: Nested2
    assert Nested.value  == 1
    assert Nested2.value == 1
  end

  test :overwriten_alias do
    alias List, as: Nested
    assert Nested.flatten([[13]]) == [13]
  end

  test :lexical do
    if true do
      alias OMG, as: List, warn: false
    else
      alias ABC, as: List, warn: false
    end

    assert List.flatten([1, [2], 3]) == [1, 2, 3]
  end

  defmodule Elixir do
    def sample, do: 1
  end

  test :nested_elixir_alias do
    assert Kernel.AliasTest.Elixir.sample == 1
  end
end

defmodule Kernel.AliasNestingGenerator do
  defmacro create do
    quote do
      defmodule Parent do
        def a, do: :a
      end

      defmodule Parent.Child do
        def b, do: Parent.a
      end
    end
  end

  defmacro record do
    quote do
      defexception Parent, message: nil

      defmodule Parent.Child do
        def b, do: Parent.new(message: "ok")
      end
    end
  end
end

defmodule Kernel.AliasNestingTest do
  use ExUnit.Case, async: true

  require Kernel.AliasNestingGenerator
  Kernel.AliasNestingGenerator.create

  test :aliases_nesting do
    assert Parent.a == :a
    assert Parent.Child.b == :a
  end
end

defmodule Kernel.AliasMacroNestingTest do
  use ExUnit.Case, async: true

  require Kernel.AliasNestingGenerator
  Kernel.AliasNestingGenerator.record

  test :aliases_nesting do
    assert is_record(Parent.new, Parent)
    assert Parent.Child.b.message == "ok"
  end
end

# Test case extracted from using records with aliases
# and @before_compile. We are basically testing that
# macro aliases are not leaking from the macro.

defmodule Macro.AliasTest.Definer do
  defmacro __using__(_options) do
    quote do
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      defrecord Record, [foo: :bar]
      defrecord Another, baz: Record.new
    end
  end
end

defmodule Macro.AliasTest.Aliaser do
  defmacro __using__(_options) do
    quote do
      alias Some.Record
    end
  end
end

defmodule Macro.AliasTest.User do
  use ExUnit.Case, async: true

  use Macro.AliasTest.Definer
  use Macro.AliasTest.Aliaser

  test "has a record defined from after compile" do
    assert is_tuple Macro.AliasTest.User.Record.new
  end
end
