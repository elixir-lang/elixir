Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.AliasTest.Nested do
  def value, do: 1
end

defmodule Kernel.AliasTest do
  alias Kernel.AliasTest.Nested, as: Nested

  use ExUnit.Case, async: true

  test :alias_erlang do
    alias :lists, as: MyList
    assert MyList.flatten([1,[2],3]) == [1,2,3]
    assert Elixir.MyList.Bar == :"Elixir-MyList-Bar"
    assert MyList.Bar == :"Elixir-lists-Bar"
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

defmodule Kernel.FullAliasNestingGenerator do
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
end

defmodule Kernel.FullAliasNestingTest do
  use ExUnit.Case, async: true

  require Kernel.FullAliasNestingGenerator
  Kernel.FullAliasNestingGenerator.create

  test :aliases_nesting do
    assert Parent.a == :a
    assert Parent.Child.b == :a
  end
end
