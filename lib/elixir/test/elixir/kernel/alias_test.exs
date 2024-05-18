Code.require_file("../test_helper.exs", __DIR__)

alias Kernel.AliasTest.Nested, as: Nested

defmodule Nested do
  def value, do: 1
end

defmodule Kernel.AliasTest do
  use ExUnit.Case, async: true

  test "alias Erlang" do
    alias :lists, as: MyList
    assert MyList.flatten([1, [2], 3]) == [1, 2, 3]
    assert Elixir.MyList.Bar == :"Elixir.MyList.Bar"
    assert MyList.Bar == :"Elixir.lists.Bar"
  end

  test "double alias" do
    alias Kernel.AliasTest.Nested, as: Nested2
    assert Nested.value() == 1
    assert Nested2.value() == 1
  end

  test "overwritten alias" do
    assert alias(List, as: Nested) == List
    assert Nested.flatten([[13]]) == [13]
  end

  test "non-recursive alias" do
    alias Billing, as: BillingLib
    alias MyApp.Billing
    assert BillingLib == :"Elixir.Billing"
    assert Billing == :"Elixir.MyApp.Billing"
  end

  test "lexical" do
    if true_fun() do
      alias OMG, as: List, warn: false
    else
      alias ABC, as: List, warn: false
    end

    assert List.flatten([1, [2], 3]) == [1, 2, 3]
  end

  defp true_fun(), do: true

  defmodule Elixir do
    def sample, do: 1
  end

  test "nested Elixir alias" do
    assert Kernel.AliasTest.Elixir.sample() == 1
  end

  test "multi-call" do
    result = alias unquote(Inspect).{Opts, Algebra}
    assert result == [Inspect.Opts, Inspect.Algebra]
    assert %Opts{} == %Inspect.Opts{}
    assert Algebra.empty() == :doc_nil
  end

  test "alias removal" do
    alias __MODULE__.Foo
    assert Foo == __MODULE__.Foo
    alias Elixir.Foo
    assert Foo == Elixir.Foo
    alias Elixir.Bar
  end
end

defmodule Kernel.AliasNestingGenerator do
  defmacro create do
    quote do
      defmodule Parent do
        def a, do: :a
      end

      defmodule Parent.Child do
        def b, do: Parent.a()
      end
    end
  end
end

defmodule Kernel.AliasNestingTest do
  use ExUnit.Case, async: true

  require Kernel.AliasNestingGenerator
  Kernel.AliasNestingGenerator.create()

  test "aliases nesting" do
    assert Parent.a() == :a
    assert Parent.Child.b() == :a
  end

  defmodule Nested do
    def value, do: 2
  end

  test "aliases nesting with previous alias" do
    assert Nested.value() == 2
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
      defmodule First do
        defstruct foo: :bar
      end

      defmodule Second do
        defstruct baz: %First{}
      end
    end
  end
end

defmodule Macro.AliasTest.Aliaser do
  defmacro __using__(_options) do
    quote do
      alias Some.First
    end
  end
end

defmodule Macro.AliasTest.User do
  use ExUnit.Case, async: true

  use Macro.AliasTest.Definer
  use Macro.AliasTest.Aliaser

  test "has a struct defined from after compile" do
    assert is_map(struct(Macro.AliasTest.User.First, []))
    assert is_map(struct(Macro.AliasTest.User.Second, []).baz)
  end
end

defmodule Kernel.AliasNestingEnvTest do
  use ExUnit.Case, async: true

  alias Another.AliasEnv, warn: false

  def aliases_before, do: __ENV__.aliases

  defmodule Elixir.AliasEnv do
    def aliases_nested, do: __ENV__.aliases
  end

  def aliases_after, do: __ENV__.aliases

  test "keeps env after overriding nested Elixir module of the same name" do
    assert aliases_before() == [
             {Elixir.Nested, Kernel.AliasTest.Nested},
             {Elixir.AliasEnv, Another.AliasEnv}
           ]

    assert Elixir.AliasEnv.aliases_nested() == [
             {Elixir.Nested, Kernel.AliasTest.Nested},
             {Elixir.AliasEnv, Another.AliasEnv}
           ]

    assert aliases_after() == [
             {Elixir.Nested, Kernel.AliasTest.Nested},
             {Elixir.AliasEnv, Another.AliasEnv}
           ]
  end
end
