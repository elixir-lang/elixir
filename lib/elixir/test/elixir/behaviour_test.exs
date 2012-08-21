Code.require_file "../test_helper.exs", __FILE__

defmodule BehaviourTest do
  use ExUnit.Case, async: true

  defmodule Sample do
    use Behaviour

    @doc "Doc"
    defcallback foo(foo, bar // [], bat // [])
  end

  test :docs_are_defined do
    assert [_, _] = Sample.__info__(:docs)
  end

  test :callbacks do
    assert [{:foo,3},{:foo,2},{:foo,1}] = Sample.behaviour_info(:callbacks)
  end
end

