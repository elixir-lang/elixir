Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ImportTest do
  use ExUnit.Case, async: true

  defmodule ImportAvailable do
    defmacro flatten do
      [flatten: 1]
    end
  end


  test "import all" do
    import :lists
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import except none" do
    import :lists, except: []
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import except one" do
    import :lists, except: [each: 2]
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import only via macro" do
    require ImportAvailable
    import :lists, only: ImportAvailable.flatten
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  defmacrop dynamic_opts do
    [only: [flatten: 1]]
  end

  test "import with options via macro" do
    import :lists, dynamic_opts
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import with double except" do
    import :lists, except: [duplicate: 2]
    import :lists, except: [each: 2]
    assert append([1], [2, 3]) == [1, 2, 3]
    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  defmodule Underscored do
    def hello(x),          do: x
    def __underscore__(x), do: x
  end

  defmodule ExplicitUnderscored do
    def __underscore__(x), do: x * 2
  end

  test "import only with underscore" do
    import Underscored, only: [__underscore__: 1]
    assert __underscore__(3) == 3
  end

  test "import non underscored" do
    import ExplicitUnderscored, only: [__underscore__: 1]
    import Underscored
    assert hello(2) == 2
    assert __underscore__(3) == 6
  end

  defmodule MessedBitwise do
    defmacro bnot(x),   do: x
    defmacro bor(x, _), do: x
  end

  import Bitwise, only: :macros

  test "conflicing imports with only and except" do
    import Bitwise, only: :macros, except: [bnot: 1]
    import MessedBitwise, only: [bnot: 1]
    assert bnot(0) == 0
    assert bor(0, 1) == 1
  end

  # This test is asserting that the imports in the
  # test above do not affect this test.
  test "imports from other functions do not leak" do
    assert band(1, 1) == 1
    assert bor(0, 1) == 1
    assert bnot(0) == -1
  end

  test "import ambiguous" do
    # Simply make sure that we can indeed import functions with
    # the same name and arity from different modules without the
    # import itself causing any errors.
    import List
    import String
  end

  test "import many" do
    [import(List), import(String)]
    assert capitalize("foo")  == "Foo"
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import lexical on if" do
    if false do
      import :lists
      flatten([1, [2], 3])
      flunk
    else
      # Buggy local duplicate is untouched
      assert duplicate([1], 2) == [1]
    end
  end

  test "import lexical on case" do
    case true do
      false ->
        import :lists
        flatten([1, [2], 3])
        flunk
      true ->
        # Buggy local duplicate is untouched
        assert duplicate([1], 2) == [1]
    end
  end

  test "import lexical on try" do
    try do
      import :lists
      flatten([1, [2], 3])
    catch
      _ -> flatten([:a, [:b], :c])
    end

    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  defp duplicate(list, _), do: list
end
