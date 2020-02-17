Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ImportTest do
  use ExUnit.Case, async: true

  # This should not warn due to the empty only
  import URI, only: []

  defmodule ImportAvailable do
    defmacro flatten do
      [flatten: 1]
    end
  end

  test "multi-call" do
    assert [List, String] = import(Elixir.{List, unquote(:String)})
    assert keymember?([a: 1], :a, 0)
    assert valid?("ø")
  end

  test "blank multi-call" do
    assert [] = import(List.{})
    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  test "multi-call with options" do
    assert [List] = import(Elixir.{List}, only: [])
    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  test "import all" do
    assert :lists = import(:lists)
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import except none" do
    import :lists, except: []
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import except one" do
    import :lists, except: [duplicate: 2]
    assert flatten([1, [2], 3]) == [1, 2, 3]
    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  test "import only via macro" do
    require ImportAvailable
    import :lists, only: ImportAvailable.flatten()
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  defmacrop dynamic_opts do
    [only: [flatten: 1]]
  end

  test "import with options via macro" do
    import :lists, dynamic_opts()
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import with double except" do
    import :lists, except: [duplicate: 2]
    import :lists, except: [each: 2]
    assert append([1], [2, 3]) == [1, 2, 3]
    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  test "import except none respects previous import with except" do
    import :lists, except: [duplicate: 2]
    import :lists, except: []
    assert append([1], [2, 3]) == [1, 2, 3]
    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  test "import except none respects previous import with only" do
    import :lists, only: [append: 2]
    import :lists, except: []
    assert append([1], [2, 3]) == [1, 2, 3]
    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  defmodule Underscored do
    def hello(x), do: x
    def __underscore__(x), do: x
  end

  defmodule ExplicitUnderscored do
    def __underscore__(x), do: x * 2
  end

  test "import only with underscore" do
    import Underscored, only: [__underscore__: 1]
    assert __underscore__(3) == 3
  end

  test "import non-underscored" do
    import ExplicitUnderscored, only: [__underscore__: 1]
    import Underscored
    assert hello(2) == 2
    assert __underscore__(3) == 6
  end

  defmodule MessedBitwise do
    defmacro bnot(x), do: x
    defmacro bor(x, _), do: x
  end

  import Bitwise, only: :functions

  test "conflicting imports with only and except" do
    import Bitwise, only: :functions, except: [bnot: 1]
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

  defmodule Sigils do
    # when imported it should cause conflict
    def is_integer(_), do: false

    def sigil_X(_, _), do: :x

    defmacro sigil_Y(_, _), do: :y

    def sigil__(_, _), do: :not_a_sigil
  end

  test "import only sigils" do
    import Sigils, only: :sigils
    assert is_integer(42)
    assert ~X"" == :x
    assert ~Y"" == :y
    assert __ENV__.functions[Sigils] == [sigil_X: 2]
    assert __ENV__.macros[Sigils] == [sigil_Y: 2]
  end

  test "import many" do
    [import(List), import(String)]
    assert capitalize("foo") == "Foo"
    assert flatten([1, [2], 3]) == [1, 2, 3]
  end

  test "import only removes the non-import part" do
    import List
    import List, only: :macros
    # Buggy local duplicate is used because we asked only for macros
    assert duplicate([1], 2) == [1]
  end

  test "import lexical on if" do
    if false do
      import List
      flatten([1, [2], 3])
      flunk()
    else
      # Buggy local duplicate is untouched
      assert duplicate([1], 2) == [1]
    end
  end

  test "import lexical on case" do
    case true do
      false ->
        import List
        flatten([1, [2], 3])
        flunk()

      true ->
        # Buggy local duplicate is untouched
        assert duplicate([1], 2) == [1]
    end
  end

  test "import lexical on try" do
    try do
      import List
      flatten([1, [2], 3])
      flunk()
    catch
      _, _ ->
        # Buggy local duplicate is untouched
        assert duplicate([1], 2) == [1]
    end

    # Buggy local duplicate is untouched
    assert duplicate([1], 2) == [1]
  end

  defp duplicate(list, _), do: list
end
