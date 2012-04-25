Code.require_file "../../test_helper", __FILE__

defmodule Kernel.ImportOnlyTest do
  use ExUnit.Case

  import Erlang.lists, only: [flatten: 1]

  test :import_erlang do
    assert_equal [1,2,3], flatten [1,[2],3]
  end
end

defmodule Kernel.ImportAllTest do
  use ExUnit.Case

  import Erlang.lists

  test :import_erlang do
    assert_equal [1,2,3], flatten [1,[2],3]
  end
end

defmodule Kernel.ImportExceptTest do
  use ExUnit.Case

  import Erlang.lists, except: [each: 2]

  test :import_erlang do
    assert_equal [1,2,3], flatten [1,[2],3]
  end
end

defmodule Kernel.ImportTwiceWithExceptTest do
  use ExUnit.Case

  import Erlang.lists, except: [flatten: 1]
  import Erlang.lists, except: [each: 2]

  test :import_erlang do
    assert_equal [1,[2],3], flatten [1,[2],3]
  end

  def flatten(list), do: list
end

defmodule Kernel.MessedBitwise do
  defmacro bnot(x),   do: x
  defmacro bor(x, _), do: x
end

defmodule Kernel.ImportMacrosTest do
  use ExUnit.Case

  import :macros, Bitwise

  test :import_true do
    assert_equal 1, band(1, 1)
    assert_equal 1, bor(0, 1)
    assert_equal -1, bnot(0)
  end

  test :function_import_with_only do
    import :macros, Bitwise, except: [bnot: 1]
    import :macros, Kernel.MessedBitwise, only: [bnot: 1]
    assert_equal 0, bnot(0)
    assert_equal 1, bor(0, 1)
  end

  # This test is asserting that the requires done
  # inside the function do not affect outer ones.
  test :import_true_not_affected do
    assert_equal 1 , band(1, 1)
    assert_equal 1 , bor(0, 1)
    assert_equal -1, bnot(0)
  end
end
