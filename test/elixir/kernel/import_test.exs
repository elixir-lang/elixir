Code.require_file "../../test_helper", __FILE__

defmodule Kernel::ImportOnlyTest do
  use ExUnit::Case

  import Erlang.lists, only: [flatten: 1]

  def test_import_erlang do
    [1,2,3] = flatten [1,[2],3]
  end
end

defmodule Kernel::ImportAllTest do
  use ExUnit::Case

  import Erlang.lists

  def test_import_erlang do
    [1,2,3] = flatten [1,[2],3]
  end
end

defmodule Kernel::ImportExceptTest do
  use ExUnit::Case

  import Erlang.lists, except: [each: 1]

  def test_import_erlang do
    [1,2,3] = flatten [1,[2],3]
  end
end

defmodule Kernel::MessedBitwise do
  defmacro bnot(x),   do: x
  defmacro bor(x, _), do: x
end

defmodule Kernel::ImportMacrosTest do
  use ExUnit::Case

  import :macros, Bitwise

  def test_import_true do
    1  = band(1, 1)
    1  = bor(0, 1)
    -1 = bnot(0)
  end

  def test_function_import_with_only do
    import :macros, Bitwise, except: [bnot: 1]
    import :macros, Kernel::MessedBitwise, only: [bnot: 1]
    0 = bnot(0)
    1 = bor(0, 1)
  end

  # This test is asserting that the requires done
  # inside the function do not affect outer ones.
  def test_import_true_not_affected do
    1  = band(1, 1)
    1  = bor(0, 1)
    -1 = bnot(0)
  end
end