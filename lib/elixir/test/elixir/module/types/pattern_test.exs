Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.PatternTest do
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr

  describe "variables" do
    test "captures variables from simple assignment in head" do
      assert typecheck!([x = :foo], x) == dynamic(atom([:foo]))
      assert typecheck!([:foo = x], x) == dynamic(atom([:foo]))
    end

    test "captures variables from simple assignment in =" do
      assert typecheck!(
               (
                 x = :foo
                 x
               )
             ) == dynamic(atom([:foo]))
    end
  end

  describe "maps" do
    test "matching struct name" do
      assert typecheck!([%x{}], x) == dynamic(atom())
    end

    test "matching map" do
      assert typecheck!([x = %{}], x.foo.bar) == dynamic()
    end
  end

  describe "binaries" do
    test "ok" do
      assert typecheck!([<<x>>], x) == dynamic(integer())
      assert typecheck!([<<x::float>>], x) == dynamic(float())
      assert typecheck!([<<x::binary>>], x) == dynamic(binary())
      assert typecheck!([<<x::utf8>>], x) == dynamic(integer())
    end

    test "error" do
      assert typeerror!([<<x::binary-size(2), x::float>>], x) == ~l"""
             incompatible types assigned to "x":

                 binary() !~ float()

             where "x" was given the types:

                 # type: binary()
                 # from: types_test.ex:LINE
                 <<x::binary-size(2), ...>>

                 # type: float()
                 # from: types_test.ex:LINE
                 <<..., x::float>>

             typing violation found at:\
             """

      assert typeerror!([<<x::float, x>>], x) == ~l"""
             incompatible types assigned to "x":

                 float() !~ integer()

             where "x" was given the types:

                 # type: float()
                 # from: types_test.ex:LINE
                 <<x::float, ...>>

                 # type: integer()
                 # from: types_test.ex:LINE
                 <<..., x>>

             #{hints(:inferred_bitstring_spec)}

             typing violation found at:\
             """
    end
  end
end
