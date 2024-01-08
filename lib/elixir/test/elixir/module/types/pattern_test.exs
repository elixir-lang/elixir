Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.PatternTest do
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr

  describe "binaries" do
    test "ok" do
      assert typecheck!([<<x>>], x) == integer()
      assert typecheck!([<<x::float>>], x) == float()
      assert typecheck!([<<x::binary>>], x) == binary()
      assert typecheck!([<<x::utf8>>], x) == integer()
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
