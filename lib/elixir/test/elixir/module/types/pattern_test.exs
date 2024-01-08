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

                 # types_test.ex:LINE:
                 <<x::binary-size(2), ...>>
                 => binary()

                 # types_test.ex:LINE:
                 <<..., x::float>>
                 => float()
             """

      assert typeerror!([<<x::float, x>>], x) == ~l"""
             incompatible types assigned to "x":

                 float() !~ integer()

             where "x" was given the types:

                 # types_test.ex:LINE:
                 <<x::float, ...>>
                 => float()

                 # types_test.ex:LINE:
                 <<..., x>>
                 => integer()

             #{hint()} all expressions given to binaries are assumed to be of type \
             integer() unless said otherwise. For example, <<expr>> assumes "expr" \
             is an integer. Pass a modifier, such as <<expr::float>> or <<expr::binary>>, \
             to change the default behaviour.
             """
    end
  end
end
