Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.ExprTest do
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr

  test "literal" do
    assert typecheck!(true) == atom(true)
    assert typecheck!(false) == atom(false)
    assert typecheck!(:foo) == atom(:foo)
    assert typecheck!(0) == integer()
    assert typecheck!(0.0) == float()
    assert typecheck!("foo") == binary()
    assert typecheck!([]) == empty_list()
    assert typecheck!([1, 2]) == non_empty_list()
    assert typecheck!({1, 2}) == tuple()
    assert typecheck!(%{}) == map()
  end

  describe "undefined functions" do
    test "warnings" do
      assert typewarn!(URI.unknown("foo")) ==
               {dynamic(), "URI.unknown/1 is undefined or private"}

      assert typewarn!(if(true, do: URI.unknown("foo"))) ==
               {dynamic(), "URI.unknown/1 is undefined or private"}

      assert typewarn!(try(do: :ok, after: URI.unknown("foo"))) ==
               {dynamic(), "URI.unknown/1 is undefined or private"}
    end
  end

  describe "binaries" do
    test "warnings" do
      assert typewarn!([<<x::binary-size(2)>>], <<x::float>>) ==
               {binary(),
                ~l"""
                incompatible types in expression:

                    <<x::float>>

                expected type:

                    float() or integer()

                but got type:

                    binary()

                where "x" was given the type:

                    # types_test.ex:LINE-2:
                    <<x::binary-size(2)>>
                    => binary()
                """}

      assert typewarn!([<<x::binary>>], <<x>>) ==
               {binary(),
                ~l"""
                incompatible types in expression:

                    <<x>>

                expected type:

                    integer()

                but got type:

                    binary()

                where "x" was given the type:

                    # types_test.ex:LINE-2:
                    <<x::binary>>
                    => binary()

                #{hint()} all expressions given to binaries are assumed to be of type \
                integer() unless said otherwise. For example, <<expr>> assumes "expr" \
                is an integer. Pass a modifier, such as <<expr::float>> or <<expr::binary>>, \
                to change the default behaviour.
                """}

      assert typewarn!([<<x>>], <<x::binary>>) ==
               {binary(),
                ~l"""
                incompatible types in expression:

                    <<x::binary>>

                expected type:

                    binary()

                but got type:

                    integer()

                where "x" was given the type:

                    # types_test.ex:LINE-2:
                    <<x>>
                    => integer()

                #{hint()} all expressions given to binaries are assumed to be of type \
                integer() unless said otherwise. For example, <<expr>> assumes "expr" \
                is an integer. Pass a modifier, such as <<expr::float>> or <<expr::binary>>, \
                to change the default behaviour.
                """}
    end
  end
end
