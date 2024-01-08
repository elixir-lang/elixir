Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.DescrTest do
  use ExUnit.Case, async: true

  import Module.Types.Descr

  describe "union" do
    test "bitmap" do
      assert union(integer(), float()) == union(float(), integer())
    end

    test "term" do
      assert union(term(), float()) == term()
      assert union(term(), binary()) == term()
    end

    test "none" do
      assert union(none(), float()) == float()
      assert union(none(), binary()) == binary()
    end
  end

  describe "intersection" do
    test "bitmap" do
      assert intersection(integer(), union(integer(), float())) == integer()
      assert intersection(integer(), float()) == none()
    end

    test "term" do
      assert intersection(term(), float()) == float()
      assert intersection(term(), binary()) == binary()
    end

    test "none" do
      assert intersection(none(), float()) == none()
      assert intersection(none(), binary()) == none()
    end
  end

  describe "difference" do
    test "bitmap" do
      assert difference(float(), integer()) == float()
      assert difference(union(float(), integer()), integer()) == float()
      assert difference(union(float(), integer()), binary()) == union(float(), integer())
    end

    test "term" do
      assert difference(float(), term()) == none()
      assert difference(integer(), term()) == none()
    end

    test "none" do
      assert difference(none(), integer()) == none()
      assert difference(none(), float()) == none()

      assert difference(integer(), none()) == integer()
      assert difference(float(), none()) == float()
    end
  end

  describe "to_quoted" do
    test "bitmap" do
      assert union(integer(), union(float(), binary())) |> to_quoted_string() ==
               "binary() or float() or integer()"
    end

    test "none" do
      assert none() |> to_quoted_string() == "none()"
    end
  end
end
