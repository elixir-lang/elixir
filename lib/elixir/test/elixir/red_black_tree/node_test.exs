defmodule RedBlackTree.NodeTest do
  use ExUnit.Case, async: true
  alias RedBlackTree.Node

  test "#new" do
    assert %Node{
      key: :walrus,
      value: :bubbles,
      color: :black,
      left: nil,
      right: nil
    } == Node.new(:walrus, :bubbles)
  end

  test "#color" do
    assert :red == Node.color(%Node{color: :black}, :red).color
    assert :black == Node.color(%Node{color: :red}, :black).color
  end
end
