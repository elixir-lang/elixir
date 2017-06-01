defmodule RedBlackTree do
  @moduledoc """
  Red-black trees are key-value stores.
  While not guaranteed to be perfectly balanced, they guarantee O(log(n)) search
  time.

  The RedBlackTree module contains an eponymous struct and various useful
  functions.

  Nodes know their depth (automatically updated on insert/delete)
  """
  alias RedBlackTree.Node
  use Dict

  defstruct root: nil, size: 0

  @key_hash_bucket 4294967296

  # Inline key hashing
  @compile {:inline, key_less_than?: 2, hash_key: 1, fallback_key_hash: 1}

  def new() do
    %RedBlackTree{}
  end

  def new(values) when is_list(values) do
    new(%RedBlackTree{}, values)
  end

  defp new(tree, []) do
    tree
  end

  # Allow initialization with key/value tuples
  defp new(tree, [{key, value}|tail]) do
    new(RedBlackTree.insert(tree, key, value), tail)
  end

  # Allow initialization with individual values, in which case they will be both
  # the key and the value
  defp new(tree, [key|tail]) do
    new(RedBlackTree.insert(tree, key, key), tail)
  end

  ## Dict behaviour functions
  def size(%RedBlackTree{size: size}) do
    size
  end

  def put(tree, key, value) do
    insert(tree, key, value)
  end

  def fetch(tree, key) do
    if has_key?(tree, key) do
      {:ok, search(tree, key)}
    else
      :error
    end
  end

  def reduce(tree, acc, fun) do
    RedBlackTree.to_list(tree)
    |> Enumerable.List.reduce(acc, fun)
  end

  def insert(%RedBlackTree{root: nil}, key, value) do
    %RedBlackTree{root: Node.new(key, value), size: 1}
  end

  def insert(%RedBlackTree{root: root, size: size}=tree, key, value) do
    {nodes_added, new_root} = do_insert(root, key, value, 1)
    %RedBlackTree{
      tree |
      root: make_node_black(new_root),
      size: size + nodes_added
    }
  end

  def delete(%RedBlackTree{root: root, size: size}=tree, key) do
    {nodes_removed, new_root} = do_delete(root, key)
    %RedBlackTree{
      tree |
      root: new_root,
      size: size - nodes_removed
    }
  end

  def search(%RedBlackTree{root: root}, key) do
    do_search(root, key)
  end

  defp do_search(nil, _key) do
    nil
  end

  defp do_search(%Node{key: node_key, value: value}, search_key) when node_key === search_key do
    value
  end

  defp do_search(%Node{key: node_key, left: left}, search_key) when search_key < node_key do
    do_search(left, search_key)
  end

  defp do_search(%Node{key: node_key, right: right}, search_key) when search_key > node_key do
    do_search(right, search_key)
  end

  # For cases when `insert_key !== node_key` but `insert_key == node_key` (e.g.
  # `1` and `1.0`,) hash the keys to provide consistent ordering.
  defp do_search(%Node{key: node_key, left: left, right: right}, search_key) when search_key == node_key do
    if key_less_than?(search_key, node_key) do
      do_search(left, search_key)
    else
      do_search(right, search_key)
    end
  end

  def has_key?(%RedBlackTree{root: root}, key) do
    do_has_key?(root, key)
  end

  defp do_has_key?(nil, _key) do
    false
  end

  defp do_has_key?(%Node{key: node_key}, search_key) when node_key === search_key do
    true
  end

  defp do_has_key?(%Node{key: node_key, left: left}, search_key) when search_key < node_key do
    do_has_key?(left, search_key)
  end

  defp do_has_key?(%Node{key: node_key, right: right}, search_key) when search_key > node_key do
    do_has_key?(right, search_key)
  end

  # For cases when `insert_key !== node_key` but `insert_key == node_key` (e.g.
  # `1` and `1.0`,) hash the keys to provide consistent ordering.
  defp do_has_key?(%Node{key: node_key, left: left, right: right}, search_key) when search_key == node_key do
    if key_less_than?(search_key, node_key) do
      do_has_key?(left, search_key)
    else
      do_has_key?(right, search_key)
    end
  end


  @doc """
  For each node, calls the provided function passing in (node, acc)
  Optionally takes an order as the first argument which can be one of
  `:in_order`, `:pre_order`, or `:post_order`.

  Defaults to `:in_order` if no order is given.
  """
  def reduce_nodes(%RedBlackTree{}=tree, acc, fun) do
    reduce_nodes(:in_order, tree, acc, fun)
  end

  def reduce_nodes(_order, %RedBlackTree{root: nil}, acc, _fun) do
    acc
  end

  def reduce_nodes(order, %RedBlackTree{root: root}, acc, fun) do
    do_reduce_nodes(order, root, acc, fun)
  end

  @doc """
  Balances the supplied tree to adhere to the two rules of Red-Black trees:

  1. Every red node must have two black child nodes (and therefore it must have a black parent).
  2. Every path from a given node to any of its descendant NIL nodes contains the same number of black nodes.

  """
  def balance(%RedBlackTree{root: root}=tree) do
    %RedBlackTree{tree | root: do_balance(root)}
  end

  def to_list(%RedBlackTree{}=tree) do
    reduce_nodes(tree, [], fn (node, members) ->
      [{node.key, node.value} | members]
    end) |> Enum.reverse
  end

  ## Helpers

  defp make_node_black(%Node{}=node) do
    Node.color(node, :black)
  end

  # ¡This is only used as a tiebreaker!
  # For cases when `insert_key !== node_key` but `insert_key == node_key` (e.g.
  # `1` and `1.0`,) hash the keys to provide consistent ordering.
  defp hash_key(key) do
    :erlang.phash2(key, @key_hash_bucket)
  end

  # In the case that `hash_key(key1) == hash_key(key2)` we can fall back again
  # to the slower phash function distributed over @key_hash_bucket integers.
  # If these two collide, go home.
  defp fallback_key_hash(key) do
    :erlang.phash(key, @key_hash_bucket)
  end

  # Should only be used when `key1 !== key2 and key1 == key2`. In the case
  defp key_less_than?(key1, key2) do
    hashed_key1 = hash_key(key1)
    hashed_key2 = hash_key(key2)
    cond do
      hashed_key1 === hashed_key2 ->
        fallback_key_hash(key1) < fallback_key_hash(key2)
      hashed_key1 < hashed_key2 -> true
      true -> false
    end
  end

  ### Operations

  #### Insert

  defp do_insert(nil, insert_key, insert_value, depth) do
    {
      1,
      %Node{
        Node.new(insert_key, insert_value, depth) |
        color: :red
      }
    }

  end

  defp do_insert(%Node{key: node_key}=node, insert_key, insert_value, _depth) when node_key === insert_key do
    {0, %Node{node | value: insert_value}}
  end

  defp do_insert(%Node{key: node_key}=node, insert_key, insert_value, depth) when insert_key < node_key do
    do_insert_left(node, insert_key, insert_value, depth)
  end

  defp do_insert(%Node{key: node_key}=node, insert_key, insert_value, depth) when insert_key > node_key do
    do_insert_right(node, insert_key, insert_value, depth)
  end

  # For cases when `insert_key !== node_key` but `insert_key == node_key` (e.g.
  # `1` and `1.0`,) hash the keys to provide consistent ordering.
  defp do_insert(%Node{key: node_key}=node, insert_key, insert_value, depth) when insert_key == node_key do
    if key_less_than?(insert_key, node_key) do
      do_insert_left(node, insert_key, insert_value, depth)
    else
      do_insert_right(node, insert_key, insert_value, depth)
    end
  end

  defp do_insert_left(%Node{left: left}=node, insert_key, insert_value, depth) do
    {nodes_added, new_left} = do_insert(left, insert_key, insert_value, depth + 1)
    {nodes_added, %Node{node | left: do_balance(new_left)}}
  end

  defp do_insert_right(%Node{right: right}=node, insert_key, insert_value, depth) do
    {nodes_added, new_right} = do_insert(right, insert_key, insert_value, depth + 1)
    {nodes_added, %Node{node | right: do_balance(new_right)}}
  end

  #### Delete

  # If we reach a leaf and the key never matched, do nothing
  defp do_delete(nil, _key) do
    {0, nil}
  end

  # If both the right and left are nil, the new tree is nil. For example,
  # deleting A in the following tree results in B having no left
  #
  #        B
  #       / \
  #      A   C
  #
  defp do_delete(%Node{key: node_key, left: nil, right: nil}, delete_key) when node_key === delete_key do
    {1, nil}
  end

  # If left is nil and there is a right, promote the right. For example,
  # deleting C in the following tree results in B's right becoming D
  #
  #        B
  #       / \
  #      A   C
  #           \
  #            D
  #
  defp do_delete(%Node{key: node_key, left: nil, right: right}, delete_key) when node_key === delete_key do
    {1, %Node{right | depth: right.depth - 1}}
  end

  # If there is a left promote it. For example,
  # deleting B in the following tree results in C's left becoming A
  #
  #        C
  #       / \
  #      B   D
  #     /
  #    A
  #
  defp do_delete(%Node{key: node_key, left: left, right: nil}, delete_key) when node_key === delete_key do
    {1, %Node{left | depth: left.depth - 1}}
  end

  # If there are both left and right nodes, recursively promote the left-most
  # nodes. For example, deleting E below results in the following:
  #
  #        G      =>         G
  #       / \               / \
  #      E   H    =>       C   H
  #     / \               / \
  #    C   F      =>     B   D
  #   / \               /     \
  #  A   D        =>   A       F
  #   \
  #    B
  #
  #
  defp do_delete(%Node{key: node_key, left: left, right: right}, delete_key) when node_key === delete_key do
    {
      1,
      do_balance(%Node{
        left |
        depth: left.depth - 1,
        left: do_balance(promote(left)),
        right: right
      })
    }
  end

  defp do_delete(%Node{key: node_key}=node, delete_key) when delete_key < node_key do
    do_delete_left(node, delete_key)
  end

  defp do_delete(%Node{key: node_key}=node, delete_key) when delete_key > node_key do
    do_delete_right(node, delete_key)
  end

  # For cases when `delete_key !== node_key` but `delete_key == node_key` (e.g.
  # `1` and `1.0`,) hash the keys to provide consistent ordering.
  defp do_delete(%Node{key: node_key}=node, delete_key) when delete_key == node_key do
    if key_less_than?(delete_key, node_key) do
      do_delete_left(node, delete_key)
    else
      do_delete_right(node, delete_key)
    end
  end

  defp do_delete_left(%Node{left: left}=node, delete_key) do
    {nodes_removed, new_left} = do_delete(left, delete_key)
    {
      nodes_removed,
      %Node{
        node |
        left: do_balance(new_left)
      }
    }
  end

  defp do_delete_right(%Node{right: right}=node, delete_key) do
    {nodes_removed, new_right} = do_delete(right, delete_key)
    {
      nodes_removed,
      %Node{
        node |
        right: do_balance(new_right)
      }
    }
  end

  defp promote(nil) do
    nil
  end

  defp promote(%Node{left: nil, right: nil, depth: depth}=node) do
    %Node{ node | color: :red, depth: depth - 1 }
  end

  defp promote(%Node{left: left, right: nil, depth: depth}) do
    %Node{ left | color: :red, depth: depth - 1}
  end

  defp promote(%Node{left: nil, right: right, depth: depth}) do
    %Node{ right | color: :red, depth: depth - 1}
  end

  defp promote(%Node{left: left, right: right, depth: depth}) do
    balance(%Node{
      left |
      depth: depth - 1,
      left: do_balance(promote(left)),
      right: right
    })
  end

  #### Balance

  # If we have a tree that looks like this:
  #              B (Black)
  #             /         \
  #            A          D (Red)
  #                      /       \
  #                     C         F (Red)
  #                              /       \
  #                             E         G
  #
  #
  # Rotate to balance and look like this:
  #
  #                   D (Red)
  #                 /         \
  #          B (Black)        F (Black)
  #         /        \       /         \
  #        A          C     E           G
  #
  #
  defp do_balance(
    %Node{
      color: :black,
      left: a_node,
      right: %Node{
        color: :red,
        left: c_node,
        right: %Node{
          color: :red,
          left: e_node,
          right: g_node
        }=f_node
      }=d_node
    }=b_node) do

    balanced_tree(a_node, b_node, c_node, d_node, e_node, f_node, g_node)
  end

  # If we have a tree that looks like this:
  #
  #         B (Black)
  #        /         \
  #       A       F (Red)
  #              /       \
  #           D (Red)     G
  #          /       \
  #         C         E
  #
  # Rotate to balance like so:
  #
  #                D (Red)
  #               /       \
  #        B (Black)       F (Black)
  #       /         \     /         \
  #      A           C   E           G
  #
  #
  #
  defp do_balance(
    %Node{
      color: :black,
      left: a_node,
      right: %Node{
        color: :red,
        left: %Node{
          color: :red,
          left: c_node,
          right: e_node
        }=d_node,
        right: g_node
      }=f_node
    }=b_node) do

    balanced_tree(a_node, b_node, c_node, d_node, e_node, f_node, g_node)
  end

  # If we have a tree that looks like this:
  #
  #
  #                 F (Black)
  #                /         \
  #               D (Red)     G
  #              /       \
  #           B (Red)     E
  #          /       \
  #         A          C
  #
  #
  # Rebalance to look like so:
  #
  #               D (Red)
  #              /       \
  #      B (Black)        F (Black)
  #     /         \      /         \
  #    A           C    E           G
  #
  defp do_balance(%Node{
      color: :black,
      left: %Node{
        color: :red,
        left: %Node{
          color: :red,
          left: a_node,
          right: c_node
        }=b_node,
        right: e_node
      }=d_node,
      right: g_node
    }=f_node) do

    balanced_tree(a_node, b_node, c_node, d_node, e_node, f_node, g_node)
  end

  # If we have a tree that looks like this:
  #
  #               F (Black)
  #              /         \
  #          B (Red)        G
  #         /       \
  #        A         D (Red)
  #                 /       \
  #                C         E
  #
  # Rebalance to look like this:
  #
  #            D (Red)
  #           /       \
  #     B (Black)      F (Black)
  #    /         \    /         \
  #   A           C  E           G
  #
  defp do_balance(%Node{
      color: :black,
      left: %Node{
        color: :red,
        left: a_node,
        right: %Node{
          color: :red,
          left: c_node,
          right: e_node
        }=d_node
      }=b_node,
      right: g_node
    }=f_node) do

    balanced_tree(a_node, b_node, c_node, d_node, e_node, f_node, g_node)
  end


  defp do_balance(node) do
    node
  end

  defp balanced_tree(a_node, b_node, c_node, d_node, e_node, f_node, g_node) do
    min_depth = min_depth([a_node, b_node, c_node, d_node, e_node, f_node, g_node])
    %Node {
      d_node |
      color: :red,
      depth: min_depth,
      left: %Node{b_node | color: :black, depth: min_depth + 1,
        left: %Node{a_node | depth: min_depth + 2},
        right: %Node{c_node | depth: min_depth + 2}},
      right: %Node{f_node | color: :black, depth: min_depth + 1,
        left: %Node{e_node | depth: min_depth + 2},
        right: %Node{g_node | depth: min_depth + 2},}
    }
  end

  defp min_depth(list_of_nodes) do
    Enum.reduce(list_of_nodes, -1, fn (node, acc) ->
      if acc == -1 || node.depth < acc do
        node.depth
      else
        acc
      end
    end)
  end

  defp do_reduce_nodes(_order, nil, acc, _fun) do
    acc
  end

  # self, left, right
  defp do_reduce_nodes(:pre_order, %Node{left: left, right: right}=node, acc, fun) do
    acc_after_self = fun.(node, acc)
    acc_after_left = do_reduce_nodes(:pre_order, left, acc_after_self, fun)
    do_reduce_nodes(:pre_order, right, acc_after_left, fun)
  end

  # left, self, right
  defp do_reduce_nodes(:in_order, %Node{left: left, right: right}=node, acc, fun) do
    acc_after_left = do_reduce_nodes(:in_order, left, acc, fun)
    acc_after_self = fun.(node, acc_after_left)
    do_reduce_nodes(:in_order, right, acc_after_self, fun)
  end

  # left, right, self
  defp do_reduce_nodes(:post_order, %Node{left: left, right: right}=node, acc, fun) do
    acc_after_left = do_reduce_nodes(:post_order, left, acc, fun)
    acc_after_right = do_reduce_nodes(:post_order, right, acc_after_left, fun)
    fun.(node, acc_after_right)
  end
end

defimpl Enumerable, for: RedBlackTree do
  def count(%RedBlackTree{size: size}), do: size
  def member?(%RedBlackTree{}=tree, key), do: RedBlackTree.has_key?(tree, key)
  def reduce(tree, acc, fun), do: RedBlackTree.reduce(tree, acc, fun)
end

defimpl Access, for: RedBlackTree do
  def get(tree, key) do
    RedBlackTree.search(tree, key)
  end

  def get_and_update(tree, key, fun) do
    {get, update} = fun.(RedBlackTree.search(tree, key))
    {get, RedBlackTree.insert(tree, key, update)}
  end
end

defimpl Collectable, for: RedBlackTree do
  def into(original) do
    {original, fn
      tree, {:cont, {key, value}} -> RedBlackTree.insert(tree, key, value)
      tree, :done -> tree
      _, :halt -> :ok
    end}
  end
end
