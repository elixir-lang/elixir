defmodule Random do
  @moduledoc """
  Functions for Pseudo random number generation.
  """

  @doc """
  See http://www.erlang.org/doc/man/random.html#seed-0
  """
  def seed() do
    :random.seed
  end

  @doc """
  See http://www.erlang.org/doc/man/random.html#seed-3
  """
  def seed(a1, a2, a3) do
    :random.seed(a1, a2, a3)
  end

  @doc """
  See http://www.erlang.org/doc/man/random.html#seed-1
  """
  def seed({a1, a2, a3}) do
    :random.seed({a1, a2, a3})
  end

  @doc """
  See http://www.erlang.org/doc/man/random.html#seed-0
  """
  def initial_seed() do
    :random.seed0
  end

  @doc """
  See http://www.erlang.org/doc/man/random.html#uniform-0
  """
  def uniform() do
    :random.uniform
  end

  @doc """
  See http://www.erlang.org/doc/man/random.html#uniform-1
  """
  def uniform(n) do
    :random.uniform(n)
  end
end
