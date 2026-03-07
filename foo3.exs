defmodule DT do
  @abc [:a, :b]
  defguard g1(t) when tuple_size(t) == 7
  defguard g2(t, i) when elem(t, i) in @abc

  def main() do
    foo(
      {:a, :a, :a,
       {:a, :a, :a, {:c, :c, :c, :a, :a, :a, :c}, {:c, :c, :c, :c, :a, :a, :a}, :c, :c},
       {:a, :a, :a, {:c, :c, :c, :c, :a, :a, :a}, {:c, :c, :c, :a, :a, :a, :c}, :c, :c}, :c, :c}
    )
  end

  def foo(t) when g1(t) and g2(t, 0) and g2(t, 1) and g2(t, 2),
    do: {bar(elem(t, 3)), bar(elem(t, 4))}

  def foo(t) when g1(t) and g2(t, 1) and g2(t, 2) and g2(t, 3),
    do: {bar(elem(t, 4)), bar(elem(t, 5))}

  def foo(t) when g1(t) and g2(t, 2) and g2(t, 3) and g2(t, 4),
    do: {bar(elem(t, 5)), bar(elem(t, 6))}

  def foo(t) when g1(t) and g2(t, 3) and g2(t, 4) and g2(t, 5),
    do: {bar(elem(t, 6)), bar(elem(t, 0))}

  def foo(t) when g1(t) and g2(t, 4) and g2(t, 5) and g2(t, 6),
    do: {bar(elem(t, 0)), bar(elem(t, 1))}

  def foo(t) when g1(t) and g2(t, 5) and g2(t, 6) and g2(t, 0),
    do: {bar(elem(t, 1)), bar(elem(t, 2))}

  def foo(t) when g1(t) and g2(t, 6) and g2(t, 0) and g2(t, 1),
    do: {bar(elem(t, 2)), bar(elem(t, 3))}

  def bar(t) when g1(t) and g2(t, 0) and g2(t, 1) and g2(t, 2),
    do: {baz(elem(t, 3)), baz(elem(t, 4))}

  def bar(t) when g1(t) and g2(t, 1) and g2(t, 2) and g2(t, 3),
    do: {baz(elem(t, 4)), baz(elem(t, 5))}

  def bar(t) when g1(t) and g2(t, 2) and g2(t, 3) and g2(t, 4),
    do: {baz(elem(t, 5)), baz(elem(t, 6))}

  def bar(t) when g1(t) and g2(t, 3) and g2(t, 4) and g2(t, 5),
    do: {baz(elem(t, 6)), baz(elem(t, 0))}

  def bar(t) when g1(t) and g2(t, 4) and g2(t, 5) and g2(t, 6),
    do: {baz(elem(t, 0)), baz(elem(t, 1))}

  def bar(t) when g1(t) and g2(t, 5) and g2(t, 6) and g2(t, 0),
    do: {baz(elem(t, 1)), baz(elem(t, 2))}

  def bar(t) when g1(t) and g2(t, 6) and g2(t, 0) and g2(t, 1),
    do: {baz(elem(t, 2)), baz(elem(t, 3))}

  def baz(t) when g1(t) and elem(t, 0) in @abc and elem(t, 1) in @abc and elem(t, 2) in @abc,
    do: 0

  def baz(t) when g1(t) and elem(t, 1) in @abc and elem(t, 2) in @abc and elem(t, 3) in @abc,
    do: 1

  def baz(t) when g1(t) and elem(t, 2) in @abc and elem(t, 3) in @abc and elem(t, 4) in @abc,
    do: 2

  def baz(t) when g1(t) and elem(t, 3) in @abc and elem(t, 4) in @abc and elem(t, 5) in @abc,
    do: 3

  def baz(t) when g1(t) and elem(t, 4) in @abc and elem(t, 5) in @abc and elem(t, 6) in @abc,
    do: 4

  def baz(t) when g1(t) and elem(t, 5) in @abc and elem(t, 6) in @abc and elem(t, 0) in @abc,
    do: 5

  def baz(t) when g1(t) and elem(t, 6) in @abc and elem(t, 0) in @abc and elem(t, 1) in @abc,
    do: 6
end
