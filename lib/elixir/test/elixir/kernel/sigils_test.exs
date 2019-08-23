Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.SigilsTest.Macros do
  defmacro sigil_ws_macro do
    quote do
      ~w(foo, bar baz)s
    end
  end

  defmacro sigil_Ws_macro do
    quote do
      ~W(foo bar, baz)s
    end
  end

  defmacro sigil_wc_macro do
    quote do
      ~w(foo, bar baz)c
    end
  end

  defmacro sigil_Wc_macro do
    quote do
      ~W(foo bar, baz)c
    end
  end

  defmacro sigil_wa_macro do
    quote do
      ~w(foo, bar baz)a
    end
  end

  defmacro sigil_Wa_macro do
    quote do
      ~W(foo bar, baz)a
    end
  end
end

defmodule Kernel.SigilsTest.Runtime_ws do
  def run, do: :this_stub_will_be_replaced_below
end

defmodule Kernel.SigilsTest.Runtime_wa do
  def run, do: :this_stub_will_be_replaced_below
end

defmodule Kernel.SigilsTest.Runtime_wc do
  def run, do: :this_stub_will_be_replaced_below
end

defmodule Kernel.SigilsTest.Runtime_Ws do
  def run, do: :this_stub_will_be_replaced_below
end

defmodule Kernel.SigilsTest.Runtime_Wa do
  def run, do: :this_stub_will_be_replaced_below
end

defmodule Kernel.SigilsTest.Runtime_Wc do
  def run, do: :this_stub_will_be_replaced_below
end

defmodule Kernel.SigilsTest do
  use ExUnit.Case, async: true

  defp capture_err(fun) do
    ExUnit.CaptureIO.capture_io(:stderr, fun)
  end

  test "sigil s" do
    assert ~s(foo) == "foo"
    assert ~s(f#{:o}o) == "foo"
    assert ~s(f\no) == "f\no"
  end

  test "sigil s with heredoc" do
    assert "  foo\n\n" == ~s"""
             f#{:o}o\n
           """
  end

  test "sigil S" do
    assert ~S(foo) == "foo"
    assert ~S[foo] == "foo"
    assert ~S{foo} == "foo"
    assert ~S'foo' == "foo"
    assert ~S"foo" == "foo"
    assert ~S<foo> == "foo"
    assert ~S/foo/ == "foo"
    assert ~S|foo| == "foo"
    assert ~S(f#{o}o) == "f\#{o}o"
    assert ~S(f\#{o}o) == "f\\\#{o}o"
    assert ~S(f\no) == "f\\no"
    assert ~S(foo\)) == "foo)"
    assert ~S[foo\]] == "foo]"
  end

  test "sigil S newline" do
    assert ~S(foo\
bar) in ["foo\\\nbar", "foo\\\r\nbar"]
  end

  test "sigil S with heredoc" do
    assert "  f\#{o}o\\n\n" == ~S"""
             f#{o}o\n
           """
  end

  test "sigil s/S expand to binary when possible" do
    assert Macro.expand(quote(do: ~s(foo)), __ENV__) == "foo"
    assert Macro.expand(quote(do: ~S(foo)), __ENV__) == "foo"
  end

  test "sigil c" do
    assert ~c(foo) == 'foo'
    assert ~c(f#{:o}o) == 'foo'
    assert ~c(f\no) == 'f\no'
  end

  test "sigil C" do
    assert ~C(foo) == 'foo'
    assert ~C[foo] == 'foo'
    assert ~C{foo} == 'foo'
    assert ~C'foo' == 'foo'
    assert ~C"foo" == 'foo'
    assert ~C|foo| == 'foo'
    assert ~C(f#{o}o) == 'f\#{o}o'
    assert ~C(f\no) == 'f\\no'
  end

  test "sigil w" do
    assert ~w() == []
    assert ~w(foo bar baz) == ["foo", "bar", "baz"]
    assert ~w(foo #{:bar} baz) == ["foo", "bar", "baz"]

    assert ~w(#{""}) == []
    assert ~w(foo #{""}) == ["foo"]
    assert ~w(#{" foo bar "}) == ["foo", "bar"]

    assert ~w(foo\ #{:bar}) == ["foo", "bar"]
    assert ~w(foo\ bar) == ["foo", "bar"]

    assert ~w(
      foo
      bar
      baz
    ) == ["foo", "bar", "baz"]

    assert ~w(foo bar baz)s == ["foo", "bar", "baz"]
    assert ~w(foo bar baz)a == [:foo, :bar, :baz]
    assert ~w(foo bar baz)c == ['foo', 'bar', 'baz']

    bad_modifier = quote(do: ~w(foo bar baz)x)
    assert %ArgumentError{} = catch_error(Code.eval_quoted(bad_modifier))

    assert ~w(Foo Bar)a == [:Foo, :Bar]
    assert ~w(Foo.#{Bar}.Baz)a == [:"Foo.Elixir.Bar.Baz"]
    assert ~w(Foo.Bar)s == ["Foo.Bar"]
    assert ~w(Foo.#{Bar})c == ['Foo.Elixir.Bar']

    # Ensure it is fully expanded at compile time
    assert Macro.expand(quote(do: ~w(a b c)a), __ENV__) == [:a, :b, :c]
  end

  test "sigil W" do
    assert ~W() == []
    assert ~W(foo #{bar} baz) == ["foo", "\#{bar}", "baz"]

    assert ~W(foo\ bar) == ["foo\\", "bar"]

    assert ~W(
      foo
      bar
      baz
    ) == ["foo", "bar", "baz"]

    assert ~W(foo bar baz)s == ["foo", "bar", "baz"]
    assert ~W(foo bar baz)a == [:foo, :bar, :baz]
    assert ~W(foo bar baz)c == ['foo', 'bar', 'baz']

    bad_modifier = quote(do: ~W(foo bar baz)x)
    assert %ArgumentError{} = catch_error(Code.eval_quoted(bad_modifier))

    assert ~W(Foo #{Bar})a == [:Foo, :"\#{Bar}"]
    assert ~W(Foo.Bar.Baz)a == [:"Foo.Bar.Baz"]
  end

  test "sigil w/W warns on trailing comma at compile time, not runtime" do
    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Kernel.SigilsTest.Runtime_ws do
               def run, do: ~w(foo, bar baz)s
             end
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn -> Kernel.SigilsTest.Runtime_ws.run() end) == ""

    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Kernel.SigilsTest.Runtime_wa do
               def run, do: ~w(foo, bar baz)s
             end
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn -> Kernel.SigilsTest.Runtime_wa.run() end) == ""

    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Kernel.SigilsTest.Runtime_wc do
               def run, do: ~w(foo, bar baz)s
             end
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn -> Kernel.SigilsTest.Runtime_wc.run() end) == ""

    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Kernel.SigilsTest.Runtime_Ws do
               def run, do: ~w(foo, bar baz)s
             end
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn -> Kernel.SigilsTest.Runtime_Ws.run() end) == ""

    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Kernel.SigilsTest.Runtime_Wa do
               def run, do: ~w(foo, bar baz)s
             end
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn -> Kernel.SigilsTest.Runtime_Wa.run() end) == ""

    assert capture_err(fn ->
             Code.compile_string("""
             defmodule Kernel.SigilsTest.Runtime_Wc do
               def run, do: ~w(foo, bar baz)s
             end
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn -> Kernel.SigilsTest.Runtime_Wc.run() end) == ""
  end

  test "sigil w/W warns on trailing comma inside macro" do
    assert capture_err(fn ->
             Code.compile_string("""
             require Kernel.SigilsTest.Macros
             Kernel.SigilsTest.Macros.sigil_ws_macro()
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn ->
             Code.compile_string("""
             require Kernel.SigilsTest.Macros
             Kernel.SigilsTest.Macros.sigil_wa_macro()
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn ->
             Code.compile_string("""
             require Kernel.SigilsTest.Macros
             Kernel.SigilsTest.Macros.sigil_wc_macro()
             """)
           end) =~
             "item \"foo,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn ->
             Code.compile_string("""
             require Kernel.SigilsTest.Macros
             Kernel.SigilsTest.Macros.sigil_Ws_macro()
             """)
           end) =~
             "item \"bar,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn ->
             Code.compile_string("""
             require Kernel.SigilsTest.Macros
             Kernel.SigilsTest.Macros.sigil_Wa_macro()
             """)
           end) =~
             "item \"bar,\" in word list has a trailing comma; was this intentional?"

    assert capture_err(fn ->
             Code.compile_string("""
             require Kernel.SigilsTest.Macros
             Kernel.SigilsTest.Macros.sigil_Wc_macro()
             """)
           end) =~
             "item \"bar,\" in word list has a trailing comma; was this intentional?"
  end

  test "sigils matching" do
    assert ~s(f\(oo) == "f(oo"
    assert ~s(fo\)o) == "fo)o"
    assert ~s(f\(o\)o) == "f(o)o"
    assert ~s(f[oo) == "f[oo"
    assert ~s(fo]o) == "fo]o"
  end
end
