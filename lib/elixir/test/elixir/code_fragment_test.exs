Code.require_file("test_helper.exs", __DIR__)

defmodule CodeFragmentTest do
  use ExUnit.Case, async: true

  doctest Code.Fragment
  alias Code.Fragment, as: CF

  describe "cursor_context/2" do
    test "expressions" do
      assert CF.cursor_context([]) == :expr
      assert CF.cursor_context(",") == :expr
      assert CF.cursor_context("[") == :expr
      assert CF.cursor_context("<<") == :expr
      assert CF.cursor_context("=>") == :expr
      assert CF.cursor_context("->") == :expr
      assert CF.cursor_context("foo(<<") == :expr
      assert CF.cursor_context("hello: ") == :expr
      assert CF.cursor_context("\n") == :expr
      assert CF.cursor_context(~c"\n") == :expr
      assert CF.cursor_context("\n\n") == :expr
      assert CF.cursor_context(~c"\n\n") == :expr
      assert CF.cursor_context("\r\n") == :expr
      assert CF.cursor_context(~c"\r\n") == :expr
      assert CF.cursor_context("\r\n\r\n") == :expr
      assert CF.cursor_context(~c"\r\n\r\n") == :expr
    end

    test "local_or_var" do
      assert CF.cursor_context("hello_wo") == {:local_or_var, ~c"hello_wo"}
      assert CF.cursor_context("hello_world?") == {:local_or_var, ~c"hello_world?"}
      assert CF.cursor_context("hello_world!") == {:local_or_var, ~c"hello_world!"}
      assert CF.cursor_context("hello/wor") == {:local_or_var, ~c"wor"}
      assert CF.cursor_context("hello..wor") == {:local_or_var, ~c"wor"}
      assert CF.cursor_context("hello::wor") == {:local_or_var, ~c"wor"}
      assert CF.cursor_context("[hello_wo") == {:local_or_var, ~c"hello_wo"}
      assert CF.cursor_context("'hello_wo") == {:local_or_var, ~c"hello_wo"}
      assert CF.cursor_context("hellò_wó") == {:local_or_var, ~c"hellò_wó"}
      assert CF.cursor_context("hello? world") == {:local_or_var, ~c"world"}
      assert CF.cursor_context("hello! world") == {:local_or_var, ~c"world"}
      assert CF.cursor_context("hello: world") == {:local_or_var, ~c"world"}
      assert CF.cursor_context("__MODULE__") == {:local_or_var, ~c"__MODULE__"}
    end

    test "dot" do
      assert CF.cursor_context("hello.") == {:dot, {:var, ~c"hello"}, ~c""}
      assert CF.cursor_context(":hello.") == {:dot, {:unquoted_atom, ~c"hello"}, ~c""}
      assert CF.cursor_context("nested.map.") == {:dot, {:dot, {:var, ~c"nested"}, ~c"map"}, ~c""}

      assert CF.cursor_context("Hello.") == {:dot, {:alias, ~c"Hello"}, ~c""}
      assert CF.cursor_context("Hello.World.") == {:dot, {:alias, ~c"Hello.World"}, ~c""}
      assert CF.cursor_context("Hello.wor") == {:dot, {:alias, ~c"Hello"}, ~c"wor"}
      assert CF.cursor_context("hello.wor") == {:dot, {:var, ~c"hello"}, ~c"wor"}
      assert CF.cursor_context("Hello.++") == {:dot, {:alias, ~c"Hello"}, ~c"++"}
      assert CF.cursor_context(":hello.wor") == {:dot, {:unquoted_atom, ~c"hello"}, ~c"wor"}
      assert CF.cursor_context(":hell@o.wor") == {:dot, {:unquoted_atom, ~c"hell@o"}, ~c"wor"}
      assert CF.cursor_context(":he@ll@o.wor") == {:dot, {:unquoted_atom, ~c"he@ll@o"}, ~c"wor"}
      assert CF.cursor_context(":hell@@o.wor") == {:dot, {:unquoted_atom, ~c"hell@@o"}, ~c"wor"}
      assert CF.cursor_context("@hello.wor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context("@hello. wor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}
      assert CF.cursor_context("@hello .wor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}
      assert CF.cursor_context("@hello . wor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context("@hello.\nwor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}
      assert CF.cursor_context("@hello. \nwor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}
      assert CF.cursor_context("@hello.\n wor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context("@hello.\r\nwor") ==
               {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context("@hello\n.wor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}
      assert CF.cursor_context("@hello \n.wor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}
      assert CF.cursor_context("@hello\n .wor") == {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context("@hello. # some comment\nwor") ==
               {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context("@hello. # some comment\n\nwor") ==
               {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context("@hello. # some comment\nsub\n.wor") ==
               {:dot, {:dot, {:module_attribute, ~c"hello"}, ~c"sub"}, ~c"wor"}

      assert CF.cursor_context(~c"@hello.\nwor") ==
               {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context(~c"@hello.\r\nwor") ==
               {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context(~c"@hello\n.wor") ==
               {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context(~c"@hello. # some comment\nwor") ==
               {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context(~c"@hello. # some comment\n\nwor") ==
               {:dot, {:module_attribute, ~c"hello"}, ~c"wor"}

      assert CF.cursor_context(~c"@hello. # some comment\nsub\n.wor") ==
               {:dot, {:dot, {:module_attribute, ~c"hello"}, ~c"sub"}, ~c"wor"}

      assert CF.cursor_context("nested.map.wor") ==
               {:dot, {:dot, {:var, ~c"nested"}, ~c"map"}, ~c"wor"}

      assert CF.cursor_context("__MODULE__.") == {:dot, {:var, ~c"__MODULE__"}, ~c""}

      assert CF.cursor_context("__MODULE__.Sub.") ==
               {:dot, {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Sub"}, ~c""}

      assert CF.cursor_context("@hello.Sub.wor") ==
               {:dot, {:alias, {:module_attribute, ~c"hello"}, ~c"Sub"}, ~c"wor"}
    end

    test "local_arity" do
      assert CF.cursor_context("hello/") == {:local_arity, ~c"hello"}
    end

    test "local_call" do
      assert CF.cursor_context("hello\s") == {:local_call, ~c"hello"}
      assert CF.cursor_context("hello\t") == {:local_call, ~c"hello"}
      assert CF.cursor_context("hello(") == {:local_call, ~c"hello"}
      assert CF.cursor_context("hello(\s") == {:local_call, ~c"hello"}
      assert CF.cursor_context("hello(\t") == {:local_call, ~c"hello"}
      assert CF.cursor_context("hello(\n") == {:local_call, ~c"hello"}
      assert CF.cursor_context("hello(\r\n") == {:local_call, ~c"hello"}
      assert CF.cursor_context("...(") == {:local_call, ~c"..."}
      assert CF.cursor_context("...(\s") == {:local_call, ~c"..."}
    end

    test "dot_arity" do
      assert CF.cursor_context("Foo.hello/") == {:dot_arity, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo.+/") == {:dot_arity, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("Foo . hello /") == {:dot_arity, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo . + /") == {:dot_arity, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("foo.hello/") == {:dot_arity, {:var, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context(":foo.hello/") ==
               {:dot_arity, {:unquoted_atom, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context("@f.hello/") == {:dot_arity, {:module_attribute, ~c"f"}, ~c"hello"}
    end

    test "dot_call" do
      assert CF.cursor_context("Foo.hello\s") == {:dot_call, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo.hello\t") == {:dot_call, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo.hello(") == {:dot_call, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo.hello(\s") == {:dot_call, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo.hello(\t") == {:dot_call, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo . hello (") == {:dot_call, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo . hello (\s") == {:dot_call, {:alias, ~c"Foo"}, ~c"hello"}
      assert CF.cursor_context("Foo . hello (\t") == {:dot_call, {:alias, ~c"Foo"}, ~c"hello"}

      assert CF.cursor_context(":foo.hello\s") ==
               {:dot_call, {:unquoted_atom, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context(":foo.hello\t") ==
               {:dot_call, {:unquoted_atom, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context(":foo.hello(") == {:dot_call, {:unquoted_atom, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context(":foo.hello(\s") ==
               {:dot_call, {:unquoted_atom, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context(":foo.hello(\t") ==
               {:dot_call, {:unquoted_atom, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context(":foo.hello\s") ==
               {:dot_call, {:unquoted_atom, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context("foo.hello\s") == {:dot_call, {:var, ~c"foo"}, ~c"hello"}
      assert CF.cursor_context("foo.hello\t") == {:dot_call, {:var, ~c"foo"}, ~c"hello"}
      assert CF.cursor_context("foo.hello(") == {:dot_call, {:var, ~c"foo"}, ~c"hello"}
      assert CF.cursor_context("foo.hello(\s") == {:dot_call, {:var, ~c"foo"}, ~c"hello"}
      assert CF.cursor_context("foo.hello(\t") == {:dot_call, {:var, ~c"foo"}, ~c"hello"}
      assert CF.cursor_context("foo.hello(\n") == {:dot_call, {:var, ~c"foo"}, ~c"hello"}
      assert CF.cursor_context("foo.hello(\r\n") == {:dot_call, {:var, ~c"foo"}, ~c"hello"}

      assert CF.cursor_context("@f.hello\s") == {:dot_call, {:module_attribute, ~c"f"}, ~c"hello"}
      assert CF.cursor_context("@f.hello\t") == {:dot_call, {:module_attribute, ~c"f"}, ~c"hello"}
      assert CF.cursor_context("@f.hello(") == {:dot_call, {:module_attribute, ~c"f"}, ~c"hello"}

      assert CF.cursor_context("@f.hello(\s") ==
               {:dot_call, {:module_attribute, ~c"f"}, ~c"hello"}

      assert CF.cursor_context("@f.hello(\t") ==
               {:dot_call, {:module_attribute, ~c"f"}, ~c"hello"}

      assert CF.cursor_context("Foo.+\s") == {:dot_call, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("Foo.+\t") == {:dot_call, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("Foo.+(") == {:dot_call, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("Foo.+(\s") == {:dot_call, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("Foo.+(\t") == {:dot_call, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("Foo . + (") == {:dot_call, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("Foo . + (\s") == {:dot_call, {:alias, ~c"Foo"}, ~c"+"}
      assert CF.cursor_context("Foo . + (\t") == {:dot_call, {:alias, ~c"Foo"}, ~c"+"}

      assert CF.cursor_context("__MODULE__.Foo.hello(") ==
               {:dot_call, {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Foo"}, ~c"hello"}

      assert CF.cursor_context("@foo.Foo.hello(") ==
               {:dot_call, {:alias, {:module_attribute, ~c"foo"}, ~c"Foo"}, ~c"hello"}
    end

    test "anonymous_call" do
      assert CF.cursor_context("hello.(") == {:anonymous_call, {:var, ~c"hello"}}
      assert CF.cursor_context("hello.(\s") == {:anonymous_call, {:var, ~c"hello"}}
      assert CF.cursor_context("hello.(\t") == {:anonymous_call, {:var, ~c"hello"}}
      assert CF.cursor_context("hello.(\n") == {:anonymous_call, {:var, ~c"hello"}}
      assert CF.cursor_context("hello.(\r\n") == {:anonymous_call, {:var, ~c"hello"}}

      assert CF.cursor_context("hello . (") == {:anonymous_call, {:var, ~c"hello"}}

      assert CF.cursor_context("@hello.(") == {:anonymous_call, {:module_attribute, ~c"hello"}}
      assert CF.cursor_context("@hello . (") == {:anonymous_call, {:module_attribute, ~c"hello"}}
    end

    test "nested expressions" do
      assert CF.cursor_context("Hello.world()") == :none
      assert CF.cursor_context("hello().") == {:dot, :expr, ~c""}
      assert CF.cursor_context("Foo.hello ('(').") == {:dot, :expr, ~c""}
      assert CF.cursor_context("Foo.hello('(', ?), ?().bar") == {:dot, :expr, ~c"bar"}
      assert CF.cursor_context("Hello.bar(World.call(42), ?), ?().foo") == {:dot, :expr, ~c"foo"}
      assert CF.cursor_context("Foo.hello( ).world") == {:dot, :expr, ~c"world"}
      assert CF.cursor_context("hello.dyn_impl().call(42).bar") == {:dot, :expr, ~c"bar"}

      assert CF.cursor_context("Foo.dyn_impl().call(") == {:dot_call, :expr, ~c"call"}
      assert CF.cursor_context("hello().call(") == {:dot_call, :expr, ~c"call"}
    end

    test "alias" do
      assert CF.cursor_context("HelloWor") == {:alias, ~c"HelloWor"}
      assert CF.cursor_context("Hello.Wor") == {:alias, ~c"Hello.Wor"}
      assert CF.cursor_context("Hello.\nWor") == {:alias, ~c"Hello.Wor"}
      assert CF.cursor_context("Hello.\r\nWor") == {:alias, ~c"Hello.Wor"}
      assert CF.cursor_context("Hello . Wor") == {:alias, ~c"Hello.Wor"}
      assert CF.cursor_context("Hello::Wor") == {:alias, ~c"Wor"}
      assert CF.cursor_context("Hello..Wor") == {:alias, ~c"Wor"}

      assert CF.cursor_context("__MODULE__.Wor") ==
               {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Wor"}

      assert CF.cursor_context("@foo.Wor") == {:alias, {:module_attribute, ~c"foo"}, ~c"Wor"}
    end

    test "structs" do
      assert CF.cursor_context("%") == {:struct, ~c""}
      assert CF.cursor_context(":%") == {:unquoted_atom, ~c"%"}
      assert CF.cursor_context("::%") == {:struct, ~c""}

      assert CF.cursor_context("%HelloWor") == {:struct, ~c"HelloWor"}

      assert CF.cursor_context("%Hello.") == {:struct, {:dot, {:alias, ~c"Hello"}, ~c""}}
      assert CF.cursor_context("%Hello.nam") == {:struct, {:dot, {:alias, ~c"Hello"}, ~c"nam"}}
      assert CF.cursor_context("%Hello.Wor") == {:struct, ~c"Hello.Wor"}
      assert CF.cursor_context("% Hello . Wor") == {:struct, ~c"Hello.Wor"}

      assert CF.cursor_context("%__MODULE_") == {:struct, {:local_or_var, ~c"__MODULE_"}}
      assert CF.cursor_context("%__MODULE__") == {:struct, {:local_or_var, ~c"__MODULE__"}}

      assert CF.cursor_context("%__MODULE__.") ==
               {:struct, {:dot, {:local_or_var, ~c"__MODULE__"}, ~c""}}

      assert CF.cursor_context("%__MODULE__.Sub.") ==
               {:struct, {:dot, {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Sub"}, ~c""}}

      assert CF.cursor_context("%__MODULE__.Wor") ==
               {:struct, {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Wor"}}

      assert CF.cursor_context("%@foo") ==
               {:struct, {:module_attribute, ~c"foo"}}

      assert CF.cursor_context("%@foo.") ==
               {:struct, {:dot, {:module_attribute, ~c"foo"}, ~c""}}

      assert CF.cursor_context("%@foo.Wor") ==
               {:struct, {:alias, {:module_attribute, ~c"foo"}, ~c"Wor"}}
    end

    test "unquoted atom" do
      assert CF.cursor_context(":") == {:unquoted_atom, ~c""}
      assert CF.cursor_context(":HelloWor") == {:unquoted_atom, ~c"HelloWor"}
      assert CF.cursor_context(":HelloWór") == {:unquoted_atom, ~c"HelloWór"}
      assert CF.cursor_context(":hello_wor") == {:unquoted_atom, ~c"hello_wor"}
      assert CF.cursor_context(":Óla_mundo") == {:unquoted_atom, ~c"Óla_mundo"}
      assert CF.cursor_context(":Ol@_mundo") == {:unquoted_atom, ~c"Ol@_mundo"}
      assert CF.cursor_context(":Ol@") == {:unquoted_atom, ~c"Ol@"}
      assert CF.cursor_context("foo:hello_wor") == {:unquoted_atom, ~c"hello_wor"}

      # Operators from atoms
      assert CF.cursor_context(":+") == {:unquoted_atom, ~c"+"}
      assert CF.cursor_context(":or") == {:unquoted_atom, ~c"or"}
      assert CF.cursor_context(":<") == {:unquoted_atom, ~c"<"}
      assert CF.cursor_context(":.") == {:unquoted_atom, ~c"."}
      assert CF.cursor_context(":..") == {:unquoted_atom, ~c".."}
      assert CF.cursor_context(":->") == {:unquoted_atom, ~c"->"}
      assert CF.cursor_context(":%") == {:unquoted_atom, ~c"%"}
    end

    test "operators" do
      assert CF.cursor_context("+") == {:operator, ~c"+"}
      assert CF.cursor_context("++") == {:operator, ~c"++"}
      assert CF.cursor_context("!") == {:operator, ~c"!"}
      assert CF.cursor_context("<") == {:operator, ~c"<"}
      assert CF.cursor_context("<<<") == {:operator, ~c"<<<"}
      assert CF.cursor_context("..") == {:operator, ~c".."}
      assert CF.cursor_context("<~") == {:operator, ~c"<~"}
      assert CF.cursor_context("=~") == {:operator, ~c"=~"}
      assert CF.cursor_context("<~>") == {:operator, ~c"<~>"}
      assert CF.cursor_context("::") == {:operator, ~c"::"}

      assert CF.cursor_context("+ ") == {:operator_call, ~c"+"}
      assert CF.cursor_context("++ ") == {:operator_call, ~c"++"}
      assert CF.cursor_context("! ") == {:operator_call, ~c"!"}
      assert CF.cursor_context("< ") == {:operator_call, ~c"<"}
      assert CF.cursor_context("<<< ") == {:operator_call, ~c"<<<"}
      assert CF.cursor_context(".. ") == {:operator_call, ~c".."}
      assert CF.cursor_context("<~ ") == {:operator_call, ~c"<~"}
      assert CF.cursor_context("=~ ") == {:operator_call, ~c"=~"}
      assert CF.cursor_context("<~> ") == {:operator_call, ~c"<~>"}
      assert CF.cursor_context(":: ") == {:operator_call, ~c"::"}

      assert CF.cursor_context("+/") == {:operator_arity, ~c"+"}
      assert CF.cursor_context("++/") == {:operator_arity, ~c"++"}
      assert CF.cursor_context("!/") == {:operator_arity, ~c"!"}
      assert CF.cursor_context("</") == {:operator_arity, ~c"<"}
      assert CF.cursor_context("<<</") == {:operator_arity, ~c"<<<"}
      assert CF.cursor_context("../") == {:operator_arity, ~c".."}
      assert CF.cursor_context("<~/") == {:operator_arity, ~c"<~"}
      assert CF.cursor_context("=~/") == {:operator_arity, ~c"=~"}
      assert CF.cursor_context("<~>/") == {:operator_arity, ~c"<~>"}
      assert CF.cursor_context("::/") == {:operator_arity, ~c"::"}

      # Unknown operators altogether
      assert CF.cursor_context("***") == :none

      # Textual operators are shown as local_or_var UNLESS there is space
      assert CF.cursor_context("when") == {:local_or_var, ~c"when"}
      assert CF.cursor_context("when ") == {:operator_call, ~c"when"}
      assert CF.cursor_context("when.") == :none

      assert CF.cursor_context("not") == {:local_or_var, ~c"not"}
      assert CF.cursor_context("not ") == {:operator_call, ~c"not"}
      assert CF.cursor_context("not.") == :none
    end

    test "sigil" do
      assert CF.cursor_context("~") == {:sigil, ~c""}
      assert CF.cursor_context("~ ") == :none

      assert CF.cursor_context("~r") == {:sigil, ~c"r"}
      assert CF.cursor_context("~r/") == :none
      assert CF.cursor_context("~r<") == :none

      assert CF.cursor_context("~R") == {:sigil, ~c"R"}
      assert CF.cursor_context("~R/") == :none
      assert CF.cursor_context("~R<") == :none

      assert CF.cursor_context("Foo.~") == :none
      assert CF.cursor_context("Foo.~ ") == :none
    end

    test "module attribute" do
      assert CF.cursor_context("@") == {:module_attribute, ~c""}
      assert CF.cursor_context("@hello_wo") == {:module_attribute, ~c"hello_wo"}
    end

    test "none" do
      # Punctuation
      assert CF.cursor_context(")") == :none
      assert CF.cursor_context("}") == :none
      assert CF.cursor_context(">>") == :none
      assert CF.cursor_context("'") == :none
      assert CF.cursor_context("\"") == :none

      # Numbers
      assert CF.cursor_context("123") == :none
      assert CF.cursor_context("123?") == :none
      assert CF.cursor_context("123!") == :none
      assert CF.cursor_context("123var?") == :none
      assert CF.cursor_context("0x") == :none

      # Codepoints
      assert CF.cursor_context("?") == :none
      assert CF.cursor_context("?a") == :none
      assert CF.cursor_context("?foo") == :none

      # Dots
      assert CF.cursor_context(".") == :none
      assert CF.cursor_context("Mundo.Óla") == :none
      assert CF.cursor_context(":hello.World") == :none

      # Aliases
      assert CF.cursor_context("Hello::Wór") == :none
      assert CF.cursor_context("ÓlaMundo") == :none
      assert CF.cursor_context("HelloWór") == :none
      assert CF.cursor_context("@Hello") == :none
      assert CF.cursor_context("Hello(") == :none
      assert CF.cursor_context("Hello ") == :none
      assert CF.cursor_context("hello.World") == {:alias, {:local_or_var, ~c"hello"}, ~c"World"}

      # Identifier
      assert CF.cursor_context("foo@bar") == :none
      assert CF.cursor_context("@foo@bar") == :none
    end

    test "newlines" do
      assert CF.cursor_context("this+does-not*matter\nHello.") ==
               {:dot, {:alias, ~c"Hello"}, ~c""}

      assert CF.cursor_context(~c"this+does-not*matter\nHello.") ==
               {:dot, {:alias, ~c"Hello"}, ~c""}

      assert CF.cursor_context("this+does-not*matter\r\nHello.") ==
               {:dot, {:alias, ~c"Hello"}, ~c""}

      assert CF.cursor_context(~c"this+does-not*matter\r\nHello.") ==
               {:dot, {:alias, ~c"Hello"}, ~c""}
    end
  end

  describe "surround_context/2" do
    test "newlines" do
      for i <- 1..8 do
        assert CF.surround_context("\n\nhello_wo\n", {3, i}) == %{
                 context: {:local_or_var, ~c"hello_wo"},
                 begin: {3, 1},
                 end: {3, 9}
               }

        assert CF.surround_context("\r\n\r\nhello_wo\r\n", {3, i}) == %{
                 context: {:local_or_var, ~c"hello_wo"},
                 begin: {3, 1},
                 end: {3, 9}
               }

        assert CF.surround_context(~c"\r\n\r\nhello_wo\r\n", {3, i}) == %{
                 context: {:local_or_var, ~c"hello_wo"},
                 begin: {3, 1},
                 end: {3, 9}
               }
      end
    end

    test "column out of range" do
      assert CF.surround_context("hello", {1, 20}) == :none
    end

    test "local_or_var" do
      for i <- 1..8 do
        assert CF.surround_context("hello_wo", {1, i}) == %{
                 context: {:local_or_var, ~c"hello_wo"},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo", {1, 9}) == :none

      for i <- 2..9 do
        assert CF.surround_context(" hello_wo", {1, i}) == %{
                 context: {:local_or_var, ~c"hello_wo"},
                 begin: {1, 2},
                 end: {1, 10}
               }
      end

      assert CF.surround_context(" hello_wo", {1, 10}) == :none

      for i <- 1..6 do
        assert CF.surround_context("hello!", {1, i}) == %{
                 context: {:local_or_var, ~c"hello!"},
                 begin: {1, 1},
                 end: {1, 7}
               }
      end

      assert CF.surround_context("hello!", {1, 7}) == :none

      for i <- 1..5 do
        assert CF.surround_context("안녕_세상", {1, i}) == %{
                 context: {:local_or_var, ~c"안녕_세상"},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      assert CF.surround_context("안녕_세상", {1, 6}) == :none

      # Keywords are not local or var
      for keyword <- ~w(do end after catch else rescue fn true false nil)c do
        keyword_length = length(keyword) + 1

        assert %{
                 context: {:keyword, ^keyword},
                 begin: {1, 1},
                 end: {1, ^keyword_length}
               } = CF.surround_context(keyword, {1, 1})
      end
    end

    test "local call" do
      for i <- 1..8 do
        assert CF.surround_context("hello_wo(", {1, i}) == %{
                 context: {:local_call, ~c"hello_wo"},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo(", {1, 9}) == :none

      for i <- 1..8 do
        assert CF.surround_context("hello_wo (", {1, i}) == %{
                 context: {:local_call, ~c"hello_wo"},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo (", {1, 9}) == :none

      for i <- 1..6 do
        assert CF.surround_context("hello!(", {1, i}) == %{
                 context: {:local_call, ~c"hello!"},
                 begin: {1, 1},
                 end: {1, 7}
               }
      end

      assert CF.surround_context("hello!(", {1, 7}) == :none

      for i <- 1..5 do
        assert CF.surround_context("안녕_세상(", {1, i}) == %{
                 context: {:local_call, ~c"안녕_세상"},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      assert CF.surround_context("안녕_세상(", {1, 6}) == :none
    end

    test "local arity" do
      for i <- 1..8 do
        assert CF.surround_context("hello_wo/", {1, i}) == %{
                 context: {:local_arity, ~c"hello_wo"},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo/", {1, 9}) == :none

      for i <- 1..8 do
        assert CF.surround_context("hello_wo /", {1, i}) == %{
                 context: {:local_arity, ~c"hello_wo"},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo /", {1, 9}) == :none

      for i <- 1..6 do
        assert CF.surround_context("hello!/", {1, i}) == %{
                 context: {:local_arity, ~c"hello!"},
                 begin: {1, 1},
                 end: {1, 7}
               }
      end

      assert CF.surround_context("hello!/", {1, 7}) == :none

      for i <- 1..5 do
        assert CF.surround_context("안녕_세상/", {1, i}) == %{
                 context: {:local_arity, ~c"안녕_세상"},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      assert CF.surround_context("안녕_세상/", {1, 6}) == :none
    end

    test "textual operators" do
      for op <- ~w(when not or and in), i <- 1..byte_size(op) do
        assert CF.surround_context("#{op}", {1, i}) == %{
                 context: {:operator, String.to_charlist(op)},
                 begin: {1, 1},
                 end: {1, byte_size(op) + 1}
               }
      end
    end

    test "dot" do
      for i <- 1..5 do
        assert CF.surround_context("Hello.wor", {1, i}) == %{
                 context: {:alias, ~c"Hello"},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      for i <- 6..9 do
        assert CF.surround_context("Hello.wor", {1, i}) == %{
                 context: {:dot, {:alias, ~c"Hello"}, ~c"wor"},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      assert CF.surround_context("Hello.", {1, 6}) == :none

      for i <- 1..5 do
        assert CF.surround_context("Hello . wor", {1, i}) == %{
                 context: {:alias, ~c"Hello"},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      for i <- 6..11 do
        assert CF.surround_context("Hello . wor", {1, i}) == %{
                 context: {:dot, {:alias, ~c"Hello"}, ~c"wor"},
                 begin: {1, 1},
                 end: {1, 12}
               }
      end

      assert CF.surround_context("Hello .", {1, 6}) == :none

      for i <- 1..5 do
        assert CF.surround_context("hello.wor", {1, i}) == %{
                 context: {:local_or_var, ~c"hello"},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      for i <- 6..9 do
        assert CF.surround_context("hello.wor", {1, i}) == %{
                 context: {:dot, {:var, ~c"hello"}, ~c"wor"},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      assert CF.surround_context("hello # comment\n  .wor", {2, 4}) == %{
               context: {:dot, {:var, ~c"hello"}, ~c"wor"},
               begin: {1, 1},
               end: {2, 7}
             }

      assert CF.surround_context("123 + hello. # comment\n\n  wor", {3, 4}) == %{
               context: {:dot, {:var, ~c"hello"}, ~c"wor"},
               begin: {1, 7},
               end: {3, 6}
             }

      assert CF.surround_context("hello. # comment\n\n # wor", {3, 5}) == %{
               context: {:local_or_var, ~c"wor"},
               begin: {3, 4},
               end: {3, 7}
             }
    end

    test "alias" do
      for i <- 1..8 do
        assert CF.surround_context("HelloWor", {1, i}) == %{
                 context: {:alias, ~c"HelloWor"},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("HelloWor", {1, 9}) == :none

      for i <- 2..9 do
        assert CF.surround_context(" HelloWor", {1, i}) == %{
                 context: {:alias, ~c"HelloWor"},
                 begin: {1, 2},
                 end: {1, 10}
               }
      end

      assert CF.surround_context(" HelloWor", {1, 10}) == :none

      for i <- 1..9 do
        assert CF.surround_context("Hello.Wor", {1, i}) == %{
                 context: {:alias, ~c"Hello.Wor"},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      assert CF.surround_context("Hello.Wor", {1, 10}) == :none

      for i <- 1..11 do
        assert CF.surround_context("Hello . Wor", {1, i}) == %{
                 context: {:alias, ~c"Hello.Wor"},
                 begin: {1, 1},
                 end: {1, 12}
               }
      end

      assert CF.surround_context("Hello . Wor", {1, 12}) == :none

      for i <- 1..15 do
        assert CF.surround_context("Foo . Bar . Baz", {1, i}) == %{
                 context: {:alias, ~c"Foo.Bar.Baz"},
                 begin: {1, 1},
                 end: {1, 16}
               }
      end

      for i <- 1..3 do
        assert CF.surround_context("Foo # dc\n. Bar .\n Baz", {i, 1}) == %{
                 context: {:alias, ~c"Foo.Bar.Baz"},
                 begin: {1, 1},
                 end: {3, 5}
               }
      end

      for i <- 1..11 do
        assert CF.surround_context("Foo.Bar.Baz.foo(bar)", {1, i}) == %{
                 context: {:alias, ~c"Foo.Bar.Baz"},
                 begin: {1, 1},
                 end: {1, 12}
               }
      end
    end

    test "underscored special forms" do
      assert CF.surround_context("__MODULE__", {1, 1}) == %{
               context: {:local_or_var, ~c"__MODULE__"},
               begin: {1, 1},
               end: {1, 11}
             }

      for i <- 1..14 do
        assert CF.surround_context("__MODULE__.Foo", {1, i}) == %{
                 context: {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Foo"},
                 begin: {1, 1},
                 end: {1, 15}
               }
      end

      for i <- 1..18 do
        assert CF.surround_context("__MODULE__.Foo.Sub", {1, i}) == %{
                 context: {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Foo.Sub"},
                 begin: {1, 1},
                 end: {1, 19}
               }
      end

      assert CF.surround_context("%__MODULE__{}", {1, 5}) == %{
               context: {:struct, {:local_or_var, ~c"__MODULE__"}},
               begin: {1, 1},
               end: {1, 12}
             }

      assert CF.surround_context("%__MODULE__.Foo{}", {1, 13}) == %{
               context: {:struct, {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Foo"}},
               begin: {1, 1},
               end: {1, 16}
             }

      assert CF.surround_context("%__MODULE__.Foo.Sub{}", {1, 17}) == %{
               context: {:struct, {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Foo.Sub"}},
               begin: {1, 1},
               end: {1, 20}
             }

      assert CF.surround_context("__MODULE__.call()", {1, 13}) == %{
               context: {:dot, {:var, ~c"__MODULE__"}, ~c"call"},
               begin: {1, 1},
               end: {1, 16}
             }

      assert CF.surround_context("__MODULE__.Foo.call()", {1, 17}) == %{
               context: {:dot, {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Foo"}, ~c"call"},
               begin: {1, 1},
               end: {1, 20}
             }

      assert CF.surround_context("__MODULE__.Foo.Sub.call()", {1, 21}) == %{
               context: {:dot, {:alias, {:local_or_var, ~c"__MODULE__"}, ~c"Foo.Sub"}, ~c"call"},
               begin: {1, 1},
               end: {1, 24}
             }

      assert CF.surround_context("__ENV__.module.call()", {1, 17}) == %{
               context: {:dot, {:dot, {:var, ~c"__ENV__"}, ~c"module"}, ~c"call"},
               begin: {1, 1},
               end: {1, 20}
             }
    end

    test "attribute submodules" do
      for i <- 1..9 do
        assert CF.surround_context("@some.Foo", {1, i}) == %{
                 context: {:alias, {:module_attribute, ~c"some"}, ~c"Foo"},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      for i <- 1..13 do
        assert CF.surround_context("@some.Foo.Sub", {1, i}) == %{
                 context: {:alias, {:module_attribute, ~c"some"}, ~c"Foo.Sub"},
                 begin: {1, 1},
                 end: {1, 14}
               }
      end

      assert CF.surround_context("%@some{}", {1, 5}) == %{
               context: {:struct, {:module_attribute, ~c"some"}},
               begin: {1, 1},
               end: {1, 7}
             }

      assert CF.surround_context("%@some.Foo{}", {1, 10}) == %{
               context: {:struct, {:alias, {:module_attribute, ~c"some"}, ~c"Foo"}},
               begin: {1, 1},
               end: {1, 11}
             }

      assert CF.surround_context("%@some.Foo.Sub{}", {1, 14}) == %{
               context: {:struct, {:alias, {:module_attribute, ~c"some"}, ~c"Foo.Sub"}},
               begin: {1, 1},
               end: {1, 15}
             }

      assert CF.surround_context("@some.call()", {1, 8}) == %{
               context: {:dot, {:module_attribute, ~c"some"}, ~c"call"},
               begin: {1, 1},
               end: {1, 11}
             }

      assert CF.surround_context("@some.Foo.call()", {1, 12}) == %{
               context: {:dot, {:alias, {:module_attribute, ~c"some"}, ~c"Foo"}, ~c"call"},
               begin: {1, 1},
               end: {1, 15}
             }

      assert CF.surround_context("@some.Foo.Sub.call()", {1, 16}) == %{
               context: {:dot, {:alias, {:module_attribute, ~c"some"}, ~c"Foo.Sub"}, ~c"call"},
               begin: {1, 1},
               end: {1, 19}
             }
    end

    test "struct" do
      assert CF.surround_context("%", {1, 1}) == :none
      assert CF.surround_context("::%", {1, 1}) == :none
      assert CF.surround_context("::%", {1, 2}) == :none
      assert CF.surround_context("::%Hello", {1, 1}) == :none
      assert CF.surround_context("::%Hello", {1, 2}) == :none

      assert CF.surround_context("::%Hello", {1, 3}) == %{
               context: {:struct, ~c"Hello"},
               begin: {1, 3},
               end: {1, 9}
             }

      assert CF.surround_context("::% Hello", {1, 3}) == %{
               context: {:struct, ~c"Hello"},
               begin: {1, 3},
               end: {1, 10}
             }

      assert CF.surround_context("::% Hello", {1, 4}) == %{
               context: {:struct, ~c"Hello"},
               begin: {1, 3},
               end: {1, 10}
             }

      # Alias
      assert CF.surround_context("%HelloWor", {1, 1}) == %{
               context: {:struct, ~c"HelloWor"},
               begin: {1, 1},
               end: {1, 10}
             }

      assert CF.surround_context("%HelloWor.some", {1, 12}) == %{
               context: {:struct, {:dot, {:alias, ~c"HelloWor"}, ~c"some"}},
               begin: {1, 1},
               end: {1, 15}
             }

      for i <- 2..9 do
        assert CF.surround_context("%HelloWor", {1, i}) == %{
                 context: {:struct, ~c"HelloWor"},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      assert CF.surround_context("%HelloWor", {1, 10}) == :none

      # With dot
      assert CF.surround_context("%Hello.Wor", {1, 1}) == %{
               context: {:struct, ~c"Hello.Wor"},
               begin: {1, 1},
               end: {1, 11}
             }

      for i <- 2..10 do
        assert CF.surround_context("%Hello.Wor", {1, i}) == %{
                 context: {:struct, ~c"Hello.Wor"},
                 begin: {1, 1},
                 end: {1, 11}
               }
      end

      assert CF.surround_context("%Hello.Wor", {1, 11}) == :none

      # With spaces
      assert CF.surround_context("% Hello . Wor", {1, 1}) == %{
               context: {:struct, ~c"Hello.Wor"},
               begin: {1, 1},
               end: {1, 14}
             }

      for i <- 2..13 do
        assert CF.surround_context("% Hello . Wor", {1, i}) == %{
                 context: {:struct, ~c"Hello.Wor"},
                 begin: {1, 1},
                 end: {1, 14}
               }
      end

      assert CF.surround_context("% Hello . Wor", {1, 14}) == :none
    end

    test "module attributes" do
      for i <- 1..10 do
        assert CF.surround_context("@hello_wor", {1, i}) == %{
                 context: {:module_attribute, ~c"hello_wor"},
                 begin: {1, 1},
                 end: {1, 11}
               }
      end

      assert CF.surround_context("@Hello", {1, 1}) == :none
    end

    test "operators" do
      for i <- 2..4 do
        assert CF.surround_context("1<<<3", {1, i}) == %{
                 context: {:operator, ~c"<<<"},
                 begin: {1, 2},
                 end: {1, 5}
               }
      end

      for i <- 3..5 do
        assert CF.surround_context("1 <<< 3", {1, i}) == %{
                 context: {:operator, ~c"<<<"},
                 begin: {1, 3},
                 end: {1, 6}
               }
      end

      for i <- 2..3 do
        assert CF.surround_context("1::3", {1, i}) == %{
                 context: {:operator, ~c"::"},
                 begin: {1, 2},
                 end: {1, 4}
               }
      end

      for i <- 3..4 do
        assert CF.surround_context("1 :: 3", {1, i}) == %{
                 context: {:operator, ~c"::"},
                 begin: {1, 3},
                 end: {1, 5}
               }
      end

      for i <- 2..3 do
        assert CF.surround_context("x..y", {1, i}) == %{
                 context: {:operator, ~c".."},
                 begin: {1, 2},
                 end: {1, 4}
               }
      end

      for i <- 3..4 do
        assert CF.surround_context("x .. y", {1, i}) == %{
                 context: {:operator, ~c".."},
                 begin: {1, 3},
                 end: {1, 5}
               }
      end

      assert CF.surround_context("@", {1, 1}) == %{
               context: {:operator, ~c"@"},
               begin: {1, 1},
               end: {1, 2}
             }

      assert CF.surround_context("!", {1, 1}) == %{
               context: {:operator, ~c"!"},
               begin: {1, 1},
               end: {1, 2}
             }

      assert CF.surround_context("!foo", {1, 1}) == %{
               context: {:operator, ~c"!"},
               begin: {1, 1},
               end: {1, 2}
             }

      assert CF.surround_context("foo !bar", {1, 5}) == %{
               context: {:operator, ~c"!"},
               begin: {1, 5},
               end: {1, 6}
             }

      # invalid
      assert CF.surround_context("->", {1, 2}) == :none
    end

    test "sigil" do
      assert CF.surround_context("~", {1, 1}) == :none
      assert CF.surround_context("~~r", {1, 1}) == :none
      assert CF.surround_context("~~r", {1, 2}) == :none

      assert CF.surround_context("~r/foo/", {1, 1}) == %{
               begin: {1, 1},
               context: {:sigil, ~c"r"},
               end: {1, 3}
             }

      assert CF.surround_context("~r/foo/", {1, 2}) == %{
               begin: {1, 1},
               context: {:sigil, ~c"r"},
               end: {1, 3}
             }

      assert CF.surround_context("~r/foo/", {1, 3}) == :none

      assert CF.surround_context("~R<foo>", {1, 1}) == %{
               begin: {1, 1},
               context: {:sigil, ~c"R"},
               end: {1, 3}
             }

      assert CF.surround_context("~R<foo>", {1, 2}) == %{
               begin: {1, 1},
               context: {:sigil, ~c"R"},
               end: {1, 3}
             }

      assert CF.surround_context("~R<foo>", {1, 3}) == :none
    end

    test "dot operator" do
      for i <- 4..7 do
        assert CF.surround_context("Foo.<<<", {1, i}) == %{
                 context: {:dot, {:alias, ~c"Foo"}, ~c"<<<"},
                 begin: {1, 1},
                 end: {1, 8}
               }
      end

      for i <- 4..9 do
        assert CF.surround_context("Foo . <<<", {1, i}) == %{
                 context: {:dot, {:alias, ~c"Foo"}, ~c"<<<"},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      for i <- 4..6 do
        assert CF.surround_context("Foo.::", {1, i}) == %{
                 context: {:dot, {:alias, ~c"Foo"}, ~c"::"},
                 begin: {1, 1},
                 end: {1, 7}
               }
      end

      for i <- 4..8 do
        assert CF.surround_context("Foo . ::", {1, i}) == %{
                 context: {:dot, {:alias, ~c"Foo"}, ~c"::"},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end
    end

    test "capture operator" do
      assert CF.surround_context("& &123 + 1", {1, 1}) == %{
               context: {:operator, ~c"&"},
               begin: {1, 1},
               end: {1, 2}
             }

      for i <- 3..6 do
        assert CF.surround_context("& &123 + 1", {1, i}) == %{
                 context: {:capture_arg, ~c"&123"},
                 begin: {1, 3},
                 end: {1, 7}
               }
      end
    end

    test "capture operator false positive" do
      assert CF.surround_context("1&&2", {1, 3}) == %{
               context: {:operator, ~c"&&"},
               begin: {1, 2},
               end: {1, 4}
             }

      assert CF.surround_context("1&&2", {1, 4}) == :none

      assert CF.surround_context("&a", {1, 2}) == %{
               context: {:local_or_var, ~c"a"},
               begin: {1, 2},
               end: {1, 3}
             }
    end

    test "unquoted atom" do
      for i <- 1..10 do
        assert CF.surround_context(":hello_wor", {1, i}) == %{
                 context: {:unquoted_atom, ~c"hello_wor"},
                 begin: {1, 1},
                 end: {1, 11}
               }
      end

      for i <- 1..10 do
        assert CF.surround_context(":Hello@Wor", {1, i}) == %{
                 context: {:unquoted_atom, ~c"Hello@Wor"},
                 begin: {1, 1},
                 end: {1, 11}
               }
      end

      assert CF.surround_context(":", {1, 1}) == :none
    end

    test "keyword keys" do
      for i <- 2..4 do
        assert CF.surround_context("[foo:", {1, i}) == %{
                 context: {:key, ~c"foo"},
                 begin: {1, 2},
                 end: {1, 5}
               }
      end

      for i <- 10..12 do
        assert CF.surround_context("[foo: 1, bar: 2]", {1, i}) == %{
                 context: {:key, ~c"bar"},
                 begin: {1, 10},
                 end: {1, 13}
               }
      end

      assert CF.surround_context("if foo?, do: bar()", {1, 10}) == %{
               context: {:key, ~c"do"},
               begin: {1, 10},
               end: {1, 12}
             }
    end

    test "keyword false positives" do
      assert CF.surround_context("<<foo::", {1, 3}) == %{
               context: {:local_or_var, ~c"foo"},
               begin: {1, 3},
               end: {1, 6}
             }

      assert CF.surround_context("[foo  :atom", {1, 2}) == %{
               context: {:local_or_var, ~c"foo"},
               begin: {1, 2},
               end: {1, 5}
             }
    end
  end

  describe "container_cursor_to_quoted/2" do
    def s2q!(arg, opts \\ []), do: Code.string_to_quoted!(arg, opts)

    def cc2q!(arg, opts \\ []) do
      {:ok, res} = CF.container_cursor_to_quoted(arg, opts)
      res
    end

    test "completes terminators" do
      assert cc2q!("(") == s2q!("(__cursor__())")
      assert cc2q!("[") == s2q!("[__cursor__()]")
      assert cc2q!("{") == s2q!("{__cursor__()}")
      assert cc2q!("<<") == s2q!("<<__cursor__()>>")
      assert cc2q!("foo do") == s2q!("foo do __cursor__() end")
      assert cc2q!("foo do true else") == s2q!("foo do true else __cursor__() end")
    end

    test "inside interpolation" do
      assert cc2q!(~S|"foo #{(|) == s2q!(~S|"foo #{(__cursor__())}"|)
      assert cc2q!(~S|"foo #{"bar #{{|) == s2q!(~S|"foo #{"bar #{{__cursor__()}}"}"|)
    end

    test "keeps operators" do
      assert cc2q!("1 + 2") == s2q!("1 + __cursor__()")
      assert cc2q!("&foo") == s2q!("&__cursor__()")
      assert cc2q!("&foo/") == s2q!("&foo/__cursor__()")
    end

    test "keeps function calls without parens" do
      assert cc2q!("alias") == s2q!("__cursor__()")
      assert cc2q!("alias ") == s2q!("alias __cursor__()")
      assert cc2q!("alias foo") == s2q!("alias __cursor__()")
      assert cc2q!("alias Foo.Bar") == s2q!("alias __cursor__()")
      assert cc2q!("alias Foo.Bar,") == s2q!("alias Foo.Bar, __cursor__()")
      assert cc2q!("alias Foo.Bar, as: ") == s2q!("alias Foo.Bar, as: __cursor__()")
    end

    test "do-end blocks" do
      assert cc2q!("foo do baz") == s2q!("foo do __cursor__() end")
      assert cc2q!("foo do bar; baz") == s2q!("foo do bar; __cursor__() end")
      assert cc2q!("foo do bar\nbaz") == s2q!("foo do bar\n__cursor__() end")

      assert cc2q!("foo(bar do baz") == s2q!("foo(bar do __cursor__() end)")
      assert cc2q!("foo(bar do baz ") == s2q!("foo(bar do baz(__cursor__()) end)")
      assert cc2q!("foo(bar do baz(") == s2q!("foo(bar do baz(__cursor__()) end)")
      assert cc2q!("foo(bar do baz bat,") == s2q!("foo(bar do baz(bat, __cursor__()) end)")
      assert cc2q!("foo(bar do baz, bat") == s2q!("foo(bar do baz, __cursor__() -> nil end)")
    end

    test "keyword lists" do
      assert cc2q!("[bar: ") == s2q!("[bar: __cursor__()]")
      assert cc2q!("[bar: baz,") == s2q!("[bar: baz, __cursor__()]")
      assert cc2q!("[arg, bar: baz,") == s2q!("[arg, bar: baz, __cursor__()]")
      assert cc2q!("[arg: val, bar: baz,") == s2q!("[arg: val, bar: baz, __cursor__()]")

      assert cc2q!("{arg, bar: ") == s2q!("{arg, bar: __cursor__()}")
      assert cc2q!("{arg, bar: baz,") == s2q!("{arg, bar: baz, __cursor__()}")

      assert cc2q!("foo(bar: ") == s2q!("foo(bar: __cursor__())")
      assert cc2q!("foo(bar: baz,") == s2q!("foo([bar: baz, __cursor__()])")
      assert cc2q!("foo(arg, bar: ") == s2q!("foo(arg, bar: __cursor__())")
      assert cc2q!("foo(arg, bar: baz,") == s2q!("foo(arg, [bar: baz, __cursor__()])")
      assert cc2q!("foo(arg: val, bar: ") == s2q!("foo(arg: val, bar: __cursor__())")
      assert cc2q!("foo(arg: val, bar: baz,") == s2q!("foo([arg: val, bar: baz, __cursor__()])")

      assert cc2q!("foo bar: ") == s2q!("foo(bar: __cursor__())")
      assert cc2q!("foo bar: baz,") == s2q!("foo([bar: baz, __cursor__()])")
      assert cc2q!("foo arg, bar: ") == s2q!("foo(arg, bar: __cursor__())")
      assert cc2q!("foo arg, bar: baz,") == s2q!("foo(arg, [bar: baz, __cursor__()])")
      assert cc2q!("foo arg: val, bar: ") == s2q!("foo(arg: val, bar: __cursor__())")
      assert cc2q!("foo arg: val, bar: baz,") == s2q!("foo([arg: val, bar: baz, __cursor__()])")
    end

    test "maps and structs" do
      assert cc2q!("%") == s2q!("__cursor__()")
      assert cc2q!("%{") == s2q!("%{__cursor__()}")
      assert cc2q!("%{bar:") == s2q!("%{__cursor__()}")
      assert cc2q!("%{bar: ") == s2q!("%{bar: __cursor__()}")
      assert cc2q!("%{bar: baz,") == s2q!("%{bar: baz, __cursor__()}")
      assert cc2q!("%{foo | ") == s2q!("%{foo | __cursor__()}")
      assert cc2q!("%{foo | bar:") == s2q!("%{foo | __cursor__()}")
      assert cc2q!("%{foo | bar: ") == s2q!("%{foo | bar: __cursor__()}")
      assert cc2q!("%{foo | bar: baz,") == s2q!("%{foo | bar: baz, __cursor__()}")

      assert cc2q!("%Foo") == s2q!("__cursor__()")
      assert cc2q!("%Foo{") == s2q!("%Foo{__cursor__()}")
      assert cc2q!("%Foo{bar: ") == s2q!("%Foo{bar: __cursor__()}")
      assert cc2q!("%Foo{bar: baz,") == s2q!("%Foo{bar: baz, __cursor__()}")
      assert cc2q!("%Foo{foo | ") == s2q!("%Foo{foo | __cursor__()}")
      assert cc2q!("%Foo{foo | bar:") == s2q!("%Foo{foo | __cursor__()}")
      assert cc2q!("%Foo{foo | bar: ") == s2q!("%Foo{foo | bar: __cursor__()}")
      assert cc2q!("%Foo{foo | bar: baz,") == s2q!("%Foo{foo | bar: baz, __cursor__()}")
    end

    test "binaries" do
      assert cc2q!("<<") == s2q!("<<__cursor__()>>")
      assert cc2q!("<<foo") == s2q!("<<__cursor__()>>")
      assert cc2q!("<<foo, bar") == s2q!("<<foo, __cursor__()>>")
      assert cc2q!("<<foo, bar::baz") == s2q!("<<foo, bar::__cursor__()>>")
    end

    test "removes tokens until opening" do
      assert cc2q!("(123") == s2q!("(__cursor__())")
      assert cc2q!("[foo") == s2q!("[__cursor__()]")
      assert cc2q!("{'foo'") == s2q!("{__cursor__()}")
      assert cc2q!("foo do :atom") == s2q!("foo do __cursor__() end")
      assert cc2q!("foo(:atom") == s2q!("foo(__cursor__())")
    end

    test "removes tokens until comma" do
      assert cc2q!("[bar, 123") == s2q!("[bar, __cursor__()]")
      assert cc2q!("{bar, 'foo'") == s2q!("{bar, __cursor__()}")
      assert cc2q!("<<bar, \"sample\"") == s2q!("<<bar, __cursor__()>>")
      assert cc2q!("foo(bar, :atom") == s2q!("foo(bar, __cursor__())")
      assert cc2q!("foo bar, :atom") == s2q!("foo(bar, __cursor__())")
    end

    test "removes anonymous functions" do
      assert cc2q!("(fn") == s2q!("(fn __cursor__() -> nil end)")
      assert cc2q!("(fn x") == s2q!("(fn __cursor__() -> nil end)")
      assert cc2q!("(fn x ->") == s2q!("(fn x -> __cursor__() end)")
      assert cc2q!("(fn x -> x") == s2q!("(fn x -> __cursor__() end)")
      assert cc2q!("(fn x, y -> x + y") == s2q!("(fn x, y -> x + __cursor__() end)")
      assert cc2q!("(fn x, y -> x + y end") == s2q!("(__cursor__())")
    end

    test "removes closed terminators" do
      assert cc2q!("foo([1, 2, 3]") == s2q!("foo(__cursor__())")
      assert cc2q!("foo({1, 2, 3}") == s2q!("foo(__cursor__())")
      assert cc2q!("foo((1, 2, 3)") == s2q!("foo(__cursor__())")
      assert cc2q!("foo(<<1, 2, 3>>") == s2q!("foo(__cursor__())")
      assert cc2q!("foo(bar do :done end") == s2q!("foo(__cursor__())")
    end

    test "incomplete expressions" do
      assert cc2q!("foo(123, :") == s2q!("foo(123, __cursor__())")
      assert cc2q!("foo(123, %") == s2q!("foo(123, __cursor__())")
      assert cc2q!("foo(123, 0x") == s2q!("foo(123, __cursor__())")
      assert cc2q!("foo(123, ~") == s2q!("foo(123, __cursor__())")
      assert cc2q!("foo(123, ~r") == s2q!("foo(123, __cursor__())")
      assert cc2q!("foo(123, ~r/") == s2q!("foo(123, __cursor__())")
    end

    test "no warnings" do
      assert cc2q!(~s"?\\ ") == s2q!("__cursor__()")
      assert cc2q!(~s"{fn -> end, ") == s2q!("{fn -> nil end, __cursor__()}")
    end

    test "options" do
      opts = [columns: true]
      assert cc2q!("foo(", opts) == s2q!("foo(__cursor__())", opts)
      assert cc2q!("foo(123,", opts) == s2q!("foo(123,__cursor__())", opts)

      opts = [token_metadata: true]
      assert cc2q!("foo(", opts) == s2q!("foo(__cursor__())", opts)
      assert cc2q!("foo(123,", opts) == s2q!("foo(123,__cursor__())", opts)

      opts = [literal_encoder: fn ast, _ -> {:ok, {:literal, ast}} end]
      assert cc2q!("foo(", opts) == s2q!("foo(__cursor__())", opts)
      assert cc2q!("foo(123,", opts) == s2q!("foo({:literal, 123},__cursor__())", [])
    end
  end
end
