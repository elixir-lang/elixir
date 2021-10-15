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
      assert CF.cursor_context('\n') == :expr
      assert CF.cursor_context("\n\n") == :expr
      assert CF.cursor_context('\n\n') == :expr
    end

    test "local_or_var" do
      assert CF.cursor_context("hello_wo") == {:local_or_var, 'hello_wo'}
      assert CF.cursor_context("hello_world?") == {:local_or_var, 'hello_world?'}
      assert CF.cursor_context("hello_world!") == {:local_or_var, 'hello_world!'}
      assert CF.cursor_context("hello/wor") == {:local_or_var, 'wor'}
      assert CF.cursor_context("hello..wor") == {:local_or_var, 'wor'}
      assert CF.cursor_context("hello::wor") == {:local_or_var, 'wor'}
      assert CF.cursor_context("[hello_wo") == {:local_or_var, 'hello_wo'}
      assert CF.cursor_context("'hello_wo") == {:local_or_var, 'hello_wo'}
      assert CF.cursor_context("hellò_wó") == {:local_or_var, 'hellò_wó'}
      assert CF.cursor_context("hello? world") == {:local_or_var, 'world'}
      assert CF.cursor_context("hello! world") == {:local_or_var, 'world'}
      assert CF.cursor_context("hello: world") == {:local_or_var, 'world'}
    end

    test "dot" do
      assert CF.cursor_context("hello.") == {:dot, {:var, 'hello'}, ''}
      assert CF.cursor_context(":hello.") == {:dot, {:unquoted_atom, 'hello'}, ''}
      assert CF.cursor_context("nested.map.") == {:dot, {:dot, {:var, 'nested'}, 'map'}, ''}

      assert CF.cursor_context("Hello.") == {:dot, {:alias, 'Hello'}, ''}
      assert CF.cursor_context("Hello.World.") == {:dot, {:alias, 'Hello.World'}, ''}
      assert CF.cursor_context("Hello.wor") == {:dot, {:alias, 'Hello'}, 'wor'}
      assert CF.cursor_context("hello.wor") == {:dot, {:var, 'hello'}, 'wor'}
      assert CF.cursor_context("Hello.++") == {:dot, {:alias, 'Hello'}, '++'}
      assert CF.cursor_context(":hello.wor") == {:dot, {:unquoted_atom, 'hello'}, 'wor'}
      assert CF.cursor_context(":hell@o.wor") == {:dot, {:unquoted_atom, 'hell@o'}, 'wor'}
      assert CF.cursor_context(":he@ll@o.wor") == {:dot, {:unquoted_atom, 'he@ll@o'}, 'wor'}
      assert CF.cursor_context(":hell@@o.wor") == {:dot, {:unquoted_atom, 'hell@@o'}, 'wor'}
      assert CF.cursor_context("@hello.wor") == {:dot, {:module_attribute, 'hello'}, 'wor'}

      assert CF.cursor_context("nested.map.wor") ==
               {:dot, {:dot, {:var, 'nested'}, 'map'}, 'wor'}
    end

    test "local_arity" do
      assert CF.cursor_context("hello/") == {:local_arity, 'hello'}
    end

    test "local_call" do
      assert CF.cursor_context("hello\s") == {:local_call, 'hello'}
      assert CF.cursor_context("hello\t") == {:local_call, 'hello'}
      assert CF.cursor_context("hello(") == {:local_call, 'hello'}
      assert CF.cursor_context("hello(\s") == {:local_call, 'hello'}
      assert CF.cursor_context("hello(\t") == {:local_call, 'hello'}
    end

    test "dot_arity" do
      assert CF.cursor_context("Foo.hello/") == {:dot_arity, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo.+/") == {:dot_arity, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("Foo . hello /") == {:dot_arity, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo . + /") == {:dot_arity, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("foo.hello/") == {:dot_arity, {:var, 'foo'}, 'hello'}
      assert CF.cursor_context(":foo.hello/") == {:dot_arity, {:unquoted_atom, 'foo'}, 'hello'}
      assert CF.cursor_context("@f.hello/") == {:dot_arity, {:module_attribute, 'f'}, 'hello'}
    end

    test "dot_call" do
      assert CF.cursor_context("Foo.hello\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo.hello\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo.hello(") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo.hello(\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo.hello(\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo . hello (") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo . hello (\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert CF.cursor_context("Foo . hello (\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}

      assert CF.cursor_context(":foo.hello\s") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert CF.cursor_context(":foo.hello\t") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert CF.cursor_context(":foo.hello(") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert CF.cursor_context(":foo.hello(\s") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert CF.cursor_context(":foo.hello(\t") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert CF.cursor_context(":foo.hello\s") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}

      assert CF.cursor_context("foo.hello\s") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert CF.cursor_context("foo.hello\t") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert CF.cursor_context("foo.hello(") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert CF.cursor_context("foo.hello(\s") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert CF.cursor_context("foo.hello(\t") == {:dot_call, {:var, 'foo'}, 'hello'}

      assert CF.cursor_context("@f.hello\s") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert CF.cursor_context("@f.hello\t") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert CF.cursor_context("@f.hello(") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert CF.cursor_context("@f.hello(\s") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert CF.cursor_context("@f.hello(\t") == {:dot_call, {:module_attribute, 'f'}, 'hello'}

      assert CF.cursor_context("Foo.+\s") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("Foo.+\t") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("Foo.+(") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("Foo.+(\s") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("Foo.+(\t") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("Foo . + (") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("Foo . + (\s") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert CF.cursor_context("Foo . + (\t") == {:dot_call, {:alias, 'Foo'}, '+'}
    end

    test "alias" do
      assert CF.cursor_context("HelloWor") == {:alias, 'HelloWor'}
      assert CF.cursor_context("Hello.Wor") == {:alias, 'Hello.Wor'}
      assert CF.cursor_context("Hello . Wor") == {:alias, 'Hello.Wor'}
      assert CF.cursor_context("Hello::Wor") == {:alias, 'Wor'}
      assert CF.cursor_context("Hello..Wor") == {:alias, 'Wor'}
    end

    test "structs" do
      assert CF.cursor_context("%") == {:struct, ''}
      assert CF.cursor_context(":%") == {:unquoted_atom, '%'}
      assert CF.cursor_context("::%") == {:struct, ''}

      assert CF.cursor_context("%HelloWor") == {:struct, 'HelloWor'}
      assert CF.cursor_context("%Hello.") == {:struct, 'Hello.'}
      assert CF.cursor_context("%Hello.Wor") == {:struct, 'Hello.Wor'}
      assert CF.cursor_context("% Hello . Wor") == {:struct, 'Hello.Wor'}
    end

    test "unquoted atom" do
      assert CF.cursor_context(":") == {:unquoted_atom, ''}
      assert CF.cursor_context(":HelloWor") == {:unquoted_atom, 'HelloWor'}
      assert CF.cursor_context(":HelloWór") == {:unquoted_atom, 'HelloWór'}
      assert CF.cursor_context(":hello_wor") == {:unquoted_atom, 'hello_wor'}
      assert CF.cursor_context(":Óla_mundo") == {:unquoted_atom, 'Óla_mundo'}
      assert CF.cursor_context(":Ol@_mundo") == {:unquoted_atom, 'Ol@_mundo'}
      assert CF.cursor_context(":Ol@") == {:unquoted_atom, 'Ol@'}
      assert CF.cursor_context("foo:hello_wor") == {:unquoted_atom, 'hello_wor'}

      # Operators from atoms
      assert CF.cursor_context(":+") == {:unquoted_atom, '+'}
      assert CF.cursor_context(":or") == {:unquoted_atom, 'or'}
      assert CF.cursor_context(":<") == {:unquoted_atom, '<'}
      assert CF.cursor_context(":.") == {:unquoted_atom, '.'}
      assert CF.cursor_context(":..") == {:unquoted_atom, '..'}
      assert CF.cursor_context(":->") == {:unquoted_atom, '->'}
      assert CF.cursor_context(":%") == {:unquoted_atom, '%'}
    end

    test "operators" do
      assert CF.cursor_context("+") == {:operator, '+'}
      assert CF.cursor_context("++") == {:operator, '++'}
      assert CF.cursor_context("!") == {:operator, '!'}
      assert CF.cursor_context("<") == {:operator, '<'}
      assert CF.cursor_context("<<<") == {:operator, '<<<'}
      assert CF.cursor_context("..") == {:operator, '..'}
      assert CF.cursor_context("<~") == {:operator, '<~'}
      assert CF.cursor_context("=~") == {:operator, '=~'}
      assert CF.cursor_context("<~>") == {:operator, '<~>'}
      assert CF.cursor_context("::") == {:operator, '::'}

      assert CF.cursor_context("+ ") == {:operator_call, '+'}
      assert CF.cursor_context("++ ") == {:operator_call, '++'}
      assert CF.cursor_context("! ") == {:operator_call, '!'}
      assert CF.cursor_context("< ") == {:operator_call, '<'}
      assert CF.cursor_context("<<< ") == {:operator_call, '<<<'}
      assert CF.cursor_context(".. ") == {:operator_call, '..'}
      assert CF.cursor_context("<~ ") == {:operator_call, '<~'}
      assert CF.cursor_context("=~ ") == {:operator_call, '=~'}
      assert CF.cursor_context("<~> ") == {:operator_call, '<~>'}
      assert CF.cursor_context(":: ") == {:operator_call, '::'}

      assert CF.cursor_context("+/") == {:operator_arity, '+'}
      assert CF.cursor_context("++/") == {:operator_arity, '++'}
      assert CF.cursor_context("!/") == {:operator_arity, '!'}
      assert CF.cursor_context("</") == {:operator_arity, '<'}
      assert CF.cursor_context("<<</") == {:operator_arity, '<<<'}
      assert CF.cursor_context("../") == {:operator_arity, '..'}
      assert CF.cursor_context("<~/") == {:operator_arity, '<~'}
      assert CF.cursor_context("=~/") == {:operator_arity, '=~'}
      assert CF.cursor_context("<~>/") == {:operator_arity, '<~>'}
      assert CF.cursor_context("::/") == {:operator_arity, '::'}

      # Unknown operators altogether
      assert CF.cursor_context("***") == :none

      # Textual operators are shown as local_or_var UNLESS there is space
      assert CF.cursor_context("when") == {:local_or_var, 'when'}
      assert CF.cursor_context("when ") == {:operator_call, 'when'}
      assert CF.cursor_context("when.") == :none

      assert CF.cursor_context("not") == {:local_or_var, 'not'}
      assert CF.cursor_context("not ") == {:operator_call, 'not'}
      assert CF.cursor_context("not.") == :none
    end

    test "incomplete operators" do
      assert CF.cursor_context("~~") == {:operator, '~~'}
      assert CF.cursor_context("~~ ") == :none
      assert CF.cursor_context("^^") == {:operator, '^^'}
      assert CF.cursor_context("^^ ") == :none

      assert CF.cursor_context("Foo.~") == {:dot, {:alias, 'Foo'}, '~'}
      assert CF.cursor_context("Foo . ~") == {:dot, {:alias, 'Foo'}, '~'}
      assert CF.cursor_context("Foo.~~") == {:dot, {:alias, 'Foo'}, '~~'}
      assert CF.cursor_context("Foo . ~~") == {:dot, {:alias, 'Foo'}, '~~'}
      assert CF.cursor_context("Foo.~ ") == :none
      assert CF.cursor_context("Foo.~~ ") == :none
      assert CF.cursor_context("Foo.^^") == {:dot, {:alias, 'Foo'}, '^^'}
      assert CF.cursor_context("Foo . ^^") == {:dot, {:alias, 'Foo'}, '^^'}
      assert CF.cursor_context("Foo.^^ ") == :none
    end

    test "sigil" do
      assert CF.cursor_context("~") == {:sigil, ''}
      assert CF.cursor_context("~ ") == :none

      assert CF.cursor_context("~r") == {:sigil, 'r'}
      assert CF.cursor_context("~r/") == :none
      assert CF.cursor_context("~r<") == :none

      assert CF.cursor_context("~R") == {:sigil, 'R'}
      assert CF.cursor_context("~R/") == :none
      assert CF.cursor_context("~R<") == :none
    end

    test "module attribute" do
      assert CF.cursor_context("@") == {:module_attribute, ''}
      assert CF.cursor_context("@hello_wo") == {:module_attribute, 'hello_wo'}
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
      assert CF.cursor_context("hello.World") == :none

      # Identifier
      assert CF.cursor_context("foo@bar") == :none
      assert CF.cursor_context("@foo@bar") == :none
    end

    test "newlines" do
      assert CF.cursor_context("this+does-not*matter\nHello.") == {:dot, {:alias, 'Hello'}, ''}
      assert CF.cursor_context('this+does-not*matter\nHello.') == {:dot, {:alias, 'Hello'}, ''}
    end
  end

  describe "surround_context/2" do
    test "newlines" do
      for i <- 1..8 do
        assert CF.surround_context("\n\nhello_wo\n", {3, i}) == %{
                 context: {:local_or_var, 'hello_wo'},
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
                 context: {:local_or_var, 'hello_wo'},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo", {1, 9}) == :none

      for i <- 2..9 do
        assert CF.surround_context(" hello_wo", {1, i}) == %{
                 context: {:local_or_var, 'hello_wo'},
                 begin: {1, 2},
                 end: {1, 10}
               }
      end

      assert CF.surround_context(" hello_wo", {1, 10}) == :none

      for i <- 1..6 do
        assert CF.surround_context("hello!", {1, i}) == %{
                 context: {:local_or_var, 'hello!'},
                 begin: {1, 1},
                 end: {1, 7}
               }
      end

      assert CF.surround_context("hello!", {1, 7}) == :none

      for i <- 1..5 do
        assert CF.surround_context("안녕_세상", {1, i}) == %{
                 context: {:local_or_var, '안녕_세상'},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      assert CF.surround_context("안녕_세상", {1, 6}) == :none

      # Keywords are not local or var
      for keyword <- ~w(do end after catch else rescue) do
        assert CF.surround_context(keyword, {1, 1}) == :none
      end
    end

    test "local call" do
      for i <- 1..8 do
        assert CF.surround_context("hello_wo(", {1, i}) == %{
                 context: {:local_call, 'hello_wo'},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo(", {1, 9}) == :none

      for i <- 1..8 do
        assert CF.surround_context("hello_wo (", {1, i}) == %{
                 context: {:local_call, 'hello_wo'},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo (", {1, 9}) == :none

      for i <- 1..6 do
        assert CF.surround_context("hello!(", {1, i}) == %{
                 context: {:local_call, 'hello!'},
                 begin: {1, 1},
                 end: {1, 7}
               }
      end

      assert CF.surround_context("hello!(", {1, 7}) == :none

      for i <- 1..5 do
        assert CF.surround_context("안녕_세상(", {1, i}) == %{
                 context: {:local_call, '안녕_세상'},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      assert CF.surround_context("안녕_세상(", {1, 6}) == :none
    end

    test "local arity" do
      for i <- 1..8 do
        assert CF.surround_context("hello_wo/", {1, i}) == %{
                 context: {:local_arity, 'hello_wo'},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo/", {1, 9}) == :none

      for i <- 1..8 do
        assert CF.surround_context("hello_wo /", {1, i}) == %{
                 context: {:local_arity, 'hello_wo'},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("hello_wo /", {1, 9}) == :none

      for i <- 1..6 do
        assert CF.surround_context("hello!/", {1, i}) == %{
                 context: {:local_arity, 'hello!'},
                 begin: {1, 1},
                 end: {1, 7}
               }
      end

      assert CF.surround_context("hello!/", {1, 7}) == :none

      for i <- 1..5 do
        assert CF.surround_context("안녕_세상/", {1, i}) == %{
                 context: {:local_arity, '안녕_세상'},
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
                 context: {:alias, 'Hello'},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      for i <- 6..9 do
        assert CF.surround_context("Hello.wor", {1, i}) == %{
                 context: {:dot, {:alias, 'Hello'}, 'wor'},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      assert CF.surround_context("Hello.", {1, 6}) == :none

      for i <- 1..5 do
        assert CF.surround_context("Hello . wor", {1, i}) == %{
                 context: {:alias, 'Hello'},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      for i <- 6..11 do
        assert CF.surround_context("Hello . wor", {1, i}) == %{
                 context: {:dot, {:alias, 'Hello'}, 'wor'},
                 begin: {1, 1},
                 end: {1, 12}
               }
      end

      assert CF.surround_context("Hello .", {1, 6}) == :none

      for i <- 1..5 do
        assert CF.surround_context("hello.wor", {1, i}) == %{
                 context: {:local_or_var, 'hello'},
                 begin: {1, 1},
                 end: {1, 6}
               }
      end

      for i <- 6..9 do
        assert CF.surround_context("hello.wor", {1, i}) == %{
                 context: {:dot, {:var, 'hello'}, 'wor'},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end
    end

    test "alias" do
      for i <- 1..8 do
        assert CF.surround_context("HelloWor", {1, i}) == %{
                 context: {:alias, 'HelloWor'},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end

      assert CF.surround_context("HelloWor", {1, 9}) == :none

      for i <- 2..9 do
        assert CF.surround_context(" HelloWor", {1, i}) == %{
                 context: {:alias, 'HelloWor'},
                 begin: {1, 2},
                 end: {1, 10}
               }
      end

      assert CF.surround_context(" HelloWor", {1, 10}) == :none

      for i <- 1..9 do
        assert CF.surround_context("Hello.Wor", {1, i}) == %{
                 context: {:alias, 'Hello.Wor'},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      assert CF.surround_context("Hello.Wor", {1, 10}) == :none

      for i <- 1..11 do
        assert CF.surround_context("Hello . Wor", {1, i}) == %{
                 context: {:alias, 'Hello.Wor'},
                 begin: {1, 1},
                 end: {1, 12}
               }
      end

      assert CF.surround_context("Hello . Wor", {1, 12}) == :none

      for i <- 1..15 do
        assert CF.surround_context("Foo . Bar . Baz", {1, i}) == %{
                 context: {:alias, 'Foo.Bar.Baz'},
                 begin: {1, 1},
                 end: {1, 16}
               }
      end
    end

    test "struct" do
      assert CF.surround_context("%", {1, 1}) == :none
      assert CF.surround_context("::%", {1, 1}) == :none
      assert CF.surround_context("::%", {1, 2}) == :none
      assert CF.surround_context("::%Hello", {1, 1}) == :none
      assert CF.surround_context("::%Hello", {1, 2}) == :none

      assert CF.surround_context("::%Hello", {1, 3}) == %{
               context: {:struct, 'Hello'},
               begin: {1, 3},
               end: {1, 9}
             }

      assert CF.surround_context("::% Hello", {1, 3}) == %{
               context: {:struct, 'Hello'},
               begin: {1, 3},
               end: {1, 10}
             }

      assert CF.surround_context("::% Hello", {1, 4}) == %{
               context: {:struct, 'Hello'},
               begin: {1, 3},
               end: {1, 10}
             }

      # Alias
      assert CF.surround_context("%HelloWor", {1, 1}) == %{
               context: {:struct, 'HelloWor'},
               begin: {1, 1},
               end: {1, 10}
             }

      for i <- 2..9 do
        assert CF.surround_context("%HelloWor", {1, i}) == %{
                 context: {:struct, 'HelloWor'},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      assert CF.surround_context("%HelloWor", {1, 10}) == :none

      # With dot
      assert CF.surround_context("%Hello.Wor", {1, 1}) == %{
               context: {:struct, 'Hello.Wor'},
               begin: {1, 1},
               end: {1, 11}
             }

      for i <- 2..10 do
        assert CF.surround_context("%Hello.Wor", {1, i}) == %{
                 context: {:struct, 'Hello.Wor'},
                 begin: {1, 1},
                 end: {1, 11}
               }
      end

      assert CF.surround_context("%Hello.Wor", {1, 11}) == :none

      # With spaces
      assert CF.surround_context("% Hello . Wor", {1, 1}) == %{
               context: {:struct, 'Hello.Wor'},
               begin: {1, 1},
               end: {1, 14}
             }

      for i <- 2..13 do
        assert CF.surround_context("% Hello . Wor", {1, i}) == %{
                 context: {:struct, 'Hello.Wor'},
                 begin: {1, 1},
                 end: {1, 14}
               }
      end

      assert CF.surround_context("% Hello . Wor", {1, 14}) == :none
    end

    test "module attributes" do
      for i <- 1..10 do
        assert CF.surround_context("@hello_wor", {1, i}) == %{
                 context: {:module_attribute, 'hello_wor'},
                 begin: {1, 1},
                 end: {1, 11}
               }
      end

      assert CF.surround_context("@Hello", {1, 1}) == :none
    end

    test "operators" do
      for i <- 2..4 do
        assert CF.surround_context("1<<<3", {1, i}) == %{
                 context: {:operator, '<<<'},
                 begin: {1, 2},
                 end: {1, 5}
               }
      end

      for i <- 3..5 do
        assert CF.surround_context("1 <<< 3", {1, i}) == %{
                 context: {:operator, '<<<'},
                 begin: {1, 3},
                 end: {1, 6}
               }
      end

      for i <- 2..3 do
        assert CF.surround_context("1::3", {1, i}) == %{
                 context: {:operator, '::'},
                 begin: {1, 2},
                 end: {1, 4}
               }
      end

      for i <- 3..4 do
        assert CF.surround_context("1 :: 3", {1, i}) == %{
                 context: {:operator, '::'},
                 begin: {1, 3},
                 end: {1, 5}
               }
      end

      for i <- 2..3 do
        assert CF.surround_context("x..y", {1, i}) == %{
                 context: {:operator, '..'},
                 begin: {1, 2},
                 end: {1, 4}
               }
      end

      for i <- 3..4 do
        assert CF.surround_context("x .. y", {1, i}) == %{
                 context: {:operator, '..'},
                 begin: {1, 3},
                 end: {1, 5}
               }
      end

      assert CF.surround_context("@", {1, 1}) == %{
               context: {:operator, '@'},
               begin: {1, 1},
               end: {1, 2}
             }

      assert CF.surround_context("!", {1, 1}) == %{
               context: {:operator, '!'},
               begin: {1, 1},
               end: {1, 2}
             }

      assert CF.surround_context("!foo", {1, 1}) == %{
               context: {:operator, '!'},
               begin: {1, 1},
               end: {1, 2}
             }

      assert CF.surround_context("foo !bar", {1, 5}) == %{
               context: {:operator, '!'},
               begin: {1, 5},
               end: {1, 6}
             }
    end

    test "sigil" do
      assert CF.surround_context("~", {1, 1}) == :none
      assert CF.surround_context("~~r", {1, 1}) == :none
      assert CF.surround_context("~~r", {1, 2}) == :none

      assert CF.surround_context("~~~", {1, 1}) == %{
               begin: {1, 1},
               context: {:operator, '~~~'},
               end: {1, 4}
             }

      assert CF.surround_context("~r/foo/", {1, 1}) == %{
               begin: {1, 1},
               context: {:sigil, 'r'},
               end: {1, 3}
             }

      assert CF.surround_context("~r/foo/", {1, 2}) == %{
               begin: {1, 1},
               context: {:sigil, 'r'},
               end: {1, 3}
             }

      assert CF.surround_context("~r/foo/", {1, 3}) == :none

      assert CF.surround_context("~R<foo>", {1, 1}) == %{
               begin: {1, 1},
               context: {:sigil, 'R'},
               end: {1, 3}
             }

      assert CF.surround_context("~R<foo>", {1, 2}) == %{
               begin: {1, 1},
               context: {:sigil, 'R'},
               end: {1, 3}
             }

      assert CF.surround_context("~R<foo>", {1, 3}) == :none
    end

    test "dot operator" do
      for i <- 4..7 do
        assert CF.surround_context("Foo.<<<", {1, i}) == %{
                 context: {:dot, {:alias, 'Foo'}, '<<<'},
                 begin: {1, 1},
                 end: {1, 8}
               }
      end

      for i <- 4..9 do
        assert CF.surround_context("Foo . <<<", {1, i}) == %{
                 context: {:dot, {:alias, 'Foo'}, '<<<'},
                 begin: {1, 1},
                 end: {1, 10}
               }
      end

      for i <- 4..6 do
        assert CF.surround_context("Foo.::", {1, i}) == %{
                 context: {:dot, {:alias, 'Foo'}, '::'},
                 begin: {1, 1},
                 end: {1, 7}
               }
      end

      for i <- 4..8 do
        assert CF.surround_context("Foo . ::", {1, i}) == %{
                 context: {:dot, {:alias, 'Foo'}, '::'},
                 begin: {1, 1},
                 end: {1, 9}
               }
      end
    end

    test "unquoted atom" do
      for i <- 1..10 do
        assert CF.surround_context(":hello_wor", {1, i}) == %{
                 context: {:unquoted_atom, 'hello_wor'},
                 begin: {1, 1},
                 end: {1, 11}
               }
      end

      for i <- 1..10 do
        assert CF.surround_context(":Hello@Wor", {1, i}) == %{
                 context: {:unquoted_atom, 'Hello@Wor'},
                 begin: {1, 1},
                 end: {1, 11}
               }
      end

      assert CF.surround_context(":", {1, 1}) == :none
    end
  end

  describe "argument_cursor_to_quoted/2" do
    # TODO: maps
    # TODO: &(.. must be ignored
    # TODO: completed terminators too
    test "completes terminators" do
      assert CF.argument_cursor_to_quoted("(") == Code.string_to_quoted("(__cursor__())")
      assert CF.argument_cursor_to_quoted("[") == Code.string_to_quoted("[__cursor__()]")
      assert CF.argument_cursor_to_quoted("{") == Code.string_to_quoted("{__cursor__()}")
      assert CF.argument_cursor_to_quoted("<<") == Code.string_to_quoted("<<__cursor__()>>")

      assert CF.argument_cursor_to_quoted("foo do") ==
               Code.string_to_quoted("foo do __cursor__() end")
    end

    test "inside interpolation" do
      assert CF.argument_cursor_to_quoted(~S|"foo #{(|) ==
               Code.string_to_quoted(~S|"foo #{(__cursor__())}"|)

      assert CF.argument_cursor_to_quoted(~S|"foo #{"bar #{{|) ==
               Code.string_to_quoted(~S|"foo #{"bar #{{__cursor__()}}"}"|)
    end
  end
end
