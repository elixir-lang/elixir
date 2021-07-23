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
      assert CF.cursor_context("Hello::Wor") == {:alias, 'Wor'}
      assert CF.cursor_context("Hello..Wor") == {:alias, 'Wor'}
      assert CF.cursor_context("%Hello.Wor") == {:alias, 'Hello.Wor'}
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
      assert CF.cursor_context("~") == {:operator, '~'}
      assert CF.cursor_context("~~") == {:operator, '~~'}
      assert CF.cursor_context("~ ") == :none
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
end
