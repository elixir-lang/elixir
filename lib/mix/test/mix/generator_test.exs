Code.require_file "../../test_helper.exs", __FILE__

defmodule Mix.GeneratorTest do
  use MixTest.Case

  import Mix.Generator

  embed_text :foo,     "foo"
  embed_text :self,    from_file("../generator_test.exs")
  embed_template :bar, "<%= @a + @b %>"

  test :embed_text do
    assert foo_text == "foo"
  end

  test :embed_template do
    assert bar_template(a: 1, b: 2) == "3"
  end

  test :from_file do
    assert self_text =~ "import Mix.Generator"
  end

  test :create_file do
    in_tmp "create_file", fn ->
      create_file "foo", "HELLO"
      assert File.read!("foo") == "HELLO"
      assert_received { :mix_shell, :info, ["* creating foo"] }
    end
  end

  test :create_with_conflict_returning_true do
    in_tmp "create_file", fn ->
      File.write! "foo", "HELLO"
      self <- { :mix_shell_input, :yes?, true }

      create_file "foo", "WORLD"
      assert File.read!("foo") == "WORLD"

      assert_received { :mix_shell, :yes?, ["foo already exists, overwrite?"] }
    end
  end

  test :create_with_conflict_returning_false do
    in_tmp "create_file", fn ->
      File.write! "foo", "HELLO"
      self <- { :mix_shell_input, :yes?, false }

      create_file "foo", "WORLD"
      assert File.read!("foo") == "HELLO"

      assert_received { :mix_shell, :yes?, ["foo already exists, overwrite?"] }
    end
  end
end
