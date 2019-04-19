Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.GeneratorTest do
  use MixTest.Case

  import Mix.Generator

  embed_text(:foo, "foo")
  embed_text(:self, from_file: __ENV__.file)
  embed_template(:bar, "<%= @a + @b %>")

  describe "embed_text/2" do
    test "with contents" do
      assert foo_text() == "foo"
    end

    test "from file" do
      assert self_text() =~ "import Mix.Generator"
    end
  end

  test "embed template" do
    assert bar_template(a: 1, b: 2) == "3"
  end

  describe "overwrite?/1" do
    test "without conflict" do
      in_tmp("ovewrite", fn ->
        assert overwrite?("foo")
        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning true" do
      in_tmp("ovewrite", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, true})

        assert overwrite?("foo")
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning false" do
      in_tmp("ovewrite", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, false})

        refute overwrite?("foo")
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end
  end

  describe "overwrite?/2" do
    test "without conflict" do
      in_tmp("ovewrite", fn ->
        assert overwrite?("foo", "HELLO")
        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with same contents" do
      in_tmp("ovewrite", fn ->
        File.write!("foo", "HELLO")
        refute overwrite?("foo", "HELLO")
        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning true" do
      in_tmp("ovewrite", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, true})

        assert overwrite?("foo", "WORLD")
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning false" do
      in_tmp("ovewrite", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, false})

        refute overwrite?("foo", "WORLD")
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end
  end

  describe "create_file/3" do
    test "creates file" do
      in_tmp("create_file", fn ->
        create_file("foo", "HELLO")
        assert File.read!("foo") == "HELLO"
        assert_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with quiet" do
      in_tmp("create_file", fn ->
        create_file("foo", "HELLO", quiet: true)
        assert File.read!("foo") == "HELLO"
        refute_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with force" do
      in_tmp("create_file", fn ->
        File.write!("foo", "HELLO")
        create_file("foo", "WORLD", force: true)
        assert File.read!("foo") == "WORLD"

        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
        assert_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with same contents" do
      in_tmp("create_file", fn ->
        File.write!("foo", "HELLO")
        create_file("foo", "HELLO")
        assert File.read!("foo") == "HELLO"
        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning true" do
      in_tmp("create_file", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, true})

        create_file("foo", "WORLD")
        assert File.read!("foo") == "WORLD"
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning false" do
      in_tmp("create_file", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, false})

        create_file("foo", "WORLD")
        assert File.read!("foo") == "HELLO"
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end
  end

  describe "copy_file/3" do
    test "copies file" do
      in_tmp("copy_file", fn ->
        copy_file(__ENV__.file, "foo")
        assert File.read!("foo") =~ ~s[describe "copy_file/3" do]
        assert_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with quiet" do
      in_tmp("copy_file", fn ->
        copy_file(__ENV__.file, "foo", quiet: true)
        assert File.read!("foo") =~ ~s[describe "copy_file/3" do]
        refute_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with force" do
      in_tmp("copy_file", fn ->
        File.write!("foo", "HELLO")
        copy_file(__ENV__.file, "foo", force: true)
        assert File.read!("foo") =~ ~s[describe "copy_file/3" do]

        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
        assert_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with same contents" do
      in_tmp("copy_file", fn ->
        copy_file(__ENV__.file, "foo")
        copy_file(__ENV__.file, "foo")
        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning true" do
      in_tmp("copy_file", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, true})

        copy_file(__ENV__.file, "foo")
        assert File.read!("foo") =~ ~s[describe "copy_file/3" do]
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning false" do
      in_tmp("copy_file", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, false})

        copy_file(__ENV__.file, "foo")
        assert File.read!("foo") == "HELLO"
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end
  end

  describe "copy_template/4" do
    test "copies template" do
      in_tmp("copy_template", fn ->
        copy_template(__ENV__.file, "foo", [a: 1, b: 2])
        assert File.read!("foo") =~ ~s[embed_template(:bar, "3")]
        assert_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with quiet" do
      in_tmp("copy_template", fn ->
        copy_template(__ENV__.file, "foo", [a: 1, b: 2], quiet: true)
        assert File.read!("foo") =~ ~s[embed_template(:bar, "3")]
        refute_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with force" do
      in_tmp("copy_template", fn ->
        File.write!("foo", "HELLO")
        copy_template(__ENV__.file, "foo", [a: 1, b: 2], force: true)
        assert File.read!("foo") =~ ~s[embed_template(:bar, "3")]

        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
        assert_received {:mix_shell, :info, ["* creating foo"]}
      end)
    end

    test "with same contents" do
      in_tmp("copy_template", fn ->
        copy_template(__ENV__.file, "foo", [a: 1, b: 2])
        copy_template(__ENV__.file, "foo", [a: 1, b: 2])
        refute_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning true" do
      in_tmp("copy_template", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, true})

        copy_template(__ENV__.file, "foo", [a: 1, b: 2])
        assert File.read!("foo") =~ ~s[embed_template(:bar, "3")]
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end

    test "with conflict returning false" do
      in_tmp("copy_template", fn ->
        File.write!("foo", "HELLO")
        send(self(), {:mix_shell_input, :yes?, false})

        copy_template(__ENV__.file, "foo", [a: 1, b: 2])
        assert File.read!("foo") == "HELLO"
        assert_received {:mix_shell, :yes?, ["foo already exists, overwrite?"]}
      end)
    end
  end
end
