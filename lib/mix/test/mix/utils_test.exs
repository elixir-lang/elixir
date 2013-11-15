Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.UtilsTest do
  use MixTest.Case

  test :command_to_module do
    assert Mix.Utils.command_to_module("hello", Mix.Tasks)   == { :module, Mix.Tasks.Hello }
    assert Mix.Utils.command_to_module("unknown", Mix.Tasks) == { :error, :nofile }
  end

  test :module_name_to_command do
    assert Mix.Utils.module_name_to_command(Mix.Tasks.Foo, 2)       == "foo"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.Foo", 2)     == "foo"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.Foo.Bar", 2) == "foo.bar"
  end

  test :command_to_module_name do
    assert Mix.Utils.command_to_module_name("foo")     == "Foo"
    assert Mix.Utils.command_to_module_name("foo.bar") == "Foo.Bar"
  end

  test :config_merge do
    old = [
      foo: "hello",
      bar: [1, 2],
      baz: [some: "option"],
      bat: "man"
    ]

    new = [
      foo: "bye",
      bar: [3, 4],
      baz: [yet: "another"]
    ]

    assert Keyword.equal? Mix.Utils.config_merge(old, new), [
      foo: "bye",
      bar: [1, 2, 3, 4],
      baz: [some: "option", yet: "another"],
      bat: "man"
    ]
  end

  test :underscore do
    assert Mix.Utils.underscore("foo") == "foo"
    assert Mix.Utils.underscore("foo_bar") == "foo_bar"
    assert Mix.Utils.underscore("Foo") == "foo"
    assert Mix.Utils.underscore("FooBar") == "foo_bar"
    assert Mix.Utils.underscore("FOOBar") == "foo_bar"
    assert Mix.Utils.underscore("FooBAR") == "foo_bar"
    assert Mix.Utils.underscore("FoBaZa") == "fo_ba_za"
    assert Mix.Utils.underscore("Foo.Bar") == "foo/bar"
    assert Mix.Utils.underscore(Foo.Bar) == "foo/bar"
  end

  test :camelize do
    assert Mix.Utils.camelize("Foo") == "Foo"
    assert Mix.Utils.camelize("FooBar") == "FooBar"
    assert Mix.Utils.camelize("foo") == "Foo"
    assert Mix.Utils.camelize("foo_bar") == "FooBar"
    assert Mix.Utils.camelize("foo_") == "Foo"
    assert Mix.Utils.camelize("_foo") == "Foo"
    assert Mix.Utils.camelize("foo__bar") == "FooBar"
    assert Mix.Utils.camelize("foo/bar") == "Foo.Bar"
  end

  test :extract_files do
    files = Mix.Utils.extract_files [Path.join(fixture_path, "archive")], "*.ex"
    assert length(files) == 1
    assert Path.basename(hd(files)) == "local.sample.ex"
  end

  test :extract_stale do
    time = { { 2030, 1, 1 }, { 0, 0, 0 } }
    assert Mix.Utils.extract_stale([{ "hello", time }], [__FILE__]) == [{ "hello", time }]

    time = { { 2000, 1, 1 }, { 0, 0, 0 } }
    assert Mix.Utils.extract_stale([{ "hello", time }], [__FILE__]) == []
  end

  test :symlink_or_copy do
    in_fixture "archive", fn ->
      File.mkdir_p!("_build/archive")
      assert Mix.Utils.symlink_or_copy(Path.expand("ebin"), "_build/archive/ebin") == :ok
      assert :file.read_link("_build/archive/ebin") == { :ok, Path.expand('ebin') }
    end
  end

  test :symlink_or_copy_removes_previous_directories do
    in_fixture "archive", fn ->
      File.mkdir_p!("_build/archive/ebin")
      assert Mix.Utils.symlink_or_copy(Path.expand("ebin"), "_build/archive/ebin") == :ok
      assert :file.read_link("_build/archive/ebin") == { :ok, Path.expand('ebin') }
    end
  end

  test :symlink_or_copy_erases_wrong_symblinks do
    in_fixture "archive", fn ->
      File.mkdir_p!("_build/archive")
      Mix.Utils.symlink_or_copy(Path.expand("priv"), "_build/archive/ebin")
      Mix.Utils.symlink_or_copy(Path.expand("ebin"), "_build/archive/ebin")
      assert :file.read_link("_build/archive/ebin") == { :ok, Path.expand('ebin') }
    end
  end
end
