Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.Tasks.Cheers do
end

defmodule Mix.UtilsTest do
  use MixTest.Case
  doctest Mix.Utils

  test :command_to_module do
    assert Mix.Utils.command_to_module("cheers", Mix.Tasks)   == {:module, Mix.Tasks.Cheers}
    assert Mix.Utils.command_to_module("unknown", Mix.Tasks) == {:error, :nofile}
  end

  test :module_name_to_command do
    assert Mix.Utils.module_name_to_command(Mix.Tasks.Foo, 2)       == "foo"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.Foo", 2)     == "foo"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.Foo.Bar", 2) == "foo.bar"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.FooBar.Bing", 2) == "foo_bar.bing"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.FooBar.BingBang", 2) == "foo_bar.bing_bang"
  end

  test :command_to_module_name do
    assert Mix.Utils.command_to_module_name("foo")     == "Foo"
    assert Mix.Utils.command_to_module_name("foo.bar") == "Foo.Bar"
    assert Mix.Utils.command_to_module_name("foo_bar.baz") == "FooBar.Baz"
    assert Mix.Utils.command_to_module_name("foo_bar.baz_bing") == "FooBar.BazBing"
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
    assert Mix.Utils.underscore("") == ""
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
    assert Mix.Utils.camelize("") == ""
  end

  test :extract_files do
    files = Mix.Utils.extract_files [Path.join(fixture_path, "archive")], "*.ex"
    assert length(files) == 1
    assert Path.basename(hd(files)) == "local.sample.ex"
  end

  test :extract_stale do
    time = {{2030, 1, 1}, {0, 0, 0}}
    assert Mix.Utils.extract_stale([__ENV__.file], [time]) == []

    time = {{2000, 1, 1}, {0, 0, 0}}
    assert Mix.Utils.extract_stale([__ENV__.file], [time]) == [__ENV__.file]

    assert Mix.Utils.extract_stale([__ENV__.file], [__ENV__.file]) == []
  end

  test :symlink_or_copy do
    in_fixture "archive", fn ->
      File.mkdir_p!("_build/archive")
      result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), Path.expand("_build/archive/ebin"))
      assert_ebin_symlinked_or_copied(result)
    end
  end

  test :symlink_or_copy_removes_previous_directories do
    in_fixture "archive", fn ->
      File.mkdir_p!("_build/archive/ebin")
      result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), Path.expand("_build/archive/ebin"))
      assert_ebin_symlinked_or_copied(result)
    end
  end

  test :symlink_or_copy_erases_wrong_symblinks do
    in_fixture "archive", fn ->
      File.mkdir_p!("_build/archive")
      Mix.Utils.symlink_or_copy(Path.expand("priv"), Path.expand("_build/archive/ebin"))
      result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), Path.expand("_build/archive/ebin"))
      assert_ebin_symlinked_or_copied(result)
    end
  end

  defp assert_ebin_symlinked_or_copied(result) do
    case result do
      {:ok, paths} -> assert Path.expand("_build/archive/ebin") in paths
      :ok -> assert :file.read_link("_build/archive/ebin") == {:ok, '../../ebin'}
      _ -> flunk "expected symlink_or_copy to return :ok or {:ok, list_of_paths}, got: #{inspect result}"
    end
  end
end
