Code.require_file "../../test_helper", __FILE__

defmodule Mix.UtilsTest do
  use MixTest.Case

  test :get_module do
    assert Mix.Utils.get_module("hello", Mix.Tasks)   == { :module, Mix.Tasks.Hello }
    assert Mix.Utils.get_module("unknown", Mix.Tasks) == { :error, :nofile }
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
end