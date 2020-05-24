Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Format.GenTest do
  use MixTest.Case

  @config_path ".formatter.exs"

  describe "mix format.gen" do
    test "already exists" do
      in_tmp("format.gen exists", fn ->
        Mix.Tasks.New.run(["hello_world"])

        File.cd!("hello_world", fn ->
          message = @config_path <> " already exists"

          assert_raise Mix.Error, message, fn -> Mix.Tasks.Format.Gen.run([]) end
        end)
      end)
    end

    test "in umbrella" do
      in_tmp("format.gen umbrella", fn ->
        Mix.Tasks.New.run(["hello_world", "--umbrella"])

        File.cd!("hello_world", fn ->
          File.rm!(@config_path)

          assert :ok == Mix.Tasks.Format.Gen.run([])

          assert_file(@config_path, fn contents ->
            assert contents == Mix.Tasks.New.formatter_umbrella_template()
          end)
        end)
      end)
    end

    test "not in umbrella" do
      in_tmp("format.gen not umbrella", fn ->
        Mix.Tasks.New.run(["hello_world"])

        File.cd!("hello_world", fn ->
          File.rm!(@config_path)

          assert :ok == Mix.Tasks.Format.Gen.run([])

          assert_file(@config_path, fn contents ->
            assert contents == Mix.Tasks.New.formatter_template()
          end)
        end)
      end)
    end
  end

  defp assert_file(file, match) do
    assert File.regular?(file), "Expected #{file} to exist, but does not"
    match.(File.read!(file))
  end
end
