defmodule Logger.Backend.FileTest do
  use Logger.Case
  require Logger

  setup do
    Logger.remove_backend(:console, [:flush])
    in_tmp fn -> Logger.add_backend(:file) end
    on_exit fn ->
      in_tmp fn ->
        Logger.configure_backend(:file,
          [format: nil, level: nil, metadata: []])
      end
      Logger.remove_backend(:file, [:flush])
      Logger.add_backend(:console)
    end
  end

  test "can configure format" do
    in_tmp fn ->
      :ok = Logger.configure_backend(:file, format: "$message [$level]")
      Logger.debug("hello")
      assert last_file_line =~ "hello [debug]"
    end
  end

  test "can configure metadata" do
    in_tmp fn ->
      :ok = Logger.configure_backend(:file, format: "$metadata$message", metadata: [:user_id])

      Logger.debug("hello")
      assert last_file_line =~ "hello"

      Logger.metadata(user_id: 11)
      Logger.metadata(user_id: 13)

      Logger.debug("hello")
      assert last_file_line =~ "user_id=13 hello"
    end
  end

  test "can configure level" do
    in_tmp fn ->
      :ok = Logger.configure_backend(:file, level: :info)

      Logger.debug("hello")
      assert last_file_line == ""
    end
  end
end
