defmodule ReleaseTest do
  use Application

  def start(_type, _args) do
    cookie = System.get_env("RELEASE_COOKIE")
    {:ok, [[sys_config]]} = :init.get_argument(:config)

    info = %{
      app_dir: Application.app_dir(:release_test),
      cookie_env: cookie,
      encoding: Application.get_env(:release_test, :encoding),
      mode: :code.get_mode(),
      node: node(),
      protocols_consolidated?: Protocol.consolidated?(Enumerable),
      release_name: System.get_env("RELEASE_NAME"),
      release_node: System.get_env("RELEASE_NODE"),
      release_root: System.get_env("RELEASE_ROOT"),
      release_vsn: System.get_env("RELEASE_VSN"),
      root_dir: :code.root_dir() |> to_string(),
      runtime_config: Application.fetch_env(:release_test, :runtime),
      static_config: Application.fetch_env(:release_test, :static),
      sys_config_env: System.get_env("RELEASE_SYS_CONFIG"),
      sys_config_init: to_string(sys_config)
    }

    path = Path.join(System.get_env("RELEASE_ROOT"), "RELEASE_BOOTED")
    File.write!(path, :erlang.term_to_binary(info))

    if System.get_env("RELEASE_NAME") =~ "permanent" do
      Supervisor.start_link([], strategy: :one_for_one)
    else
      System.halt(0)
    end
  end

  def hello_world do
    IO.puts("hello world")
  end
end
