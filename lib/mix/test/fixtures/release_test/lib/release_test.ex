defmodule ReleaseTest do
  use Application

  def start(_type, _args) do
    cookie = System.get_env("RELEASE_COOKIE")

    info = %{
      protocols_consolidated?: Protocol.consolidated?(Enumerable),
      app_dir: Application.app_dir(:release_test),
      release_root: System.get_env("RELEASE_ROOT"),
      release_name: System.get_env("RELEASE_NAME"),
      release_node: System.get_env("RELEASE_NODE"),
      release_vsn: System.get_env("RELEASE_VSN"),
      cookie_env: cookie,
      node: node(),
      root_dir: :code.root_dir() |> to_string(),
      static_config: Application.fetch_env!(:release_test, :static)
    }

    path = Path.join(System.get_env("RELEASE_ROOT"), "RELEASE_BOOTED")
    File.write!(path, inspect(info))

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
