defmodule Mix.SCM.Path do
  @behaviour Mix.SCM
  @moduledoc false

  @impl true
  def fetchable? do
    false
  end

  @impl true
  def format(opts) do
    opts[:path]
  end

  @impl true
  def format_lock(_opts) do
    nil
  end

  @impl true
  def accepts_options(app, opts) do
    cond do
      raw = opts[:path] ->
        Keyword.put(opts, :dest, Path.expand(raw))

      opts[:in_umbrella] ->
        if opts[:override] do
          Mix.shell().error("""
          warning: in-umbrella application #{inspect(app)} has the flag :override \
          set to true, but the flag has no effect when running from the umbrella root
          """)
        end

        if opts[:optional] do
          Mix.shell().error("""
          warning: in-umbrella application #{inspect(app)} has the flag :optional \
          set to true, but the flag is not supported for umbrella dependencies
          """)
        end

        path = "../#{app}"

        opts
        |> Keyword.put(:dest, Path.expand(path))
        |> Keyword.put_new(:path, path)
        |> Keyword.put_new(:env, Mix.env())

      true ->
        nil
    end
  end

  @impl true
  def checked_out?(opts) do
    File.dir?(opts[:dest])
  end

  @impl true
  def lock_status(_opts) do
    :ok
  end

  @impl true
  def equal?(opts1, opts2) do
    opts1[:dest] == opts2[:dest]
  end

  @impl true
  def managers(_opts) do
    []
  end

  @impl true
  def checkout(opts) do
    path = Path.relative_to_cwd(opts[:dest])
    Mix.raise("Cannot checkout path dependency, expected a dependency at #{path}")
  end

  @impl true
  def update(opts) do
    opts[:lock]
  end
end
