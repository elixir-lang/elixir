defrecord IEx.Config, binding: nil, cache: '', counter: 1, scope: nil, result: nil

defmodule IEx do
  @moduledoc """
  This module implements Interactive Elixir.

  The interactive elixir needs to be set as the
  proper `-user` when starting the Erlang VM and
  so can be done with the help of IEx.CLI.

  If possible, Elixir will start a tty (smart terminal)
  which makes all control commands available in tty
  available to the developer.

  In case `tty` is not available (for example, Windows),
  a dumb terminal version is started instead.
  """

  @doc """
  Registers a function to be invoked after IEx process is spawned.
  """
  def after_spawn(fun) when is_function(fun) do
    :application.set_env(:iex, :after_spawn, [fun|after_spawn])
  end

  @doc """
  Returns registered after spawn callbacks.
  """
  def after_spawn do
    { :ok, list } = :application.get_env(:iex, :after_spawn)
    list
  end

  @doc """
  Returns true if IEx was properly started.
  """
  def started? do
    match?({ :ok, true }, :application.get_env(:iex, :started))
  end

  @doc """
  Registers options used on inspect.
  """
  def inspect_opts(opts) when is_list(opts) do
    :application.set_env(:iex, :inspect_opts, Keyword.merge(inspect_opts, opts))
  end

  @doc """
  Returns currently registered inspect options.
  """
  def inspect_opts do
    { :ok, opts } = :application.get_env(:iex, :inspect_opts)
    opts
  end

  # This is a callback invoked by Erlang shell utilities
  # when someone press Ctrl+G and adds 's Elixir-IEx'.
  @doc false
  def start(config // [], callback // fn -> end) do
    spawn fn ->
      config =
        case config do
          IEx.Config[] -> config
          opts -> boot_config(opts)
        end

      case :init.notify_when_started(self()) do
        :started -> :ok
        _        -> :init.wait_until_started()
      end

      Process.flag(:trap_exit, true)

      start_iex()
      callback.()

      set_expand_fun()
      run_after_spawn()
      IEx.Server.start(config)
    end
  end

  ## Boot Helpers

  defp start_iex do
    :application.start(:elixir)
    :application.start(:iex)
  end

  defp boot_config(opts) do
    scope = :elixir.scope_for_eval(
      file: "iex",
      delegate_locals_to: IEx.Helpers
    )

    if opts[:inspect_opts] do
      IEx.inspect_opts(opts[:inspect_opts])
    end

    IEx.Config[
      binding: opts[:binding] || [],
      scope: scope
    ]
  end

  defp set_expand_fun do
    gl = Process.group_leader
    glnode = node gl

    if glnode != node do
      ensure_module_exists glnode, IEx.Remsh
      expand_fun = IEx.Remsh.expand node
    else
      expand_fun = IEx.Autocomplete.expand &1
    end

    :io.setopts gl, [expand_fun: expand_fun, binary: true]
  end

  defp ensure_module_exists(node, mod) do
    unless :rpc.call node, :code, :is_loaded, [mod] do
      { m, b, f } = :code.get_object_code mod
      { :module, _ } = :rpc.call node, :code, :load_binary, [m,f,b]
    end
  end

  defp run_after_spawn do
    lc fun inlist Enum.reverse(after_spawn), do: fun.()
  end
end
