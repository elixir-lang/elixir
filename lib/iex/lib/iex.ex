defrecord IEx.Config, binding: nil, cache: '', counter: 1, scope: nil, result: nil

defmodule IEx do
  @moduledoc """
  This module implements interactive Elixir. It provides a main
  function, `start` which will either delegate to `tty` or `simple`.
  The former is meant for systems where tty is available and relies
  on it in order to work properly. This makes all control commands
  available in tty available to the developer.

  In case `tty` is not available (for example, Windows), a
  developer may invoke `simple` which starts a stripped
  down version.
  """

  @doc """
  Simply loads IEx application. Most of the times it is
  preloaded on demand, but if you want to pre-configure
  IEx, you need to preload it.
  """
  def preload do
    :application.start(:iex)
    __MODULE__
  end

  @doc """
  Registers a function to be invoked after IEx
  process is spawned. Requires IEx.preload to
  be invoked.
  """
  def after_spawn(fun) when is_function(fun) do
    :application.set_env(:iex, :after_spawn, [fun|after_spawn])
  end

  @doc """
  Returns registered after spawn callbacks.
  """
  def after_spawn do
    case :application.get_env(:iex, :after_spawn) do
      { :ok, list } -> list
      :undefined -> []
    end
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
    case :application.get_env(:iex, :inspect_opts) do
      { :ok, list } -> list
      :undefined -> [limit: 50]
    end
  end

  @doc """
  Interface to start IEx from CLI.
  """
  def cli do
    run([remsh: get_remsh(:init.get_plain_arguments)])
  end

  defp get_remsh(['--remsh',h|_]), do: list_to_binary(h)
  defp get_remsh([_|t]), do: get_remsh(t)
  defp get_remsh([]),    do: nil

  @doc """
  Runs IEx checking if tty is available or not.
  If so, invoke tty, otherwise go with the simple iex.
  """
  def run(opts // []) when is_list(opts) do
    if tty_works? do
      tty(opts)
    else
      IO.puts "Warning: could not run smart terminal, falling back to dumb one"
      simple(opts)
    end
  end

  # Check if tty works. If it does not, we fall back to the
  # simple/dumb terminal. This is starting the linked in
  # driver twice, it would be nice and appropriate if we had
  # to do it just once.
  defp tty_works? do
    # Dettack the error logger since we are spawning a new one
    :error_logger.tty(false)

    try do
      port = Port.open { :spawn, :"tty_sl -c -e" }, [:eof]
      Port.close(port)
    catch
      _, _ -> false
    end
  end

  @doc """
  Starts IEx using a tty server.
  """
  def tty(opts // []) when is_list(opts) do
    config = boot_config(opts)

    remote =
      if remsh = opts[:remsh] do
        unless is_alive do
          raise ArgumentError, message: "In order to use --remsh, you need to name the current node"
        end
        if is_atom(remsh), do: remsh, else: binary_to_atom(remsh)
      end

    function = fn ->
      # We are inside the new tty and in a new process,
      # reattach it the error logger.
      :error_logger.tty(true)
      start config
    end

    # Dettach the error logger because we are going to unregister
    # the user process and start a new tty which will get control
    # over the standardio. Dettaching it here allows us to get rid
    # of warnings. We reattach it again when we get the new tty.
    :error_logger.tty(false)

    # Unregister the user process, user_drv command below
    # will register the new one.
    unregister_user_process

    args =
      if remote do
        { remote, :erlang, :apply, [function, []] }
      else
        { :erlang, :apply, [function, []] }
      end

    Erlang.user_drv.start([:"tty_sl -c -e", args])
  end

  @doc """
  Starts IEx simply using the current stdio.
  """
  def simple(opts // []) when is_list(opts) do
    start boot_config(opts)
  end

  # This is a callback invoked by Erlang shell utilities.
  @doc false
  def start(config // nil) do
    preload

    spawn fn ->
      config = config || boot_config([])

      case :init.notify_when_started(self()) do
        :started -> :ok
        _        -> :init.wait_until_started()
      end

      Process.flag(:trap_exit, true)

      set_expand_fun()
      run_after_spawn()
      IEx.Loop.start(config)
    end
  end

  ## Boot Helpers

  defp boot_config(opts) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit"

    scope  = Erlang.elixir.scope_for_eval(
      file: 'iex',
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

  defp unregister_user_process do
    if is_pid(Process.whereis(:user)), do: Process.unregister :user
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

    :io.setopts gl, [expand_fun: expand_fun]
  end

  defp ensure_module_exists(node, mod) do
    unless :rpc.call node, :code, :is_loaded, [mod] do
      {m,b,f} = :code.get_object_code mod
      {:module, mod} = :rpc.call node, :code, :load_binary, [m,f,b]
    end
  end

  defp run_after_spawn do
    lc fun inlist Enum.reverse(after_spawn), do: fun.()
  end
end
