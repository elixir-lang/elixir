defrecord IEx.Config, io: nil, binding: nil, cache: '', counter: 1, scope: nil, result: nil

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
    case :os.type do
      { :unix, _ } -> tty(opts)
      _            -> simple(opts)
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
      start_loop(config)
    end
  end

  ## Boot Helpers

  defp boot_config(opts) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit"

    scope  = Erlang.elixir.scope_for_eval(
      file: 'iex',
      delegate_locals_to: IEx.Helpers
    )

    IEx.Config[
      io: opts[:io] || IEx.UnicodeIO,
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

  defp run_after_spawn do
    lc fun inlist Enum.reverse(after_spawn), do: fun.()
  end

  ## Loop helpers

  defp start_loop(config) do
    Process.put :iex_history, []
    { _, _, scope } = Erlang.elixir.eval('import IEx.Helpers', [], 0, config.scope)
    do_loop(config.scope(scope))
  end

  defp do_loop(config) do
    io = config.io

    counter = config.counter
    cache   = config.cache
    code    = cache ++ io.get(config)

    new_config =
      try do
        { result, new_binding, scope } =
          Erlang.elixir.eval(code, config.binding, counter, config.scope)

        io.put result

        config = config.result(result)
        update_history(config.cache(code).scope(nil))
        config.increment_counter.cache('').binding(new_binding).scope(scope)
      rescue
        TokenMissingError ->
          config.cache(code)
        exception ->
          io.error "** (#{inspect exception.__record__(:name)}) #{exception.message}"
          io.error Exception.formatted_stacktrace
          config.cache('')
      catch
        kind, error ->
          io.error "** (#{kind}) #{inspect(error)}"
          io.error Exception.formatted_stacktrace
          config.cache('')
      end

    do_loop(new_config)
  end

  defp update_history(config) do
    current = Process.get :iex_history
    Process.put :iex_history, [config|current]
  end

  ## Code injection helper

  defp ensure_module_exists(node, mod) do
    unless :rpc.call node, :code, :is_loaded, [mod] do
      {m,b,f} = :code.get_object_code mod
      {:module, mod} = :rpc.call node, :code, :load_binary, [m,f,b]
    end
  end

  defp after_spawn do
    case :application.get_env(:iex, :after_spawn) do
      { :ok, list } -> list
      :undefined -> []
    end
  end
end
