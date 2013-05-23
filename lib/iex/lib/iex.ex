defrecord IEx.Config, binding: nil, cache: '', counter: 1, scope: nil,
                      result: nil, dot_iex_path: nil

defmodule IEx do
  @moduledoc %B"""
  Welcome to IEx.

  This module is the main entry point for Interactive Elixir and
  in this documentation we will talk a bit about how IEx works.

  Notice some of the functionality described here will be available
  depending on your terminal. In particular, if you get a message
  saying that the smart terminal could not be run, some of the
  features described here won't work.

  ## The Break command

  Inside IEx, hitting Ctrl+C will open up the BREAK menu. In this
  menu you can quit the shell, see process and ets tables information
  and much more.

  ## The User Switch command

  Besides the break command, one can type Ctrl+G to get to the
  user switch command menu. When reached, you can type `h` to
  get more information.

  In this menu, developers are able to start new shells and
  alternate in between them. Let's give it a try:

      User switch command
       --> s 'Elixir.IEx'
       --> c

  The command above will start a new shell and connect to it.
  Create a new variable called hello and assign some value to it:

      hello = :world

  Now, let's roll back to the first shell:

      User switch command
       --> c 1

  Now, try to access the hello variable again:

      hello
      ** (UndefinedFunctionError) undefined function: IEx.Helpers.hello/0

  The command above fails because we have switched the shells.
  Since shells are isolated from each other, you can't access the
  variables defined in one shell from the other one.

  The user switch command menu also allows developers to connect to remote
  shells using the "r" command. Keep in mind that you can't connect to a
  remote node if you haven't given a name to the current node
  (i.e. Process.is_alive? must return true).

  ## The .iex file

  When starting IEx, it will look for a local .iex file (located in the current
  working directory), then a global one (located at ~/.iex) and will load the
  first one it finds (if any). The code in the chosen .iex file will be
  evaluated in the shell's context. So, for instance, any modules that are
  loaded or variables that are bound in the .iex file will be available in the
  shell after it has booted.

  Sample contents of a local .iex file:

      # source another .iex file
      import_file "~/.iex"

      # print something before the shell starts
      IO.puts "hello world"

      # bind a variable that'll be accessible in the shell
      value = 13

  Running the shell in the directory where the above .iex file is located
  results in

      $ iex
      Erlang R15B03 (erts-5.9.3.1) [...]

      hello world
      Interactive Elixir (0.8.3.dev) - press Ctrl+C to exit (type h() ENTER for help)
      iex(1)> value
      13

  It is possible to override the default loading sequence for .iex file by
  supplying the --dot-iex option to iex. See `iex --help`.

  ## Expressions in IEx

  As an interactive shell, IEx evaluates expressions. This has some
  interesting consequences that are worth discussing.

  The first one is that the code is truly evaluated and not compiled.
  This means that any benchmarking done in the shell is going to have
  skewed results. So never run any profiling nor benchmarks in the shell.

  Second of all, IEx alows you to break an expression into many lines,
  since this is common in Elixir. For example:

      iex(1)> "ab
      ...(1)> c"
      "ab\nc"

  In the example above, the shell will be expecting more input until it
  finds the closing quote. Sometimes it is not obvious which character
  the shell is expecting, and the user may find themselves trapped in
  the state of incomplete expression with no ability to terminate it other
  than by exiting the shell.

  For such cases, there is a special break-trigger ("#iex:break") that when
  encountered on a line by itself will force the shell to break out of any
  pending expression and return to its normal state:

      iex(1)> ["ab
      ...(1)> c"
      ...(1)> "
      ...(1)> ]
      ...(1)> #iex:break
      ** (TokenMissingError) iex:1: incomplete expression

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
    old_opts = inspect_opts()
    filtered_opts = Enum.filter opts, fn {name, _} ->
      name in old_opts
    end
    :application.set_env(:iex, :inspect_opts, Keyword.merge(old_opts, filtered_opts))
  end

  @doc """
  Returns currently registered inspect options.
  """
  def inspect_opts do
    { :ok, opts } = :application.get_env(:iex, :inspect_opts)
    opts
  end

  # This is a callback invoked by Erlang shell utilities
  # when someone press Ctrl+G and adds 's Elixir.IEx'.
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
      scope: scope,
      dot_iex_path: Keyword.get(opts, :dot_iex_path),
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

  @doc """
  Returns `string` escaped using the specified color.
  """
  def color(color_name, string) do
    colors = IEx.Options.get(:colors)
    IO.ANSI.escape "%{#{colors[color_name]}}#{string}", colors[:enabled]
  end

  @doc """
  Returns an escaped fragment using the specified color.
  """
  def color_fragment(color_name) do
    colors = IEx.Options.get(:colors)
    IO.ANSI.escape_fragment "%{#{colors[color_name]}}", colors[:enabled]
  end

  @doc """
  Returns an escaped fragment that resets colors and attributes.
  """
  def color_reset() do
    colors = IEx.Options.get(:colors)
    IO.ANSI.escape_fragment "%{reset}", colors[:enabled]
  end
end
