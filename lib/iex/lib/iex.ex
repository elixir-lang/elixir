defmodule IEx do
  @moduledoc ~S"""
  Elixir's interactive shell.

  This module is the main entry point for Interactive Elixir and
  in this documentation we will talk a bit about how IEx works.

  Notice that some of the functionalities described here will not be available
  depending on your terminal. In particular, if you get a message
  saying that the smart terminal could not be run, some of the
  features described here won't work.

  ## Helpers

  IEx provides a bunch of helpers. They can be accessed by typing
  `h()` into the shell or as a documentation for the `IEx.Helpers` module.

  ## Autocomplete

  To discover all available functions for a module, type the module name
  followed by a dot, then press tab to trigger autocomplete. For example:

      Enum.

  Such function may not be available on some Windows shells. You may need
  to pass the `--werl` flag when starting iex, as in `iex --werl` for it
  to work. `--werl` may be permanently enabled by setting the `IEX_WITH_WERL`
  environment variable.

  ## The Break command

  Inside IEx, hitting `Ctrl+C` will open up the `BREAK` menu. In this
  menu you can quit the shell, see process and ets tables information
  and much more.

  ## The User Switch command

  Besides the break command, one can type `Ctrl+G` to get to the
  user switch command menu. When reached, you can type `h` to
  get more information.

  In this menu, developers are able to start new shells and
  alternate between them. Let's give it a try:

      User switch command
       --> s 'Elixir.IEx'
       --> c

  The command above will start a new shell and connect to it.
  Create a new variable called `hello` and assign some value to it:

      hello = :world

  Now, let's roll back to the first shell:

      User switch command
       --> c 1

  Now, try to access the `hello` variable again:

      hello
      ** (UndefinedFunctionError) undefined function hello/0

  The command above fails because we have switched shells.
  Since shells are isolated from each other, you can't access the
  variables defined in one shell from the other one.

  The user switch command menu also allows developers to connect to remote
  shells using the `r` command. A topic which we will discuss next.

  ## Remote shells

  IEx allows you to connect to another node in two fashions.
  First of all, we can only connect to a shell if we give names
  both to the current shell and the shell we want to connect to.

  Let's give it a try. First start a new shell:

      $ iex --sname foo
      iex(foo@HOST)1>

  The string between the parentheses in the prompt is the name
  of your node. We can retrieve it by calling the `node/0`
  function:

      iex(foo@HOST)1> node()
      :"foo@HOST"
      iex(foo@HOST)2> Node.alive?()
      true

  For fun, let's define a simple module in this shell too:

      iex(foo@HOST)3> defmodule Hello do
      ...(foo@HOST)3>   def world, do: "it works!"
      ...(foo@HOST)3> end

  Now, let's start another shell, giving it a name as well:

      $ iex --sname bar
      iex(bar@HOST)1>

  If we try to dispatch to `Hello.world`, it won't be available
  as it was defined only in the other shell:

      iex(bar@HOST)1> Hello.world
      ** (UndefinedFunctionError) undefined function Hello.world/0

  However, we can connect to the other shell remotely. Open up
  the User Switch prompt (Ctrl+G) and type:

      User switch command
       --> r 'foo@HOST' 'Elixir.IEx'
       --> c

  Now we are connected into the remote node, as the prompt shows us,
  and we can access the information and modules defined over there:

      rem(foo@macbook)1> Hello.world
      "it works"

  In fact, connecting to remote shells is so common that we provide
  a shortcut via the command line as well:

      $ iex --sname baz --remsh foo@HOST

  Where "remsh" means "remote shell". In general, Elixir supports:

    * remsh from an Elixir node to an Elixir node
    * remsh from a plain Erlang node to an Elixir node (through the ^G menu)
    * remsh from an Elixir node to a plain Erlang node (and get an `erl` shell there)

  Connecting an Elixir shell to a remote node without Elixir is
  **not** supported.

  ## The .iex.exs file

  When starting, IEx looks for a local `.iex.exs` file (located in the current
  working directory), then a global one (located at `~/.iex.exs`) and loads the
  first one it finds (if any). The code in the loaded `.iex.exs` file is
  evaluated in the shell's context. So, for instance, any modules that are
  loaded or variables that are bound in the `.iex.exs` file will be available in the
  shell after it has booted.

  For example, take the following `.iex.exs` file:

      # Load another ".iex.exs" file
      import_file "~/.iex.exs"

      # Import some module from lib that may not yet have been defined
      import_if_available MyApp.Mod

      # Print something before the shell starts
      IO.puts "hello world"

      # Bind a variable that'll be accessible in the shell
      value = 13

  Running IEx in the directory where the above `.iex.exs` file is located
  results in:

      $ iex
      Erlang 19 [...]

      hello world
      Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)
      iex(1)> value
      13

  It is possible to load another file by supplying the `--dot-iex`
  option to IEx. See `iex --help`.

  ## Configuring the shell

  There are a number of customization options provided by IEx. Take a look
  at the docs for the `IEx.configure/1` function by typing `h IEx.configure/1`.

  Those options can be configured in your project configuration file or globally
  by calling `IEx.configure/1` from your `~/.iex.exs` file. For example:

      # .iex.exs
      IEx.configure(inspect: [limit: 3])

  Now run the shell:

      $ iex
      Erlang 19 [...]

      Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)
      iex(1)> [1, 2, 3, 4, 5]
      [1, 2, 3, ...]

  ## Expressions in IEx

  As an interactive shell, IEx evaluates expressions. This has some
  interesting consequences that are worth discussing.

  The first one is that the code is truly evaluated and not compiled.
  This means that any benchmarking done in the shell is going to have
  skewed results. So never run any profiling nor benchmarks in the shell.

  Second, IEx allows you to break an expression into many lines,
  since this is common in Elixir. For example:

      iex(1)> "ab
      ...(1)> c"
      "ab\nc"

  In the example above, the shell will be expecting more input until it
  finds the closing quote. Sometimes it is not obvious which character
  the shell is expecting, and the user may find themselves trapped in
  the state of incomplete expression with no ability to terminate it other
  than by exiting the shell.

  For such cases, there is a special break-trigger (`#iex:break`) that when
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
  Configures IEx.

  The supported options are:

    * `:colors`
    * `:inspect`
    * `:width`
    * `:history_size`
    * `:default_prompt`
    * `:alive_prompt`

  They are discussed individually in the sections below.

  ## Colors

  A keyword list that encapsulates all color settings used by the
  shell. See documentation for the `IO.ANSI` module for the list of
  supported colors and attributes.

  List of supported keys in the keyword list:

    * `:enabled`      - boolean value that allows for switching the coloring on and off
    * `:eval_result`  - color for an expression's resulting value
    * `:eval_info`    - ... various informational messages
    * `:eval_error`   - ... error messages
    * `:stack_app`    - ... the app in stack traces
    * `:stack_info`   - ... the remaining info in stacktraces
    * `:ls_directory` - ... for directory entries (ls helper)
    * `:ls_device`    - ... device entries (ls helper)

  When printing documentation, IEx will convert the Markdown
  documentation to ANSI as well. Colors for this can be configured
  via:

    * `:doc_code`        - the attributes for code blocks (cyan, bright)
    * `:doc_inline_code` - inline code (cyan)
    * `:doc_headings`    - h1 and h2 (yellow, bright)
    * `:doc_title`       - the overall heading for the output (reverse, yellow, bright)
    * `:doc_bold`        - (bright)
    * `:doc_underline`   - (underline)

  IEx will also color inspected expressions using the `:syntax_colors`
  option. Such can be disabled with:

      IEx.configure [colors: [syntax_colors: false]]

  You can also configure the syntax colors, however, as desired:

      IEx.configure [colors: [syntax_colors: [atom: :red]]]

  Configuration for most built-in data types are supported: `:atom`,
  `:string`, `:binary`, `:list`, `:number`, `:boolean`, `:nil`, etc.
  The default is:

      [number: :magenta, atom: :cyan, string: :green,
       boolean: :magenta, nil: :magenta]

  ## Inspect

  A keyword list containing inspect options used by the shell
  when printing results of expression evaluation. Default to
  pretty formatting with a limit of 50 entries.

  To show all entries, configure the limit to `:infinity`:

      IEx.configure [inspect: [limit: :infinity]]

  See `Inspect.Opts` for the full list of options.

  ## Width

  An integer indicating the number of columns to use in documentation
  output. Default is 80 columns or result of `:io.columns`, whichever
  is smaller. The configured value will be used unless it is too large,
  which in that case `:io.columns` is used. This way you can configure
  IEx to be your largest screen size and it should always take up the
  full width of your terminal screen.

  ## History size

  Number of expressions and their results to keep in the history.
  The value is an integer. When it is negative, the history is unlimited.

  ## Prompt

  This is an option determining the prompt displayed to the user
  when awaiting input.

  The value is a keyword list with two possible keys representing prompt types:

    * `:default_prompt` - used when `Node.alive?/0` returns `false`
    * `:alive_prompt`   - used when `Node.alive?/0` returns `true`

  The following values in the prompt string will be replaced appropriately:

    * `%counter` - the index of the history
    * `%prefix`  - a prefix given by `IEx.Server`
    * `%node`    - the name of the local node

  """
  def configure(options) do
    IEx.Config.configure(options)
  end

  @doc """
  Returns IEx configuration.
  """
  def configuration do
    IEx.Config.configuration()
  end

  @doc """
  Registers a function to be invoked after the IEx process is spawned.
  """
  def after_spawn(fun) when is_function(fun) do
    IEx.Config.after_spawn(fun)
  end

  @doc """
  Returns registered `after_spawn` callbacks.
  """
  def after_spawn do
    IEx.Config.after_spawn()
  end

  @doc """
  Returns `true` if IEx was started.
  """
  def started? do
    IEx.Config.started?()
  end

  @doc """
  Returns `string` escaped using the specified `color`.

  ANSI escapes in `string` are not processed in any way.
  """
  def color(color, string) do
    case IEx.Config.color(color) do
      nil ->
        string
      ansi ->
        [ansi | string] |> IO.ANSI.format(true) |> IO.iodata_to_binary()
    end
  end

  @doc """
  Gets the IEx width for printing.

  Used by helpers and it has a maximum cap of 80 chars.
  """
  def width do
    IEx.Config.width()
  end

  @doc """
  Gets the options used for inspecting.
  """
  def inspect_opts do
    IEx.Config.inspect_opts()
  end

  @doc """
  Pries into the process environment.

  This is useful for debugging a particular chunk of code
  and inspect the state of a particular process. The process
  is temporarily changed to trap exits (i.e. the process flag
  `:trap_exit` is set to `true`) and has the `group_leader` changed
  to support ANSI escape codes. Those values are reverted by
  calling `respawn`, which starts a new IEx shell, freeing up
  the pried one.

  When a process is pried, all code runs inside IEx and, as
  such, it is evaluated and cannot access private functions
  of the module being pried. Module functions still need to be
  accessed via `Mod.fun(args)`.

  ## Examples

  Let's suppose you want to investigate what is happening
  with some particular function. By invoking `IEx.pry/1` from
  the function, IEx will allow you to access its binding
  (variables), verify its lexical information and access
  the process information. Let's see an example:

      import Enum, only: [map: 2]
      require IEx

      defmodule Adder do
        def add(a, b) do
          c = a + b
          IEx.pry
        end
      end

  When invoking `Adder.add(1, 2)`, you will receive a message in
  your shell to pry the given environment. By allowing it,
  the shell will be reset and you gain access to all variables
  and the lexical scope from above:

      pry(1)> map([a, b, c], &IO.inspect(&1))
      1
      2
      3

  Keep in mind that `IEx.pry/1` runs in the caller process,
  blocking the caller during the evaluation cycle. The caller
  process can be freed by calling `respawn`, which starts a
  new IEx evaluation cycle, letting this one go:

      pry(2)> respawn
      true

      Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)

  Setting variables or importing modules in IEx does not
  affect the caller the environment (hence it is called `pry`).
  """
  defmacro pry(timeout \\ 5000) do
    quote do
      IEx.pry(binding(), __ENV__, unquote(timeout))
    end
  end

  @doc """
  Callback for `IEx.pry/1`.

  You can invoke this function directly when you are not able to invoke
  `IEx.pry/1` as a macro. This function expects the binding (from
  `Kernel.binding/0`), the environment (from `__ENV__/0`) and the timeout
  (a sensible default is 5000).
  """
  def pry(binding, env, timeout) do
    opts = [binding: binding, dot_iex_path: "", env: env, prefix: "pry"]
    meta = "#{inspect self()} at #{Path.relative_to_cwd(env.file)}:#{env.line}"
    desc =
      if File.regular?(env.file) do
        parse_file(env)
      else
        ""
      end

    res = IEx.Server.take_over("Request to pry #{meta}#{desc}", opts, timeout)

    # We cannot use colors because IEx may be off.
    case res do
      {:error, :no_iex} ->
        extra =
          case :os.type do
            {:win32, _} -> " If you are Windows, you may need to start IEx with the --werl flag."
            _           -> ""
          end
        IO.puts :stdio, "Cannot pry #{meta}. Is an IEx shell running?" <> extra
      _ ->
        :ok
    end

    res
  end

  defp parse_file(env) do
    lines =
      env.file
      |> File.stream!
      |> Enum.slice(max(env.line - 3, 0), 5)
    Enum.intersperse(["\n\n" | lines], "    ")
  end

  ## Callbacks

  # This is a callback invoked by Erlang shell utilities
  # when someone press Ctrl+G and adds 's Elixir.IEx'.
  @doc false
  def start(opts \\ [], mfa \\ {IEx, :dont_display_result, []}) do
    spawn fn ->
      case :init.notify_when_started(self()) do
        :started -> :ok
        _        -> :init.wait_until_started()
      end

      :ok = start_iex()
      :ok = set_expand_fun()
      :ok = run_after_spawn()
      IEx.Server.start(opts, mfa)
    end
  end

  @doc false
  def dont_display_result, do: :"do not show this result in output"

  ## Helpers

  defp start_iex() do
    {:ok, _} = Application.ensure_all_started(:iex)
    :ok
  end

  defp set_expand_fun do
    gl = Process.group_leader
    glnode = node gl

    expand_fun =
      if glnode != node() do
        _ = ensure_module_exists glnode, IEx.Remsh
        IEx.Remsh.expand node()
      else
        &IEx.Autocomplete.expand(&1)
      end

    # expand_fun is not supported by a shell variant
    # on Windows, so we do two IO calls, not caring
    # about the result of the expand_fun one.
    _ = :io.setopts(gl, expand_fun: expand_fun)
    :io.setopts(gl, binary: true, encoding: :unicode)
  end

  defp ensure_module_exists(node, mod) do
    unless :rpc.call node, :code, :is_loaded, [mod] do
      {m, b, f} = :code.get_object_code mod
      {:module, _} = :rpc.call node, :code, :load_binary, [m, f, b]
    end
  end

  defp run_after_spawn do
    _ = for fun <- Enum.reverse(after_spawn()), do: fun.()
    :ok
  end
end
