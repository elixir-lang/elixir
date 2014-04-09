defmodule IEx do
  @moduledoc ~S"""
  Welcome to IEx.

  This module is the main entry point for Interactive Elixir and
  in this documentation we will talk a bit about how IEx works.

  Notice that some of the functionality described here will not be available
  depending on your terminal. In particular, if you get a message
  saying that the smart terminal could not be run, some of the
  features described here won't work.

  ## Helpers

  IEx provides a bunch of helpers. They can be accessed by typing
  `h()` into the shell or as a documentation for the `IEx.Helpers` module.

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
      ** (UndefinedFunctionError) undefined function: hello/0

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

  The string in between parenthesis in the prompt is the name
  of your node. We can retrieve it by calling the `node()`
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
      ** (UndefinedFunctionError) undefined function: Hello.world/0

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

  * remsh from an elixir node to an elixir node
  * remsh from a plain erlang node to an elixir node (through the ^G menu)
  * remsh from an elixir node to a plain erlang node (and get an erl shell there)

  Connecting an Elixir shell to a remote node without Elixir is
  **not** supported.

  ## The .iex.exs file

  When starting IEx, it will look for a local `.iex.exs` file (located in the current
  working directory), then a global one (located at `~/.iex.exs`) and will load the
  first one it finds (if any). The code in the chosen .iex file will be
  evaluated in the shell's context. So, for instance, any modules that are
  loaded or variables that are bound in the .iex file will be available in the
  shell after it has booted.

  Sample contents of a local .iex file:

      # source another `.iex` file
      import_file "~/.iex.exs"

      # print something before the shell starts
      IO.puts "hello world"

      # bind a variable that'll be accessible in the shell
      value = 13

  Running the shell in the directory where the above .iex file is located
  results in

      $ iex
      Erlang 17 [...]

      hello world
      Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)
      iex(1)> value
      13

  It is possible to override the default loading sequence for `.iex.exs` file by
  supplying the `--dot-iex` option to iex. See `iex --help`.

  ## Configuring the shell

  There are a number of customization options provided by the shell. Take a look
  at the docs for the `IEx.Options` module by typing `h IEx.Options`.

  The main functions there are `IEx.Options.get/1` and `IEx.Options.set/2`. One
  can also use `IEx.Options.list/0` to get the list of all supported options.
  `IEx.Options.print_help/1` will print documentation for the given option.

  In particular, it might be convenient to customize those options inside your
  `.iex.exs` file like this:

      # .iex
      IEx.Options.set :inspect, limit: 3

      ### now run the shell ###

      $ iex
      Erlang 17 (erts-5.10.1) [...]

      Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)
      iex(1)> [1, 2, 3, 4, 5]
      [1,2,3,...]

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
  Registers a function to be invoked after the IEx process is spawned.
  """
  def after_spawn(fun) when is_function(fun) do
    :application.set_env(:iex, :after_spawn, [fun|after_spawn])
  end

  @doc """
  Returns registered `after_spawn` callbacks.
  """
  def after_spawn do
    { :ok, list } = :application.get_env(:iex, :after_spawn)
    list
  end

  @doc """
  Returns `true` if IEx was properly started.
  """
  def started? do
    :application.get_env(:iex, :started) == { :ok, true }
  end

  @doc """
  Returns `string` escaped using the specified color.
  ANSI escapes in `string` are not processed in any way.
  """
  def color(color_name, string) do
    colors = IEx.Options.get(:colors)
    enabled = colors[:enabled]
    IO.ANSI.escape_fragment("%{#{colors[color_name]}}", enabled)
      <> string <> IO.ANSI.escape_fragment("%{reset}", enabled)
  end

  @doc """
  Pries into the process environment.

  This is useful for debugging a particular chunk of code
  and inspect the state of a particular process. The process
  is temporarily changed to trap exits (i.e. the process flag
  `:trap_exit` is set to true) and has the `group_leader` changed
  to support ANSI escape codes. Those values are reverted by
  calling `respawn`, which starts a new IEx shell, freeing up
  the pried one.

  When a process is pried, all code runs inside IEx and, as
  such, it is evaluated and cannot access private functions
  of the module being pried. Module functions still need to be
  accessed via `Mod.fun(args)`.

  Status: This feature is experimental.

  ## Examples

  Let's suppose you want to investigate what is happening
  with some particular function. By invoking `IEx.pry` from
  the function, IEx will allow you to access its binding
  (variables), verify its lexical information and access
  the process information. Let's see an example:

      import Enum, only: [map: 2]

      def add(a, b) do
        c = a + b
        IEx.pry
      end

  When invoking `add(1, 2)`, you will receive a message in
  your shell to pry the given environment. By allowing it,
  the shell will be reset and you gain access to all variables
  and the lexical scope from above:

      iex(1)> map([a,b,c], &IO.inspect(&1))
      1
      2
      3

  Keep in mind that `IEx.pry` runs in the caller process,
  blocking the caller during the evaluation cycle. The caller
  process can be freed by calling `respawn`, which starts a
  new IEx evaluation cycle, letting this one go:

      iex(2)> respawn
      true

      Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)

  Setting variables or importing modules in IEx does not
  affect the caller the environment (hence it is called `pry`).
  """
  defmacro pry(timeout \\ 1000) do
    quote do
      env  = __ENV__
      meta = "#{inspect self} at #{Path.relative_to_cwd(env.file)}:#{env.line}"
      opts = [binding: binding, dot_iex_path: "", env: env, prefix: "pry"]
      res  = IEx.Server.take_over("Request to pry #{meta}", opts, unquote(timeout))

      # We cannot use colors because IEx may be off.
      case res do
        { :error, :self } = err ->
          IO.puts :stdio, "IEx cannot pry itself."
        { :error, :no_iex } = err ->
          IO.puts :stdio, "Cannot pry #{meta}. Is an IEx shell running?"
        _ ->
          :ok
      end

      res
    end
  end

  ## Callbacks

  # This is a callback invoked by Erlang shell utilities
  # when someone press Ctrl+G and adds 's Elixir.IEx'.
  @doc false
  def start(opts \\ [], callback \\ fn -> end) do
    spawn fn ->
      case :init.notify_when_started(self()) do
        :started -> :ok
        _        -> :init.wait_until_started()
      end

      start_iex()
      set_expand_fun()
      run_after_spawn()
      IEx.Server.start(opts, callback)
    end
  end

  @doc false
  def dont_display_result, do: :"do not show this result in output"

  ## Helpers

  defp start_iex do
    unless started? do
      :application.start(:elixir)
      :application.start(:iex)
      :application.set_env(:iex, :started, true)

      # Disable ANSI-escape-sequence-based coloring on Windows
      # Can be overriden in .iex
      if match?({ :win32, _ }, :os.type()) do
        IEx.Options.set :colors, enabled: false
      end
    end
  end

  defp set_expand_fun do
    gl = Process.group_leader
    glnode = node gl

    if glnode != node do
      ensure_module_exists glnode, IEx.Remsh
      expand_fun = IEx.Remsh.expand node
    else
      expand_fun = &IEx.Autocomplete.expand(&1)
    end

    :io.setopts gl, [expand_fun: expand_fun, binary: true, encoding: :unicode]
  end

  defp ensure_module_exists(node, mod) do
    unless :rpc.call node, :code, :is_loaded, [mod] do
      { m, b, f } = :code.get_object_code mod
      { :module, _ } = :rpc.call node, :code, :load_binary, [m, f, b]
    end
  end

  defp run_after_spawn do
    for fun <- Enum.reverse(after_spawn), do: fun.()
  end
end
