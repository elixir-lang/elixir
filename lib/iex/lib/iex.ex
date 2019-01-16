defmodule IEx do
  @moduledoc ~S"""
  Elixir's interactive shell.

  Some of the functionalities described here will not be available
  depending on your terminal. In particular, if you get a message
  saying that the smart terminal could not be run, some of the
  features described here won't work.

  ## Helpers

  IEx provides a bunch of helpers. They can be accessed by typing
  `h()` into the shell or as a documentation for the `IEx.Helpers` module.

  ## Autocomplete

  To discover a module's public functions or other modules, type the module name
  followed by a dot, then press tab to trigger autocomplete. For example:

      Enum.

  A module may export functions that are not meant to be used directly: these
  functions won't be autocompleted by IEx. IEx will not autocomplete functions
  annotated with `@doc false`, `@impl true`, or functions that aren't explicitly
  documented and where the function name is in the form of `__foo__`.

  Autocomplete may not be available on some Windows shells. You may need
  to pass the `--werl` option when starting IEx, as in `iex --werl` for it
  to work. `--werl` may be permanently enabled by setting the `IEX_WITH_WERL`
  environment variable.

  ## Shell history

  It is possible to get shell history by passing some options that enable it
  in the VM. This can be done on a per-need basis when starting IEx:

      iex --erl "-kernel shell_history enabled"

  If you would rather enable it on your system as a whole, you can use
  the `ERL_AFLAGS` environment variable and make sure that it is set
  accordingly on your terminal/shell configuration.

  On Unix-like / Bash:

      export ERL_AFLAGS="-kernel shell_history enabled"

  On Windows:

      set ERL_AFLAGS "-kernel shell_history enabled"

  On Windows 10 / PowerShell:

      $env:ERL_AFLAGS = "-kernel shell_history enabled"

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

  ## Pasting multiline expressions into IEx

  IEx evaluates its input line by line in an eagerly fashion which means
  that if at the end of a line the code seen so far is a complete expression
  IEx will evaluate it at that point. This behaviour may produce errors for
  expressions that have been formatted across multiple lines which is often
  the case for piped expressions. Consider the following expression using
  the `|>/2` operator:

      iex(1)> [1, [2], 3] |> List.flatten()
      [1, 2, 3]

  When written in multiline form and pasted into IEx this valid expression
  produces a syntax error:

      iex(1)> [1, [2], 3]
      [1, [2], 3]
      iex(2)> |> List.flatten()
      ** (SyntaxError) iex:2: syntax error before: '|>'

  As IEx evaluates its input line by line, it will first encounter
  `[1, [2], 3]`. As a list is a valid expression, IEx will evaluate
  it immediately before looking at the next input line. Only then
  will IEx attempt to evaluate the now incomplete expression
  `|> List.flatten()`, which on its own is missing its left operand.
  The evaluation thus fails with the above syntax error.

  In order to help IEx understand that an expression consists of multiple
  lines we can wrap it into parentheses:

      iex(1)> (
      ...(1)> [1, [2], 3]
      ...(1)> |> List.flatten()
      ...(1)> )
      [1, 2, 3]

  Note that this not only works with single expressions but also with
  arbitrary code blocks.

  ## The BREAK menu

  Inside IEx, hitting `Ctrl+C` will open up the `BREAK` menu. In this
  menu you can quit the shell, see process and ETS tables information
  and much more.

  ## Exiting the shell

  There are a few ways to quit the IEx shell:

    * via the `BREAK` menu (available via `Ctrl+C`) by typing `q`, pressing enter
    * by hitting `Ctrl+C`, `Ctrl+C`
    * by hitting `Ctrl+\ `

  If you are connected to remote shell, it remains alive after disconnection.

  ## Prying and breakpoints

  IEx also has the ability to set breakpoints on Elixir code and
  "pry" into running processes. This allows the developer to have
  an IEx session run inside a given function.

  `IEx.pry/0` can be used when you are able to modify the source
  code directly and recompile it:

      def my_fun(arg1, arg2) do
        require IEx; IEx.pry()
        ... implementation ...
      end

  When the code is executed, it will ask you for permission to be
  introspected.

  Alternatively, you can use `IEx.break!/4` to setup a breakpoint
  on a given module, function and arity you have no control of.
  While `IEx.break!/4` is more flexible, it does not contain
  information about imports and aliases from the source code.

  ## The User switch command

  Besides the `BREAK` menu, one can type `Ctrl+G` to get to the
  `User switch command` menu. When reached, you can type `h` to
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

  The `User switch command` can also be used to terminate an existing
  session, for example when the evaluator gets stuck in an infinite
  loop or when you are stuck typing an expression:

      User switch command
       --> i
       --> c

  The `User switch command` menu also allows developers to connect to
  remote shells using the `r` command. A topic which we will discuss next.

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
  the `User switch command` prompt (Ctrl+G) and type:

      User switch command
       --> r 'foo@HOST' 'Elixir.IEx'
       --> c

  Now we are connected into the remote node, as the prompt shows us,
  and we can access the information and modules defined over there:

      iex(foo@HOST)1> Hello.world
      "it works!"

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
      Erlang/OTP 20 [...]

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
      Erlang/OTP 20 [...]

      Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)
      iex(1)> [1, 2, 3, 4, 5]
      [1, 2, 3, ...]

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

    * `:enabled` - boolean value that allows for switching the coloring on and off
    * `:eval_result` - color for an expression's resulting value
    * `:eval_info` - ... various informational messages
    * `:eval_error` - ... error messages
    * `:eval_interrupt` - ... interrupt messages
    * `:stack_info` - ... the stacktrace color
    * `:blame_diff` - ... when blaming source with no match
    * `:ls_directory` - ... for directory entries (ls helper)
    * `:ls_device` - ... device entries (ls helper)

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

  An integer indicating the maximum number of columns to use in output.
  The default value is 80 columns. The actual output width is the minimum
  of this number and result of `:io.columns`. This way you can configure IEx
  to be your largest screen size and it should always take up the full width
  of your current terminal screen.

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

  Used by helpers and it has a default maximum cap of 80 chars.
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
  when executed by a particular process. The process becomes
  the evaluator of IEx commands and is temporarily changed to
  have a custom group leader. Those values are reverted by
  calling `IEx.Helpers.respawn/0`, which starts a new IEx shell,
  freeing up the pried one.

  When a process is pried, all code runs inside IEx and has
  access to all imports and aliases from the original code.
  However, the code is evaluated and therefore cannot access
  private functions of the module being pried. Module functions
  still need to be accessed via `Mod.fun(args)`.

  Alternatively, you can use `IEx.break!/4` to setup a breakpoint
  on a given module, function and arity you have no control of.
  While `IEx.break!/4` is more flexible,  it does not contain
  information about imports and aliases from the source code.

  ## Examples

  Let's suppose you want to investigate what is happening
  with some particular function. By invoking `IEx.pry/0` from
  the function, IEx will allow you to access its binding
  (variables), verify its lexical information and access
  the process information. Let's see an example:

      import Enum, only: [map: 2]

      defmodule Adder do
        def add(a, b) do
          c = a + b
          require IEx; IEx.pry()
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

  Keep in mind that `IEx.pry/0` runs in the caller process,
  blocking the caller during the evaluation cycle. The caller
  process can be freed by calling `respawn/0`, which starts a
  new IEx evaluation cycle, letting this one go:

      pry(2)> respawn()
      true

      Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)

  Setting variables or importing modules in IEx does not
  affect the caller's environment. However, sending and
  receiving messages will change the process state.

  ## Pry and macros

  When setting up Pry inside a code defined by macros, such as:

      defmacro __using__(_) do
        quote do
          def add(a, b) do
            c = a + b
            require IEx; IEx.pry()
          end
        end
      end

  The variables defined inside `quote` won't be available during
  prying due to the hygiene mechanism in quoted expressions. The
  hygiene mechanism changes the variable names in quoted expressions
  so they don't collide with variables defined by the users of the
  macros. Therefore the original names are not available.

  ## Pry and `mix test`

  To use `IEx.pry/0` during tests, you need to run `mix` inside
  the `iex` command and pass the `--trace` to `mix test` to avoid running
  into timeouts:

      iex -S mix test --trace
      iex -S mix test path/to/file:line --trace

  """
  defmacro pry() do
    quote do
      IEx.Pry.pry(binding(), __ENV__)
    end
  end

  @doc """
  Macro-based shortcut for `IEx.break!/4`.
  """
  @doc since: "1.5.0"
  defmacro break!(ast, stops \\ 1) do
    quote do
      IEx.__break__!(unquote(Macro.escape(ast)), unquote(Macro.escape(stops)), __ENV__)
    end
  end

  def __break__!({:/, _, [call, arity]} = ast, stops, env) when is_integer(arity) do
    with {module, fun, []} <- Macro.decompose_call(call),
         module when is_atom(module) <- Macro.expand(module, env) do
      IEx.Pry.break!(module, fun, arity, stops)
    else
      _ ->
        raise_unknown_break_ast!(ast)
    end
  end

  def __break__!({{:., _, [module, fun]}, _, args} = ast, stops, env) do
    __break__!(ast, module, fun, args, true, stops, env)
  end

  def __break__!({:when, _, [{{:., _, [module, fun]}, _, args}, guards]} = ast, stops, env) do
    __break__!(ast, module, fun, args, guards, stops, env)
  end

  def __break__!(ast, _stops) do
    raise_unknown_break_ast!(ast)
  end

  defp __break__!(ast, module, fun, args, guards, stops, env) do
    module = Macro.expand(module, env)

    unless is_atom(module) do
      raise_unknown_break_ast!(ast)
    end

    IEx.Pry.break!(module, fun, args, guards, env, stops)
  end

  defp raise_unknown_break_ast!(ast) do
    raise ArgumentError, """
    unknown expression to break on, expected one of:

      * Mod.fun/arity, such as: URI.parse/1
      * Mod.fun(arg1, arg2, ...), such as: URI.parse(_)
      * Mod.fun(arg1, arg2, ...) when guard, such as: URI.parse(var) when is_binary(var)

    Got #{Macro.to_string(ast)}
    """
  end

  @doc """
  Sets up a breakpoint in `module`, `function` and `arity` with
  the given number of `stops`.

  This function will instrument the given module and load a new
  version in memory with breakpoints at the given function and
  arity. If the module is recompiled, all breakpoints are lost.

  When a breakpoint is reached, IEx will ask if you want to `pry`
  the given function and arity. In other words, this works similar
  to `IEx.pry/0` as the running process becomes the evaluator of
  IEx commands and is temporarily changed to have a custom group
  leader. However, differently from `IEx.pry/0`, aliases and imports
  from the source code won't be available in the shell.

  IEx helpers includes many conveniences related to breakpoints.
  Below they are listed with the full module, such as `IEx.Helpers.breaks/0`,
  but remember it can be called directly as `breaks()` inside IEx.
  They are:

    * `IEx.Helpers.break!/2` - sets up a breakpoint for a given `Mod.fun/arity`
    * `IEx.Helpers.break!/4` - sets up a breakpoint for the given module, function, arity
    * `IEx.Helpers.breaks/0` - prints all breakpoints and their IDs
    * `IEx.Helpers.continue/0` - continues until the next breakpoint in the same shell
    * `IEx.Helpers.open/0` - opens editor on the current breakpoint
    * `IEx.Helpers.remove_breaks/0` - removes all breakpoints in all modules
    * `IEx.Helpers.remove_breaks/1` - removes all breakpoints in a given module
    * `IEx.Helpers.reset_break/1` - sets the number of stops on the given ID to zero
    * `IEx.Helpers.reset_break/3` - sets the number of stops on the given module, function, arity to zero
    * `IEx.Helpers.respawn/0` - starts a new shell (breakpoints will ask for permission once more)
    * `IEx.Helpers.whereami/1` - shows the current location

  By default, the number of stops in a breakpoint is 1. Any follow-up
  call won't stop the code execution unless another breakpoint is set.

  Alternatively, the number of stops can be increased by passing the `stops`
  argument. `IEx.Helpers.reset_break/1` and `IEx.Helpers.reset_break/3`
  can be used to reset the number back to zero. Note the module remains
  "instrumented" even after all stops on all breakpoints are consumed.
  You can remove the instrumentation in a given module by calling
  `IEx.Helpers.remove_breaks/1` and on all modules by calling
  `IEx.Helpers.remove_breaks/0`.

  To exit a breakpoint, the developer can either invoke `continue()`,
  which will block the shell until the next breakpoint is found or
  the process terminates, or invoke `respawn()`, which starts a new IEx
  shell, freeing up the pried one.

  ## Examples

  The examples below will use `break!`, assuming that you are setting
  a breakpoint directly from your IEx shell. But you can set up a break
  from anywhere by using the fully qualified name `IEx.break!`.

  The following sets up a breakpoint on `URI.decode_query/2`:

      break! URI, :decode_query, 2

  This call will setup a breakpoint that stops once.
  To set a breakpoint that will stop 10 times:

      break! URI, :decode_query, 2, 10

  `IEx.break!/2` is a convenience macro that allows breakpoints
  to be given in the `Mod.fun/arity` format:

      break! URI.decode_query/2

  Or to set a breakpoint that will stop 10 times:

      break! URI.decode_query/2, 10

  This function returns the breakpoint ID and will raise if there
  is an error setting up the breakpoint.

  ## Patterns and guards

  `IEx.break!/2` allows patterns to be given, triggering the
  breakpoint only in some occasions. For example, to trigger
  the breakpoint only when the first argument is the "foo=bar"
  string:

      break! URI.decode_query("foo=bar", _)

  Or to trigger it whenever the second argument is a map with
  more than one element:

      break! URI.decode_query(_, map) when map_size(map) > 0

  Only a single break point can be set per function. So if you call
  `IEx.break!` multiple times with different patterns, only the last
  pattern is kept.

  Notice that, while patterns may be given to macros, remember that
  macros receive ASTs as arguments, and not values. For example, if
  you try to break on a macro with the following pattern:

      break! MyModule.some_macro(pid) when pid == self()

  This breakpoint will never be reached, because a macro never receives
  a PID. Even if you call the macro as `MyModule.some_macro(self())`,
  the macro will receive the AST representing the `self()` call, and not
  the PID itself.

  ## Breaks and `mix test`

  To use `IEx.break!/4` during tests, you need to run `mix` inside
  the `iex` command and pass the `--trace` to `mix test` to avoid running
  into timeouts:

      iex -S mix test --trace
      iex -S mix test path/to/file:line --trace

  """
  @doc since: "1.5.0"
  def break!(module, function, arity, stops \\ 1) when is_integer(arity) do
    IEx.Pry.break!(module, function, arity, stops)
  end

  ## Callbacks

  # This is a callback invoked by Erlang shell utilities
  # when someone presses Ctrl+G and adds 's Elixir.IEx'.
  @doc false
  def start(opts \\ [], mfa \\ {IEx, :dont_display_result, []}) do
    spawn(fn ->
      case :init.notify_when_started(self()) do
        :started -> :ok
        _ -> :init.wait_until_started()
      end

      :ok = start_iex()
      :ok = set_expand_fun()
      :ok = run_after_spawn()
      IEx.Server.run_from_shell(opts, mfa)
    end)
  end

  @doc false
  def dont_display_result, do: :"do not show this result in output"

  ## Helpers

  defp start_iex() do
    {:ok, _} = Application.ensure_all_started(:iex)
    :ok
  end

  defp set_expand_fun do
    gl = Process.group_leader()

    # expand_fun is not supported by a shell variant
    # on Windows, so we do two IO calls, not caring
    # about the result of the expand_fun one.
    _ = :io.setopts(gl, expand_fun: &IEx.Autocomplete.expand/1)
    :io.setopts(gl, binary: true, encoding: :unicode)
  end

  defp run_after_spawn do
    _ = for fun <- Enum.reverse(after_spawn()), do: fun.()
    :ok
  end
end
