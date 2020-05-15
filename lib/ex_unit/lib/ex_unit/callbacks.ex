defmodule ExUnit.Callbacks do
  @moduledoc ~S"""
  Defines ExUnit callbacks.

  This module defines the `setup/1`, `setup/2`, `setup_all/1`, and
  `setup_all/2` callbacks, as well as the `on_exit/2`, `start_supervised/2`
  and `stop_supervised/1` functions.

  The setup callbacks may be used to define [test fixtures](https://en.wikipedia.org/wiki/Test_fixture#Software)
  and run any initialization code which help bring the system into a known
  state. They are defined via macros and each one can optionally receive a map
  with test state and metadata, usually referred to as the `context`.
  Optionally, the context to be used in the tests can be extended by the
  setup callbacks by returning a properly structured value (see below).

  The `setup_all` callbacks are invoked only once per module, before any
  test is run. All `setup` callbacks are run before each test. No callback
  is run if the test case has no tests or all tests have been filtered out.

  `setup` and `setup_all` callbacks can be defined by a block, by passing
  an atom naming a one-arity function, or by passing a list of such
  atoms. Both can opt to receive the current context by specifying it
  as parameter if defined by a block. Functions used to define a test
  setup must accept the context as single argument.

  A test module can define multiple `setup` and `setup_all` callbacks,
  and they are invoked in order of appearance.

  `start_supervised/2` is used to start processes under a supervisor. The
  supervisor is linked to the current test process. The supervisor as well
  as all child processes are guaranteed to terminate before any `on_exit/2`
  callback runs.

  `on_exit/2` callbacks are registered on demand, usually to undo an action
  performed by a setup callback. `on_exit/2` may also take a reference,
  allowing the callback to be overridden in the future. A registered `on_exit/2`
  callback will always run, while failures in `setup` and `setup_all` will stop
  all remaining setup callbacks from executing.

  Finally, `setup_all` callbacks run in a separate process per module, while
  all `setup` callbacks run in the same process as the test itself. `on_exit/2`
  callbacks always run in a separate process, as implied by their name. The
  test process always exits with reason `:shutdown`, which means any process
  linked to the test process will also exit, although asynchronously. Therefore
  it is preferred to use `start_supervised/2` to guarantee synchronous termination.

  Here is a rundown of the life-cycle of the test process:

    1. the test process is spawned
    2. it runs `setup/2` callbacks
    3. it runs the test itself
    4. it stops all supervised processes
    5. the test process exits with reason `:shutdown`
    6. `on_exit/2` callbacks are executed in a separate process

  ## Context

  If `setup_all` or `setup` return a keyword list, a map, or a tuple in the shape
  of `{:ok, keyword() | map()}`, the keyword list or map will be merged into the
  current context and will be available in all subsequent `setup_all`,
  `setup`, and the `test` itself.

  Returning `:ok` leaves the context unchanged (in `setup` and `setup_all`
  callbacks).

  Returning anything else from `setup_all` will force all tests to fail,
  while a bad response from `setup` causes the current test to fail.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        # "setup_all" is called once per module before any test runs
        setup_all do
          IO.puts("Starting AssertionTest")

          # Context is not updated here
          :ok
        end

        # "setup" is called before each test
        setup do
          IO.puts("This is a setup callback for #{inspect(self())}")

          on_exit(fn ->
            IO.puts("This is invoked once the test is done. Process: #{inspect(self())}")
          end)

          # Returns extra metadata to be merged into context.
          # Any of the following would also work:
          #
          #     {:ok, %{hello: "world"}}
          #     {:ok, [hello: "world"]}
          #     %{hello: "world"}
          #
          [hello: "world"]
        end

        # Same as above, but receives the context as argument
        setup context do
          IO.puts("Setting up: #{context.test}")

          # We can simply return :ok when we don't want add any extra metadata
          :ok
        end

        # Setups can also invoke a local or imported function that returns a context
        setup :invoke_local_or_imported_function

        test "always pass" do
          assert true
        end

        test "uses metadata from setup", context do
          assert context[:hello] == "world"
          assert context[:from_named_setup] == true
        end

        defp invoke_local_or_imported_function(context) do
          [from_named_setup: true]
        end
      end

  It is also common to define your setup as a series of functions,
  which are put together by calling `setup` or `setup_all` with a
  list of atoms. Each of these functions receive the context and can
  return any of the values allowed in `setup` blocks:

      defmodule ExampleContextTest do
        use ExUnit.Case

        setup [:step1, :step2, :step3]

        defp step1(_context), do: [step_one: true]
        defp step2(_context), do: {:ok, step_two: true} # return values with shape of {:ok, keyword() | map()} allowed
        defp step3(_context), do: :ok  # Context not modified

        test "context was modified", context do
          assert context[:step_one] == true
          assert context[:step_two] == true
        end
      end

  Finally, as discussed in the `ExUnit.Case` documentation, remember
  that the initial context metadata can also be set via `@tag`s, which
  can then be accessed in the `setup` block:

      defmodule ExampleTagModificationTest do
        use ExUnit.Case

        setup %{login_as: username} do
          {:ok, current_user: username}
        end

        @tag login_as: "max"
        test "tags modify context", context do
          assert context[:login_as] == "max"
          assert context[:current_user] == "max"
        end
      end
  """

  @doc false
  defmacro __using__(_) do
    quote do
      @ex_unit_describe nil
      @ex_unit_setup []
      @ex_unit_setup_all []

      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__)
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    [compile_callbacks(env, :setup), compile_callbacks(env, :setup_all)]
  end

  @doc """
  Defines a callback to be run before each test in a case.

  Accepts a block or the name of a one-arity function in the form of an atom,
  or a list of such atoms.

  Can return values to be merged into the context, to set up the state for
  tests. For more details, see the "Context" section shown above.

  `setup/1` callbacks are executed in the same process as the test process.

  ## Examples

      def clean_up_tmp_directory(context) do
        # perform setup
        :ok
      end

      setup :clean_up_tmp_directory

      setup do
        [conn: Plug.Conn.build_conn()]
      end

  """
  defmacro setup(block) do
    if Keyword.keyword?(block) do
      do_setup(quote(do: _), block)
    else
      quote do
        @ex_unit_setup ExUnit.Callbacks.__callback__(unquote(block), @ex_unit_describe) ++
                         @ex_unit_setup
      end
    end
  end

  @doc """
  Defines a callback to be run before each test in a case.

  Accepts a block or the name of a one-arity function in the form of an atom,
  or a list of such atoms.

  Can return values to be merged into the `context`, to set up the state for
  tests. For more details, see the "Context" section shown above.

  ## Examples

      setup context do
        [conn: Plug.Conn.build_conn()]
      end

  """
  defmacro setup(context, block) do
    do_setup(context, block)
  end

  defp do_setup(context, block) do
    quote bind_quoted: [context: escape(context), block: escape(block)] do
      name = :"__ex_unit_setup_#{length(@ex_unit_setup)}"
      defp unquote(name)(unquote(context)), unquote(block)
      @ex_unit_setup [{name, @ex_unit_describe} | @ex_unit_setup]
    end
  end

  @doc """
  Defines a callback to be run before all tests in a case.

  Accepts a block or the name of a one-arity function in the form of an atom,
  or a list of such atoms.

  Can return values to be merged into the `context`, to set up the state for
  tests. For more details, see the "Context" section shown above.

  `setup_all/1` callbacks are executed in a separate process than tests.
  All `setup_all/1` callbacks are executed in order in the same process.

  ## Examples

      # One-arity function name
      setup_all :clean_up_tmp_directory

      def clean_up_tmp_directory(_context) do
        # perform setup
        :ok
      end

      # Block
      setup_all do
        [conn: Plug.Conn.build_conn()]
      end

  The context returned by `setup_all/1` will be available in all subsequent
  `setup_all`, `setup`, and the `test` itself. For instance, the `conn` from
  the previous example can be accessed as:

      test "fetches current users", %{conn: conn} do
        # ...
      end

  """
  defmacro setup_all(block) do
    if Keyword.keyword?(block) do
      do_setup_all(quote(do: _), block)
    else
      quote do
        @ex_unit_describe &&
          raise "cannot invoke setup_all/1 inside describe as setup_all/1 " <>
                  "always applies to all tests in a module"

        @ex_unit_setup_all ExUnit.Callbacks.__callback__(unquote(block), nil) ++
                             @ex_unit_setup_all
      end
    end
  end

  @doc """
  Defines a callback to be run before all tests in a case.

  Same as `setup_all/1` but also takes a context. See
  the "Context" section in the module documentation.

  ## Examples

      setup_all _context do
        [conn: Plug.Conn.build_conn()]
      end

  """
  defmacro setup_all(context, block) do
    do_setup_all(context, block)
  end

  defp do_setup_all(context, block) do
    quote bind_quoted: [context: escape(context), block: escape(block)] do
      @ex_unit_describe && raise "cannot invoke setup_all/2 inside describe"
      name = :"__ex_unit_setup_all_#{length(@ex_unit_setup_all)}"
      defp unquote(name)(unquote(context)), unquote(block)
      @ex_unit_setup_all [{name, nil} | @ex_unit_setup_all]
    end
  end

  @doc """
  Defines a callback that runs once the test exits.

  `callback` is a function that receives no arguments and
  runs in a separate process than the caller.

  `on_exit/2` is usually called from `setup` and `setup_all`
  callbacks, often to undo the action performed during the setup.
  However, `on_exit/2` may also be called dynamically, where a
  reference can be used to guarantee the callback will be invoked
  only once.

  `on_exit/2` gets executed in a blocking fashion after a test
  exits and **before** running the next test. This means that no
  other test from the same test case will be running while the
  `on_exit/2` callback for a previous test is running.

  `on_exit/2` is executed in a different process than the test
  process.

  ## Examples

      setup do
        File.write!("fixture.json", "{}")
        on_exit(fn -> File.rm!("fixture.json") end)
      end

  """
  @spec on_exit(term, (() -> term)) :: :ok
  def on_exit(name_or_ref \\ make_ref(), callback) when is_function(callback, 0) do
    case ExUnit.OnExitHandler.add(self(), name_or_ref, callback) do
      :ok ->
        :ok

      :error ->
        raise ArgumentError, "on_exit/2 callback can only be invoked from the test process"
    end
  end

  @doc """
  Starts a child process under the test supervisor.

  It expects a child specification or a module, similar to the ones
  given to `Supervisor.start_link/2`. For example, if your application
  starts a supervision tree by running:

      Supervisor.start_link([MyServer, {OtherSupervisor, ...}], ...)

  You can start those processes under test in isolation by running:

      start_supervised(MyServer)
      start_supervised({OtherSupervisor, :initial_value})

  A keyword list can also be given if there is a need to change
  the child specification for the given child process:

      start_supervised({MyServer, :initial_value}, restart: :temporary)

  See the `Supervisor` module for a discussion on child specifications
  and the available specification keys.

  The advantage of starting a process under the test supervisor is that
  it is guaranteed to exit before the next test starts. Therefore, you
  don't need to remove the process at the end of your tests via
  `stop_supervised/1`. You only need to use `stop_supervised/1 ` if you
  want to remove a process from the supervision tree in the middle of a
  test, as simply shutting down the process would cause it to be restarted
  according to its `:restart` value.

  This function returns `{:ok, pid}` in case of success, otherwise it
  returns `{:error, reason}`.
  """
  @doc since: "1.5.0"
  @spec start_supervised(Supervisor.child_spec() | module | {module, term}, keyword) ::
          Supervisor.on_start_child()
  def start_supervised(child_spec_or_module, opts \\ []) do
    sup =
      case ExUnit.fetch_test_supervisor() do
        {:ok, sup} ->
          sup

        :error ->
          raise ArgumentError, "start_supervised/2 can only be invoked from the test process"
      end

    child_spec = Supervisor.child_spec(child_spec_or_module, opts)

    case Supervisor.start_child(sup, child_spec) do
      {:error, {:already_started, _pid}} ->
        {:error, {:duplicate_child_name, child_spec.id}}

      other ->
        other
    end
  end

  @doc """
  Same as `start_supervised/2` but returns the PID on success and raises if
  not started properly.
  """
  @doc since: "1.6.0"
  @spec start_supervised!(Supervisor.child_spec() | module | {module, term}, keyword) :: pid
  def start_supervised!(child_spec_or_module, opts \\ []) do
    case start_supervised(child_spec_or_module, opts) do
      {:ok, pid} ->
        pid

      {:ok, pid, _info} ->
        pid

      {:error, reason} ->
        raise "failed to start child with the spec #{inspect(child_spec_or_module)}.\n" <>
                "Reason: #{start_supervised_error(reason)}"
    end
  end

  defp start_supervised_error({{:EXIT, reason}, info}) when is_tuple(info),
    do: Exception.format_exit(reason)

  defp start_supervised_error({reason, info}) when is_tuple(info),
    do: Exception.format_exit(reason)

  defp start_supervised_error(reason), do: Exception.format_exit({:start_spec, reason})

  @doc """
  Stops a child process started via `start_supervised/2`.

  This function expects the `id` in the child specification.
  For example:

      {:ok, _} = start_supervised(MyServer)
      :ok = stop_supervised(MyServer)

  It returns `:ok` if there is a supervised process with such
  `id`, `{:error, :not_found}` otherwise.
  """
  @doc since: "1.5.0"
  @spec stop_supervised(id :: term()) :: :ok | {:error, :not_found}
  def stop_supervised(id) do
    case ExUnit.OnExitHandler.get_supervisor(self()) do
      {:ok, nil} ->
        {:error, :not_found}

      {:ok, sup} ->
        with :ok <- Supervisor.terminate_child(sup, id),
             :ok <- Supervisor.delete_child(sup, id),
             do: :ok

      :error ->
        raise ArgumentError, "stop_supervised/1 can only be invoked from the test process"
    end
  end

  @doc """
  Same as `stop_supervised/1` but raises if it cannot be stopped.
  """
  @doc since: "1.10.0"
  @spec stop_supervised!(id :: term()) :: :ok
  def stop_supervised!(id) do
    case stop_supervised(id) do
      :ok ->
        :ok

      {:error, :not_found} ->
        raise "could not stop child ID #{inspect(id)} because it was not found"
    end
  end

  ## Helpers

  @reserved [:case, :file, :line, :test, :async, :registered, :describe]

  @doc false
  def __callback__(callback, describe) do
    for k <- List.wrap(callback) do
      if not is_atom(k) do
        raise ArgumentError,
              "setup/setup_all expect a callback name as an atom or " <>
                "a list of callback names, got: #{inspect(k)}"
      end

      {k, describe}
    end
    |> Enum.reverse()
  end

  @doc false
  def __merge__(mod, context, value) do
    merge(mod, context, value, value)
  end

  defp merge(_mod, context, :ok, _original_value) do
    context
  end

  defp merge(mod, context, {:ok, value}, original_value) do
    merge(mod, context, value, original_value)
  end

  defp merge(mod, _context, %_{}, original_value) do
    raise_merge_failed!(mod, original_value)
  end

  defp merge(mod, context, data, original_value) when is_list(data) do
    merge(mod, context, Map.new(data), original_value)
  end

  defp merge(mod, context, data, _original_value) when is_map(data) do
    context_merge(mod, context, data)
  end

  defp merge(mod, _, _return_value, original_value) do
    raise_merge_failed!(mod, original_value)
  end

  defp context_merge(mod, context, data) do
    Map.merge(context, data, fn
      k, v1, v2 when k in @reserved ->
        if v1 == v2, do: v1, else: raise_merge_reserved!(mod, k, v2)

      _, _, v ->
        v
    end)
  end

  defp raise_merge_failed!(mod, return_value) do
    raise "expected ExUnit callback in #{inspect(mod)} to return :ok | keyword | map, " <>
            "got #{inspect(return_value)} instead"
  end

  defp raise_merge_reserved!(mod, key, value) do
    raise "ExUnit callback in #{inspect(mod)} is trying to set " <>
            "reserved field #{inspect(key)} to #{inspect(value)}"
  end

  defp escape(contents) do
    Macro.escape(contents, unquote: true)
  end

  defp compile_callbacks(env, kind) do
    callbacks = Module.get_attribute(env.module, :"ex_unit_#{kind}") |> Enum.reverse()

    acc =
      case callbacks do
        [] ->
          quote(do: context)

        [h | t] ->
          Enum.reduce(t, compile_merge(h), fn callback_describe, acc ->
            quote do
              context = unquote(acc)
              unquote(compile_merge(callback_describe))
            end
          end)
      end

    quote do
      def __ex_unit__(unquote(kind), context) do
        describe = Map.get(context, :describe, nil)
        unquote(acc)
      end
    end
  end

  defp compile_merge({callback, nil}) do
    quote do
      unquote(__MODULE__).__merge__(__MODULE__, context, unquote(callback)(context))
    end
  end

  defp compile_merge({callback, {_line, describe}}) do
    quote do
      if unquote(describe) == describe do
        unquote(compile_merge({callback, nil}))
      else
        context
      end
    end
  end
end
