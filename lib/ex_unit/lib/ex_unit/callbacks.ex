defmodule ExUnit.Callbacks do
  @moduledoc ~S"""
  Defines ExUnit callbacks.

  This module defines both `setup_all` and `setup` callbacks, as well as
  the `on_exit/2`, `start_supervised/2` and `stop_supervised/1` functions.

  The setup callbacks are defined via macros and each one can optionally
  receive a map with metadata, usually referred to as `context`. The
  callback may optionally put extra data into the `context` to be used in
  the tests.

  The `setup_all` callbacks are invoked only once per module, before any
  test runs. All `setup` callbacks are run before each test. No callback
  runs if the test case has no tests or all tests have been filtered out.

  `start_supervised/2` is used to start processes under a supervisor. The
  supervisor is linked to the current test process. The supervisor as well
  as all child processes are guaranteed to terminate before any `on_exit/2`
  callback runs.

  `on_exit/2` callbacks are registered on demand, usually to undo an action
  performed by a setup callback. `on_exit/2` may also take a reference,
  allowing callback to be overridden in the future. A registered `on_exit/2`
  callback always runs, while failures in `setup` and `setup_all` will stop
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

  If you return a keyword list, a map, or `{:ok, keywords | map}` from
  `setup_all`, the keyword list/map will be merged into the current context and
  be available in all subsequent `setup_all`, `setup`, and the `test` itself.

  Similarly, returning a keyword list, map, or `{:ok, keywords | map}` from
  `setup` means that the returned keyword list/map will be merged into the
  current context and be available in all subsequent `setup` and the `test`
  itself.

  Returning `:ok` leaves the context unchanged (both in `setup` and `setup_all`
  callbacks).

  Returning anything else from `setup_all` will force all tests to fail,
  while a bad response from `setup` causes the current test to fail.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        # "setup_all" is called once per module before any test runs
        setup_all do
          IO.puts "Starting AssertionTest"

          # No context is returned here
          :ok
        end

        # "setup" is called before each test
        setup do
          IO.puts "This is a setup callback for #{inspect self()}"

          on_exit fn ->
            IO.puts "This is invoked once the test is done. Process: #{inspect self()}"
          end

          # Returns extra metadata to be merged into context
          [hello: "world"]
        end

        # Same as above, but receives the context as argument
        setup context do
          IO.puts "Setting up: #{context.test}"
          :ok
        end

        # Setups can also invoke a local or imported function that returns a context
        setup :invoke_local_or_imported_function

        test "always pass" do
          assert true
        end

        test "uses metadata from setup", context do
          assert context[:hello] == "world"
        end

        defp invoke_local_or_imported_function(context) do
          [from_named_setup: true]
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
    [compile_callbacks(env, :setup),
     compile_callbacks(env, :setup_all)]
  end

  @doc """
  Defines a callback to be run before each test in a case.

  ## Examples

      setup :clean_up_tmp_directory

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

  ## Examples

      setup context do
        [conn: Plug.Conn.build_conn()]
      end

  """
  defmacro setup(var, block) do
    do_setup(var, block)
  end

  defp do_setup(var, block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      name = :"__ex_unit_setup_#{length(@ex_unit_setup)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_setup [{name, @ex_unit_describe} | @ex_unit_setup]
    end
  end

  @doc """
  Defines a callback to be run before all tests in a case.

  ## Examples

      setup_all :clean_up_tmp_directory

  """
  defmacro setup_all(block) do
    if Keyword.keyword?(block) do
      do_setup_all(quote(do: _), block)
    else
      quote do
        @ex_unit_describe && raise "cannot invoke setup_all/1 inside describe as setup_all/1 " <>
                                   "always applies to all tests in a module"
        @ex_unit_setup_all ExUnit.Callbacks.__callback__(unquote(block), nil) ++
                           @ex_unit_setup_all
      end
    end
  end

  @doc """
  Defines a callback to be run before all tests in a case.

  ## Examples

      setup_all context do
        [conn: Plug.Conn.build_conn()]
      end

  """
  defmacro setup_all(var, block) do
    do_setup_all(var, block)
  end

  defp do_setup_all(var, block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      @ex_unit_describe && raise "cannot invoke setup_all/2 inside describe"
      name = :"__ex_unit_setup_all_#{length(@ex_unit_setup_all)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_setup_all [{name, nil} | @ex_unit_setup_all]
    end
  end

  @doc """
  Defines a callback that runs once the test exits.

  `callback` is a function that receives no arguments and
  runs in a separate process than the caller.

  `on_exit/2` is usually called from `setup` and `setup_all`
  callbacks, often to undo the action performed during `setup`.
  However, `on_exit/2` may also be called dynamically, where a
  reference can be used to guarantee the callback will be invoked
  only once.
  """
  @spec on_exit(term, (() -> term)) :: :ok | no_return
  def on_exit(name_or_ref \\ make_ref(), callback) when is_function(callback, 0) do
    case ExUnit.OnExitHandler.add(self(), name_or_ref, callback) do
      :ok -> :ok
      :error ->
        raise ArgumentError, "on_exit/2 callback can only be invoked from the test process"
    end
  end

  @supervisor_opts [strategy: :one_for_one, max_restarts: 1_000_000, max_seconds: 1]

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
  it is guaranteed to exit before the next test starts. Furthermore,
  because the child process is supervised, it will be restarted in case
  of crashes according to the `:restart` strategy in the child
  specification, even if stopped manually. Therefore, to guarantee a
  process started with `start_supervised/2` terminates without restarts,
  see `stop_supervised/1`.

  This function returns `{:ok, pid}` in case of success, otherwise it
  returns `{:error, reason}`.
  """
  @spec start_supervised(Supervisor.child_spec | module | {module, term}, keyword) ::
        Supervisor.on_start_child
  def start_supervised(child_spec_or_module, opts \\ []) do
    sup =
      case ExUnit.OnExitHandler.get_supervisor(self()) do
        {:ok, nil} ->
          {:ok, sup} = Supervisor.start_link([], @supervisor_opts)
          ExUnit.OnExitHandler.put_supervisor(self(), sup)
          sup
        {:ok, sup} ->
          sup
        :error ->
          raise ArgumentError, "start_supervised/2 can only be invoked from the test process"
      end

    Supervisor.start_child(sup, Supervisor.child_spec(child_spec_or_module, opts))
  end

  @doc """
  Same as `start_supervised/2` but returns the PID on success and raises if
  not started porperly.
  """
  @spec start_supervised!(Supervisor.child_spec | module | {module, term}, keyword) :: pid
  def start_supervised!(child_spec_or_module, opts \\ []) do
    case start_supervised(child_spec_or_module, opts) do
      {:ok, pid} ->
        pid
      {:ok, pid, _info} ->
        pid
      {:error, reason} ->
        raise """
        Failed to start child:

        #{format_start_error_reason(reason)}
        """
    end
  end

  defp format_start_error_reason({{:EXIT, reason}, info}),
    do: format_start_error_reason(reason, info)
  defp format_start_error_reason({reason, info}),
    do: format_start_error_reason(reason, info)
  defp format_start_error_reason(reason),
    do: Exception.format_exit(reason)

  defp format_start_error_reason(reason, info) do
    inspect_info(info) <> Exception.format_exit(reason)
  end

  defp inspect_info({_, _, _, {m, f, a}, _, _, _, _})
       when is_atom(m) and is_atom(f) and (is_integer(a) or is_list(a)),
    do: Exception.format_mfa(m, f, a) <> "\n"
  defp inspect_info(_),
    do: ""

  @doc """
  Stops a child process started via `start_supervised/2`.

  This function expects the `id` in the child specification.
  For example:

      {:ok, _} = start_supervised(MyServer)
      :ok = stop_supervised(MyServer)

  It returns `:ok` if there is a supervised process with such
  `id`, `{:error, :not_found}` otherwise.
  """
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

  ## Helpers

  @reserved [:case, :file, :line, :test, :async, :registered, :describe]

  @doc false
  def __callback__(callback, describe) do
    for k <- List.wrap(callback) do
      if not is_atom(k) do
        raise ArgumentError, "setup/setup_all expect a callback name as an atom or " <>
                             "a list of callback names, got: #{inspect k}"
      end

      {k, describe}
    end |> Enum.reverse()
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
    raise "expected ExUnit callback in #{inspect mod} to return :ok | keyword | map, " <>
          "got #{inspect return_value} instead"
  end

  defp raise_merge_reserved!(mod, key, value) do
    raise "ExUnit callback in #{inspect mod} is trying to set " <>
          "reserved field #{inspect key} to #{inspect value}"
  end

  defp escape(contents) do
    Macro.escape(contents, unquote: true)
  end

  defp compile_callbacks(env, kind) do
    callbacks = Module.get_attribute(env.module, :"ex_unit_#{kind}") |> Enum.reverse

    acc =
      case callbacks do
        [] ->
          quote do: context
        [h | t] ->
          Enum.reduce t, compile_merge(h), fn callback_describe, acc ->
            quote do
              context = unquote(acc)
              unquote(compile_merge(callback_describe))
            end
          end
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

  defp compile_merge({callback, describe}) do
    quote do
      if unquote(describe) == describe do
        unquote(compile_merge({callback, nil}))
      else
        context
      end
    end
  end
end
