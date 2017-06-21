defmodule Mix.Task.Compiler do
  @moduledoc """
  This module defines the behaviour for a Mix task that does compilation.

  A Mix compiler task can be defined by simply using `Mix.Task.Compiler`
  in a module whose name starts with `Mix.Tasks.Compile.` and defining
  the `run/1` function:

      defmodule Mix.Tasks.Compile.MyLanguage do
        use Mix.Task.Compiler

        def run(_args) do
          :ok
        end
      end

  If the compiler uses manifest files to track stale sources, it should
  define `manifests/0`, and if it writes any output to disk it should
  also define `clean/0`.

  ## Attributes

  The following attributes are used the same way as in other Mix tasks:

    * `@shortdoc`  - makes the task public with a short description that appears on `mix help`
    * `@recursive` - runs the task recursively in umbrella projects

  """

  @doc """
  Receives command-line arguments and performs compilation. Returns `:noop`
  if nothing is stale and no compilation is needed, `:ok` if successful.
  """
  @callback run([binary]) :: :ok | :noop

  @doc """
  Lists manifest files for the compiler.
  """
  @callback manifests() :: [Path.t]

  @doc """
  Removes build artifacts and manifests.
  """
  @callback clean() :: term

  @doc false
  defmacro __using__(_opts) do
    quote do
      Enum.each [:shortdoc, :recursive],
        &Module.register_attribute(__MODULE__, &1, persist: true)
      @behaviour Mix.Task.Compiler

      def clean, do: :noop

      def manifests, do: []

      defoverridable(clean: 0, manifests: 0)
    end
  end
end
