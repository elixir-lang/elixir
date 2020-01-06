defmodule Code.Tracer do
  @moduledoc ~S"""
  Behaviour for compilation tracers.

  Elixir supports compilation tracers, which allows modules to observe constructs
  handled by the Elixir compiler when compiling files. A tracer is a module
  that implements the `c:trace/2` function. The function receives the event name
  as first argument and `Macro.Env` as second and it must return `:ok`. It is
  very important for a tracer to do as little work as possible synchronously
  and dispatch the bulk of the work to a separate process. **Slow tracers will
  slow down compilation**.

  You can configure your list of tracers via `Code.put_compiler_option/2`. The
  following events are available to tracers:

    * `{:import, meta, module, opts}` - traced whenever `module` is imported.
      `meta` is the import AST metadata and `opts` are the import options.

    * `{:imported_function, meta, module, name, arity}` and
      `{:imported_macro, meta, module, name, arity}` - traced whenever an
      imported function or macro is invoked. `meta` is the call AST metadata,
      `module` is the module the import is from, followed by the `name` and `arity`
      of the imported function/macro.

    * `{:alias, meta, alias, as, opts}` - traced whenever `alias` is aliased
      to `as`. `meta` is the alias AST metadata and `opts` are the alias options.

    * `{:alias_expansion, meta, as, alias}` traced whenever there is an alias
      expansion for a previously defined `alias`, i.e. when the user writes `as`
      which is expanded to `alias`. `meta` is the alias expansion AST metadata.

    * `{:alias_reference, meta, module}` - traced whenever there is an alias
      in the code, i.e. whenever the user writes `MyModule.Foo.Bar` in the code,
      regardless if it was expanded or not.

    * `{:require, meta, module, opts}` - traced whenever `module` is required.
      `meta` is the require AST metadata and `opts` are the require options.

    * `{:struct_expansion, meta, module, keys}` - traced whenever `module`'s struct
      is expanded. `meta` is the struct AST metadata and `keys` are the keys being
      used by expansion

    * `{:remote_function, meta, module, name, arity}` and
      `{:remote_macro, meta, module, name, arity}` - traced whenever a remote
      function or macro is referenced. `meta` is the call AST metadata, `module`
      is the invoked module, followed by the `name` and `arity`.

    * `{:local_function, meta, module, name, arity}` and
      `{:local_macro, meta, module, name, arity}` - traced whenever a local
      function or macro is referenced. `meta` is the call AST metadata, `module`
      is the invoked module, followed by the `name` and `arity`.

    * `{:compile_env, app, path, return}` - traced whenever `Application.compile_env/3`
      or `Application.compile_env!/2` are called. `app` is an atom, `path` is a list
      of keys to traverse in the application environemnt and `return` is either
      `{:ok, value}` or `:error`.

  The `:tracers` compiler option can be combined with the `:parser_options`
  compiler option to enrich the metadata of the traced events above.

  New events may be added at any time in the future, therefore it is advised
  for the `c:trace/2` function to have a "catch-all" clause.

  Below is an example tracer that prints all remote function invocations:

      defmodule MyTracer do
        @behaviour Code.Tracer

        @impl true
        def trace({:remote_function, _meta, module, name, arity}, env) do
          IO.puts "#{env.file}:#{env.line} #{inspect(module)}.#{name}/#{arity}"
          :ok
        end

        def trace(_event, _env) do
          :ok
        end
      end
  """

  @callback trace(term, Macro.Env.t()) :: :ok
end
