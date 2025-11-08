# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Help do
  use Mix.Task

  @shortdoc "Prints help information for tasks, aliases, modules, and applications"

  @moduledoc """
  Prints documentation for tasks, aliases, modules, and applications.

  ## Examples

  Without an explicit argument, this task lists tasks/aliases:

      $ mix help                  - prints all aliases, tasks and their short descriptions
      $ mix help --search PATTERN - prints all tasks and aliases that contain PATTERN in the name
      $ mix help --names          - prints all task names and aliases (useful for autocompletion)
      $ mix help --aliases        - prints all aliases

  You can access documentation for a given task/alias:

      $ mix help TASK/ALIAS       - prints full docs for the given task/alias

  But also for modules, functions, and applications:

      $ mix help MODULE           - prints the definition for the given module
      $ mix help MODULE.FUN       - prints the definition for the given module+function
      $ mix help app:APP          - prints a summary of all public modules in application

  ## Colors

  When possible, `mix help` is going to use coloring for formatting
  the help information. The formatting can be customized by configuring
  the Mix application either inside your project (in `config/config.exs`)
  or by using the local config (in `~/.mix/config.exs`).

  For example, to disable color, one may use the configuration:

      [mix: [colors: [enabled: false]]]

  The available color options are:

    * `:enabled`         - shows ANSI formatting (defaults to `IO.ANSI.enabled?/0`)
    * `:doc_code`        - the attributes for code blocks (cyan, bright)
    * `:doc_inline_code` - inline code (cyan)
    * `:doc_headings`    - h1 and h2 (yellow, bright)
    * `:doc_title`       - the overall heading for the output (reverse, yellow, bright)
    * `:doc_bold`        - (bright)
    * `:doc_underline`   - (underline)

  """

  @impl true
  def run(argv)

  def run([]) do
    loadpaths!()

    modules = load_tasks()
    aliases = load_aliases()

    {docs, max} = build_doc_list(modules, aliases)

    display_default_task_doc(max)
    display_doc_list(docs, max)
    display_iex_task_doc(max)
    Mix.shell().info("\nUse \"mix help <TASK>\" for more information on a particular command.")
  end

  def run(["--names"]) do
    loadpaths!()

    tasks = Enum.map(load_tasks(), &Mix.Task.task_name/1)

    aliases =
      Enum.map(Mix.Project.config()[:aliases], fn {alias_name, _} ->
        Atom.to_string(alias_name)
      end)

    for info <- Enum.sort(aliases ++ tasks) do
      Mix.shell().info(info)
    end
  end

  def run(["--aliases"]) do
    loadpaths!()

    aliases = load_aliases()

    {docs, max} = build_doc_list([], aliases)

    display_doc_list(docs, max)
  end

  def run(["--search", pattern]) do
    loadpaths!()

    modules = Enum.filter(load_tasks(), &String.contains?(Mix.Task.task_name(&1), pattern))
    aliases = Enum.filter(load_aliases(), fn {name, _} -> String.contains?(name, pattern) end)

    {docs, max} = build_doc_list(modules, aliases)
    display_doc_list(docs, max)
  end

  def run(["--search"]) do
    Mix.raise("Unexpected arguments, expected \"mix help --search PATTERN\"")
  end

  @compile {:no_warn_undefined, IEx.Introspection}

  def run(["app:" <> app]) do
    loadpaths!()
    app = String.to_atom(app)
    Mix.ensure_application!(app)

    if modules = Application.spec(app, :modules) do
      for module <- modules,
          not (module |> Atom.to_string() |> String.starts_with?("Elixir.Mix.Tasks.")),
          {:docs_v1, _, _, "text/markdown", %{"en" => <<doc::binary>>}, _, _} <-
            [Code.fetch_docs(module)] do
        leading = doc |> String.split(["\n\n", "\r\n\r\n"], parts: 2) |> hd()
        "# #{inspect(module)}\n#{leading}\n"
      end
      |> case do
        [] ->
          Mix.shell().error("No modules with accessible documentation found for #{app}")

        listing ->
          docs = listing |> Enum.sort() |> Enum.join()
          IO.ANSI.Docs.print(docs, "text/markdown", ansi_opts())
      end
    else
      Mix.shell().error("Application #{app} does not exist or is not loaded")
    end
  end

  def run([module = <<first, _::binary>>]) when first in ?A..?Z or first == ?: do
    loadpaths!()

    iex_colors = Application.get_env(:iex, :colors, [])
    mix_colors = Application.get_env(:mix, :colors, [])

    try do
      Application.put_env(:iex, :colors, mix_colors)

      module
      |> Code.string_to_quoted!()
      |> IEx.Introspection.decompose(__ENV__)
      |> case do
        :error -> Mix.raise("Invalid expression: #{module}")
        decomposition -> decomposition
      end
      |> IEx.Introspection.h()
    after
      Application.put_env(:iex, :colors, iex_colors)
    end
  end

  def run([task]) do
    loadpaths!()
    opts = ansi_opts()

    for doc <- verbose_doc(task) do
      print_doc(task, doc, opts)
    end

    :ok
  end

  def run(_) do
    Mix.raise("Unexpected arguments, expected \"mix help\" or \"mix help TASK\"")
  end

  defp ansi_opts do
    opts = Application.get_env(:mix, :colors)
    [width: width(), enabled: ansi_docs?(opts)] ++ opts
  end

  defp print_doc(task, {doc, location, note}, opts) do
    IO.ANSI.Docs.print_headings(["mix #{task}"], opts)
    IO.ANSI.Docs.print(doc, "text/markdown", opts)
    IO.puts("Location: #{location}")
    note && IO.puts("") && IO.ANSI.Docs.print(note, "text/markdown", opts)
  end

  # Loadpaths without checks because tasks may be defined in deps.
  defp loadpaths! do
    args = [
      "--no-elixir-version-check",
      "--no-deps-check",
      "--no-archives-check",
      "--no-listeners"
    ]

    Mix.Task.run("loadpaths", args)
    Mix.Task.reenable("loadpaths")
    Mix.Task.reenable("deps.loadpaths")
  end

  defp load_tasks() do
    Enum.filter(Mix.Task.load_all(), &(Mix.Task.moduledoc(&1) != false))
  end

  defp load_aliases() do
    aliases = Mix.Project.config()[:aliases]

    Map.new(aliases, fn {alias_name, alias_tasks} -> {Atom.to_string(alias_name), alias_tasks} end)
  end

  defp ansi_docs?(opts) do
    Keyword.get(opts, :enabled, IO.ANSI.enabled?())
  end

  defp width() do
    case :io.columns() do
      {:ok, width} -> min(width, 80)
      {:error, _} -> 80
    end
  end

  defp format_task(task, max, doc) do
    String.pad_trailing(task, max) <> " # " <> doc
  end

  defp where_is_file(module) do
    case :code.where_is_file(Atom.to_charlist(module) ++ ~c".beam") do
      :non_existing ->
        "not available"

      location ->
        location
        |> Path.dirname()
        |> Path.expand()
        |> Path.relative_to_cwd()
    end
  end

  defp display_default_task_doc(max) do
    project = Mix.Project.get()

    default =
      if function_exported?(project, :cli, 0) do
        project.cli()[:default_task] || "run"
      else
        "run"
      end

    message = "Runs the default task (current: \"mix #{default}\")"
    Mix.shell().info(format_task("mix", max, message))
  end

  defp display_iex_task_doc(max) do
    Mix.shell().info(format_task("iex -S mix", max, "Starts IEx and runs the default task"))
  end

  defp display_doc_list(docs, max) do
    Enum.each(Enum.sort(docs), fn {task, doc} ->
      Mix.shell().info(format_task(task, max, doc))
    end)
  end

  defp build_doc_list(modules, aliases) do
    {task_docs, task_max} = build_task_doc_list(modules)
    {alias_docs, alias_max} = build_alias_doc_list(aliases)
    {task_docs ++ alias_docs, max(task_max, alias_max)}
  end

  defp build_task_doc_list(modules) do
    Enum.reduce(modules, {[], 0}, fn module, {docs, max} ->
      if doc = Mix.Task.shortdoc(module) do
        task = "mix " <> Mix.Task.task_name(module)
        {[{task, doc} | docs], max(byte_size(task), max)}
      else
        {docs, max}
      end
    end)
  end

  defp build_alias_doc_list(aliases) do
    Enum.reduce(aliases, {[], 0}, fn {alias_name, task}, {docs, max} ->
      doc = alias_doc(task)
      task = "mix " <> alias_name
      {[{task, doc} | docs], max(byte_size(task), max)}
    end)
  end

  defp alias_doc(task) do
    "Alias for " <> format_alias_doc(task)
  end

  defp format_alias_doc(task), do: Enum.map_join(List.wrap(task), ", ", &format_alias_task/1)

  defp format_alias_task(task) when is_binary(task), do: task

  defp format_alias_task(task) when is_function(task) do
    info = Function.info(task)
    name = Atom.to_string(info[:name])

    cond do
      info[:type] == :remote -> inspect(task)
      info[:type] == :local and String.contains?(name, "/") -> "a function"
      true -> "&#{name}/#{info[:arity]}"
    end
  end

  # for invalid aliases
  defp format_alias_task(task), do: inspect(task)

  defp verbose_doc(task) do
    aliases = load_aliases()

    has_alias? = Map.has_key?(aliases, task)
    has_task? = Mix.Task.get(task)

    cond do
      has_alias? and has_task? ->
        note = "There is also a task named \"#{task}\". The documentation is shown next."
        [alias_doc(aliases[task], note), task_doc(task)]

      has_alias? ->
        [alias_doc(aliases[task], nil)]

      true ->
        [task_doc(task)]
    end
  end

  defp alias_doc(task_name, note) do
    alias_doc = """
    Alias for

        #{format_alias(task_name)}
    """

    {alias_doc, "mix.exs", note}
  end

  defp format_alias(task) do
    inspect(task, pretty: true, width: 0)
    |> String.replace("\n", "\n    ")
  end

  defp task_doc(task) do
    module = Mix.Task.get!(task)
    doc = Mix.Task.moduledoc(module) || "There is no documentation for this task"
    {doc, where_is_file(module), nil}
  end
end
