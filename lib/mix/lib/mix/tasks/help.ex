defmodule Mix.Tasks.Help do
  use Mix.Task

  @shortdoc "Prints help information for tasks"

  @moduledoc """
  Lists all tasks and aliases or prints the documentation for a given task or alias.

  ## Arguments

      mix help                  - prints all aliases, tasks and their short descriptions
      mix help ALIAS            - prints the definition for the given alias
      mix help TASK             - prints full docs for the given task
      mix help --search PATTERN - prints all tasks and aliases that contain PATTERN in the name
      mix help --names          - prints all task names and aliases
                                  (useful for autocompleting)

  ## Colors

  When possible, `mix help` is going to use coloring for formatting
  guides. The formatting can be customized by configuring the Mix
  application either inside your project (in `config/config.exs`) or
  by using the local config (in `~/.mix/config.exs`).

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

  def run([task]) do
    loadpaths!()

    opts = Application.get_env(:mix, :colors)

    opts =
      if ansi_docs?(opts) do
        [width: width()] ++ opts
      else
        opts
      end

    for doc <- verbose_doc(task) do
      print_doc(task, doc, opts)
    end
  end

  def run(_) do
    Mix.raise("Unexpected arguments, expected \"mix help\" or \"mix help TASK\"")
  end

  defp print_doc(task, {doc, location, note}, opts) do
    if ansi_docs?(opts) do
      opts = [width: width()] ++ opts
      IO.ANSI.Docs.print_heading("mix #{task}", opts)
      IO.ANSI.Docs.print(doc, opts)
      IO.puts("Location: #{location}")
      note && IO.puts("") && IO.ANSI.Docs.print(note, opts)
    else
      IO.puts("# mix #{task}\n")
      IO.puts(doc)
      IO.puts("\nLocation: #{location}")
      note && IO.puts([?\n, note, ?\n, ?\n])
    end
  end

  # Loadpaths without checks because tasks may be defined in deps.
  defp loadpaths! do
    args = ["--no-elixir-version-check", "--no-deps-check", "--no-archives-check"]
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
    case :code.where_is_file(Atom.to_charlist(module) ++ '.beam') do
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
    message = "Runs the default task (current: \"mix #{Mix.Project.config()[:default_task]}\")"
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
    Enum.reduce(aliases, {[], 0}, fn {alias_name, _task_name}, {docs, max} ->
      doc = "Alias defined in mix.exs"
      task = "mix " <> alias_name
      {[{task, doc} | docs], max(byte_size(task), max)}
    end)
  end

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
    {"Alias for " <> inspect(task_name), "mix.exs", note}
  end

  defp task_doc(task) do
    module = Mix.Task.get!(task)
    doc = Mix.Task.moduledoc(module) || "There is no documentation for this task"
    {doc, where_is_file(module), nil}
  end
end
