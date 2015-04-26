defmodule Mix.Tasks.Help do
  use Mix.Task

  @shortdoc "Print help information for tasks"

  @moduledoc """
  Lists all tasks or prints the documentation for a given task.

  ## Arguments

      mix help                  - prints all tasks and their shortdoc
      mix help TASK             - prints full docs for the given task
      mix help --search PATTERN - prints all tasks that contain PATTERN in the name
      mix help --names          - prints all task names and aliases
                                  (useful for autocompleting)

  ## Colors

  When possible, `mix help` is going to use coloring for formatting
  guides. The formatting can be customized by configuring the Mix
  application either inside your project (in `config/config.exs`) or
  by using the local config (in `~/.mix/config.exs`).

  For example, to disable, one may:

      [mix: [colors: [enabled: false]]]

  The available color options are:

    * `:enabled`         - show ANSI formatting (defaults to `IO.ANSI.enabled?`)
    * `:doc_code`        — the attributes for code blocks (cyan, bright)
    * `:doc_inline_code` - inline code (cyan)
    * `:doc_headings`    - h1 and h2 (yellow, bright)
    * `:doc_title`       — the overall heading for the output (reverse, yellow, bright)
    * `:doc_bold`        - (bright)
    * `:doc_underline`   - (underline)

  """

  @spec run(OptionParser.argv) :: :ok
  def run(argv)

  def run([]) do
    loadpaths!

    modules = Mix.Task.load_all()

    {docs, max} = build_task_doc_list(modules)

    display_default_task_doc(max)
    display_task_doc_list(docs, max)
    display_iex_task_doc(max)
  end

  def run(["--names"]) do
    loadpaths!

    tasks =
      Mix.Task.load_all()
      |> Enum.map(&Mix.Task.task_name/1)

    aliases =
      Mix.Project.config[:aliases]
      |> Dict.keys
      |> Enum.map(&Atom.to_string/1)

    for info <- Enum.sort(aliases ++ tasks) do
      Mix.shell.info info
    end
  end

  def run(["--search", pattern]) do
    loadpaths!

    modules =
      Mix.Task.load_all()
      |> Enum.filter(&(String.contains?(Mix.Task.task_name(&1), pattern)))

    {docs, max} = build_task_doc_list(modules)

    display_task_doc_list(docs, max)
  end

  def run(["--search"]) do
    Mix.raise "Unexpected arguments, expected `mix help --search PATTERN`"
  end

  def run([task]) do
    loadpaths!

    module = Mix.Task.get!(task)
    doc    = Mix.Task.moduledoc(module) || "There is no documentation for this task"
    opts   = Application.get_env(:mix, :colors)

    if ansi_docs?(opts) do
      opts = [width: width] ++ opts
      IO.ANSI.Docs.print_heading("mix #{task}", opts)
      IO.ANSI.Docs.print(doc, opts)
    else
      IO.puts "# mix #{task}\n"
      IO.puts doc
    end

    IO.puts "Location: #{where_is_file(module)}"
  end

  def run(_) do
    Mix.raise "Unexpected arguments, expected `mix help` or `mix help TASK`"
  end

  # Loadpaths without checks because tasks may be defined in deps.
  defp loadpaths! do
    Mix.Task.run "loadpaths", ["--no-elixir-version-check", "--no-deps-check"]
    Mix.Task.reenable "loadpaths"
  end

  defp ansi_docs?(opts) do
    if Keyword.has_key?(opts, :enabled) do
      opts[:enabled]
    else
      IO.ANSI.enabled?
    end
  end

  defp width() do
    case :io.columns() do
      {:ok, width} -> min(width, 80)
      {:error, _}  -> 80
    end
  end

  defp format_task(task, max, doc) do
    String.ljust(task, max) <> " # " <> doc
  end

  defp where_is_file(module) do
    case :code.where_is_file(Atom.to_char_list(module) ++ '.beam') do
      :non_existing ->
        "not available"
      location ->
        location
        |> Path.dirname
        |> Path.expand
        |> Path.relative_to_cwd
    end
  end

  defp display_default_task_doc(max) do
    Mix.shell.info format_task("mix", max,
                    "Run the default task (current: mix #{Mix.Project.config[:default_task]})")
  end

  defp display_iex_task_doc(max) do
    Mix.shell.info format_task("iex -S mix", max,
                    "Start IEx and run the default task")
  end

  defp display_task_doc_list(docs, max) do
    Enum.each Enum.sort(docs), fn({task, doc}) ->
      Mix.shell.info format_task(task, max, doc)
    end
  end

  defp build_task_doc_list(modules) do
    Enum.reduce modules, {[], 0}, fn module, {docs, max} ->
      doc = Mix.Task.shortdoc(module)
      if doc do
        task = "mix " <> Mix.Task.task_name(module)
        docs = [{task, doc} | docs]
        max  = max(byte_size(task), max)
      end
      {docs, max}
    end
  end
end
