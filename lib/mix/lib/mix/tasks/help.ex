defmodule Mix.Tasks.Help do
  use Mix.Task

  @shortdoc "Print help information for tasks"

  @moduledoc """
  Lists all tasks or prints the documentation for a given task.

  ## Arguments

      mix help      - prints all tasks and their shortdoc
      mix help TASK - prints full docs for the given task

  ## Colors

  When possible, `mix help` is going to use coloring for formatting
  guides. The formatting can be customized by configuring the Mix
  application either inside your project (in `config/config.exs`) or
  by using the local config (in `~/.mix/config.exs`).

  For example, to disable, one may:

      [mix: [colors: [enabled: false]]]

  The available color options are:

  * `:enabled`         - show ANSI formatting (defaults to IO.ANSI.terminal?)
  * `:doc_code`        — the attributes for code blocks (cyan, bright)
  * `:doc_inline_code` - inline code (cyan)
  * `:doc_headings`    - h1 and h2 (yellow, bright)
  * `:doc_title`       — the overall heading for the output (reverse,yellow,bright)
  * `:doc_bold`        - (bright)
  * `:doc_underline`   - (underline)

  """

  def run([]) do
    Mix.Task.load_all

    shell   = Mix.shell
    modules = Mix.Task.all_modules

    docs = for module <- modules,
        doc = Mix.Task.shortdoc(module) do
      {"mix " <> Mix.Task.task_name(module), doc}
    end

    max = Enum.reduce docs, 0, fn({task, _}, acc) ->
      max(size(task), acc)
    end

    display_default_task_doc(max)

    Enum.each Enum.sort(docs), fn({task, doc}) ->
      shell.info format_task(task, max, doc)
    end

    display_iex_task_doc(max)
  end

  def run([task]) do
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
    raise Mix.Error, message: "Unexpected arguments, expected `mix help` or `mix help TASK`"
  end

  defp ansi_docs?(opts) do
    if Keyword.has_key?(opts, :enabled) do
      opts[:enabled]
    else
      IO.ANSI.terminal?
    end
  end

  defp width() do
    case :io.columns(:standard_input) do
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
end
