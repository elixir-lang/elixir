defmodule IEx.Config do
  @moduledoc false

  @table __MODULE__
  @agent __MODULE__
  @keys [:colors, :inspect, :history_size, :default_prompt, :alive_prompt, :width]
  @colors [:eval_interrupt, :eval_result, :eval_error, :eval_info, :stack_app,
    :stack_info, :ls_directory, :ls_device]
  @syntax_colors [number: [:bright, :blue], atom: [:bright, :green],
                  regex: [:bright, :magenta], string: :cyan]


  def new() do
    tab = :ets.new(@table, [:named_table, :public])
    true = :ets.insert_new(tab, [after_spawn: []])
    tab
  end

  def delete(__MODULE__) do
    :ets.delete(__MODULE__)
  end

  def configure(options) do
    Agent.update(@agent, __MODULE__, :handle_configure, [options])
  end

  def handle_configure(tab, options) do
    options = :lists.ukeysort(1, options)
    get_config()
    |> Keyword.merge(options, &merge_option/3)
    |> put_config()
    tab
  end

  defp get_config() do
    Application.get_all_env(:iex)
    |> Keyword.take(@keys)
  end

  defp put_config(config) do
    put = fn({key, value}) when key in @keys ->
      Application.put_env(:iex, key, value)
    end
    Enum.each(config, put)
  end

  defp merge_option(:colors, old, new) when is_list(new), do: Keyword.merge(old, new)
  defp merge_option(:inspect, old, new) when is_list(new), do: Keyword.merge(old, new)
  defp merge_option(:history_size, _old, new) when is_integer(new), do: new
  defp merge_option(:default_prompt, _old, new) when is_binary(new), do: new
  defp merge_option(:alive_prompt, _old, new) when is_binary(new), do: new
  defp merge_option(:width, _old, new) when is_integer(new), do: new

  defp merge_option(k, _old, new) do
    raise ArgumentError, "invalid configuration or value for pair #{inspect k} - #{inspect new}"
  end

  def configuration() do
    Keyword.merge(default_config(), get_config(), &merge_option/3)
  end

  defp default_config() do
    Enum.map(@keys, &{&1, default_option(&1)})
  end

  defp default_option(:colors), do: [{:enabled, IO.ANSI.enabled?} | default_colors()]
  defp default_option(:inspect), do: []
  defp default_option(:history_size), do: 20
  defp default_option(:width), do: width()

  defp default_option(prompt) when prompt in [:default_prompt, :alive_prompt] do
    "%prefix(%counter)>"
  end

  defp default_colors() do
    Enum.map(@colors, &{&1, default_color(&1)}) ++ default_doc_colors()
  end

  defp default_doc_colors() do
    Keyword.drop(IO.ANSI.Docs.default_options(), [:enabled, :width])
  end

  # Used by default on evaluation cycle
  defp default_color(:eval_interrupt), do: [:yellow]
  defp default_color(:eval_result), do: [:yellow]
  defp default_color(:eval_error), do: [:red]
  defp default_color(:eval_info), do: [:normal]
  defp default_color(:stack_app), do: [:red, :bright]
  defp default_color(:stack_info), do: [:red]

  # Used by ls
  defp default_color(:ls_directory), do: [:blue]
  defp default_color(:ls_device), do: [:green]

  # Used by ansi docs
  defp default_color(doc_color) do
    IO.ANSI.Docs.default_options()
    |> Keyword.fetch!(doc_color)
  end

  def color(color) do
    colors = Application.get_env(:iex, :colors, [])
    if colors_enabled?(colors) do
      case Keyword.fetch(colors, color) do
        {:ok, value} ->
          value
        :error ->
          default_color(color)
      end
    else
      nil
    end
  end

  defp colors_enabled?(colors \\ Application.get_env(:iex, :colors, [])) do
    case Keyword.fetch(colors, :enabled) do
      {:ok, enabled} ->
        enabled
      :error ->
        IO.ANSI.enabled?()
    end
  end

  def ansi_docs() do
    colors = Application.get_env(:iex, :colors, [])
    if enabled = colors_enabled?(colors) do
      [width: width(), enabled: enabled] ++ colors
    end
  end

  def inspect_opts() do
    inspect_options = Application.get_env(:iex, :inspect, []) ++ [pretty: true]
    cond do
      Keyword.has_key?(inspect_options, :width) ->
        inspect_options
      colors_enabled?() ->
        eval_color = List.flatten([:normal, color(:eval_result)])
        syntax_colors = [{:reset, eval_color} | @syntax_colors]
        [width: width(), syntax_colors: syntax_colors] ++ inspect_options
      true ->
        inspect_options
    end
  end

  def width() do
    columns = columns()
    value = Application.get_env(:iex, :width) || min(columns, 80)
    min(value, columns)
  end

  defp columns() do
    case :io.columns() do
      {:ok, width} -> width
      {:error, _}  -> 80
    end
  end

  def after_spawn(fun) do
    Agent.update(@agent, __MODULE__, :handle_after_spawn, [fun])
  end

  def handle_after_spawn(tab, fun) do
    :ets.update_element(tab, :after_spawn, {2, [fun | after_spawn()]})
  end

  def after_spawn() do
    :ets.lookup_element(@table, :after_spawn, 2)
  end

  def started?() do
    Process.whereis(@agent) !== nil
  end

  def history_size(), do: get(:history_size)

  def default_prompt(), do: get(:default_prompt)

  def alive_prompt(), do: get(:alive_prompt)

  defp get(key) do
    case Application.fetch_env(:iex, key) do
      {:ok, value} ->
        value
      :error ->
        default_option(key)
    end
  end

  def start_link() do
    Agent.start_link(__MODULE__, :init, [@table], [name: @agent])
  end

  def init(tab) do
    :public = :ets.info(tab, :protection)
    tab
  end
end
