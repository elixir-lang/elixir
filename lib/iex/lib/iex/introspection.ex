# Convenience helpers for showing docs, specs, types
# and opening modules. Invoked directly from IEx.Helpers.
defmodule IEx.Introspection do
  @moduledoc false

  import IEx, only: [dont_display_result: 0]

  alias Kernel.Typespec

  @doc """
  Decomposes an introspection call into `{mod, fun, arity}`,
  `{mod, fun}` or `mod`.
  """
  @default_modules [IEx.Helpers, Kernel, Kernel.SpecialForms]

  def decompose({:/, _, [call, arity]} = term) do
    case Macro.decompose_call(call) do
      {_mod, :__info__, []} when arity == 1 ->
        {:{}, [], [Module, :__info__, 1]}

      {mod, fun, []} ->
        {:{}, [], [mod, fun, arity]}

      {fun, []} ->
        {:{}, [], [find_decompose_fun_arity(fun, arity), fun, arity]}

      _ ->
        term
    end
  end

  def decompose(call) do
    case Macro.decompose_call(call) do
      {_mod, :__info__, []} ->
        Macro.escape({Module, :__info__, 1})

      {mod, fun, []} ->
        {mod, fun}

      {fun, []} ->
        {find_decompose_fun(fun), fun}

      _ ->
        call
    end
  end

  defp find_decompose_fun(fun) do
    Enum.find(@default_modules, Kernel, fn mod ->
      Keyword.has_key?(mod.__info__(:functions), fun) or
        Keyword.has_key?(mod.__info__(:macros), fun)
    end)
  end

  defp find_decompose_fun_arity(fun, arity) do
    pair = {fun, arity}

    Enum.find(@default_modules, Kernel, fn mod ->
      pair in mod.__info__(:functions) or pair in mod.__info__(:macros)
    end)
  end

  @doc """
  Opens the given module, mfa, file/line, binary.
  """
  def open(module) when is_atom(module) do
    case open_mfa(module, :__info__, 1) do
      {source, nil, _} -> open(source)
      {_, tuple, _} -> open(tuple)
      :error -> puts_error("Could not open: #{inspect(module)}. Module is not available.")
    end

    dont_display_result()
  end

  def open({module, function}) when is_atom(module) and is_atom(function) do
    case open_mfa(module, function, :*) do
      {_, _, nil} ->
        puts_error(
          "Could not open: #{inspect(module)}.#{function}. Function/macro is not available."
        )

      {_, _, tuple} ->
        open(tuple)

      :error ->
        puts_error("Could not open: #{inspect(module)}.#{function}. Module is not available.")
    end

    dont_display_result()
  end

  def open({module, function, arity})
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    case open_mfa(module, function, arity) do
      {_, _, nil} ->
        puts_error(
          "Could not open: #{inspect(module)}.#{function}/#{arity}. Function/macro is not available."
        )

      {_, _, tuple} ->
        open(tuple)

      :error ->
        puts_error(
          "Could not open: #{inspect(module)}.#{function}/#{arity}. Module is not available."
        )
    end

    dont_display_result()
  end

  def open({file, line}) when is_binary(file) and is_integer(line) do
    cond do
      not File.regular?(file) ->
        puts_error("Could not open: #{inspect(file)}. File is not available.")

      editor = System.get_env("ELIXIR_EDITOR") || System.get_env("EDITOR") ->
        command =
          if editor =~ "__FILE__" or editor =~ "__LINE__" do
            editor
            |> String.replace("__FILE__", inspect(file))
            |> String.replace("__LINE__", Integer.to_string(line))
          else
            "#{editor} #{inspect(file)}:#{line}"
          end

        IO.write(IEx.color(:eval_info, :os.cmd(String.to_charlist(command))))

      true ->
        puts_error(
          "Could not open: #{inspect(file)}. " <>
            "Please set the ELIXIR_EDITOR or EDITOR environment variables with the " <>
            "command line invocation of your favorite EDITOR."
        )
    end

    dont_display_result()
  end

  def open(invalid) do
    puts_error("Invalid arguments for open helper: #{inspect(invalid)}")
    dont_display_result()
  end

  defp open_mfa(module, fun, arity) do
    with {:module, _} <- Code.ensure_loaded(module),
         source when is_list(source) <- module.module_info(:compile)[:source] do
      source = rewrite_source(module, source)
      open_abstract_code(module, fun, arity, source)
    else
      _ -> :error
    end
  end

  defp open_abstract_code(module, fun, arity, source) do
    fun = Atom.to_string(fun)

    with [_ | _] = beam <- :code.which(module),
         {:ok, {_, [abstract_code: abstract_code]}} <- :beam_lib.chunks(beam, [:abstract_code]),
         {:raw_abstract_v1, code} <- abstract_code do
      {_, module_pair, fa_pair} =
        Enum.reduce(code, {source, nil, nil}, &open_abstract_code_reduce(&1, &2, fun, arity))

      {source, module_pair, fa_pair}
    else
      _ ->
        {source, nil, nil}
    end
  end

  defp open_abstract_code_reduce(entry, {file, module_pair, fa_pair}, fun, arity) do
    case entry do
      {:attribute, ann, :module, _} ->
        {file, {file, :erl_anno.line(ann)}, fa_pair}

      {:function, ann, ann_fun, ann_arity, _} ->
        case Atom.to_string(ann_fun) do
          "MACRO-" <> ^fun when arity == :* or ann_arity == arity + 1 ->
            {file, module_pair, fa_pair || {file, :erl_anno.line(ann)}}

          ^fun when arity == :* or ann_arity == arity ->
            {file, module_pair, fa_pair || {file, :erl_anno.line(ann)}}

          _ ->
            {file, module_pair, fa_pair}
        end

      _ ->
        {file, module_pair, fa_pair}
    end
  end

  @elixir_apps ~w(eex elixir ex_unit iex logger mix)a
  @otp_apps ~w(kernel stdlib)a
  @apps @elixir_apps ++ @otp_apps

  defp rewrite_source(module, source) do
    case :application.get_application(module) do
      {:ok, app} when app in @apps ->
        Application.app_dir(app, rewrite_source(source))

      _ ->
        beam_path = :code.which(module)

        if is_list(beam_path) and List.starts_with?(beam_path, :code.root_dir()) do
          app_vsn = beam_path |> Path.dirname() |> Path.dirname() |> Path.basename()
          Path.join([:code.root_dir(), "lib", app_vsn, rewrite_source(source)])
        else
          List.to_string(source)
        end
    end
  end

  defp rewrite_source(source) do
    {in_app, [lib_or_src | _]} =
      source
      |> Path.split()
      |> Enum.reverse()
      |> Enum.split_while(&(&1 not in ["lib", "src"]))

    Path.join([lib_or_src | Enum.reverse(in_app)])
  end

  @doc """
  Prints documentation.
  """
  def h(module) when is_atom(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        if function_exported?(module, :__info__, 1) do
          case Code.get_docs(module, :moduledoc) do
            {_, binary} when is_binary(binary) ->
              print_doc(inspect(module), [], binary)

            {_, _} ->
              no_docs(inspect(module))

            _ ->
              puts_error("#{inspect(module)} was not compiled with docs")
          end
        else
          puts_error(
            "#{inspect(module)} is an Erlang module and, as such, it does not have Elixir-style docs"
          )
        end

      {:error, reason} ->
        puts_error("Could not load module #{inspect(module)}, got: #{reason}")
    end

    dont_display_result()
  end

  def h({module, function}) when is_atom(module) and is_atom(function) do
    case h_mod_fun(module, function) do
      :ok ->
        :ok

      :behaviour_found ->
        behaviour_found("#{inspect(module)}.#{function}")

      :no_docs ->
        puts_error("#{inspect(module)} was not compiled with docs")

      :not_found ->
        no_docs("#{inspect(module)}.#{function}")
    end

    dont_display_result()
  end

  def h({module, function, arity})
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    case h_mod_fun_arity(module, function, arity) do
      :ok ->
        :ok

      :behaviour_found ->
        behaviour_found("#{inspect(module)}.#{function}/#{arity}")

      :no_docs ->
        puts_error("#{inspect(module)} was not compiled with docs")

      :not_found ->
        no_docs("#{inspect(module)}.#{function}/#{arity}")
    end

    dont_display_result()
  end

  def h(invalid) do
    puts_error("Invalid arguments for h helper: #{inspect(invalid)}")
    dont_display_result()
  end

  defp h_mod_fun(mod, fun) when is_atom(mod) do
    if docs = Code.get_docs(mod, :docs) do
      result =
        for {{^fun, arity}, _, _, _, _} = doc <- docs, has_content?(doc) do
          h_mod_fun_arity(mod, fun, arity)
        end

      cond do
        result != [] ->
          :ok

        has_callback?(mod, fun) ->
          :behaviour_found

        true ->
          :not_found
      end
    else
      :no_docs
    end
  end

  defp h_mod_fun_arity(mod, fun, arity) when is_atom(mod) do
    docs = Code.get_docs(mod, :docs)

    cond do
      is_nil(docs) ->
        :no_docs

      doc_tuple = find_doc(docs, fun, arity) ->
        print_fun(mod, doc_tuple)
        :ok

      has_callback?(mod, fun, arity) ->
        :behaviour_found

      true ->
        :not_found
    end
  end

  defp has_callback?(mod, fun) do
    mod
    |> Code.get_docs(:callback_docs)
    |> Enum.any?(&match?({{^fun, _}, _, _, _}, &1))
  end

  defp has_callback?(mod, fun, arity) do
    mod
    |> Code.get_docs(:callback_docs)
    |> Enum.any?(&match?({{^fun, ^arity}, _, _, _}, &1))
  end

  defp find_doc(docs, fun, arity) do
    doc = List.keyfind(docs, {fun, arity}, 0) || find_doc_defaults(docs, fun, arity)
    if doc != nil and has_content?(doc), do: doc
  end

  defp find_doc_defaults(docs, function, min) do
    Enum.find(docs, fn doc ->
      case elem(doc, 0) do
        {^function, arity} when arity > min ->
          defaults = Enum.count(elem(doc, 3), &match?({:\\, _, _}, &1))
          arity <= min + defaults

        _ ->
          false
      end
    end)
  end

  defp has_content?({_, _, _, _, false}), do: false
  defp has_content?({{name, _}, _, _, _, nil}), do: hd(Atom.to_charlist(name)) != ?_
  defp has_content?({_, _, _, _, _}), do: true

  defp print_fun(mod, {{fun, arity}, _line, kind, args, doc}) do
    if callback_module = is_nil(doc) and callback_module(mod, fun, arity) do
      filter = &match?({^fun, ^arity}, elem(&1, 0))

      case get_callback_docs(callback_module, filter) do
        {:ok, callback_docs} -> Enum.each(callback_docs, &print_typespec/1)
        _ -> nil
      end
    else
      args = Enum.map_join(args, ", ", &format_doc_arg(&1))
      print_doc("#{kind} #{fun}(#{args})", get_spec(mod, fun, arity), doc)
    end
  end

  defp callback_module(mod, fun, arity) do
    predicate = &match?({{^fun, ^arity}, _}, &1)

    mod.module_info(:attributes)
    |> Keyword.get_values(:behaviour)
    |> Stream.concat()
    |> Enum.find(&Enum.any?(Typespec.beam_callbacks(&1), predicate))
  end

  defp format_doc_arg({:\\, _, [left, right]}) do
    format_doc_arg(left) <> " \\\\ " <> Macro.to_string(right)
  end

  defp format_doc_arg({var, _, _}) do
    Atom.to_string(var)
  end

  defp get_spec(module, name, arity) do
    all_specs = Typespec.beam_specs(module) || []

    case List.keyfind(all_specs, {name, arity}, 0) do
      {_, specs} ->
        formatted =
          Enum.map(specs, fn spec ->
            Typespec.spec_to_ast(name, spec)
            |> format_typespec(:spec)
            |> IO.iodata_to_binary()
            |> String.replace("\n", "\n    ")
            |> prefix("    ")
            |> pair(?\n)
          end)

        [formatted, ?\n]

      nil ->
        []
    end
  end

  @doc """
  Prints the list of behaviour callbacks or a given callback.
  """
  def b(mod) when is_atom(mod) do
    case get_callback_docs(mod, fn _ -> true end) do
      :no_beam -> no_beam(mod)
      :no_docs -> puts_error("#{inspect(mod)} was not compiled with docs")
      {:ok, []} -> puts_error("No callbacks for #{inspect(mod)} were found")
      {:ok, docs} -> Enum.each(docs, fn {definition, _} -> IO.puts(definition) end)
    end

    dont_display_result()
  end

  def b({mod, fun}) when is_atom(mod) and is_atom(fun) do
    filter = &match?({^fun, _}, elem(&1, 0))

    case get_callback_docs(mod, filter) do
      :no_beam -> no_beam(mod)
      :no_docs -> puts_error("#{inspect(mod)} was not compiled with docs")
      {:ok, []} -> no_docs("#{inspect(mod)}.#{fun}")
      {:ok, docs} -> Enum.each(docs, &print_typespec/1)
    end

    dont_display_result()
  end

  def b({mod, fun, arity}) when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    filter = &match?({^fun, ^arity}, elem(&1, 0))

    case get_callback_docs(mod, filter) do
      :no_beam -> no_beam(mod)
      :no_docs -> puts_error("#{inspect(mod)} was not compiled with docs")
      {:ok, []} -> no_docs("#{inspect(mod)}.#{fun}/#{arity}")
      {:ok, docs} -> Enum.each(docs, &print_typespec/1)
    end

    dont_display_result()
  end

  def b(invalid) do
    puts_error("Invalid arguments for b helper: #{inspect(invalid)}")
    dont_display_result()
  end

  defp get_callback_docs(mod, filter) do
    callbacks = Typespec.beam_callbacks(mod)
    docs = Code.get_docs(mod, :callback_docs)

    cond do
      is_nil(callbacks) ->
        :no_beam

      is_nil(docs) ->
        :no_docs

      true ->
        docs =
          docs
          |> Enum.filter(filter)
          |> Enum.map(fn
               {{fun, arity}, _, :macrocallback, doc} ->
                 macro = {:"MACRO-#{fun}", arity + 1}
                 {format_callback(:macrocallback, fun, macro, callbacks), doc}

               {{fun, arity}, _, kind, doc} ->
                 {format_callback(kind, fun, {fun, arity}, callbacks), doc}
             end)

        {:ok, docs}
    end
  end

  defp format_callback(kind, name, key, callbacks) do
    {_, specs} = List.keyfind(callbacks, key, 0)

    Enum.map(specs, fn spec ->
      Typespec.spec_to_ast(name, spec)
      |> Macro.prewalk(&drop_macro_env/1)
      |> format_typespec(kind)
      |> pair(?\n)
    end)
  end

  defp drop_macro_env({name, meta, [{:::, _, [_, {{:., _, [Macro.Env, :t]}, _, _}]} | args]}),
    do: {name, meta, args}

  defp drop_macro_env(other), do: other

  @doc """
  Prints the types for the given module and type documentation.
  """
  def t(module) when is_atom(module) do
    case Typespec.beam_types(module) do
      nil ->
        no_beam(module)

      [] ->
        no_types(inspect(module))

      types ->
        Enum.each(types, &(&1 |> format_type() |> IO.puts()))
    end

    dont_display_result()
  end

  def t({module, type}) when is_atom(module) and is_atom(type) do
    case Typespec.beam_types(module) do
      nil ->
        no_beam(module)

      types ->
        printed =
          for {_, {^type, _, args}} = typespec <- types do
            doc = {format_type(typespec), type_doc(module, type, length(args))}
            print_typespec(doc)
          end

        if printed == [] do
          no_types("#{inspect(module)}.#{type}")
        end
    end

    dont_display_result()
  end

  def t({module, type, arity}) when is_atom(module) and is_atom(type) and is_integer(arity) do
    case Typespec.beam_types(module) do
      nil ->
        no_beam(module)

      types ->
        printed =
          for {_, {^type, _, args}} = typespec <- types, length(args) == arity do
            doc = {format_type(typespec), type_doc(module, type, arity)}
            print_typespec(doc)
          end

        if printed == [] do
          no_types("#{inspect(module)}.#{type}")
        end
    end

    dont_display_result()
  end

  def t(invalid) do
    puts_error("Invalid arguments for t helper: #{inspect(invalid)}")
    dont_display_result()
  end

  defp type_doc(module, type, arity) do
    docs = Code.get_docs(module, :type_docs)
    {_, _, _, content} = Enum.find(docs, &match?({{^type, ^arity}, _, _, _}, &1))
    content
  end

  defp format_type({:opaque, type}) do
    {:::, _, [ast, _]} = Typespec.type_to_ast(type)
    [format_typespec(ast, :opaque), ?\n]
  end

  defp format_type({kind, type}) do
    ast = Typespec.type_to_ast(type)
    [format_typespec(ast, kind), ?\n]
  end

  ## Helpers

  defp format_typespec(definition, kind) do
    definition
    |> Macro.to_string()
    |> prefix("@#{kind} ")
    |> Code.format_string!(line_length: IEx.width())
    |> IO.iodata_to_binary()
    |> color_prefix_with_line()
  end

  defp prefix(string, prefix) do
    prefix <> string
  end

  defp pair(left, right) do
    [left, right]
  end

  defp color_prefix_with_line(string) do
    [left, right] = :binary.split(string, " ")
    [IEx.color(:doc_inline_code, left), ?\s, right]
  end

  defp print_doc(heading, types, doc) do
    doc = doc || ""

    if opts = IEx.Config.ansi_docs() do
      IO.ANSI.Docs.print_heading(heading, opts)
      IO.write(types)
      IO.ANSI.Docs.print(doc, opts)
    else
      IO.puts("* #{heading}\n")
      IO.write(types)
      IO.puts(doc)
    end
  end

  defp print_typespec({types, doc}) do
    IO.puts(types)

    if opts = IEx.Config.ansi_docs() do
      doc && IO.ANSI.Docs.print(doc, opts)
    else
      doc && IO.puts(doc)
    end
  end

  defp no_beam(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        puts_error(
          "Beam code not available for #{inspect(module)} or debug info is missing, cannot load typespecs"
        )

      {:error, reason} ->
        puts_error("Could not load module #{inspect(module)}, got: #{reason}")
    end
  end

  defp no_types(for), do: no(for, "type information")
  defp no_docs(for), do: no(for, "documentation")

  defp behaviour_found(for) do
    puts_error("""
    No documentation for function #{for} was found, but there is a callback with the same name.
    You can view callback documentations with the b/1 helper.
    """)
  end

  defp no(for, type) do
    puts_error("No #{type} for #{for} was found")
  end

  defp puts_error(string) do
    IO.puts(IEx.color(:eval_error, string))
  end
end
