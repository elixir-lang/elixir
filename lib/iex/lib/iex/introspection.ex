# Convenience helpers for showing docs, specs, types
# and opening modules. Invoked directly from IEx.Helpers.
defmodule IEx.Introspection do
  @moduledoc false

  import IEx, only: [dont_display_result: 0]

  alias Code.Typespec

  @doc """
  Decomposes an introspection call into `{mod, fun, arity}`,
  `{mod, fun}` or `mod`.
  """
  def decompose({:/, _, [call, arity]} = term, context) do
    case Macro.decompose_call(call) do
      {_mod, :__info__, []} when arity == 1 ->
        {:{}, [], [Module, :__info__, 1]}

      {mod, fun, []} ->
        {:{}, [], [mod, fun, arity]}

      {fun, []} ->
        {:{}, [], [find_decompose_fun_arity(fun, arity, context), fun, arity]}

      _ ->
        term
    end
  end

  def decompose(call, context) do
    case Macro.decompose_call(call) do
      {_mod, :__info__, []} ->
        Macro.escape({Module, :__info__, 1})

      {mod, fun, []} ->
        {mod, fun}

      {fun, []} ->
        {find_decompose_fun(fun, context), fun}

      _ ->
        call
    end
  end

  defp find_decompose_fun(fun, context) do
    find_import(fun, context.functions) || find_import(fun, context.macros) ||
      find_special_form(fun) || Kernel
  end

  defp find_decompose_fun_arity(fun, arity, context) do
    pair = {fun, arity}

    find_import(pair, context.functions) || find_import(pair, context.macros) ||
      find_special_form(pair) || Kernel
  end

  defp find_import(pair, context) when is_tuple(pair) do
    Enum.find_value(context, fn {mod, functions} ->
      if pair in functions, do: mod
    end)
  end

  defp find_import(fun, context) do
    Enum.find_value(context, fn {mod, functions} ->
      if Keyword.has_key?(functions, fun), do: mod
    end)
  end

  defp find_special_form(pair) when is_tuple(pair) do
    special_form_function? = pair in Kernel.SpecialForms.__info__(:functions)
    special_form_macro? = pair in Kernel.SpecialForms.__info__(:macros)

    if special_form_function? or special_form_macro?, do: Kernel.SpecialForms
  end

  defp find_special_form(fun) do
    special_form_function? = Keyword.has_key?(Kernel.SpecialForms.__info__(:functions), fun)
    special_form_macro? = Keyword.has_key?(Kernel.SpecialForms.__info__(:macros), fun)

    if special_form_function? or special_form_macro?, do: Kernel.SpecialForms
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
        case Code.fetch_docs(module) do
          {:docs_v1, _, _, _, %{} = doc, metadata, _} ->
            print_doc(inspect(module), [], doc, metadata)

          {:docs_v1, _, _, _, _, _, _} ->
            docs_not_found(inspect(module))

          _ ->
            no_docs(module)
        end

      {:error, reason} ->
        puts_error("Could not load module #{inspect(module)}, got: #{reason}")
    end

    dont_display_result()
  end

  def h({module, function}) when is_atom(module) and is_atom(function) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        docs = get_docs(module, [:function, :macro])

        exports =
          cond do
            docs ->
              Enum.map(docs, &extract_name_and_arity/1)

            function_exported?(module, :__info__, 1) ->
              module.__info__(:functions) ++ module.__info__(:macros)

            true ->
              module.module_info(:exports)
          end
          |> Enum.sort()

        result =
          for {^function, arity} <- exports,
              (if docs do
                 find_doc_with_content(docs, function, arity)
               else
                 get_spec(module, function, arity) != []
               end) do
            h_mod_fun_arity(module, function, arity)
          end

        cond do
          result != [] ->
            :ok

          docs && has_callback?(module, function) ->
            behaviour_found("#{inspect(module)}.#{function}")

          docs && has_type?(module, function) ->
            type_found("#{inspect(module)}.#{function}")

          is_nil(docs) ->
            no_docs(module)

          true ->
            docs_not_found("#{inspect(module)}.#{function}")
        end

      {:error, reason} ->
        puts_error("Could not load module #{inspect(module)}, got: #{reason}")
    end

    dont_display_result()
  end

  def h({module, function, arity})
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        case h_mod_fun_arity(module, function, arity) do
          :ok ->
            :ok

          :behaviour_found ->
            behaviour_found("#{inspect(module)}.#{function}/#{arity}")

          :type_found ->
            type_found("#{inspect(module)}.#{function}/#{arity}")

          :no_docs ->
            no_docs(module)

          :not_found ->
            docs_not_found("#{inspect(module)}.#{function}/#{arity}")
        end

      {:error, reason} ->
        puts_error("Could not load module #{inspect(module)}, got: #{reason}")
    end

    dont_display_result()
  end

  def h(invalid) do
    puts_error(
      "The \"h\" helper expects a Module, Module.fun or Module.fun/arity, got: #{inspect(invalid)}"
    )

    puts_error(
      "If instead of accessing documentation you would like more information about a value " <>
        "or about the result of an expression, use the \"i\" helper instead"
    )

    dont_display_result()
  end

  defp h_mod_fun_arity(mod, fun, arity) when is_atom(mod) do
    docs = get_docs(mod, [:function, :macro])
    spec = get_spec(mod, fun, arity)

    cond do
      doc_tuple = find_doc_with_content(docs, fun, arity) ->
        print_fun(mod, doc_tuple, spec)
        :ok

      docs && has_callback?(mod, fun, arity) ->
        :behaviour_found

      docs && has_type?(mod, fun, arity) ->
        :type_found

      is_nil(docs) and spec != [] ->
        message = %{"en" => "Module was compiled without docs. Showing only specs."}
        print_doc("#{inspect(mod)}.#{fun}/#{arity}", spec, message, %{})
        :ok

      is_nil(docs) ->
        :no_docs

      true ->
        :not_found
    end
  end

  defp has_callback?(mod, fun) do
    case get_callback_docs(mod, &match?({_, ^fun, _}, elem(&1, 0))) do
      {:ok, [_ | _]} -> true
      _ -> false
    end
  end

  defp has_callback?(mod, fun, arity) do
    case get_callback_docs(mod, &match?({_, ^fun, ^arity}, elem(&1, 0))) do
      {:ok, [_ | _]} -> true
      _ -> false
    end
  end

  defp has_type?(mod, fun) do
    mod
    |> get_docs([:type])
    |> Enum.any?(&match?({_, ^fun, _}, elem(&1, 0)))
  end

  defp has_type?(mod, fun, arity) do
    mod
    |> get_docs([:type])
    |> Enum.any?(&match?({_, ^fun, ^arity}, elem(&1, 0)))
  end

  defp get_docs(mod, kinds) do
    case Code.fetch_docs(mod) do
      {:docs_v1, _, _, _, _, _, docs} ->
        for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in kinds, do: doc

      {:error, _} ->
        nil
    end
  end

  defp extract_name_and_arity({{_, name, arity}, _, _, _, _}), do: {name, arity}

  defp find_doc_with_content(docs, function, arity) do
    doc = find_doc(docs, function, arity)
    if doc != nil and has_content?(doc), do: doc
  end

  defp has_content?({_, _, _, :hidden, _}), do: false
  defp has_content?({{_, name, _}, _, _, :none, _}), do: hd(Atom.to_charlist(name)) != ?_
  defp has_content?({_, _, _, _, _}), do: true

  defp find_doc(nil, _fun, _arity) do
    nil
  end

  defp find_doc(docs, fun, arity) do
    Enum.find(docs, &match?({_, ^fun, ^arity}, elem(&1, 0))) ||
      find_doc_defaults(docs, fun, arity)
  end

  defp find_doc_defaults(docs, function, min) do
    Enum.find(docs, fn
      {{_, ^function, arity}, _, _, _, %{defaults: defaults}} when arity > min ->
        arity <= min + defaults

      _ ->
        false
    end)
  end

  defp print_fun(mod, {{kind, fun, arity}, _line, signature, doc, metadata}, spec) do
    if callback_module = doc == :none and callback_module(mod, fun, arity) do
      filter = &match?({_, ^fun, ^arity}, elem(&1, 0))

      case get_callback_docs(callback_module, filter) do
        {:ok, callback_docs} -> Enum.each(callback_docs, &print_typespec/1)
        _ -> nil
      end
    else
      print_doc("#{kind_to_def(kind)} #{Enum.join(signature, " ")}", spec, doc, metadata)
    end
  end

  defp kind_to_def(:function), do: :def
  defp kind_to_def(:macro), do: :defmacro

  defp callback_module(mod, fun, arity) do
    mod.module_info(:attributes)
    |> Keyword.get_values(:behaviour)
    |> Stream.concat()
    |> Enum.find(&has_callback?(&1, fun, arity))
  end

  defp get_spec(module, name, arity) do
    with {:ok, all_specs} <- Typespec.fetch_specs(module),
         {_, specs} <- List.keyfind(all_specs, {name, arity}, 0) do
      formatted =
        Enum.map(specs, fn spec ->
          Typespec.spec_to_quoted(name, spec)
          |> format_typespec(:spec, 2)
        end)

      [formatted, ?\n]
    else
      _ -> []
    end
  end

  @doc """
  Prints the list of behaviour callbacks or a given callback.
  """
  def b(mod) when is_atom(mod) do
    case get_callback_docs(mod, fn _ -> true end) do
      :no_beam ->
        no_beam(mod)

      {:ok, []} ->
        puts_error("No callbacks for #{inspect(mod)} were found")

      {:ok, docs} ->
        docs
        |> add_optional_callback_docs(mod)
        |> Enum.each(fn {definition, _, _} -> IO.puts(definition) end)
    end

    dont_display_result()
  end

  def b({mod, fun}) when is_atom(mod) and is_atom(fun) do
    filter = &match?({_, ^fun, _}, elem(&1, 0))

    case get_callback_docs(mod, filter) do
      :no_beam -> no_beam(mod)
      {:ok, []} -> docs_not_found("#{inspect(mod)}.#{fun}")
      {:ok, docs} -> Enum.each(docs, &print_typespec/1)
    end

    dont_display_result()
  end

  def b({mod, fun, arity}) when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    filter = &match?({_, ^fun, ^arity}, elem(&1, 0))

    case get_callback_docs(mod, filter) do
      :no_beam -> no_beam(mod)
      {:ok, []} -> docs_not_found("#{inspect(mod)}.#{fun}/#{arity}")
      {:ok, docs} -> Enum.each(docs, &print_typespec/1)
    end

    dont_display_result()
  end

  def b(invalid) do
    puts_error("Invalid arguments for b helper: #{inspect(invalid)}")
    dont_display_result()
  end

  defp get_callback_docs(mod, filter) do
    docs = get_docs(mod, [:callback, :macrocallback])

    case Typespec.fetch_callbacks(mod) do
      :error ->
        :no_beam

      {:ok, callbacks} ->
        docs =
          callbacks
          |> Enum.map(&translate_callback/1)
          |> Enum.filter(filter)
          |> Enum.sort()
          |> Enum.flat_map(fn {{_, function, arity}, _specs} = callback ->
            case find_doc(docs, function, arity) do
              nil -> [{format_callback(callback), :none, %{}}]
              {_, _, _, :hidden, _} -> []
              {_, _, _, doc, metadata} -> [{format_callback(callback), doc, metadata}]
            end
          end)

        {:ok, docs}
    end
  end

  defp translate_callback({name_arity, specs}) do
    case translate_callback_name_arity(name_arity) do
      {:macrocallback, _, _} = kind_name_arity ->
        # The typespec of a macrocallback differs from the one expressed
        # via @macrocallback:
        #
        #   * The function name is prefixed with "MACRO-"
        #   * The arguments contain an additional first argument: the caller
        #   * The arity is increased by 1
        #
        specs =
          Enum.map(specs, fn {:type, line1, :fun, [{:type, line2, :product, [_ | args]}, spec]} ->
            {:type, line1, :fun, [{:type, line2, :product, args}, spec]}
          end)

        {kind_name_arity, specs}

      kind_name_arity ->
        {kind_name_arity, specs}
    end
  end

  defp translate_callback_name_arity({name, arity}) do
    case Atom.to_string(name) do
      "MACRO-" <> macro_name -> {:macrocallback, String.to_atom(macro_name), arity - 1}
      _ -> {:callback, name, arity}
    end
  end

  defp format_callback({{kind, name, _arity}, specs}) do
    Enum.map(specs, fn spec ->
      Typespec.spec_to_quoted(name, spec)
      |> Macro.prewalk(&drop_macro_env/1)
      |> format_typespec(kind, 0)
    end)
  end

  defp add_optional_callback_docs(docs, mod) do
    optional_callbacks =
      if Code.ensure_loaded?(mod) and function_exported?(mod, :behaviour_info, 1) do
        mod.behaviour_info(:optional_callbacks)
        |> Enum.map(fn name_arity ->
          {_kind, name, arity} = translate_callback_name_arity(name_arity)
          {name, arity}
        end)
        |> Enum.sort()
      else
        []
      end

    if optional_callbacks == [] do
      docs
    else
      docs ++ [{format_optional_callbacks(optional_callbacks), "", %{}}]
    end
  end

  defp format_optional_callbacks(callbacks) do
    format_typespec(callbacks, :optional_callbacks, 0)
  end

  defp drop_macro_env({name, meta, [{:::, _, [_, {{:., _, [Macro.Env, :t]}, _, _}]} | args]}),
    do: {name, meta, args}

  defp drop_macro_env(other), do: other

  @doc """
  Prints the types for the given module and type documentation.
  """
  def t(module) when is_atom(module) do
    case Typespec.fetch_types(module) do
      :error ->
        no_beam(module)

      {:ok, []} ->
        types_not_found(inspect(module))

      {:ok, types} ->
        Enum.each(types, &(&1 |> format_type() |> IO.puts()))
    end

    dont_display_result()
  end

  def t({module, type}) when is_atom(module) and is_atom(type) do
    case Typespec.fetch_types(module) do
      :error ->
        no_beam(module)

      {:ok, types} ->
        printed =
          for {kind, {^type, _, args}} = typespec <- types,
              kind != :typep do
            type_doc(module, type, length(args), typespec)
            |> print_typespec()
          end

        if printed == [] do
          types_not_found_or_private("#{inspect(module)}.#{type}")
        end
    end

    dont_display_result()
  end

  def t({module, type, arity}) when is_atom(module) and is_atom(type) and is_integer(arity) do
    case Typespec.fetch_types(module) do
      :error ->
        no_beam(module)

      {:ok, types} ->
        printed =
          for {kind, {^type, _, args}} = typespec <- types,
              kind != :typep,
              length(args) == arity do
            type_doc(module, type, arity, typespec)
            |> print_typespec()
          end

        if printed == [] do
          types_not_found_or_private("#{inspect(module)}.#{type}")
        end
    end

    dont_display_result()
  end

  def t(invalid) do
    puts_error("Invalid arguments for t helper: #{inspect(invalid)}")
    dont_display_result()
  end

  defp type_doc(module, type, arity, typespec) do
    if docs = get_docs(module, [:type]) do
      {_, _, _, content, metadata} = Enum.find(docs, &match?({:type, ^type, ^arity}, elem(&1, 0)))
      {format_type(typespec), content, metadata}
    else
      {format_type(typespec), :none, %{}}
    end
  end

  defp format_type({:opaque, type}) do
    {:::, _, [ast, _]} = Typespec.type_to_quoted(type)
    format_typespec(ast, :opaque, 0)
  end

  defp format_type({kind, type}) do
    ast = Typespec.type_to_quoted(type)
    format_typespec(ast, kind, 0)
  end

  ## Helpers

  defp format_typespec(definition, kind, nesting) do
    "@#{kind} #{Macro.to_string(definition)}"
    |> Code.format_string!(line_length: IEx.width() - 2 * nesting)
    |> IO.iodata_to_binary()
    |> color_prefix_with_line()
    |> indent(nesting)
  end

  defp indent(content, 0) do
    [content, ?\n]
  end

  defp indent(content, nesting) do
    whitespace = String.duplicate(" ", nesting)
    [whitespace, String.replace(content, "\n", "\n#{whitespace}"), ?\n]
  end

  defp color_prefix_with_line(string) do
    [left, right] = :binary.split(string, " ")
    IEx.color(:doc_inline_code, left) <> " " <> right
  end

  defp print_doc(heading, types, doc, metadata) do
    doc = translate_doc(doc) || ""

    if opts = IEx.Config.ansi_docs() do
      IO.ANSI.Docs.print_heading(heading, opts)
      IO.write(types)
      IO.ANSI.Docs.print_metadata(metadata, opts)
      IO.ANSI.Docs.print(doc, opts)
    else
      IO.puts("* #{heading}\n")
      IO.write(types)
      IO.ANSI.Docs.print_metadata(metadata, enabled: false)
      IO.puts(doc)
    end
  end

  defp print_typespec({types, doc, metadata}) do
    IO.puts(types)
    doc = translate_doc(doc)

    if opts = IEx.Config.ansi_docs() do
      IO.ANSI.Docs.print_metadata(metadata, opts)
      doc && IO.ANSI.Docs.print(doc, opts)
    else
      IO.ANSI.Docs.print_metadata(metadata, enabled: false)
      doc && IO.puts(doc)
    end
  end

  defp translate_doc(:none), do: nil
  defp translate_doc(:hidden), do: nil
  defp translate_doc(%{"en" => doc}), do: doc

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

  defp no_docs(module) do
    puts_error("#{inspect(module)} was not compiled with docs")
  end

  defp types_not_found(for), do: not_found(for, "type information")
  defp docs_not_found(for), do: not_found(for, "documentation")

  defp types_not_found_or_private(for) do
    puts_error("No type information for #{for} was found or #{for} is private")
  end

  defp behaviour_found(for) do
    puts_error("""
    No documentation for function #{for} was found, but there is a callback with the same name.
    You can view callback documentation with the b/1 helper.
    """)
  end

  defp type_found(for) do
    puts_error("""
    No documentation for function #{for} was found, but there is a type with the same name.
    You can view type documentation with the t/1 helper.
    """)
  end

  defp not_found(for, type) do
    puts_error("No #{type} for #{for} was found")
  end

  defp puts_error(string) do
    IO.puts(IEx.color(:eval_error, string))
  end
end
