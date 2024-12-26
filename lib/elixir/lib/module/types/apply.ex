defmodule Module.Types.Apply do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false

  # We limit the size of the union for two reasons:
  # To avoid really large outputs in reports and to
  # reduce the computation cost of inferred code.
  @max_clauses 16

  alias Module.ParallelChecker
  import Module.Types.{Helpers, Descr}

  ## Signatures

  # Define strong arrows found in the standard library.
  # A strong arrow means that, if a type outside of its
  # domain is given, an error is raised. We are also
  # ensuring that domains for the same function have
  # no overlaps.

  # Remote for callback info functions

  kw = fn kw ->
    kw
    |> Enum.map(fn {key, type} when is_atom(key) ->
      tuple([atom([key]), type])
    end)
    |> Enum.reduce(&union/2)
    |> list()
  end

  fas = list(tuple([atom(), integer()]))

  shared_info = [
    attributes: list(tuple([atom(), list(term())])),
    compile: kw.(version: list(integer()), source: list(integer()), options: list(term())),
    exports: fas,
    md5: binary(),
    module: atom()
  ]

  infos =
    %{
      behaviour_info: [
        callbacks: fas,
        optional_callbacks: fas
      ],
      module_info: [functions: fas, nifs: fas] ++ shared_info,
      __info__:
        [
          deprecated: list(tuple([tuple([atom(), integer()]), binary()])),
          exports_md5: binary(),
          functions: fas,
          macros: fas,
          struct:
            list(closed_map(default: if_set(term()), field: atom()))
            |> union(atom([nil]))
        ] ++ shared_info,
      __protocol__: [
        module: atom(),
        functions: fas,
        consolidated?: boolean(),
        impls: union(atom([:not_consolidated]), tuple([atom([:consolidated]), list(atom())]))
      ]
    }

  for {name, clauses} <- infos do
    domain = atom(Keyword.keys(clauses))
    clauses = Enum.map(clauses, fn {key, return} -> {[atom([key])], return} end)

    defp signature(unquote(name), 1) do
      {:strong, [unquote(Macro.escape(domain))], unquote(Macro.escape(clauses))}
    end
  end

  defp signature(:module_info, 0) do
    {:strong, nil, [{[], unquote(Macro.escape(kw.(infos.module_info)))}]}
  end

  defp signature(_, _), do: :none

  # Remote for compiler functions

  mfargs = [atom(), atom(), list(term())]

  send_destination =
    pid()
    |> union(reference())
    |> union(port())
    |> union(atom())
    |> union(tuple([atom(), atom()]))

  basic_arith_2_args_clauses = [
    {[integer(), integer()], integer()},
    {[integer(), float()], float()},
    {[float(), integer()], float()},
    {[float(), float()], float()}
  ]

  is_clauses = [{[term()], boolean()}]

  args_or_arity = union(list(term()), integer())
  args_or_none = union(list(term()), atom([:none]))
  extra_info = kw.(file: list(integer()), line: integer(), error_info: open_map())

  raise_stacktrace =
    list(
      tuple([atom(), atom(), args_or_arity, extra_info])
      |> union(tuple([atom(), atom(), args_or_arity]))
      |> union(tuple([fun(), args_or_arity, extra_info]))
      |> union(tuple([fun(), args_or_arity]))
    )

  and_signature =
    for left <- [true, false], right <- [true, false] do
      {[atom([left]), atom([right])], atom([left and right])}
    end

  or_signature =
    for left <- [true, false], right <- [true, false] do
      {[atom([left]), atom([right])], atom([left or right])}
    end

  for {mod, fun, clauses} <- [
        # :binary
        {:binary, :copy, [{[binary(), integer()], binary()}]},

        # :erlang
        {:erlang, :+, [{[integer()], integer()}, {[float()], float()}]},
        {:erlang, :+, basic_arith_2_args_clauses},
        {:erlang, :-, [{[integer()], integer()}, {[float()], float()}]},
        {:erlang, :-, basic_arith_2_args_clauses},
        {:erlang, :*, basic_arith_2_args_clauses},
        {:erlang, :/, [{[union(integer(), float()), union(integer(), float())], float()}]},
        {:erlang, :"/=", [{[term(), term()], boolean()}]},
        {:erlang, :"=/=", [{[term(), term()], boolean()}]},
        {:erlang, :<, [{[term(), term()], boolean()}]},
        {:erlang, :"=<", [{[term(), term()], boolean()}]},
        {:erlang, :==, [{[term(), term()], boolean()}]},
        {:erlang, :"=:=", [{[term(), term()], boolean()}]},
        {:erlang, :>, [{[term(), term()], boolean()}]},
        {:erlang, :>=, [{[term(), term()], boolean()}]},
        {:erlang, :abs, [{[integer()], integer()}, {[float()], float()}]},
        {:erlang, :and, and_signature},
        {:erlang, :atom_to_binary, [{[atom()], binary()}]},
        {:erlang, :atom_to_list, [{[atom()], list(integer())}]},
        {:erlang, :band, [{[integer(), integer()], integer()}]},
        {:erlang, :binary_part, [{[binary(), integer(), integer()], binary()}]},
        {:erlang, :binary_to_atom, [{[binary()], atom()}]},
        {:erlang, :binary_to_existing_atom, [{[binary()], atom()}]},
        {:erlang, :binary_to_integer, [{[binary()], integer()}]},
        {:erlang, :binary_to_integer, [{[binary(), integer()], integer()}]},
        {:erlang, :binary_to_float, [{[binary()], float()}]},
        {:erlang, :bit_size, [{[binary()], integer()}]},
        {:erlang, :bnot, [{[integer()], integer()}]},
        {:erlang, :bor, [{[integer(), integer()], integer()}]},
        {:erlang, :bsl, [{[integer(), integer()], integer()}]},
        {:erlang, :bsr, [{[integer(), integer()], integer()}]},
        {:erlang, :bxor, [{[integer(), integer()], integer()}]},
        {:erlang, :byte_size, [{[binary()], integer()}]},
        {:erlang, :ceil, [{[union(integer(), float())], integer()}]},
        {:erlang, :div, [{[integer(), integer()], integer()}]},
        {:erlang, :error, [{[term()], none()}]},
        {:erlang, :error, [{[term(), args_or_none], none()}]},
        {:erlang, :error, [{[term(), args_or_none, kw.(error_info: open_map())], none()}]},
        {:erlang, :floor, [{[union(integer(), float())], integer()}]},
        {:erlang, :function_exported, [{[atom(), atom(), integer()], boolean()}]},
        {:erlang, :integer_to_binary, [{[integer()], binary()}]},
        {:erlang, :integer_to_binary, [{[integer(), integer()], binary()}]},
        {:erlang, :integer_to_list, [{[integer()], non_empty_list(integer())}]},
        {:erlang, :integer_to_list, [{[integer(), integer()], non_empty_list(integer())}]},
        {:erlang, :is_atom, is_clauses},
        {:erlang, :is_binary, is_clauses},
        {:erlang, :is_bitstring, is_clauses},
        {:erlang, :is_boolean, is_clauses},
        {:erlang, :is_float, is_clauses},
        {:erlang, :is_function, is_clauses},
        {:erlang, :is_function, [{[term(), integer()], boolean()}]},
        {:erlang, :is_integer, is_clauses},
        {:erlang, :is_list, is_clauses},
        {:erlang, :is_map, is_clauses},
        {:erlang, :is_map_key, [{[term(), open_map()], boolean()}]},
        {:erlang, :is_number, is_clauses},
        {:erlang, :is_pid, is_clauses},
        {:erlang, :is_port, is_clauses},
        {:erlang, :is_reference, is_clauses},
        {:erlang, :is_tuple, is_clauses},
        {:erlang, :length, [{[list(term())], integer()}]},
        {:erlang, :list_to_atom, [{[list(integer())], atom()}]},
        {:erlang, :list_to_existing_atom, [{[list(integer())], atom()}]},
        {:erlang, :list_to_float, [{[non_empty_list(integer())], float()}]},
        {:erlang, :list_to_integer, [{[non_empty_list(integer())], integer()}]},
        {:erlang, :list_to_integer, [{[non_empty_list(integer()), integer()], integer()}]},
        {:erlang, :make_ref, [{[], reference()}]},
        {:erlang, :map_size, [{[open_map()], integer()}]},
        {:erlang, :node, [{[], atom()}]},
        {:erlang, :node, [{[pid() |> union(reference()) |> union(port())], atom()}]},
        {:erlang, :not, [{[atom([false])], atom([true])}, {[atom([true])], atom([false])}]},
        {:erlang, :or, or_signature},
        {:erlang, :raise, [{[atom([:error, :exit, :throw]), term(), raise_stacktrace], none()}]},
        {:erlang, :rem, [{[integer(), integer()], integer()}]},
        {:erlang, :round, [{[union(integer(), float())], integer()}]},
        {:erlang, :self, [{[], pid()}]},
        {:erlang, :spawn, [{[fun()], pid()}]},
        {:erlang, :spawn, [{mfargs, pid()}]},
        {:erlang, :spawn_link, [{[fun()], pid()}]},
        {:erlang, :spawn_link, [{mfargs, pid()}]},
        {:erlang, :spawn_monitor, [{[fun()], tuple([reference(), pid()])}]},
        {:erlang, :spawn_monitor, [{mfargs, tuple([reference(), pid()])}]},
        {:erlang, :tuple_size, [{[open_tuple([])], integer()}]},
        {:erlang, :trunc, [{[union(integer(), float())], integer()}]},

        # TODO: Replace term()/dynamic() by parametric types
        {:erlang, :++, [{[list(term()), term()], dynamic(list(term(), term()))}]},
        {:erlang, :--, [{[list(term()), list(term())], dynamic(list(term()))}]},
        {:erlang, :andalso, [{[boolean(), term()], dynamic()}]},
        {:erlang, :delete_element, [{[integer(), open_tuple([])], dynamic(open_tuple([]))}]},
        {:erlang, :hd, [{[non_empty_list(term(), term())], dynamic()}]},
        {:erlang, :element, [{[integer(), open_tuple([])], dynamic()}]},
        {:erlang, :insert_element,
         [{[integer(), open_tuple([]), term()], dynamic(open_tuple([]))}]},
        {:erlang, :list_to_tuple, [{[list(term())], dynamic(open_tuple([]))}]},
        {:erlang, :max, [{[term(), term()], dynamic()}]},
        {:erlang, :min, [{[term(), term()], dynamic()}]},
        {:erlang, :orelse, [{[boolean(), term()], dynamic()}]},
        {:erlang, :send, [{[send_destination, term()], dynamic()}]},
        {:erlang, :setelement, [{[integer(), open_tuple([]), term()], dynamic(open_tuple([]))}]},
        {:erlang, :tl, [{[non_empty_list(term(), term())], dynamic()}]},
        {:erlang, :tuple_to_list, [{[open_tuple([])], dynamic(list(term()))}]}
      ] do
    [arity] = Enum.map(clauses, fn {args, _return} -> length(args) end) |> Enum.uniq()

    true =
      Code.ensure_loaded?(mod) and
        (function_exported?(mod, fun, arity) or fun in [:orelse, :andalso])

    domain_clauses =
      case clauses do
        [_] ->
          {:strong, nil, clauses}

        _ ->
          domain =
            clauses
            |> Enum.map(fn {args, _} -> args end)
            |> Enum.zip_with(fn types -> Enum.reduce(types, &union/2) end)

          {:strong, domain, clauses}
      end

    defp signature(unquote(mod), unquote(fun), unquote(arity)),
      do: unquote(Macro.escape(domain_clauses))
  end

  defp signature(_mod, _fun, _arity), do: :none

  @doc """
  Applies a function in unknown modules.

  Used only by info functions.
  """
  def remote(_name, _args_types, _expr, %{mode: :traversal}, context) do
    {dynamic(), context}
  end

  def remote(name, args_types, expr, stack, context) do
    arity = length(args_types)

    case signature(name, arity) do
      :none -> {dynamic(), context}
      info -> apply_remote(nil, name, info, args_types, expr, stack, context)
    end
  end

  @doc """
  Applies a function in a given module.
  """
  def remote(_module, _fun, _args_types, _expr, %{mode: :traversal}, context) do
    {dynamic(), context}
  end

  def remote(:erlang, :element, [_, tuple], {_, meta, [index, _]} = expr, stack, context)
      when is_integer(index) do
    case tuple_fetch(tuple, index - 1) do
      {_optional?, value_type} ->
        {value_type, context}

      :badtuple ->
        {error_type(),
         badremote_error(:erlang, :element, expr, [integer(), tuple], stack, context)}

      :badindex ->
        mfac = mfac(expr, :erlang, :element, 2)

        {error_type(),
         error({:badindex, mfac, expr, tuple, index - 1, context}, meta, stack, context)}
    end
  end

  def remote(
        :erlang,
        :insert_element,
        [_, tuple, value],
        {_, meta, [index, _, _]} = expr,
        stack,
        context
      )
      when is_integer(index) do
    case tuple_insert_at(tuple, index - 1, value) do
      value_type when is_descr(value_type) ->
        {value_type, context}

      :badtuple ->
        args_types = [integer(), tuple, value]

        {error_type(),
         badremote_error(:erlang, :insert_element, expr, args_types, stack, context)}

      :badindex ->
        mfac = mfac(expr, :erlang, :insert_element, 3)

        {error_type(),
         error({:badindex, mfac, expr, tuple, index - 2, context}, meta, stack, context)}
    end
  end

  def remote(:erlang, :delete_element, [_, tuple], {_, meta, [index, _]} = expr, stack, context)
      when is_integer(index) do
    case tuple_delete_at(tuple, index - 1) do
      value_type when is_descr(value_type) ->
        {value_type, context}

      :badtuple ->
        args_types = [integer(), tuple]

        {error_type(),
         badremote_error(:erlang, :delete_element, expr, args_types, stack, context)}

      :badindex ->
        mfac = mfac(expr, :erlang, :delete_element, 2)

        {error_type(),
         error({:badindex, mfac, expr, tuple, index - 1, context}, meta, stack, context)}
    end
  end

  def remote(:erlang, :make_tuple, [_, elem], {_, _meta, [size, _]}, _stack, context)
      when is_integer(size) and size >= 0 do
    {tuple(List.duplicate(elem, size)), context}
  end

  def remote(:erlang, :hd, [list], expr, stack, context) do
    case list_hd(list) do
      {_, value_type} ->
        {value_type, context}

      :badnonemptylist ->
        {error_type(), badremote_error(:erlang, :hd, expr, [list], stack, context)}
    end
  end

  def remote(:erlang, :tl, [list], expr, stack, context) do
    case list_tl(list) do
      {_, value_type} ->
        {value_type, context}

      :badnonemptylist ->
        {error_type(), badremote_error(:erlang, :tl, expr, [list], stack, context)}
    end
  end

  def remote(:erlang, name, [left, right] = args_types, expr, stack, context)
      when name in [:>=, :"=<", :>, :<, :min, :max] do
    context =
      cond do
        stack.mode == :infer ->
          context

        match?({false, _}, map_fetch(left, :__struct__)) or
            match?({false, _}, map_fetch(right, :__struct__)) ->
          warning = {:struct_comparison, expr, name, left, right, context}
          warn(__MODULE__, warning, elem(expr, 1), stack, context)

        number_type?(left) and number_type?(right) ->
          context

        disjoint?(left, right) ->
          warning = {:mismatched_comparison, expr, name, left, right, context}
          warn(__MODULE__, warning, elem(expr, 1), stack, context)

        true ->
          context
      end

    if name in [:min, :max] do
      {union(left, right), context}
    else
      {return(boolean(), args_types, stack), context}
    end
  end

  def remote(
        :erlang,
        name,
        [left, right] = args_types,
        {_, _, args} = expr,
        stack,
        context
      )
      when name in [:==, :"/=", :"=:=", :"=/="] do
    context =
      cond do
        # We ignore quoted literals as they most likely come from generated code.
        stack.mode == :infer or Macro.quoted_literal?(args) ->
          context

        name in [:==, :"/="] and number_type?(left) and number_type?(right) ->
          context

        disjoint?(left, right) ->
          warning = {:mismatched_comparison, expr, name, left, right, context}
          warn(__MODULE__, warning, elem(expr, 1), stack, context)

        true ->
          context
      end

    {return(boolean(), args_types, stack), context}
  end

  def remote(mod, fun, args_types, expr, stack, context) do
    arity = length(args_types)

    case :elixir_rewrite.inline(mod, fun, arity) do
      {new_mod, new_fun} ->
        expr = inline_meta(expr, mod, fun)
        remote(new_mod, new_fun, args_types, expr, stack, context)

      false ->
        {info, context} = signature(mod, fun, arity, elem(expr, 1), stack, context)
        apply_remote(mod, fun, info, args_types, expr, stack, context)
    end
  end

  defp apply_remote(mod, fun, info, args_types, expr, stack, context) do
    case apply_signature(info, args_types, stack) do
      {:ok, _indexes, type} ->
        {type, context}

      {:error, domain, clauses} ->
        mfac = mfac(expr, mod, fun, length(args_types))
        error = {:badremote, mfac, expr, args_types, domain, clauses, context}
        {error_type(), error(error, elem(expr, 1), stack, context)}
    end
  end

  defp inline_meta({node, meta, args}, mod, fun) do
    {node, [inline: {mod, fun}] ++ meta, args}
  end

  @doc """
  Returns the type of a remote capture.
  """
  def remote_capture(modules, fun, arity, meta, stack, context) do
    # TODO: We cannot return the unions of functions. Do we forbid this?
    # Do we check it is always the same return type? Do we simply say it is a function?
    if stack.mode == :traversal do
      {dynamic(fun()), context}
    else
      context =
        Enum.reduce(
          modules,
          context,
          &(signature(&1, fun, arity, meta, stack, &2) |> elem(1))
        )

      {dynamic(fun()), context}
    end
  end

  @doc """
  Gets a mfa signature.

  It returns either a tuple with the remote information and the context.
  The remote information may be one of:

    * `:none` - no typing information found.

    * `{:infer, clauses}` - clauses from inferences. You must check all
      all clauses and return the union between them. They are dynamic
      and they can only be converted into arrows by computing the union
      of all arguments.

    * `{:strong, domain or nil, clauses}` - clauses from signatures. So far
      these are strong arrows with non-overlapping domains

  """
  def signature(module, fun, arity, meta, stack, context) when is_atom(module) do
    if Keyword.get(meta, :runtime_module, false) do
      {:none, context}
    else
      case signature(module, fun, arity) do
        :none -> export(module, fun, arity, meta, stack, context)
        clauses -> {clauses, context}
      end
    end
  end

  defp export(_module, :module_info, arity, _meta, _stack, context) when arity in [0, 1] do
    {signature(:module_info, arity), context}
  end

  defp export(module, fun, arity, meta, %{cache: cache} = stack, context) do
    cond do
      cache == nil or stack.mode == :traversal ->
        {:none, context}

      stack.mode == :infer and not builtin_module?(module) ->
        {:none, context}

      true ->
        case ParallelChecker.fetch_export(stack.cache, module, fun, arity) do
          {:ok, mode, reason, info} ->
            info = if info == :none, do: signature(fun, arity), else: info
            {info, check_deprecated(mode, module, fun, arity, reason, meta, stack, context)}

          {:error, type} ->
            context =
              if warn_undefined?(module, fun, arity, stack) do
                warn(__MODULE__, {:undefined, type, module, fun, arity}, meta, stack, context)
              else
                context
              end

            {:none, context}
        end
    end
  end

  defp check_deprecated(:elixir, module, fun, arity, reason, meta, stack, context) do
    if reason do
      warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)
    else
      context
    end
  end

  defp check_deprecated(:erlang, module, fun, arity, _reason, meta, stack, context) do
    case :otp_internal.obsolete(module, fun, arity) do
      {:deprecated, string} when is_list(string) ->
        reason = string |> List.to_string() |> :string.titlecase()
        warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)

      {:deprecated, string, removal} when is_list(string) and is_list(removal) ->
        reason = string |> List.to_string() |> :string.titlecase()
        reason = "It will be removed in #{removal}. #{reason}"
        warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)

      _ ->
        context
    end
  end

  defp builtin_module?(module) do
    is_map_key(builtin_modules(), module)
  end

  @builtin_protocols [
    Collectable,
    Enumerable,
    IEx.Info,
    Inspect,
    JSON.Encoder,
    List.Chars,
    String.Chars
  ]

  defp builtin_modules do
    case :persistent_term.get(__MODULE__, nil) do
      nil ->
        {:ok, mods} = :application.get_key(:elixir, :modules)
        mods = Map.from_keys(mods -- @builtin_protocols, [])
        :persistent_term.put(__MODULE__, mods)
        mods

      %{} = mods ->
        mods
    end
  end

  defp warn_undefined?(_, _, _, %{no_warn_undefined: %Macro.Env{}}) do
    false
  end

  defp warn_undefined?(_, _, _, %{no_warn_undefined: :all}) do
    false
  end

  defp warn_undefined?(module, fun, arity, stack) do
    not Enum.any?(stack.no_warn_undefined, &(&1 == module or &1 == {module, fun, arity}))
  end

  ## Local

  @doc """
  Deal with local functions.
  """
  def local(fun, args_types, {_, meta, _} = expr, stack, context) do
    fun_arity = {fun, length(args_types)}

    case stack.local_handler.(meta, fun_arity, stack, context) do
      false ->
        {dynamic(), context}

      {_kind, _info, context} when stack.mode == :traversal ->
        {dynamic(), context}

      {kind, info, context} ->
        case apply_signature(info, args_types, stack) do
          {:ok, indexes, type} ->
            context =
              if stack.mode != :infer and kind == :defp do
                update_in(context.local_used[fun_arity], fn current ->
                  if info == :none do
                    []
                  else
                    (current || used_from_clauses(info)) -- indexes
                  end
                end)
              else
                context
              end

            {type, context}

          {:error, domain, clauses} ->
            error = {:badlocal, expr, args_types, domain, clauses, context}
            {error_type(), error(error, with_span(meta, fun), stack, context)}
        end
    end
  end

  defp used_from_clauses({:infer, clauses}),
    do: Enum.with_index(clauses, fn _, i -> i end)

  defp used_from_clauses({:strong, _, clauses}),
    do: Enum.with_index(clauses, fn _, i -> i end)

  @doc """
  Deal with local captures.
  """
  def local_capture(fun, arity, meta, stack, context) do
    fun_arity = {fun, arity}

    case stack.local_handler.(meta, fun_arity, stack, context) do
      false ->
        {dynamic(fun()), context}

      {_kind, _info, context} when stack.mode == :traversal ->
        {dynamic(fun()), context}

      {kind, _info, context} ->
        if stack.mode != :infer and kind == :defp do
          # Mark all clauses as used, as the function is being exported.
          {dynamic(fun()), put_in(context.local_used[fun_arity], [])}
        else
          {dynamic(fun()), context}
        end
    end
  end

  ## Application helpers

  defp return(type, args_types, stack) do
    cond do
      stack.mode == :static -> type
      Enum.any?(args_types, &gradual?/1) -> dynamic(type)
      true -> type
    end
  end

  defp apply_signature(:none, _args_types, _stack) do
    {:ok, [], dynamic()}
  end

  defp apply_signature({:strong, nil, [{expected, return}] = clauses}, args_types, stack) do
    # Optimize single clauses as the domain is the single clause args.
    case zip_compatible?(args_types, expected) do
      true -> {:ok, [0], return(return, args_types, stack)}
      false -> {:error, expected, clauses}
    end
  end

  defp apply_signature({:strong, domain, clauses}, args_types, stack) do
    # If the type is only gradual, the compatibility check is the same
    # as a non disjoint check. So we skip checking compatibility twice.
    with true <- zip_compatible_or_only_gradual?(args_types, domain),
         {count, used, returns} when count > 0 <- apply_clauses(clauses, args_types, 0, 0, [], []) do
      {:ok, used, returns |> Enum.reduce(&union/2) |> return(args_types, stack)}
    else
      _ -> {:error, domain, clauses}
    end
  end

  defp apply_signature({:infer, clauses}, args_types, _stack) do
    case apply_clauses(clauses, args_types, 0, 0, [], []) do
      {0, [], []} ->
        domain =
          clauses
          |> Enum.map(fn {args, _} -> args end)
          |> Enum.zip_with(fn types -> Enum.reduce(types, &union/2) end)

        {:error, domain, clauses}

      {count, used, _returns} when count > @max_clauses ->
        {:ok, used, dynamic()}

      {_count, used, returns} ->
        {:ok, used, returns |> Enum.reduce(&union/2) |> dynamic()}
    end
  end

  defp apply_clauses([{expected, return} | clauses], args_types, index, count, used, returns) do
    if zip_not_disjoint?(args_types, expected) do
      apply_clauses(clauses, args_types, index + 1, count + 1, [index | used], [return | returns])
    else
      apply_clauses(clauses, args_types, index + 1, count, used, returns)
    end
  end

  defp apply_clauses([], _args_types, _index, count, used, returns) do
    {count, used, returns}
  end

  defp zip_compatible_or_only_gradual?([actual | actuals], [expected | expecteds]) do
    (only_gradual?(actual) or compatible?(actual, expected)) and
      zip_compatible_or_only_gradual?(actuals, expecteds)
  end

  defp zip_compatible_or_only_gradual?([], []), do: true

  defp zip_compatible?([actual | actuals], [expected | expecteds]) do
    compatible?(actual, expected) and zip_compatible?(actuals, expecteds)
  end

  defp zip_compatible?([], []), do: true

  defp zip_not_disjoint?([actual | actuals], [expected | expecteds]) do
    not disjoint?(actual, expected) and zip_not_disjoint?(actuals, expecteds)
  end

  defp zip_not_disjoint?([], []), do: true

  ## Error handling

  defp error(warning, meta, stack, context) do
    error(__MODULE__, warning, meta, stack, context)
  end

  defp badremote_error(mod, fun, {_, meta, _} = expr, args_types, stack, context) do
    arity = length(args_types)
    mfac = mfac(expr, mod, fun, arity)
    {_type, domain, [{args, _} | _] = clauses} = signature(mod, fun, arity)
    domain = domain || args
    tuple = {:badremote, mfac, expr, args_types, domain, clauses, context}
    error(tuple, meta, stack, context)
  end

  ## Diagnostics

  def format_diagnostic({:badindex, mfac, expr, type, index, context}) do
    traces = collect_traces(expr, context)
    {mod, fun, arity, _converter} = mfac
    mfa = Exception.format_mfa(mod, fun, arity)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a tuple with at least #{pluralize(index + 1, "element", "elements")} in #{mfa}:

              #{expr_to_string(expr) |> indent(4)}

          the given type does not have the given index:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badlocal, expr, args_types, domain, clauses, context}) do
    traces = collect_traces(expr, context)
    converter = &Function.identity/1
    {fun, _, _} = expr

    explanation =
      empty_arg_reason(args_types) ||
        """
        but expected one of:
        #{clauses_args_to_quoted_string(clauses, converter, [])}
        """

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types given to #{fun}/#{length(args_types)}:

              #{expr_to_string(expr) |> indent(4)}

          given types:

              #{args_to_quoted_string(args_types, domain, converter) |> indent(4)}

          """,
          explanation,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badremote, mfac, expr, args_types, domain, clauses, context}) do
    traces = collect_traces(expr, context)
    {mod, fun, arity, converter} = mfac
    meta = elem(expr, 1)

    # Protocol errors can be very verbose, so we collapse structs
    {banner, hints, opts} =
      cond do
        meta[:from_interpolation] ->
          {_, _, [arg]} = expr

          {"""
           incompatible value given to string interpolation:

               #{expr_to_string(arg) |> indent(4)}

           it has type:
           """, [:interpolation], [collapse_structs: true]}

        Code.ensure_loaded?(mod) and
            Keyword.has_key?(mod.module_info(:attributes), :__protocol__) ->
          {nil, [{:protocol, mod}], [collapse_structs: true]}

        true ->
          {nil, [], []}
      end

    explanation =
      empty_arg_reason(converter.(args_types)) ||
        """
        but expected one of:
        #{clauses_args_to_quoted_string(clauses, converter, opts)}
        """

    mfa_or_fa = if mod, do: Exception.format_mfa(mod, fun, arity), else: "#{fun}/#{arity}"

    banner =
      banner ||
        """
        incompatible types given to #{mfa_or_fa}:

            #{expr_to_string(expr) |> indent(4)}

        given types:
        """

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          banner,
          """

              #{args_to_quoted_string(args_types, domain, converter) |> indent(4)}

          """,
          explanation,
          format_traces(traces),
          format_hints(hints)
        ])
    }
  end

  def format_diagnostic({:mismatched_comparison, expr, name, left, right, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          comparison between distinct types found:

              #{expr_to_string(expr) |> indent(4)}

          given types:

              #{type_comparison_to_string(name, left, right) |> indent(4)}
          """,
          format_traces(traces),
          """

          While Elixir can compare across all types, you are comparing \
          across types which are always disjoint, and the result is either \
          always true or always false
          """
        ])
    }
  end

  def format_diagnostic({:struct_comparison, expr, name, left, right, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          comparison with structs found:

              #{expr_to_string(expr) |> indent(4)}

          given types:

              #{type_comparison_to_string(name, left, right) |> indent(4)}
          """,
          format_traces(traces),
          """

          Comparison operators (>, <, >=, <=, min, and max) perform structural \
          and not semantic comparison. Comparing with a struct won't give meaningful \
          results. Structs that can be compared typically define a compare/2 function \
          within their modules that can be used for semantic comparison.
          """
        ])
    }
  end

  def format_diagnostic({:undefined, :module, module, fun, arity}) do
    top =
      if fun == :__struct__ and arity == 0 do
        "struct #{inspect(module)}"
      else
        Exception.format_mfa(module, fun, arity)
      end

    %{
      message:
        IO.iodata_to_binary([
          top,
          " is undefined (module ",
          inspect(module),
          " is not available or is yet to be defined)",
          UndefinedFunctionError.hint_for_missing_module(module, fun, arity)
        ]),
      group: true
    }
  end

  def format_diagnostic({:undefined, :function, module, :__struct__, 0}) do
    %{
      message:
        "struct #{inspect(module)} is undefined (there is such module but it does not define a struct)",
      group: true
    }
  end

  def format_diagnostic({:undefined, :function, module, fun, arity}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is undefined or private",
          UndefinedFunctionError.hint_for_loaded_module(module, fun, arity)
        ]),
      group: true
    }
  end

  def format_diagnostic({:deprecated, module, fun, arity, reason}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is deprecated. ",
          reason
        ]),
      group: true
    }
  end

  defp empty_arg_reason(args_types) do
    if i = Enum.find_index(args_types, &empty?/1) do
      """
      the #{integer_to_ordinal(i + 1)} argument is empty (often represented as none()), \
      most likely because it is the result of an expression that always fails, such as \
      a `raise` or a previous invalid call. This causes any function called with this \
      value to fail
      """
    end
  end

  defp pluralize(1, singular, _), do: "1 #{singular}"
  defp pluralize(i, _, plural), do: "#{i} #{plural}"

  defp mfac({_, [inline: {mod, fun}] ++ _, _}, _mod, _fun, arity) do
    {mod, fun, arity, & &1}
  end

  defp mfac({{:., _, [mod, fun]}, _, args}, _mod, _fun, _arity)
       when is_atom(mod) and is_atom(fun) do
    {mod, fun, args, converter} = :elixir_rewrite.erl_to_ex(mod, fun, args)
    {mod, fun, length(args), converter}
  end

  defp mfac(_, mod, fun, arity)
       when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    {mod, fun, arity, & &1}
  end

  ## Algebra helpers

  alias Inspect.Algebra, as: IA

  defp type_comparison_to_string(fun, left, right) do
    {Kernel, fun, [left, right], _} = :elixir_rewrite.erl_to_ex(:erlang, fun, [left, right])

    {fun, [], [to_quoted(left, collapse_structs: true), to_quoted(right, collapse_structs: true)]}
    |> Code.Formatter.to_algebra()
    |> Inspect.Algebra.format(98)
    |> IO.iodata_to_binary()
  end

  defp clauses_args_to_quoted_string([{args, _return}], converter, opts) do
    "\n    " <> (clause_args_to_quoted_string(args, converter, opts) |> indent(4))
  end

  defp clauses_args_to_quoted_string(clauses, converter, opts) do
    clauses
    |> Enum.with_index(fn {args, _return}, index ->
      """

      ##{index + 1}
      #{clause_args_to_quoted_string(args, converter, opts)}\
      """
      |> indent(4)
    end)
    |> Enum.join("\n")
  end

  defp clause_args_to_quoted_string(args, converter, opts) do
    docs = Enum.map(args, &(&1 |> to_quoted(opts) |> Code.Formatter.to_algebra()))
    args_docs_to_quoted_string(converter.(docs))
  end

  defp args_to_quoted_string(args_types, domain, converter) do
    ansi? = IO.ANSI.enabled?()

    docs =
      Enum.zip_with(args_types, domain, fn actual, expected ->
        doc = actual |> to_quoted() |> Code.Formatter.to_algebra()

        cond do
          compatible?(actual, expected) -> doc
          ansi? -> IA.concat(IA.color(doc, IO.ANSI.red()), IA.color(IA.empty(), IO.ANSI.reset()))
          true -> IA.concat(["-", doc, "-"])
        end
      end)

    args_docs_to_quoted_string(converter.(docs))
  end

  defp args_docs_to_quoted_string(docs) do
    doc = IA.fold(docs, fn doc, acc -> IA.glue(IA.concat(doc, ","), acc) end)

    wrapped_docs =
      case docs do
        [_] -> IA.concat("(", IA.concat(doc, ")"))
        _ -> IA.group(IA.glue(IA.nest(IA.glue("(", "", doc), 2), "", ")"))
      end

    wrapped_docs
    |> IA.format(98)
    |> IO.iodata_to_binary()
    |> case do
      "(\n" <> _ = multiple_lines -> multiple_lines
      single_line -> binary_slice(single_line, 1..-2//1)
    end
  end
end
