defmodule Record.Extractor do
  @moduledoc false

  # Retrieve a record definition from an Erlang file using
  # the same lookup as the *include* attribute from Erlang modules.
  def retrieve(name, from: string) do
    file = to_char_list(string)

    case :code.where_is_file(file) do
      :non_existing -> realfile = file
      realfile -> nil
    end

    retrieve_record(name, realfile)
  end

  # Retrieve a record definition from an Erlang file using
  # the same lookup as the *include_lib* attribute from Erlang modules.
  def retrieve(name, from_lib: file) do
    [app|path] = :filename.split(to_char_list(file))

    case :code.lib_dir(to_char_list(app)) do
      { :error, _ } ->
        raise ArgumentError, "lib file #{to_binary(file)} could not be found"
      libpath ->
        retrieve_record name, :filename.join([libpath|path])
    end
  end

  # Retrieve the record with the given name from the given file
  defp retrieve_record(name, file) do
    form = read_file(file)
    records = retrieve_records(form)
    if record = List.keyfind(records, name, 0) do
      parse_record(record, form)
    else
      raise ArgumentError, "no record #{name} found at #{to_binary(file)}"
    end
  end

  # Parse the given file and retrieve all existent records.
  defp retrieve_records(form) do
    lc { :attribute, _, :record, record } inlist form, do: record
  end

  # Read a file and return its abstract syntax form that also
  # includes record and other preprocessor modules. This is done
  # by using Erlang's epp_dodger.
  defp read_file(file) do
    case :epp_dodger.quick_parse_file(file) do
      { :ok, form } ->
        form
      other ->
        raise "error parsing file #{to_binary(file)}, got: #{inspect(other)}"
    end
  end

  # Parse a tuple with name and fields and returns a
  # list of tuples where the first element is the field
  # and the second is its default value.
  defp parse_record({ _name, fields }, form) do
    cons = List.foldr fields, { nil, 0 }, fn f, acc ->
      { :cons, 0, parse_field(f), acc }
    end
    eval_record(cons, form)
  end

  defp parse_field({ :typed_record_field, record_field, _type }) do
    parse_field(record_field)
  end

  defp parse_field({ :record_field, _, key }) do
    { :tuple, 0, [key, {:atom, 0, :undefined}] }
  end

  defp parse_field({ :record_field, _, key, value }) do
    { :tuple, 0, [key, value] }
  end

  defp eval_record(cons, form) do
    form = form ++
      [ { :function, 0, :hello, 0, [
          { :clause, 0, [], [], [ cons ] } ] } ]

    { :function, 0, :hello, 0, [
      { :clause, 0, [], [], [ record_ast ] } ] } = :erl_expand_records.module(form, []) |> List.last

    { :value, record, _ } = :erl_eval.expr(record_ast, [])
    record
  end
end