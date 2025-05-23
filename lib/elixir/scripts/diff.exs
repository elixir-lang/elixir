# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule Diff do
  @moduledoc """
  Utilities for comparing build artifacts.
  """

  @atom_chunks ~w(
    atoms
    attributes
    compile_info
    debug_info
    exports
    labeled_exports
    imports
    indexed_imports
    locals
    labeled_locals
  )a

  @binary_chunks ~w(
    Attr
    AtU8
    CInf
    Dbgi
    Docs
    ExCk
    ExpT
    ImpT
    LocT
  )c

  @doc """
  Compares the build artifacts of two build directories.
  """
  @spec compare_dirs(Path.t(), Path.t()) ::
          {
            only1_paths :: list(Path.t()),
            only2_paths :: list(Path.t()),
            diff :: list({Path.t(), diff :: String.t()})
          }
  def compare_dirs(dir1, dir2) do
    dir1 = Path.expand(dir1)
    dir2 = Path.expand(dir2)

    assert_dir!(dir1)
    assert_dir!(dir2)

    dir1_paths = relative_paths(dir1)
    dir2_paths = relative_paths(dir2)

    only1_paths = dir1_paths -- dir2_paths
    only2_paths = dir2_paths -- dir1_paths
    common_paths = dir1_paths -- only1_paths
    common_files = Enum.reject(common_paths, &File.dir?/1)

    diff =
      Enum.flat_map(common_files, fn path ->
        file1 = Path.join(dir1, path)
        file2 = Path.join(dir2, path)

        case compare_files(file1, file2) do
          :eq -> []
          {:diff, diff} -> [{path, diff}]
        end
      end)

    {only1_paths, only2_paths, diff}
  end

  @doc """
  Compares the contents of two files.

  If the files are BEAM files, it performs a more human-friendly
  "BEAM-diff".
  """
  @spec compare_files(Path.t(), Path.t()) :: :eq | {:diff, diff :: String.t()}
  def compare_files(file1, file2) do
    content1 = File.read!(file1)
    content2 = File.read!(file2)

    if content1 == content2 do
      :eq
    else
      diff =
        if String.ends_with?(file1, ".beam") do
          beam_diff(file1, content1, file2, content2)
        else
          file_diff(file1, file2)
        end

      {:diff, diff}
    end
  end

  defp beam_diff(file1, content1, file2, content2) do
    chunk_diff(content1, content2, @atom_chunks, &inspect(&1, pretty: true, limit: :infinity)) ||
      chunk_diff(content1, content2, @binary_chunks, &(&1 |> write_tmp() |> xxd_dump())) ||
      (
        tmp_file1 =
          file1
          |> xxd_dump()
          |> write_tmp()

        tmp_file2 =
          file2
          |> xxd_dump()
          |> write_tmp()

        file_diff(tmp_file1, tmp_file2)
      )
  end

  defp chunk_diff(content1, content2, names, formatter) do
    with {:ok, {module, chunks1}} <- :beam_lib.chunks(content1, names),
         {:ok, {^module, chunks2}} <- :beam_lib.chunks(content2, names),
         true <- chunks1 != chunks2 do
      if length(chunks1) != length(chunks2) do
        """
        Different chunks:
        * #{inspect(chunks1)}
        * #{inspect(chunks2)}
        """
      else
        for {{name1, chunk1}, {name2, chunk2}} <- Enum.zip(chunks1, chunks2),
            true = name1 == name2,
            chunk1 != chunk2 do
          tmp_file1 = chunk1 |> formatter.() |> write_tmp()
          tmp_file2 = chunk2 |> formatter.() |> write_tmp()
          [to_string(name1), ?\n, file_diff(tmp_file1, tmp_file2)]
        end
      end
    else
      _ -> nil
    end
  end

  defp xxd_dump(file) do
    {dump, _} = System.cmd("xxd", [file])
    dump
  end

  defp file_diff(file1, file2) do
    {diff, _} = System.cmd("diff", ["--suppress-common-lines", file1, file2])
    diff
  end

  defp relative_paths(dir) do
    dir
    |> Path.join("**")
    |> Path.wildcard()
    |> Enum.map(&Path.relative_to(&1, dir))
  end

  defp assert_dir!(dir) do
    if not File.dir?(dir) do
      raise ArgumentError, "#{inspect(dir)} is not a directory"
    end
  end

  defp write_tmp(content) do
    filename = "tmp-#{System.unique_integer([:positive])}"
    File.mkdir_p!("tmp")
    File.write!(Path.join("tmp", filename), content)
    Path.join("tmp", filename)
  end
end

if :deterministic not in :compile.env_compiler_options() do
  IO.puts("Cannot validate if reproducible without setting ERL_COMPILER_OPTIONS=deterministic")
  System.halt(1)
end

case System.argv() do
  [dir1, dir2] ->
    case Diff.compare_dirs(dir1, dir2) do
      {[], [], []} ->
        IO.puts("#{inspect(dir1)} and #{inspect(dir2)} are equal")

      {only1, only2, diff} ->
        for path <- only1, do: IO.puts("Only in #{dir1}: #{path}")
        for path <- only2, do: IO.puts("Only in #{dir2}: #{path}")
        for {path, diff} <- diff, do: IO.puts("Diff #{path}:\n#{diff}")

        System.halt(1)
    end

  _ ->
    IO.puts("Please, provide two directories as arguments")
    System.halt(1)
end
