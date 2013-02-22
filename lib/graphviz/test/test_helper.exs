# This assumes that the patched version of `cover.erl` was installed. Arguably,
# the ExUnit library should provide direct support for this, including some
# better reporting of the aggregated coverage results.
if System.argv |> List.member?("--coverage") do
  IO.puts "Preparing for collecting coverage..."
  :cover.start
  beam = Path.expand("../../ebin", __FILE__) |> to_char_list
  :cover.compile_beam_directory(beam)

  System.at_exit fn(_) ->
    IO.puts "Generating cover results..."

    cover_dir = Path.expand("../../cover", __FILE__)
    File.mkdir_p!(cover_dir)

    Enum.each :cover.modules, fn(mod) ->
      :cover.analyse_to_file(mod, '#{cover_dir}/#{mod}.html', [:html]) |> IO.inspect
    end
  end
end

ExUnit.start []

# This is a subset of what the mix test helper does. Arguably, the ExUnit
# library should provide some sort of temporary file management.
defmodule GvTest.Case do
  use ExUnit.CaseTemplate

  using do
    quote do
      import GvTest.Case
    end
  end

  teardown do
    del_tmp_paths
    :ok
  end

  def fixture_path do
    Path.expand("../fixtures", __FILE__)
  end

  def fixture_path(extension) do
    Path.join fixture_path, extension
  end

  def tmp_path do
    Path.expand("../../tmp", __FILE__)
  end

  def tmp_path(extension) do
    Path.join tmp_path, extension
  end

  def del_tmp_paths do
    tmp = tmp_path |> binary_to_list
    to_remove = Enum.filter :code.get_path, fn(path) -> :string.str(path, tmp) != 0 end
    Enum.map to_remove, :code.del_path(&1)
  end

  def in_tmp(which, function) do
    path = tmp_path(which)
    File.rm_rf! path
    File.mkdir_p! path
    File.cd! path, function
  end

  def assert_prints(function, expected) do
    path = tmp_path
    File.rm_rf! path
    { :ok, file } = File.open(path, [ :write ])
    function.(file)
    :ok = File.close(file)
    assert_file_content(path, expected)
  end

  def assert_file_content(path, expected) do
    { :ok, actual } = File.read(path)
    assert actual == expected
    File.rm_rf! path
    :ok
  end
end
