# bin/elixir scripts/random_file.exs

defmodule RandomFile do
  @pattern "lib/*/{lib,unicode,test}/**/*.{ex,exs}"

  def run(["--stat"]) do
    files = Path.wildcard(@pattern)
    {opts, _} = Code.eval_file(".formatter.exs")
    formatted = Enum.filter(files, &formatted?(&1, opts))
    IO.puts("#{length(formatted)} out of #{length(files)} files formatted")
  end

  def run(args) do
    popper =
      if "--alphabetically" in args do
        &{hd(&1), tl(&1)}
      else
        fn collection ->
          random = Enum.random(collection)
          {random, List.delete(collection, random)}
        end
      end

    filter =
      if "--easy" in args do
        &easy?/2
      else
        &(not formatted?(&1, &2))
      end

    halt? = "--no-halt" not in args
    {opts, _} = Code.eval_file(".formatter.exs")

    @pattern
    |> Path.wildcard()
    |> random_file(popper, filter, halt?, opts)
  end

  defp random_file([], _popper, _filter, _halt?, _opts) do
    IO.puts("No pending files found, hooray!")
  end

  defp random_file(collection, popper, filter, halt?, opts) do
    {file, collection} = popper.(collection)
    IO.write("Checking #{file}... ")

    if filter.(file, opts) do
      IO.write("""
      jackpot!

      The file above needs formatting.
      Please format it with the command below:

          bin/elixir bin/mix format #{file}

      Once formatted, check for any faults and submit a pull request.
      For more information see: https://github.com/elixir-lang/elixir/issues/6643

      Have fun!
      """)

      if halt? do
        System.halt(0)
      end
    else
      IO.puts("")
    end

    random_file(collection, popper, filter, halt?, opts)
  end

  defp formatted?(file, opts) do
    input = File.read!(file)
    output = IO.iodata_to_binary([Code.format_string!(input, opts), ?\n])
    input == output
  end

  defp easy?(file, opts) do
    input = File.read!(file)
    output = IO.iodata_to_binary([Code.format_string!(input, opts), ?\n])
    length(String.myers_difference(input, output)) in 2..60
  end
end

RandomFile.run(System.argv())
