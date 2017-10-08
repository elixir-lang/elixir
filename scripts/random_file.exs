# bin/elixir scripts/random_file.exs

defmodule RandomFile do
  @pattern "lib/*/{lib,unicode,test}/**/*.{ex,exs}"

  def run(args) do
    popper =
      if "--alphabetically" in args do
        &Enum.split(&1, 1)
      else
        fn collection ->
          random = Enum.random(collection)
          {random, List.delete(collection, random)}
        end
      end

    {opts, _} = Code.eval_file(".formatter.exs")

    @pattern
    |> Path.wildcard()
    |> random_file(popper, opts)
  end

  defp random_file([], _popper, _opts) do
    IO.puts("All files are formatted, hooray!")
  end

  defp random_file(collection, popper, opts) do
    {file, collection} = popper.(collection)

    IO.write("Checking #{file}... ")
    input = File.read!(file)
    output = IO.iodata_to_binary([Code.format_string!(file, opts), ?\n])

    if input == output do
      IO.puts("already formatted.")
      random_file(collection, popper, opts)
    else
      IO.write("""
      jackpot!

      The file above needs formatting.
      Please format it with the command below:

          bin/elixir bin/mix format #{file}

      Once formatted, check for any faults and submit a pull request.
      For more information see: https://github.com/elixir-lang/elixir/issues/6643

      Have fun!
      """)
    end
  end
end

RandomFile.run(System.argv())
