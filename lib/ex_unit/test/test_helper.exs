Logger.configure_backend(:console, colors: [enabled: false])
ExUnit.start [trace: "--trace" in System.argv, exclude: :wip]

# Beam files compiled on demand
path = Path.expand("../tmp/beams", __DIR__)
File.rm_rf!(path)
File.mkdir_p!(path)
Code.prepend_path(path)

defmodule ExUnit.TestHelpers do
  def write_beam({:module, name, bin, _} = res) do
    beam_path = Path.join(unquote(path), Atom.to_string(name) <> ".beam")
    File.write!(beam_path, bin)
    res
  end
end
