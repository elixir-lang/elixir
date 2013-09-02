defmodule IEx.Mixfile do
  use Mix.Project

  def project do
    [app: :iex, version: System.version]
  end

  def application do
    [ env: [
        after_spawn: [],
        inspect_opts: [limit: 50, raw: false, pretty: true],
        colors: [     enabled: true,
                  eval_result: "yellow",
                        error: "red",
                         info: "normal",
                    directory: "blue",
                       device: "green",
                # These are used for `h` output                       
                         code: "cyan,bright",
                  inline_code: "cyan",
                         bold: "bright",
                    underline: "underline",
                     headings: "yellow,bright",
                  doc_heading: "reverse,yellow,bright"
                ],
        history_size: 20,
        started: true ] ]
  end
end
