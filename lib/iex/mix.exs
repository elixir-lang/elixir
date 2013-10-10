defmodule IEx.Mixfile do
  use Mix.Project

  def project do
    [app: :iex, version: System.version]
  end

  def application do
    [ env: [
        after_spawn: [],
        inspect_opts: [limit: 50, raw: false, pretty: true],
        colors: [ enabled: true,

                  # Used by default on evaluation cycle
                  eval_result: "yellow",
                  eval_error: "red",
                  eval_info: "normal",

                  # Used by ls
                  ls_directory: "blue",
                  ls_device: "green",

                  # Used by ansi docs
                  doc_bold: "bright",
                  doc_code: "cyan,bright",
                  doc_headings: "yellow,bright",
                  doc_inline_code: "cyan",
                  doc_underline: "underline",
                  doc_title: "reverse,yellow,bright" ],
        history_size: 20,
        started: true ] ]
  end
end
