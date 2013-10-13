defmodule IEx.Mixfile do
  use Mix.Project

  def project do
    [app: :iex, version: System.version]
  end

  def application do
    [ env: [
        after_spawn: [],
        colors: [ enabled: true,

                  # Used by default on evaluation cycle
                  eval_interrupt: "yellow",
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
        inspect: [limit: 50, raw: false, pretty: true],
        history_size: 20 ] ]
  end
end
