# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

# Returns config for other apps except Elixir
canonical =
  case System.fetch_env!("CANONICAL") do
    "" -> System.version() <> "/"
    canonical -> canonical
  end

[
  search: [
    %{
      name: "Elixir + libraries",
      help: "Search Elixir, EEx, ExUnit, IEx, Logger, and Mix",
      packages:
        Enum.map(
          [:elixir, :eex, :ex_unit, :iex, :logger, :mix],
          &{&1, String.trim(canonical, "/")}
        )
    },
    %{
      name: "Current project",
      help: "Search only this project"
    }
  ],
  deps: [
    eex: "https://eex.hexdocs.pm/#{canonical}",
    elixir: "https://elixir.hexdocs.pm/#{canonical}",
    ex_unit: "https://ex-unit.hexdocs.pm/#{canonical}",
    iex: "https://iex.hexdocs.pm/#{canonical}",
    logger: "https://logger.hexdocs.pm/#{canonical}",
    mix: "https://mix.hexdocs.pm/#{canonical}"
  ],
  formatters: ["html", "markdown", "epub"],
  before_closing_body_tag: fn
    :html ->
      """
      <script defer src="https://cdn.jsdelivr.net/npm/mermaid@11.6.0/dist/mermaid.min.js"></script>
      <script>
        let initialized = false;

        window.addEventListener("exdoc:loaded", () => {
          if (!initialized) {
            mermaid.initialize({
              startOnLoad: false,
              theme: document.body.className.includes("dark") ? "dark" : "default"
            });
            initialized = true;
          }

          let id = 0;
          for (const codeEl of document.querySelectorAll("pre code.mermaid")) {
            const preEl = codeEl.parentElement;
            const graphDefinition = codeEl.textContent;
            const graphEl = document.createElement("div");
            const graphId = "mermaid-graph-" + id++;
            mermaid.render(graphId, graphDefinition).then(({svg, bindFunctions}) => {
              graphEl.innerHTML = svg;
              bindFunctions?.(graphEl);
              preEl.insertAdjacentElement("afterend", graphEl);
              preEl.remove();
            });
          }
        });
      </script>
      """

    _ ->
      ""
  end
]
