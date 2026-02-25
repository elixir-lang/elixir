# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

# Returns config for other apps except Elixir
canonical = System.fetch_env!("CANONICAL")

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
    eex: "https://hexdocs.pm/eex/#{canonical}",
    elixir: "https://hexdocs.pm/elixir/#{canonical}",
    ex_unit: "https://hexdocs.pm/ex_unit/#{canonical}",
    iex: "https://hexdocs.pm/iex/#{canonical}",
    logger: "https://hexdocs.pm/logger/#{canonical}",
    mix: "https://hexdocs.pm/mix/#{canonical}"
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
