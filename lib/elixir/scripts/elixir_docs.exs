# Returns config for Elixir docs (exclusively)
canonical = System.fetch_env!("CANONICAL")

[
  assets: "lib/elixir/pages/images",
  extras: [
    "lib/elixir/pages/getting-started/introduction.md",
    "lib/elixir/pages/getting-started/basic-types.md",
    "lib/elixir/pages/getting-started/lists-and-tuples.md",
    "lib/elixir/pages/getting-started/pattern-matching.md",
    "lib/elixir/pages/getting-started/case-cond-and-if.md",
    "lib/elixir/pages/getting-started/anonymous-functions.md",
    "lib/elixir/pages/getting-started/binaries-strings-and-charlists.md",
    "lib/elixir/pages/getting-started/keywords-and-maps.md",
    "lib/elixir/pages/getting-started/modules-and-functions.md",
    "lib/elixir/pages/getting-started/recursion.md",
    "lib/elixir/pages/getting-started/enumerable-and-streams.md",
    "lib/elixir/pages/getting-started/processes.md",
    "lib/elixir/pages/getting-started/io-and-the-file-system.md",
    "lib/elixir/pages/getting-started/alias-require-and-import.md",
    "lib/elixir/pages/getting-started/module-attributes.md",
    "lib/elixir/pages/getting-started/structs.md",
    "lib/elixir/pages/getting-started/protocols.md",
    "lib/elixir/pages/getting-started/comprehensions.md",
    "lib/elixir/pages/getting-started/sigils.md",
    "lib/elixir/pages/getting-started/try-catch-and-rescue.md",
    "lib/elixir/pages/getting-started/writing-documentation.md",
    "lib/elixir/pages/getting-started/optional-syntax.md",
    "lib/elixir/pages/getting-started/erlang-libraries.md",
    "lib/elixir/pages/getting-started/debugging.md",
    "lib/elixir/pages/cheatsheets/enum-cheat.cheatmd",
    "lib/elixir/pages/anti-patterns/what-anti-patterns.md",
    "lib/elixir/pages/anti-patterns/code-anti-patterns.md",
    "lib/elixir/pages/anti-patterns/design-anti-patterns.md",
    "lib/elixir/pages/anti-patterns/process-anti-patterns.md",
    "lib/elixir/pages/anti-patterns/macro-anti-patterns.md",
    "lib/elixir/pages/references/compatibility-and-deprecations.md",
    "lib/elixir/pages/references/gradual-set-theoretic-types.md",
    "lib/elixir/pages/references/library-guidelines.md",
    "lib/elixir/pages/references/naming-conventions.md",
    "lib/elixir/pages/references/operators.md",
    "lib/elixir/pages/references/patterns-and-guards.md",
    "lib/elixir/pages/references/syntax-reference.md",
    "lib/elixir/pages/references/typespecs.md",
    "lib/elixir/pages/references/unicode-syntax.md",
    "lib/elixir/pages/mix-and-otp/introduction-to-mix.md",
    "lib/elixir/pages/mix-and-otp/agents.md",
    "lib/elixir/pages/mix-and-otp/genservers.md",
    "lib/elixir/pages/mix-and-otp/supervisor-and-application.md",
    "lib/elixir/pages/mix-and-otp/dynamic-supervisor.md",
    "lib/elixir/pages/mix-and-otp/erlang-term-storage.md",
    "lib/elixir/pages/mix-and-otp/dependencies-and-umbrella-projects.md",
    "lib/elixir/pages/mix-and-otp/task-and-gen-tcp.md",
    "lib/elixir/pages/mix-and-otp/docs-tests-and-with.md",
    "lib/elixir/pages/mix-and-otp/distributed-tasks.md",
    "lib/elixir/pages/mix-and-otp/config-and-releases.md",
    "lib/elixir/pages/meta-programming/quote-and-unquote.md",
    "lib/elixir/pages/meta-programming/macros.md",
    "lib/elixir/pages/meta-programming/domain-specific-languages.md",
    "CHANGELOG.md"
  ],
  deps: [
    eex: "https://hexdocs.pm/eex/#{canonical}",
    ex_unit: "https://hexdocs.pm/ex_unit/#{canonical}",
    iex: "https://hexdocs.pm/iex/#{canonical}",
    logger: "https://hexdocs.pm/logger/#{canonical}",
    mix: "https://hexdocs.pm/mix/#{canonical}"
  ],
  groups_for_extras: [
    "Getting started": ~r"pages/getting-started/.*\.md$",
    Cheatsheets: ~r"pages/cheatsheets/.*\.cheatmd$",
    "Anti-patterns": ~r"pages/anti-patterns/.*\.md$",
    "Meta-programming": ~r"pages/meta-programming/.*\.md$",
    "Mix & OTP": ~r"pages/mix-and-otp/.*\.md$",
    References: ~r"pages/references/.*\.md$"
  ],
  groups_for_functions: [
    Guards: &(&1[:guard] == true)
  ],
  skip_undefined_reference_warnings_on: [
    "lib/elixir/pages/references/compatibility-and-deprecations.md"
  ],
  skip_code_autolink_to: [
    "Enumerable.List",
    "Inspect.MapSet"
  ],
  formatters: ["html", "epub"],
  groups_for_modules: [
    # [Kernel, Kernel.SpecialForms],

    "Data Types": [
      Atom,
      Base,
      Bitwise,
      Date,
      DateTime,
      Duration,
      Exception,
      Float,
      Function,
      Integer,
      Module,
      NaiveDateTime,
      Record,
      Regex,
      String,
      Time,
      Tuple,
      URI,
      Version,
      Version.Requirement
    ],
    "Collections & Enumerables": [
      Access,
      Date.Range,
      Enum,
      Keyword,
      List,
      Map,
      MapSet,
      Range,
      Stream
    ],
    "IO & System": [
      File,
      File.Stat,
      File.Stream,
      IO,
      IO.ANSI,
      IO.Stream,
      OptionParser,
      Path,
      Port,
      StringIO,
      System
    ],
    Calendar: [
      Calendar,
      Calendar.ISO,
      Calendar.TimeZoneDatabase,
      Calendar.UTCOnlyTimeZoneDatabase
    ],
    "Processes & Applications": [
      Agent,
      Application,
      Config,
      Config.Provider,
      Config.Reader,
      DynamicSupervisor,
      GenServer,
      Node,
      PartitionSupervisor,
      Process,
      Registry,
      Supervisor,
      Task,
      Task.Supervisor
    ],
    Protocols: [
      Collectable,
      Enumerable,
      Inspect,
      Inspect.Algebra,
      Inspect.Opts,
      List.Chars,
      Protocol,
      String.Chars
    ],
    "Code & Macros": [
      Code,
      Code.Fragment,
      Kernel.ParallelCompiler,
      Macro,
      Macro.Env
    ]

    ## Automatically detected groups

    # Deprecated: [
    #   Behaviour,
    #   Dict,
    #   GenEvent,
    #   HashDict,
    #   HashSet,
    #   Set,
    #   Supervisor.Spec
    # ]
  ],
  before_closing_body_tag: fn
    :html ->
      """
      <script>
        function mermaidLoaded() {
          mermaid.initialize({
            startOnLoad: false,
            theme: document.body.className.includes("dark") ? "dark" : "default"
          });
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
        }
      </script>
      <script async src="https://cdn.jsdelivr.net/npm/mermaid@10.2.3/dist/mermaid.min.js" onload="mermaidLoaded();"></script>
      """

    _ ->
      ""
  end
]
