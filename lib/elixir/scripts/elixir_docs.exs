# Returns config for Elixir docs (exclusively)
canonical = System.fetch_env!("CANONICAL")

[
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
    "lib/elixir/pages/anti-patterns/what-anti-patterns.md",
    "lib/elixir/pages/anti-patterns/code-anti-patterns.md",
    "lib/elixir/pages/anti-patterns/design-anti-patterns.md",
    "lib/elixir/pages/anti-patterns/process-anti-patterns.md",
    "lib/elixir/pages/anti-patterns/macro-anti-patterns.md",
    "lib/elixir/pages/references/compatibility-and-deprecations.md",
    "lib/elixir/pages/references/library-guidelines.md",
    "lib/elixir/pages/references/naming-conventions.md",
    "lib/elixir/pages/references/operators.md",
    "lib/elixir/pages/references/patterns-and-guards.md",
    "lib/elixir/pages/references/syntax-reference.md",
    "lib/elixir/pages/references/typespecs.md",
    "lib/elixir/pages/references/unicode-syntax.md",
    "lib/elixir/pages/references/writing-documentation.md",
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
    "Anti-patterns": ~r"pages/anti-patterns/.*\.md$",
    References: ~r"pages/references/.*\.md$",
    "Meta-programming": ~r"pages/meta-programming/.*\.md$"
  ],
  groups_for_functions: [
    Guards: &(&1[:guard] == true)
  ],
  skip_undefined_reference_warnings_on: [
    "lib/elixir/pages/references/compatibility-and-deprecations.md"
  ],
  groups_for_modules: [
    # [Kernel, Kernel.SpecialForms],

    "Data Types": [
      Atom,
      Base,
      Bitwise,
      Date,
      DateTime,
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
  ]
]
