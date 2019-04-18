# Returns config for Elixir docs
[
  extras: Path.wildcard("lib/elixir/pages/*.md"),
  groups_for_functions: [
    Guards: & &1[:guard] == true
  ],
  skip_undefined_reference_warnings_on: ["compatibility-and-deprecations"],
  groups_for_modules: [
    # [Kernel, Kernel.SpecialForms],

    "Basic Types": [
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
      Version
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
    "Calendar": [
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
      Kernel.ParallelCompiler,
      Macro,
      Macro.Env
    ],
    Deprecated: [
      Behaviour,
      Dict,
      GenEvent,
      HashDict,
      HashSet,
      Set,
      Supervisor.Spec
    ]
  ]
]
