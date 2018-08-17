# Returns config for Elixir docs
[
  extras: Path.wildcard("lib/elixir/pages/*.md"),
  groups_for_functions: [
    Guards: & &1[:guard] == true
  ],
  groups_for_modules: [
    # [Kernel, Kernel.SpecialForms],

    "Basic Types": [
      Atom,
      Base,
      Bitwise,
      Calendar,
      Calendar.ISO,
      Date,
      DateTime,
      Exception,
      Float,
      Function,
      Integer,
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
    "Modules & Code": [
      Code,
      Kernel.ParallelCompiler,
      Macro,
      Macro.Env,
      Module
    ],
    "Processes & Applications": [
      Agent,
      Application,
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
