# Returns config for Elixir docs
[
  extras: Path.wildcard("lib/elixir/pages/*.md"),
  groups_for_modules: [
    # [Kernel, Kernel.SpecialForms],

    "Data Types": [
      Access,
      Atom,
      Base,
      Bitwise,
      Enum,
      Exception,
      Float,
      Integer,
      Keyword,
      List,
      Map,
      MapSet,
      Range,
      Record,
      Regex,
      Stream,
      String,
      Tuple,
      URI,
      Version,
    ],

    "Date & Time": [
      Calendar,
      Calendar.ISO,
      Date,
      Date.Range,
      DateTime,
      NaiveDateTime,
      Time,
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
      System,
    ],

    "Processes & Applications": [
      Agent,
      Application,
      GenServer,
      Node,
      Process,
      Registry,
      Supervisor,
      Task,
      Task.Supervisor,
    ],

    "Protocols": [
      Collectable,
      Enumerable,
      Inspect,
      Inspect.Algebra,
      Inspect.Opts,
      List.Chars,
      Protocol,
      String.Chars,
    ],

    "Code": [
      Code,
      Kernel.ParallelCompiler,
      Macro,
      Macro.Env,
      Module,
    ],

    "Deprecated": [
      Behaviour,
      Dict,
      GenEvent,
      HashDict,
      HashSet,
      Set,
      Supervisor.Spec
    ],
  ]
]
