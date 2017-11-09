# Returns config for Elixir docs
[
  extras: Path.wildcard("lib/elixir/pages/*.md"),
  groups_for_modules: [
    # [Kernel, Kernel.SpecialForms],

    "Data & Behaviours": [
      Access,
      Atom,
      Base,
      Bitwise,
      Calendar,
      Calendar.ISO,
      Date,
      Date.Range,
      DateTime,
      Enum,
      Exception,
      Float,
      Integer,
      Keyword,
      List,
      Map,
      MapSet,
      NaiveDateTime,
      Range,
      Record,
      Regex,
      Stream,
      String,
      Time,
      Tuple,
      URI,
      Version,
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

    "Modules & Code": [
      Code,
      Kernel.ParallelCompiler,
      Macro,
      Macro.Env,
      Module,
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
