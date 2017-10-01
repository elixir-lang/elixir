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
      Inspect.Algebra,
      Inspect.Opts,
      Integer,
      Keyword,
      List,
      Map,
      MapSet,
      Port,
      Process,
      Protocol,
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

    "Code": [
      Code,
      Kernel.ParallelCompiler,
      Macro,
      Macro.Env,
      Module,
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
      StringIO,
      System,
    ],

    "OTP": [
      Agent,
      Application,
      GenServer,
      Node,
      Registry,
      Supervisor,
      Task,
      Task.Supervisor,
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
