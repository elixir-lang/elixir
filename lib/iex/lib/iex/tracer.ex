defmodule IEx.Tracer do
  alias IEx.Tracer.Pattern
  alias IEx.Tracer.Server

  def trace(to_trace) do
    run
    Pattern.compile(to_trace)
      |> Pattern.set
  end

  defp run do
    pid =
      case Process.whereis Dbg do
        nil ->
          pid = :proc_lib.spawn_link(Server, :loop, [])
          Process.register(pid, Dbg)
          pid
        pid -> pid
      end
    :erlang.trace(:all, true, [{:tracer, pid}, :call])
  end

  defmodule Pattern do
   def compile({:/, _, [{{:., _, [module, function]}, _, []}, arity]}) do
     {mfa(module, function, arity), [match_spec(:'_', [])]}
   end

   def compile({{:".", _, [module, function]}, _, args}) do
     {mfa(module, function), [match_spec(args, [])]}
   end

   def set({pattern, options}) do
     :erlang.trace_pattern(pattern, options, [:local])
   end

   def module_name(module_ast), do: Code.eval_quoted(module_ast) |> elem(0)

   def mfa(module, function, arity \\ :'_'), do: {module_name(module), function, arity}

   def match_spec([], conditions), do: {:'_', conditions, options}
   def match_spec(args, conditions), do: {args, conditions, options}

   def options, do: [{:return_trace}, {:exception_trace}]
 end

  defmodule Server do
    def loop() do
      receive do
        msg when elem(msg, 0) == :trace ->
          print_trace(msg)
          loop()
        :exit_tracer ->
          IO.puts("exit tracer")
      end
    end

    def print_trace({:trace, pid, :call, mfa}) do
      IO.puts("#{inspect pid} call #{call_mfa(mfa)}")
    end
    def print_trace({:trace, pid, :return_from, mfa, return}) do
      IO.puts("#{inspect pid} returned #{return_mfa(mfa)}#{inspect return}")
    end
    def print_trace({:trace, pid, :exception_from, mfa, {class, value}}) do
      IO.puts("#{inspect pid} exception #{return_mfa(mfa)}#{inspect class}:#{inspect value}")
    end
    def print_trace(msg) do
      IO.puts("unknown trace message: #{msg}")
    end

    def call_mfa({module, function, arguments}) do
      "#{inspect module}.#{function}(" <> Enum.map_join(arguments, ", ", &inspect(&1)) <> ")"
    end

    def return_mfa({module, function, argument}) do
      "#{inspect module}.#{function}/#{argument} -> "
    end

  end

end
