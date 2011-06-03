module ExUnit::Runner
  def __bound__(formatter, cases, max_cases)
    @(
      'formatter: formatter,
      'cases: cases,
      'max_cases: max_cases,
      'taken_cases: 0,
      'sync_cases: []
    )
  end

  % The runner entry point. At first, it will simply spawn cases and start
  % looping expecting messages. When all the cases are spawned and finished,
  % we start running the sync cases. When sync cases finish, tell the formatter
  % we finished and exit.
  def start
    if @cases.empty?
      if @taken_cases > 0
        loop
      elsif @sync_cases.empty?
        @formatter.finish
      else
        spawn_sync_cases.loop
      end
    else
      spawn_cases.loop
    end
  end

  % Loop expecting messages to be sent to the formatter. Whenever a test
  % case has finished executing, decrease the taken cases counter and
  % attempt to spawn new ones.
  %
  % TODO Add timeout.
  def loop
    receive
    match { pid, 'each, { object, test, final } }
      new_formatter = @formatter.each(object, test, final)
      @('formatter: new_formatter).loop
    match { pid, 'each_case, object }
      new_formatter = @formatter.each_case(object)
      @('formatter: new_formatter, 'taken_cases: @taken_cases - 1).start
    end
  end

  % Spawn the maximum possible of cases according to the max_cases value.
  % If any of the cases are set to run synchronously, put them in a list
  % of tests that will be executed at the end.
  def spawn_cases
    case @cases
    match [h|t]
      testcase = h.to_constant

      if testcase.synchronous?
        @('sync_cases: [h|@sync_cases], 'cases: t).spawn_cases
      elsif @taken_cases < @max_cases
        spawn_case(testcase)
        @('taken_cases: @taken_cases + 1, 'cases: t).spawn_cases
      else
        self
      end
    match []
      self
    end
  end

  % After all cases were run, it is time to run the asynchronous ones.
  def spawn_sync_cases
    [h|t] = @sync_cases
    spawn_case h.to_constant
    @('sync_cases: t)
  end

  private

  % Run each test case in its own process.
  def spawn_case(testcase)
    pid = Process.current
    instance = #testcase()
    Process.spawn_link -> run_tests(pid, testcase, instance, instance.__tests__)
  end

  % For each instanciated object, dispatch each test in it.
  def run_tests(pid, object, instance, [test|t])
    final = try
      subject = instance.setup(test)

      partial = try
        subject.send(test)
        nil
      catch kind1: error1
        {kind1, error1, self.__stacktrace__}
      end

      subject.teardown(test)
      partial
    catch kind2: error2
      {kind2, error2, self.__stacktrace__}
    end

    pid <- { Process.current, 'each, { object, test, final } }
    run_tests(pid, object, instance, t)
  end

  % When all tests in a testcase were run, notify the runner.
  def run_tests(pid, object, _, [])
    pid <- { Process.current, 'each_case, object }
  end
end