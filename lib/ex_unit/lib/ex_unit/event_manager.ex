# This module publishes events during the test suite run.
# This is used, for example, by formatters to print user
# information as well as internal statistics for ExUnit.
defmodule ExUnit.EventManager do
  @moduledoc false

  def start_link() do
    :gen_event.start_link()
  end

  def add_handler(ref, handler, args) do
    :gen_event.add_handler(ref, handler, args)
  end

  def delete_handler(ref, handler, args) do
    :gen_event.delete_handler(ref, handler, args)
  end

  def which_handlers(ref) do
    :gen_event.which_handlers(ref)
  end

  def call(ref, handler, request) do
    :gen_event.call(ref, handler, request)
  end

  def call(ref, handler, request, timeout) do
    :gen_event.call(ref, handler, request, timeout)
  end

  def suite_started(ref, opts) do
    :gen_event.notify(ref, { :suite_started, opts })
  end

  def suite_finished(ref, load_us, run_us) do
    :gen_event.notify(ref, { :suite_finished, load_us, run_us })
  end

  def case_started(ref, test_case) do
    :gen_event.notify(ref, { :case_started, test_case })
  end

  def case_finished(ref, test_case) do
    :gen_event.notify(ref, { :case_finished, test_case })
  end

  def test_started(ref, test) do
    :gen_event.notify(ref, { :test_started, test })
  end

  def test_finished(ref, test) do
    :gen_event.notify(ref, { :test_finished, test })
  end
end
