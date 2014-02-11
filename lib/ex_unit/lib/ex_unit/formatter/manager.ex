defmodule ExUnit.Formatter.Manager do
  @moduledoc """
  Event manager that formatters can register at.

  See `ExUnit.Formatter` for the events formatters are expected to handle.

  This module also contains some wrapper functions for calling the local
  formatter manager. These translate to simple calls to `:gen_event` but they
  are somewhat shorter.
  """
  def start_link() do
    :gen_event.start_link({ :local, __MODULE__ })
  end

  @moduledoc """
  See `:gen_event.add_handler/3`.
  """
  def add_handler(handler, args) do
    :gen_event.add_handler(__MODULE__, handler, args)
  end
  
  @moduledoc """
  See `:gen_event.delete_handler/3`.
  """
  def delete_handler(handler, args) do
    :gen_event.delete_handler(__MODULE__, handler, args)
  end

  @moduledoc """
  See `:gen_event.call/3`.
  """
  def call(handler, request) do
    :gen_event.call(__MODULE__, handler, request)
  end

  @moduledoc """
  See `:gen_event.call/4`.
  """
  def call(handler, request, timeout) do
    :gen_event.call(__MODULE__, handler, request, timeout)
  end
  
  @moduledoc """
  See `:gen_event.which_handlers/1`.
  """
  def which_handlers() do
    :gen_event.which_handlers(__MODULE__)
  end

  @moduledoc """
  Send a notify with `:suite_started`.
  """
  def suite_started(opts) do
    :gen_event.notify(__MODULE__, { :suite_started, opts })
  end
  
  @moduledoc """
  Send a notify with `:suite_finished`.
  """
  def suite_finished(load_us, run_us) do
    :gen_event.notify(__MODULE__, { :suite_finished, load_us, run_us })
  end
  
  @moduledoc """
  Send a notify with `:case_started`.
  """
  def case_started(test_case) do
    :gen_event.notify(__MODULE__, { :case_started, test_case })
  end
  
  @moduledoc """
  Send a notify with `:case_finished`.
  """
  def case_finished(test_case) do
    :gen_event.notify(__MODULE__, { :case_finished, test_case })
  end
  
  @moduledoc """
  Send a notify with `:test_started`.
  """
  def test_started(test) do
    :gen_event.notify(__MODULE__, { :test_started, test })
  end
  
  @moduledoc """
  Send a notify with `:test_finished`.
  """
  def test_finished(test) do
    :gen_event.notify(__MODULE__, { :test_finished, test })
  end
end
