Code.require_file "../test_helper", __FILE__

defmodule Application.Test do
  use ExUnit.Case

  def setup_all do
    :error_logger.delete_report_handler(:error_logger_tty_h)
    assert Application.stop(:crypto) == :ok
    assert Application.stop(:public_key) == :ok
    assert Application.stop(:ssl) == :ok
    :error_logger.add_report_handler(:error_logger_tty_h)
  end
  defdelegate [teardown_all: 0], to: __MODULE__
    
  test "starting application without dependencies" do
    assert Application.start(:ssl, dependencies: false) == {:error, {:not_started, :crypto}}
  end

  test "starting application with dependencies" do
    assert Application.start(:ssl, dependencies: true) == :ok
  end

  test "starting application with dependencies (by default)" do
    assert Application.start(:ssl) == :ok
  end

end