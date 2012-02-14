Code.require_file "../../test_helper", __FILE__

defmodule Kernel::ForwardingLibrary do
  defmacro __using__(_) do
    quote do
      defforward [sample: 1], to: ::MyLibrary
    end
  end

  def sample(module, arg) do
    { module, arg }
  end
end

defmodule Kernel::ForwardedExample do
  use Kernel::ForwardingLibrary

  def sample(arg) do
    arg
  end
end

defmodule Kernel::ForwardingExample do
  use Kernel::ForwardingLibrary
end