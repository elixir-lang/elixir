# We need to ensure it won't block even after multiple calls.
# So we use both behaviour and struct expansion below.
defmodule Bat do
  # @behaviour will call ensure_compiled().
  @behaviour :unknown
  # Struct expansion calls it as well.
  %ThisModuleWillNeverBeAvailable{}
end
