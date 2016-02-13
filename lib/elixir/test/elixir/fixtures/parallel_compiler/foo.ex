defmodule Foo do
  # We use this ensure_compiled? clause so both Foo and
  # Bar block. Foo depends on Unknown and Bar depends on
  # Foo. The compiler will see this dependency and first
  # release Foo and then Bar, compiling with success.
  false = Code.ensure_compiled?(Unknown)
  def message, do: "message_from_foo"
end