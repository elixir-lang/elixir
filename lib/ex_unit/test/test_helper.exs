# Configure ExUnit, no options supported yet.
ExUnit.start []
ExUnit.after_spawn fn -> Process.put(:after_spawn, :ex_unit) end