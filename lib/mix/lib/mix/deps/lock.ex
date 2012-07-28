defmodule Mix.Deps.Lock do
  @moduledoc """
  This is the module responsible to manage mix.lock file.
  """

  @doc """
  Read the file, returns a keywords list containing
  the app name and its current lock information.
  """
  def read do
    case File.read("mix.lock") do
      { :ok, info } ->
        { value, _binding } = Code.eval(info)
        value
      { :error, _ } ->
        []
    end
  end

  @doc """
  Receives a keywords list and writes it to the disk.
  """
  def write(dict) do
    lines = lc { app, rev } inlist dict, rev != nil do
      %b("#{app}": #{inspect rev})
    end

    File.write! "mix.lock", "[ " <> Enum.join(lines, ",\n  ") <> " ]"
  end

  @doc """
  Loop the given dependencies triggering the callback.
  The callback receives the dependency and its current lock
  (may be nil). The callback must return a lock or nil in
  case a lock could not be retrieved.

  This function returns a list with the app names in the
  given dependencies that got a lock.
  """
  def update_lock(deps, callback) do
    { apps, lock } =
      Enum.reduce deps, { [], read }, fn(Mix.Dep[app: app] = dep, { apps, lock }) ->
        if rev = callback.(dep, lock[app]) do
          lock = Keyword.put(lock, app, rev)
          { [app|apps], lock }
        else
          { apps, lock }
        end
      end
    write(lock)
    apps
  end
end