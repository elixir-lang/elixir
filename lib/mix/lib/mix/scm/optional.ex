defmodule Mix.SCM.Optional do
  @behavior Mix.SCM
  @moduledoc false

  def format(_opts) do
    "[optional]"
  end

  def format_lock(_lock) do
    nil
  end

  def accepts_options(_app, opts) do
    if opts[:optional], do: opts
  end

  def checked_out?(_opts) do
    true
  end

  def lock_status(_opts) do
    :ok
  end

  def equal?(_opts1, _opts2) do
    true
  end

  def checkout(_opts) do
    raise Mix.Error, message: "Cannot checkout optional dependency"
  end

  def update(_opts) do
    raise Mix.Error, message: "Cannot update optional dependency"
  end

  def clean(_opts) do
    raise Mix.Error, message: "Cannot clean optional dependency"
  end
end
