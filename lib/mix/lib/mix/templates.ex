defmodule Mix.Templates do
  import Mix.Generator

  @moduledoc """
  Defines shared templates for use by tasks that generate files.
  """

  embed_template :lib, """
  defmodule <%= @mod %> do
  end
  """
end
