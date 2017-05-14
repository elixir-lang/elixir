defmodule Mix.Local.Target do

  @moduledoc ~S"""
  A mix local target is a repository for various kinds of system-wide assets.
  For example, the Escript target is the store for escripts that you install, and
  the Archive target is typically used to hold archives implementing mix tasks.

  Previously, these two targets were hard-wired into mix. This new interface
  allows brave souls to add new local targets. For example, @pragdave's templating
  system uses its own mix local assets, managed by the its own Target behviour.

  ### Adding a new target

  Define a module that uses the `Mix.Local.Target` behaviour. In that module,
  write the following functions:

      defmodule MyMixExtension.Target do
        @behaviour Mix.Local.Target

        @doc “““
        Given a project configuration keyword list, return the name to be
        used in the local storage
        ”””

        @spec name_for(Keyword.t) :: String.t
        def name_for(project_config) do
          version = if version = project[:version], do: "-#{version}"
          "#{project[:app]}#{version}.ez"
        end

        @doc “““
        Return the path where items of this target should be stored.
        ”””

        @spec path_for() :: String.t
        def path_for() do
          Path.join(Mix.Utils.mix_home, "templates")
        end


        @doc “““
        Return the singular and plural forms of the human name for this
        target.
        ”””

        @spec printable_name() :: { String.t, String.t }
        def printable_name() do
          { "template", "templates" }
        end

        @doc “““
        Return the base part of the mix task name for this target. 
        For example, the Escript target would return "escript", as
        its tasks have names such as "escript.build" and "escript.install"
        ”””
        @spec task_name() :: String.t
  """

  @callback name_for(Keyword.t) :: String.t
  @callback path_for()          :: String.t
  @callback printable_name()    :: { String.t, String.t }
  @callback task_name()         :: String.t
end
