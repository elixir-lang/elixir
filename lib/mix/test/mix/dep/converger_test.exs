Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Dep.ConvergerTest do
  use MixTest.Case

  describe "topological_sort" do
    test "raises if there is a cycle in the dependency graph" do
      app1 = %Mix.Dep{app: :app1}
      app2 = %Mix.Dep{app: :app2}
      app3 = %Mix.Dep{app: :app3}

      deps = [
        # not really part of the cycle, just depend on it
        %Mix.Dep{app: :app5, deps: [app1, app3]},
        %Mix.Dep{app: :app4, deps: [app2]},

        # cycle
        %{app1 | deps: [app2]},
        %{app2 | deps: [app3]},
        %{app3 | deps: [app1]}
      ]

      assert_raise Mix.Error,
                   "Could not sort dependencies. " <>
                     "The following dependencies form a cycle: app1, app2, app3",
                   fn -> Mix.Dep.Converger.topological_sort(deps) end
    end

    test "raises if an app depends upon itself" do
      app = %Mix.Dep{app: :self_dependent}
      deps = [%{app | deps: [app]}]

      assert_raise Mix.Error,
                   "App self_dependent lists itself as a dependency",
                   fn -> Mix.Dep.Converger.topological_sort(deps) end
    end
  end
end
