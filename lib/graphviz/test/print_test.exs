Code.require_file "../test_helper.exs", __FILE__

defmodule PrintTest do

  use GvTest.Case, async: true

  test "print empty graphs" do
    assert_prints GraphViz.print(&1, GraphViz.Graph[ name: "name", is_strict: false, is_directed: false ]), """
    graph "name" {
    }
    """
    assert_prints GraphViz.print(&1, GraphViz.Graph[ name: "name", is_strict: true, is_directed: false ]), """
    strict graph "name" {
    }
    """
    assert_prints GraphViz.print(&1, GraphViz.Graph[ name: "name", is_strict: false, is_directed: true ]), """
    digraph "name" {
    }
    """
  end

  test "print directed graph" do
    graph = GraphViz.Graph[ name: "name", is_strict: true, is_directed: true, attributes: [ label: "A label", rankdir: :LR ] ]
         |> GraphViz.add(GraphViz.SubGraph[ id: :cluster, is_cluster: true, attributes: [ label: "Cluster" ] ])
         |> GraphViz.add(GraphViz.SubGraph[ id: :sub_graph, parent: :cluster, is_cluster: false ])
         |> GraphViz.add(GraphViz.Node[ id: :source, parent: :sub_graph, attributes: [ label: "Source" ] ])
         |> GraphViz.add(GraphViz.Node[ id: :target, attributes: [ label: "Target" ] ])
         |> GraphViz.add(GraphViz.Edge[ id: :edge, source: :source, target: :target, attributes: [ label: "Edge" ] ])
    assert_prints GraphViz.print(&1, graph), """
    strict digraph "name" {
    ":target" [
    label = "Target",
    ];
    subgraph "cluster_:cluster" {
    subgraph ":sub_graph" {
    ":source" [
    label = "Source",
    ];
    }
    label = "Cluster";
    }
    ":source" -> ":target" [
    label = "Edge",
    ];
    label = "A label";
    rankdir = LR;
    }
    """
  end

end
