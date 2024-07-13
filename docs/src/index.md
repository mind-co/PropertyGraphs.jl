# PropertyGraphs.jl Documentation


## Overview

The main data structure is the `PropertyGraph`, which is a collection of nodes and edges. Nodes are created using the `Node` function and edges are created using the `Edge` function.

```julia
using PropertyGraphs

# Create a graph
g = PropertyGraph()

# You may also add a metadata dictionary
g = PropertyGraph(metadata=Dict("source" => "twitter"))

# If you already have nodes and edges, you can instantiate the graph with them
g = PropertyGraph(nodes, edges; metadata=Dict("source" => "twitter"))
```

A `PropertyGraph` is composed of `Node` and `Edge` types. 

- A `Node` has a unique `UInt` identifier, string labels, and properties. Nodes may have multiple labels (i.e. `"person"` and `"human"`).
- An `Edge` connects two nodes with a type and properties. An edge may have only one type, but it may have multiple properties.

```julia
# Add nodes to the graph
alice = Node(1, "person", "human"; name="Alice", age=30)
bob = Node(2, "person", "human"; name="Bob", age=35)
car = Node(3, "vehicle"; brand="Toyota", model="Prius", year=2024)
```

In the above example, `alice` and `bob` are both `"person"` and `"human"`. `car` is a `"vehicle"` and has brand, model, and year properties.

You can `push!` nodes to the graph like so:

```julia
# Add nodes to the graph
push!(g, alice)
push!(g, bob)
push!(g, car)
```

Adding edges is quite simple:

```julia
# Create an edge
alice_bob = Edge(alice, bob, "friend"; since=2020)
alice_car = Edge(alice, car, "owns"; since=2020)
alice_bob_angry = Edge(alice, bob, "mad at"; since=2020)
bob_car = Edge(bob, car, "hit"; on="2024-02-29")

# Add the edge to the graph
push!(g, alice_bob)
push!(g, alice_car)
push!(g, alice_bob_angry)
push!(g, bob_car)
```

Now, we have edges demonstrating that `alice` is friends with `bob` and owns `car`. `bob` hit `car` on `2024-02-29`, which resulted in `alice` being angry with `bob`.

### Querying the graph

Currently, querying the graph requires specifying a `NodePattern` and/or an `EdgePattern` that contain properties that match the graph.

```julia
# Match all nodes with label "person"
match(g, NodePattern("person"))
```
```
PropertyGraph(Dict{Any, Any}(), Node[Node(ID(1), person, human, EasyConfig.Config(:name => "Alice", :age => 30)), Node(ID(2), person, human, EasyConfig.Config(:name => "Bob", :age => 35))], Edge[], PropertyGraphs.Index[])
```

will return a subgraph containing `alice` and `bob`, as they both have `"person"` as a label.

You can also query specific properties:

```julia
# Match all nodes with label "person" and property "name" == "Alice"
match(g, NodePattern("person"; name="Alice"))
```
```
PropertyGraph(Dict{Any, Any}(), Node[Node(ID(1), person, human, EasyConfig.Config(:name => "Alice", :age => 30))], Edge[], PropertyGraphs.Index[])
```

will return a subgraph containing `alice`.

A convenience pattern match uses `Pair`, such that you can query a graph with a specific edge type between node patterns:

```julia
match(g, NodePattern("person") => "hit" => NodePattern("vehicle"))
```
```
PropertyGraph(Dict{Any, Any}(), Node[Node(ID(2), person, human, EasyConfig.Config(:name => "Bob", :age => 35)), Node(ID(3), vehicle, EasyConfig.Config(:brand => "Toyota", :model => "Prius", :year => 2024))], Edge[ID(2)-[hit]->ID(3), EasyConfig.Config(:on => "2024-02-29")], PropertyGraphs.Index[])
```

Unfortunately, this syntax only supports a clause, i.e. you cannot provide multiple pairs:

```julia
match(
    g, 
    NodePattern("person") => "hit" => NodePattern("vehicle") => "hit" => NodePattern("person")
)
```

Contributions welcome!


