# PropertyGraphs.jl

PropertyGraphs.jl is a Julia package for working with property graphs. It provides a simple and flexible way to create, manipulate, and query graphs with labeled nodes and typed edges, where both nodes and edges can have associated properties.

## Motivation

Property graphs are a powerful data structure for representing complex relationships and attributes in various domains, including social networks, knowledge graphs, and biological systems. This package aims to provide an intuitive and efficient implementation of property graphs in Julia.

## Features

- Create nodes with labels and properties
- Create edges with types and properties
- Build and manipulate property graphs
- Query graphs using pattern matching
- Perform graph operations like union and intersection

## Installation

You can install PropertyGraphs.jl using Julia's package manager. Currently, the package is not registered, so you will need to pull from the GitHub repository.

```julia
using Pkg
Pkg.add(url="https://github.com/cpfiffer/PropertyGraphs.jl")
```

When the package is registered (who knows when that will be), PropertyGraphs.jl can be installed with

```julia
using Pkg
Pkg.add("PropertyGraphs")
```

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

## Future stuff

### Graphs.jl interface

Adding a Graphs.jl interface is tricky because Graphs.jl does not natively support multigraphs (meaning that two nodes may have multiple edges between them). [WrappedMultiGraphs.jl](https://github.com/UniStuttgart-IKR/WrappedMultiGraphs.jl) handles this reasonably well by changing a few lines of code. Handling this in PropertyGraphs.jl is likely the next big goal, as it would allow to the powerful analytic tools in the Julia graph ecosystem.

### Pattern matching

The original goal of this package was to serve as part of a graph database in Julia. Graph databases typically support a query language that extracts subgraphs from the graph based on a pattern. 

PropertyGraphs.jl provides a pattern matching interface that allows you to extract subgraphs from the graph based on a pattern. It is however relatively unergonomic.

#### Cypher-style `match` queries

As an example:

```julia
# Match edges between john and jane
match(
    graph,
    NodePattern("person", "human"; name="John Doe", age=25),
)
```

My preference would be to support an arbitrary Cypher-like syntax:

```julia
match(
    graph,
    # Get all 1-hop edges from john to jane
    NodePattern("person") => "knows" => NodePattern("person"),

    # Match people who know or like jane
    NodePattern("person") => ["knows", "likes"] => NodePattern("person"),

    # Match people who are within 2 hops of jane
    NodePattern("person") => MultiHop(2) => NodePattern("person"),

    # Match people who are within 2 hops of jane and know or like jane
    NodePattern("person") => MultiHop(2, ["knows", "likes"]) => NodePattern("person"),
)
```

etc. This is a very basic example and the syntax is not finalized, but would be nice to have.

#### Functional matching

Matching should also work when a function is provided, such that you can match nodes based on a function. For example, to match all nodes that have a name that starts with "A", you could do:

```julia
match(g, NodePattern("person"; name=startswith("A")))
```

### Data storage

PropertyGraphs.jl is designed to be a lightweight package that does not require a database backend. It is intended to be used as a building block for more complex graph databases.

I'd love to have a performant serialization format, but I don't how to do that. :shrug:

### Indexing

Searching the graph currently requires a full scan of the graph. This is fine for small graphs, but for large graphs, this can be slow.

The correct way to handle this would be to provide an invested index that tracks user-requested labels, properties, and edge types. For example, an ideal graph constructor would provide an interface like

```julia
indices = [
    # Index all nodes with label "person"
    NodeIndex("person"),

    # Index all nodes with label "vehicle" and brand "Toyota"
    NodeIndex("vehicle", brand="Toyota"),

    # Index all edges of type "knows"
    EdgeIndex("knows"),
]

g = PropertyGraph(indices)
```