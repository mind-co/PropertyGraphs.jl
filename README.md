# PropertyGraphs.jl

PropertyGraphs.jl is a Julia package for working with property graphs. It provides a simple and flexible way to create, manipulate, and query graphs with labeled nodes and typed edges, where both nodes and edges can have associated properties.

This package is designed to be the internal representation of the [comind](https://blog.comind.me) project, run by [Cameron Pfiffer](https://cameron.pfiffer.org).

## Motivation

Property graphs are a powerful data structure for representing complex relationships and attributes in various domains, including social networks, knowledge graphs, and biological systems. This package aims to provide an intuitive and efficient implementation of property graphs in Julia.

## Features

- Create nodes with labels and properties
- Create edges with types and properties
- Build and manipulate property graphs
- Query graphs using pattern matching
- Perform graph operations like union and intersection

## Installation

You can install PropertyGraphs.jl with Pkg:

```julia
using Pkg
Pkg.add("PropertyGraphs")
```

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

## Notes

Inspired by [SQLiteGraph.jl](https://github.com/JuliaComputing/SQLiteGraph.jl/), a demo package by [Josh Day](https://github.com/joshday).
