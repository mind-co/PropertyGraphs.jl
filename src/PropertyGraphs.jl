"""

Inspired by https://github.com/JuliaComputing/SQLiteGraph.jl/
"""
module PropertyGraphs

import Graphs
using EasyConfig

export Node, Edge
export NodePattern, EdgePattern
export ID, NoID, toid, value
export EdgeReference, PropertyGraph, match, update_edge_references
export edges, nodes, metadata, valid_id
export labels, properties, edge_references, id
export pretty_print

abstract type AbstractPattern end
abstract type AbstractNodeLike <: AbstractPattern end
abstract type AbstractEdgeLike <: AbstractPattern end

"""
    AbstractID

A type used to match edges by their source and target IDs. Use `NoID`
when a pattern does not need to match by ID.

The main ID type is `ID`, which is a simple wrapper around `UInt`.
If provided an integer ID, it will be converted to `UInt`.
"""
abstract type AbstractID end

struct ID <: AbstractID
    id::UInt
end

Base.show(io::IO, id::ID) = print(io, "ID($(id.id))")

Base.:(==)(a::ID, b::ID) = a.id == b.id
Base.:(==)(a::ID, b::Integer) = a.id == b
Base.:(==)(a::Integer, b::ID) = a == b.id

Base.isless(a::ID, b::ID) = a.id < b.id

"""
    NoID
    
A type used to match edges by their source and target IDs. Use `NoID`
when a pattern does not need to match by ID.
"""
struct NoID <: AbstractID end

Base.show(io::IO, id::NoID) = print(io, "NoID")

"""
    value(id::Union{ID,NoID})

Get the value of an ID. If the ID is `NoID`, return `nothing`.
"""
value(id::ID) = id.id
value(id::NoID) = nothing

"""
    CASTABLE_TO_ID

A type that can be converted to an `ID` or `NoID`.
"""
const CASTABLE_TO_ID = Union{Int,UInt,ID,NoID}

"""
    toid(x)

Convert `x` to an `ID`. If `x` is convertible to an `ID`, return it.
Otherwise, return `NoID`.
"""
toid(id::Integer) = toid(UInt(id))
toid(id::UInt) = ID(id)
toid(id::ID) = id
toid(x::CASTABLE_TO_ID) = ID(x)
toid(x::NoID) = x
toid(::Nothing) = NoID()

"""
    toid(id::Union{Integer,UInt,ID})

Convert an integer or `UInt` to an `ID`.
"""

"""
    Edge(source::UInt, target::UInt, type::String, properties::Config)

Represents an edge in the property graph.

# Fields
- `source::UInt`: The unique identifier of the source node.
- `target::UInt`: The unique identifier of the target node. 
- `type::String`: The type or label of the edge.
- `properties::Config`: A `Config` object representing the properties (key-value pairs) associated with the edge.

# Constructors 
- `Edge(src::Integer, tgt::Integer, type::String; props...)`: Convenience constructor to create an `Edge` with the given `src` and `tgt` node IDs (converted to `UInt`), `type`, and optional `props` which are passed to the `Config` constructor.
- `Edge(src::UInt, tgt::UInt, type::String; props...)`: Convenience constructor to create an `Edge` with the given `src` and `tgt` node IDs, `type`, and optional `props` which are passed to the `Config` constructor.

# Examples
```julia
Edge(1, 2, "friend"; since=2010)
```
"""
struct Edge <: AbstractEdgeLike
    source::ID
    target::ID
    type::String
    properties::Config

    Edge(s, t, type, props) = new(toid(s), toid(t), type, props)
end

function Edge(src::CASTABLE_TO_ID, tgt::CASTABLE_TO_ID, type::String; props...)
    return Edge(toid(src), toid(tgt), type, Config(props))
end

function Base.:(==)(a::Edge, b::Edge)
    return a.source == b.source &&
           a.target == b.target &&
           a.type == b.type &&
           a.properties == b.properties
end

function Base.show(io::IO, e::Edge)
    print(io, "$(source(e))-[$(type(e))]->$(target(e)), $(properties(e))")
end

Base.getindex(e::Edge, key::String) = getindex(e.properties, key)
function Base.setindex!(e::Edge, value, key::String)
    setindex!(e.properties, value, key)
end

source(e::Edge) = e.source
target(e::Edge) = e.target
type(e::Edge) = e.type
properties(e::Edge) = e.properties

struct EdgePattern{
    SourceIDType<:AbstractID,
    TargetIDType<:AbstractID,
    EdgeType<:Union{String,Nothing},
    EdgePropertyType<:Union{Config,Nothing}
} <: AbstractEdgeLike
    source::SourceIDType
    target::TargetIDType
    type::EdgeType
    properties::EdgePropertyType

    function EdgePattern(source, target, type, properties)
        sid, tid = toid(source), toid(target)
        new{typeof(sid),typeof(tid),typeof(type),typeof(properties)}(sid, tid, type, properties)
    end
end

function EdgePattern(e::Edge)
    EdgePattern(e.source, e.target, e.type, e.properties)
end
function EdgePattern(type::String; kwargs...)
    EdgePattern(nothing, nothing, type, Config(kwargs))
end
function EdgePattern(source::Integer, target::Integer; kwargs...)
    EdgePattern(source, target, nothing, Config(kwargs))
end
function EdgePattern(source; kwargs...)
    EdgePattern(source, nothing, nothing, Config(kwargs))
end
function EdgePattern(; source=nothing, target=nothing, type=nothing, kwargs...)
    EdgePattern(source, target, type, Config(kwargs))
end

source(e::EdgePattern) = e.source
target(e::EdgePattern) = e.target
type(e::EdgePattern) = e.type
properties(e::EdgePattern) = e.properties

function Base.show(io::IO, e::EdgePattern)
    print(io, "EdgePattern($(source(e))-[$(type(e))]->$(target(e)), $(properties(e)))")
end

"""
    EdgeReference

A reference to edges in a property graph. Nodes will store these 
references to edges for fast traversal.
"""
@kwdef struct EdgeReference
    in_edges::Vector{Edge} = Edge[]
    out_edges::Vector{Edge} = Edge[]
end

function EdgeReference(in_edges::Vector{<:Edge}, out_edges::Vector{<:Edge})
    EdgeReference(in_edges, out_edges)
end

function Base.:(==)(a::EdgeReference, b::EdgeReference)
    return a.in_edges == b.in_edges && a.out_edges == b.out_edges
end


"""
    Node(id::UInt, labels::Vector{String}, properties::Config) <: AbstractNodeLike

Represents a node in the property graph. 

# Fields
- `id::UInt`: The unique identifier for the node.
- `labels::Vector{String}`: A vector of labels associated with the node. Labels are used to group or categorize nodes.
- `properties::Config`: A `Config` object representing the properties (key-value pairs) associated with the node.

# Constructors
- `Node(id::UInt, labels::String...; props...)`: Convenience constructor to create a `Node` with the given `id`, one or more `labels`, and optional `props` which are passed to the `Config` constructor.

# Examples

```julia
Node(1, :person, :human, name="John Doe", age=25)
```
"""
struct Node <: AbstractNodeLike
    id::ID
    labels::Vector{String}
    properties::Config

    # Store edge locations
    edge_references::EdgeReference
end

function Base.show(io::IO, n::Node)
    print(io, "Node($(id(n)), $(join(labels(n), ", ")), $(properties(n)))")
end

Node(id::Integer, args...; kwargs...) = Node(toid(id), args...; kwargs...)
function Node(
    id::CASTABLE_TO_ID,
    labels::String...;
    edge_references::EdgeReference=EdgeReference(),
    props...
)
    Node(toid(id), collect(labels), Config(props), edge_references)
end
function Node(
    id::CASTABLE_TO_ID,
    labels::Vector{String};
    edge_references::EdgeReference=EdgeReference(),
    props...
)
    Node(toid(id), labels, Config(props), edge_references)
end

id(n::Node) = n.id
labels(n::Node) = n.labels
properties(n::Node) = n.properties
edge_references(n::Node) = n.edge_references

function Base.:(==)(a::Node, b::Node)
    return a.id == b.id &&
           a.labels == b.labels &&
           a.properties == b.properties
end

Base.getindex(n::Node, key::String) = getindex(n.properties, key)
function Base.setindex!(n::Node, value, key::String)
    setindex!(n.properties, value, key)
end

# Utility functions for constructing an edge from two nodes
function Edge(source::Node, target::Node, type::String; kwargs...)
    Edge(id(source), id(target), type; kwargs...)
end

# Extra pattern matching stuff for nodes
function EdgePattern(
    source_node::Node,
    target_node::Node,
    type::Union{String,Nothing}=nothing;
    kwargs...
)
    EdgePattern(
        id(source_node),
        id(target_node),
        type,
        Config(kwargs)
    )
end

"""
    NodePattern <: AbstractNodeLike

A `NodePattern` is a `Node`-like object that can be used to match nodes 
in a graph.

We use `IDType` to allow for matching by ID, or by other properties. 
If `IDType` is not nothing, then the pattern will match by ID. If 
`IDType` is nothing, the pattern will match by labels and properties, 
otherwise, it will start by matching by ID.

The only difference between a `NodePattern` and a `Node` is that a 
`NodePattern` may not have an ID.
"""
struct NodePattern{IDType<:AbstractID} <: AbstractNodeLike
    id::IDType
    labels::Vector{String}
    properties::Config
    edge_references::EdgeReference

    function NodePattern(id, labels, props, refs)
        newid = toid(id)
        new{typeof(newid)}(newid, labels, props, refs)
    end
end

NodePattern(n::Node) = NodePattern(n.id, n.labels, n.properties, n.edge_references)

function NodePattern(
    labels::String...;
    edge_references::EdgeReference=EdgeReference(),
    kwargs...
)
    NodePattern(NoID(), collect(labels), Config(kwargs), edge_references)
end
function NodePattern(
    labels;
    edge_references::EdgeReference=EdgeReference(),
    kwargs...
)
    label_vec = isempty(labels) ? String[] : labels
    NodePattern(NoID(), label_vec, Config(kwargs), edge_references)
end
function NodePattern(id, args...; kwargs...)
    NodePattern(toid(id), args...; kwargs...)
end
function NodePattern(id, labels::String...; edge_references::EdgeReference=EdgeReference(), props...)
    NodePattern(id, collect(labels), Config(props), edge_references)
end
function NodePattern(id, labels::Vector{String}; edge_references::EdgeReference=EdgeReference(), props...)
    NodePattern(id, labels, Config(props), edge_references)
end


id(n::NodePattern) = n.id
labels(n::NodePattern) = n.labels
properties(n::NodePattern) = n.properties
edge_references(n::NodePattern) = n.edge_references

propertymatch(c1::AbstractPattern, c2::AbstractPattern) = propertymatch(properties(c1), properties(c2))
propertymatch(c1::AbstractPattern, c2::Config) = propertymatch(properties(c1), c2)
propertymatch(c1::Config, c2::AbstractPattern) = propertymatch(c1, properties(c2))
function propertymatch(c1::Config, c2::Config)
    for (key, value) in c1
        if get(c2, key, value) != value
            return false
        end
    end
    return true
end

"""
    _matches(subnode::AbstractNodeLike, node::AbstractNodeLike)

Check if the subnode matches the node -- labels and properties must be a subset.
"""
function _matches(subnode::AbstractNodeLike, node::AbstractNodeLike)
    for label in labels(subnode)
        if !in(label, labels(node))
            return false
        end
    end
    for (key, value) in properties(subnode)
        if get(properties(node), key, value) != value
            return false
        end
    end
    return true
end

"""
    matchequal(thing, other)

Checks if `thing` matches `other`, handling `Nothing` types.

If either `thing` or `other` is `Nothing`, it is considered a match.
Otherwise, the standard equality operator `==` is used.

# Examples
```julia
matchequal(1, 1) # true
matchequal(1, 2) # false
matchequal(1, 1.0) # false
matchequal(1, nothing) # true
matchequal(nothing, 1) # true
matchequal(nothing, nothing) # true
```
"""
matchequal(thing, other) = thing == other
matchequal(thing::Union{NoID,Nothing}, other::Union{NoID,Nothing}) = true
matchequal(thing::Union{NoID,Nothing}, other) = true
matchequal(thing, other::Union{NoID,Nothing}) = true

sourcematch(subedge::AbstractEdgeLike, edge::AbstractEdgeLike) = matchequal(source(subedge), source(edge))
targetmatch(subedge::AbstractEdgeLike, edge::AbstractEdgeLike) = matchequal(target(subedge), target(edge))
typematch(subedge::AbstractEdgeLike, edge::AbstractEdgeLike) = matchequal(type(subedge), type(edge))

function _matches(subedge::AbstractEdgeLike, edge::AbstractEdgeLike)
    # Check IDs
    if !sourcematch(subedge, edge)
        return false
    end
    if !targetmatch(subedge, edge)
        return false
    end
    if !typematch(subedge, edge)
        return false
    end
    if !propertymatch(subedge, edge)
        return false
    end

    return true
end

matches(s::NodePattern{NoID}, n::Node) = _matches(s, n)
matches(s::NodePattern{ID}, n::Node) = id(s) == id(n) && _matches(s, n)
matches(s::AbstractNodeLike, n::AbstractNodeLike) = id(s) == id(n) && _matches(s, n)

matches(e1::AbstractEdgeLike, e2::AbstractEdgeLike) = _matches(e1, e2)

mutable struct PropertyGraph
    metadata::Config
    nodes::Vector{Node}
    edges::Vector{Edge}
end

PropertyGraph(nodes::Vector{Node}, edges::Vector{Edge}; metadata::Config=Config()) = PropertyGraph(metadata, nodes, edges)
PropertyGraph(; metadata::Config=Config(), nodes::Vector{Node}=Node[], edges::Vector{Edge}=Edge[]) = PropertyGraph(metadata, nodes, edges)
PropertyGraph(nodes::Vector{Node}; metadata::Config=Config(), edges::Vector{Edge}=Edge[]) = PropertyGraph(metadata, nodes, edges)
PropertyGraph(edges::Vector{Edge}; metadata::Config=Config(), nodes::Vector{Node}=Node[]) = PropertyGraph(metadata, nodes, edges)

Base.:(==)(a::PropertyGraph, b::PropertyGraph) = a.metadata == b.metadata && a.nodes == b.nodes && a.edges == b.edges
Base.intersect(a::PropertyGraph, b::PropertyGraph) = PropertyGraph(intersect(a.nodes, b.nodes), intersect(a.edges, b.edges); metadata=a.metadata)

Base.show(io::IO, g::PropertyGraph) = print(io, "PropertyGraph($(length(g.nodes)) nodes, $(length(edges(g))) edges)")

# Load error types
include("errors.jl")

"""
    valid_id(g::PropertyGraph)

Get the next available ID for a node in the graph, this is simply
the length of the graph's nodes plus one.
"""
valid_id(g::PropertyGraph) = toid(length(g.nodes) + 1)

"""
    is_valid_id(g::PropertyGraph, id::AbstractID)

Check if the given ID is valid for a node in the graph.
"""
is_valid_id(g::PropertyGraph, id::AbstractID) = id == valid_id(g)

"""
    push!(g::PropertyGraph, n::Node)

Push a node to the graph.

This function will error if the node has an ID that already exists in the graph,
or if the node's ID is non-sequential. You can use `valid_id(g)` to get the next
available ID.
"""
function Base.push!(g::PropertyGraph, n::Node)
    expected_id = valid_id(g)
    if id(n) < expected_id
        throw(NodeAlreadyExists(n, g[id(n)]))
    elseif id(n) > expected_id
        throw(NonSequentialID(id(n), expected_id))
    end
    push!(g.nodes, n)
    return g
end

"""
    push!(g::PropertyGraph, e::Edge)

Push an edge to the graph. The edge must be unique up to the combination of source,
target, and type.
"""
function Base.push!(g::PropertyGraph, e::Edge)
    if has_edge(g, e)
        # Get the existing edge
        existing_edge = match(g, e)
        throw(EdgeAlreadyExists(e, only(edges(existing_edge))))
    end
    push!(edges(g), e)
    return g
end

"""
    pretty_print(g::PropertyGraph)

Pretty print the graph if it has less than 10 nodes and 10 edges.
Otherwise, default to show.
"""
function pretty_print(g::PropertyGraph; max_nodes=100, max_edges=100)
    if n_nodes(g) < max_nodes && n_edges(g) < max_edges
        for n in g.nodes
            println(n)

            source_edges = filter(e -> e.source == n.id, edges(g))
            for e in source_edges
                println("  $(e.type) -> $(e.target.id)")
                for (key, value) in e.properties
                    println("    $key: $value")
                end
            end
        end
    else
        show(g)
    end
end

function has_edge(g::PropertyGraph, e::Edge)
    m = match(g, e)
    return length(edges(m)) > 0
end
function has_edge(
    g::PropertyGraph,
    source::AbstractID,
    target::AbstractID,
    type::String
)
    return length(match(g, Edge(source, target, type))) > 0
end

Base.length(g::PropertyGraph) = length(nodes(g))
n_edges(g::PropertyGraph) = length(edges(g))
n_nodes(g::PropertyGraph) = length(nodes(g))
nodes(g::PropertyGraph) = g.nodes # TODO convert nodes to iterator
edges(g::PropertyGraph) = g.edges # TODO convert edges to iterator
metadata(g::PropertyGraph) = g.metadata

Base.getindex(g::PropertyGraph, id::AbstractID) = getindex(g.nodes, value(id))
Base.getindex(g::PropertyGraph, id::Integer) = getindex(g, toid(id))

"""
    update_edge_references(g::PropertyGraph)

Refreshes the edge references for all nodes in the graph.
"""
function update_edge_references(g::PropertyGraph)
    # TODO: Edge references should be updated as nodes and edges are inserted
    for i in eachindex(g.nodes)
        n = g.nodes[i]
        in_edges = Edge[]
        out_edges = Edge[]
        for e in edges(g)
            if e.source == n.id
                push!(in_edges, e)
            end
            if e.target == n.id
                push!(out_edges, e)
            end
        end

        # Recreate the node with the updated edge references
        new_node = Node(n.id, n.labels, n.properties, EdgeReference(in_edges, out_edges))
        g.nodes[i] = new_node
    end
end

"""
    Base.match(g::PropertyGraph, patterns::AbstractPattern...)

Match the pattern to the graph.
"""
function Base.match(g::PropertyGraph, patterns::AbstractPattern...)
    # Get node patterns and edge patterns
    node_patterns = filter(p -> p isa AbstractNodeLike, patterns)
    edge_patterns = filter(p -> p isa AbstractEdgeLike, patterns)

    # Match all nodes that match the pattern
    nds = map(
        pattern -> filter(n -> matches(pattern, n), nodes(g)),
        node_patterns
    )

    # Reduce the list of nodes to a single list and remove duplicates
    nds = length(nds) > 0 ? unique(reduce(vcat, nds)) : Node[]

    # Match all edges that match the pattern
    es = map(
        pattern -> filter(e -> matches(pattern, e), edges(g)),
        edge_patterns
    )

    # Reduce the list of edges to a single list and remove duplicates
    es = length(es) > 0 ? unique(reduce(vcat, es)) : Edge[]

    return PropertyGraph(nds, es, metadata=metadata(g))
end

"""
    save(n::Node)

Save the node to a file. By default, nodes are written to `nodes.bin`. 

struct Node <: AbstractNodeLike
    id::ID
    labels::Vector{String}
    properties::Config

    # Store edge locations
    edge_references::EdgeReference
end

Node records have the following specification:

- 1 bit if the record is "tombstoned", meaning that it is marked for deletion. 0 indicates 
  that the record is not tombstoned and can be read.
- 64-bit unsigned integer ID
- Five 32-bit records indicating a label ID.
- 64-bit unsigned integer indicating the location of the datastructure containing edge information.Z
- 64-bit unsigned integer for the properties of the node.

# TODO: Implement node storage record
# TODO: Implement the label store for nodes
# TODO: Implement the edge reference datastructure for nodes
# TODO: Implement the property store for nodes
"""

end # module PropertyGraphs



