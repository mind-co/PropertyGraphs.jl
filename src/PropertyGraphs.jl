"""

Inspired by https://github.com/JuliaComputing/SQLiteGraph.jl/
"""
module PropertyGraphs

import Graphs
using EasyConfig

export Node, Edge
export NodePattern, EdgePattern
export ID, NoID, toid, value
export PropertyGraph, match, remap_id
export edges, nodes, metadata, valid_id
export labels, properties, id
export pretty_print
export source, target, type, properties

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
    Edge(source::UInt, target::UInt, type::AbstractString, properties::Config)

Represents an edge in the property graph.

# Fields
- `source::UInt`: The unique identifier of the source node.
- `target::UInt`: The unique identifier of the target node. 
- `type::AbstractString`: The type or label of the edge.
- `properties::Config`: A `Config` object representing the properties (key-value pairs) associated with the edge.

# Constructors 
- `Edge(src::Integer, tgt::Integer, type::AbstractString; props...)`: Convenience constructor to create an `Edge` with the given `src` and `tgt` node IDs (converted to `UInt`), `type`, and optional `props` which are passed to the `Config` constructor.
- `Edge(src::UInt, tgt::UInt, type::AbstractString; props...)`: Convenience constructor to create an `Edge` with the given `src` and `tgt` node IDs, `type`, and optional `props` which are passed to the `Config` constructor.

# Examples
```julia
Edge(1, 2, "friend"; since=2010)
```
"""
struct Edge <: AbstractEdgeLike
    source::ID
    target::ID
    type::AbstractString
    properties::Config

    Edge(s, t, type, props) = new(toid(s), toid(t), type, props)
end

function Edge(src::CASTABLE_TO_ID, tgt::CASTABLE_TO_ID, type::AbstractString; props...)
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

Base.getindex(e::Edge, key::AbstractString) = getindex(e.properties, key)
function Base.setindex!(e::Edge, value, key::AbstractString)
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
function EdgePattern(type::AbstractString; kwargs...)
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
    Node(id::UInt, labels::Vector{String}, properties::Config) <: AbstractNodeLike

Represents a node in the property graph. 

# Fields
- `id::UInt`: The unique identifier for the node.
- `labels::Vector{String}`: A vector of labels associated with the node. Labels are used to group or categorize nodes.
- `properties::Config`: A `Config` object representing the properties (key-value pairs) associated with the node.

# Constructors
- `Node(id::UInt, labels::AbstractString...; props...)`: Convenience constructor to create a `Node` with the given `id`, one or more `labels`, and optional `props` which are passed to the `Config` constructor.

# Examples

```julia
Node(1, :person, :human, name="John Doe", age=25)
```
"""
struct Node <: AbstractNodeLike
    id::ID
    labels::Vector{String}
    properties::Config
end

function Base.show(io::IO, n::Node)
    print(io, "Node($(id(n)), $(join(labels(n), ", ")), $(properties(n)))")
end

Node(id::Integer, args...; kwargs...) = Node(toid(id), args...; kwargs...)
function Node(
    id::CASTABLE_TO_ID,
    labels::AbstractString...;
    props...
)
    Node(toid(id), collect(labels), Config(props))
end
function Node(
    id::CASTABLE_TO_ID,
    labels::Vector{String};
    props...
)
    Node(toid(id), labels, Config(props))
end

id(n::Node) = n.id
labels(n::Node) = n.labels
properties(n::Node) = n.properties

function Base.:(==)(a::Node, b::Node)
    return a.id == b.id &&
           a.labels == b.labels &&
           a.properties == b.properties
end

Base.getindex(n::Node, key::AbstractString) = getindex(n.properties, key)
function Base.setindex!(n::Node, value, key::AbstractString)
    setindex!(n.properties, value, key)
end

# Utility functions for constructing an edge from two nodes
function Edge(source::Node, target::Node, type::AbstractString; kwargs...)
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

    function NodePattern(id::AbstractID, labels::Vector{String}, props::Config)
        new{typeof(id)}(id, labels, props)
    end
end

NodePattern() = NodePattern(NoID(), String[], Config())
NodePattern(n::Node) = NodePattern(n.id, n.labels, n.properties)
NodePattern(id::CASTABLE_TO_ID) = NodePattern(toid(id), String[], Config())
NodePattern(labels::AbstractString...; kwargs...) = NodePattern(NoID(), collect(labels), Config(kwargs))
NodePattern(labels::Vector{String}; kwargs...) = NodePattern(NoID(), labels, Config(kwargs))

function EdgePattern(
    source::AbstractNodeLike,
    target::AbstractNodeLike,
    type::Union{AbstractString,Nothing}=nothing;
    kwargs...
)
    EdgePattern(id(source), id(target), type, Config(kwargs))
end

id(n::NodePattern) = n.id
labels(n::NodePattern) = n.labels
properties(n::NodePattern) = n.properties

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

sourcematch(subedge::AbstractEdgeLike, node::AbstractID) = matchequal(source(subedge), node)
sourcematch(subedge::AbstractEdgeLike, node::AbstractNodeLike) = matchequal(source(subedge), id(node))
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

"""
    AbstractIndex

A super type for all index types. Indices may be constructed on various features, 
but they must index on only one feature:

- `NodeLabelIndex` for node labels
- `EdgeTypeIndex` for edge types
- `NodePropertyIndex` for node properties
- `EdgePropertyIndex` for edge properties
"""
abstract type AbstractIndex end

struct NodeLabelIndex{K} <: AbstractIndex
    index_key::K
end

struct EdgeTypeIndex{K} <: AbstractIndex
    index_key::K
end

struct NodePropertyIndex{K} <: AbstractIndex
    index_key::K
end

struct EdgePropertyIndex{K} <: AbstractIndex
    index_key::K
end

"""
    Index

The index is a helper type to provide a simple mapping pairs of nodes to each other through edges.

Index makes it possible to do fast lookups of nodes and edges by ID.
"""
mutable struct Index{IndexType<:AbstractIndex}
    key::IndexType
    nodes::Dict{ID,Node}
    edges::Dict{Tuple{ID,ID},Edge}
end

mutable struct PropertyGraph
    metadata::Dict # TODO make this less shitty, no Dict any
    nodes::Vector{Node}
    edges::Vector{Edge}
    indices::Vector{Index}
end

PropertyGraph(nodes::Vector{Node}, edges::Vector{Edge}; metadata::Dict=Dict(), index=Index[]) = PropertyGraph(metadata, nodes, edges, index)
PropertyGraph(; metadata::Dict=Dict(), nodes::Vector{Node}=Node[], edges::Vector{Edge}=Edge[], index=Index[]) = PropertyGraph(metadata, nodes, edges, index)
PropertyGraph(nodes::Vector{Node}; metadata::Dict=Dict(), edges::Vector{Edge}=Edge[], index=Index[]) = PropertyGraph(metadata, nodes, edges, index)
PropertyGraph(edges::Vector{Edge}; metadata::Dict=Dict(), nodes::Vector{Node}=Node[], index=Index[]) = PropertyGraph(metadata, nodes, edges, index)

Base.:(==)(a::PropertyGraph, b::PropertyGraph) = a.metadata == b.metadata && a.nodes == b.nodes && a.edges == b.edges
Base.intersect(a::PropertyGraph, b::PropertyGraph) = PropertyGraph(intersect(a.nodes, b.nodes), intersect(a.edges, b.edges); metadata=Dict(intersect(a.metadata, b.metadata)))
Base.union(a::PropertyGraph, b::PropertyGraph) = PropertyGraph(union(a.nodes, b.nodes), union(a.edges, b.edges); metadata=Dict(union(a.metadata, b.metadata)))

# Base.show(io::IO, g::PropertyGraph) = print(io, "PropertyGraph($(n_nodes(g)) nodes, $(n_edges(g)) edges)")

# Load error types
include("errors.jl")

"""
    valid_id(g::PropertyGraph)

Get the next available ID for a node in the graph, this is simply
the length of the graph's nodes plus one.
"""
valid_id(g::PropertyGraph) = toid(n_nodes(g) + 1)

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
    type::AbstractString
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

Base.getindex(g::PropertyGraph, id1::AbstractID, id2::AbstractID) = getindex(g.edges, value(id1), value(id2))
Base.getindex(g::PropertyGraph, id1::Integer, id2::Integer) = getindex(g, toid(id1), toid(id2))

"""
    Base.match(g::PropertyGraph, patterns::AbstractPattern...)

Match the pattern to the graph.
"""
function Base.match(g::PropertyGraph, patterns::AbstractPattern...; remap_ids::Bool=false)
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

    return remap_ids ?
           remap_id(PropertyGraph(nds, es, metadata=metadata(g))) :
           PropertyGraph(nds, es, metadata=metadata(g))
end

"""
    Base.match(g::PropertyGraph, query::Pair{NodePattern, NodePattern})

Create a subgraph containing all the nodes that match the left and right 
patterns, as well as all the edges that match the left and right patterns.
"""
function Base.match(g::PropertyGraph, query::Pair{<:AbstractNodeLike,<:AbstractNodeLike}; remap_ids::Bool=false)
    # Find all edges that connect the pair
    connections = edges(match(g, EdgePattern(query[1], query[2])))

    # Grab all nodes associated with each edge
    node_ids = unique(
        reduce(vcat,
            map(e -> [e.source, e.target], connections)
        )
    )

    # Get the nodes from the graph
    nodes = map(id -> g[id], node_ids)

    # Create a new subgraph with the nodes and edges
    return remap_ids ?
           remap_id(PropertyGraph(nodes, connections, metadata=metadata(g))) :
           PropertyGraph(nodes, connections, metadata=metadata(g))
end

function Base.match(
    g::PropertyGraph,
    query::Pair{A,Pair{B,C}};
    remap_ids::Bool=false
) where {A<:AbstractNodeLike,B<:Union{Symbol,AbstractString},C<:AbstractNodeLike}
    # Source, relation, target
    source, (relation, target) = query

    # Find all edges that connect the pair
    connections = edges(match(g, EdgePattern(source, target, String(relation))))

    # Grab all nodes associated with each edge
    node_ids = unique(
        reduce(vcat,
            map(e -> [e.source, e.target], connections)
        )
    )

    # Get the nodes from the graph
    nodes = map(id -> g[id], node_ids)

    # Create a new subgraph with the nodes and edges
    return remap_ids ?
           remap_id(PropertyGraph(nodes, connections, metadata=metadata(g))) :
           PropertyGraph(nodes, connections, metadata=metadata(g))
end

connections_between(g::PropertyGraph, n1::AbstractNodeLike, n2::AbstractNodeLike) = connections_between(g, id(n1), id(n2))
function connections_between(g::PropertyGraph, n1::AbstractID, n2::AbstractID)
    # Get all the edges that connect n1 to n2
    return filter(e -> sourcematch(e, n1) || sourcematch(e, n2), edges(g))
end


function neighbors(g::PropertyGraph, id::ID)
    # 
end

"""
    remap_id(g::PropertyGraph)

Makes all IDs monotone.
"""
function remap_id(g::PropertyGraph)
    # Get all IDs
    node_ids = map(id, nodes(g))
    source_ids = map(source, edges(g))
    target_ids = map(target, edges(g))
    new_node_ids = 1:n_nodes(g)

    # Construct a mapping from old to new
    id_mapping = Dict(zip(node_ids, new_node_ids))
    source_mapping = Dict(source_id => id_mapping[source_id] for source_id in source_ids)
    target_mapping = Dict(target_id => id_mapping[target_id] for target_id in target_ids)

    # Construct the new nodes
    new_nodes = map(n -> Node(id_mapping[id(n)], labels(n), properties(n)), nodes(g))
    new_edges = map(e -> Edge(source_mapping[source(e)], target_mapping[target(e)], type(e), properties(e)), edges(g))

    # Construct the new graph
    return PropertyGraph(new_nodes, new_edges, metadata=metadata(g))
end



"""
    save(n::Node)

Save the node to a file. By default, nodes are written to `nodes.bin`. 

struct Node <: AbstractNodeLike
    id::ID
    labels::Vector{String}
    properties::Config
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




