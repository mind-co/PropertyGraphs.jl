#
# This doesn't work, don't @ me
#

# Imports
using Pkg
Pkg.activate(@__DIR__)
using Revise
using PropertyGraphs
using PropertyGraphs: AbstractNodeLike
using PromptingTools
using CSV, DataFrames
using JSON3
using ProgressMeter

# Define the graph
g = PropertyGraph()

# Add a data directory
data_dir = joinpath(homedir(), "code", "codex", "data")
relations_path = joinpath(data_dir, "relations", "en", "relations.json")
entities_dir = joinpath(data_dir, "entities", "en", "extracts")
triples_path = joinpath(data_dir, "triples", "codex-l", "train.txt")

# Load the relations
relations = JSON3.read(relations_path)
relation_labels = map(x -> x.label, values(relations))

# Load some triples
tuples = CSV.File(triples_path, delim='\t', limit=100000, header=[:head, :relation, :tail])

# Get the list of entities
entity_candidates = unique(vcat(tuples.head, tuples.tail))

# Get all text files with entity names
entity_files = filter(isfile, map(x -> joinpath(entities_dir, x * ".txt"), entity_candidates))
entities = map(x -> string(split(basename(x), '.')[1]), entity_files)

# Remove tuples that don't have entities
tuples = filter(x -> x.head in entities && x.tail in entities, tuples)

# Get a list of entity texts
entity_texts = map(x -> read(x, String), entity_files)

# Map entity codes to IDs
id_numbers = collect(1:length(entities))
entity_ids = Dict(a => b for (a, b) in zip(entities, id_numbers))

#
# Create a node for each entity
#
function add_node_by_entity!(g, entity, entity_text)
    newnode = Node(
        entity_ids[entity],
        "entity",
        text=entity_text,
        entity=entity,
    )
    push!(
        g,
        newnode
    )
end

for (entity, entity_text) in zip(entities, entity_texts)
    @debug "Adding node for entity $entity"

    try
        add_node_by_entity!(g, entity, entity_text)
    catch e
        @warn "Error adding node for entity $entity"
    end
end

#
# Add edges for the tuples
#
function add_edge_by_tuple!(g, tuple)
    rel = relations[tuple.relation]
    label = rel.label
    desc = rel.description
    source = entity_ids[String(tuple.head)]
    target = entity_ids[String(tuple.tail)]

    try
        push!(g, Edge(source, target, label, description=desc))
    catch e
        @warn e
    end
end

map(x -> add_edge_by_tuple!(g, x), tuples);

# for node in nodes(g)
#     @info node["text"]
# end

# Get all edges from a specific node
match(g, EdgePattern(source=1))

# Get all edges from a node to any other node, as well as all the edges
match(g, NodePattern(1) => NodePattern())

# Get all connections from node 1 to any other node, keeping only
# connections with the occupation relation
match(g, NodePattern(1) => :occupation => NodePattern())

# Go through each entity and match parents
# g[100]
# g[1, 100]


