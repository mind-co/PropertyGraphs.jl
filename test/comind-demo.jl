# Imports
using Revise
using PropertyGraphs
using PropertyGraphs: AbstractNodeLike
using PromptingTools

# Define the graph
g = PropertyGraph()

# User shortcut function
function user!(g::PropertyGraph, username::String)
    n = PropertyGraphs.Node(valid_id(g), "user"; username=username)
    push!(g, n)
    return n
end

function concept!(g::PropertyGraph, name::String)
    n = PropertyGraphs.Node(valid_id(g), "concept"; name=name)
    push!(g, n)
    return n
end

function thought!(g::PropertyGraph, user::Node, text::String, concept::Node)
    n = PropertyGraphs.Node(
        valid_id(g),
        "thought";
        text=text
    )
    push!(g, n)

    # Add an edge from the user to the concept
    edge!(g, user, n, "thought of")
    edge!(g, n, concept, "has concept")

    return n
end

function edge!(
    g::PropertyGraph,
    source::Node,
    target::Node,
    type::String;
    kwargs...
)
    @info "Adding edge $source -> $target of type $type" valid_id(g)
    e = Edge(source, target, type; kwargs...)
    push!(g, e)
    return e
end

# Define some users
users = map(x -> user!(g, x), ["alice", "bob", "charlie", "dave", "edward"])

# Define some concepts
concepts = map(x -> concept!(g, x), ["physics", "chemistry", "biology", "history", "geography"])

# Go through each user
for user in users
    for concept in concepts
        # 50% chance
        if rand() < 0.5
            # Create a new thought
            schema = PromptingTools.OllamaSchema()
            content = aigenerate(
                schema,
                "Your name is $(user["username"]) and you are thinking about the concept $(concept["name"]). Produce a new thought about the concept.",
                model="phi:latest"
            ).content |> String
            @info user["username"] content
            thought!(g, user, content, concept)
        end
    end
end

open("single-bit.bin", "w") do io
    write(io, 0b1)
end

read("single-bit.bin", Bool)

alice_thoughts = match(g,
    NodePattern("user", username="alice"),
    EdgePattern("thought of"),
    EdgePattern("has concept"),
)

thoughts = match(g,
    NodePattern("thought"),
)

map(
    x -> properties(x),
    nodes(thoughts)
)

