struct NonSequentialID <: Exception
    id::AbstractID
    attempted_id::AbstractID
end

struct NodeAlreadyExists <: Exception
    existing_node::Node
    new_node::Node
end

struct EdgeAlreadyExists <: Exception
    existing_edge::Edge
    new_edge::Edge
end
