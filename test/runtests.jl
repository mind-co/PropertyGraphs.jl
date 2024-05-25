using Test
using Revise
using PropertyGraphs

macro no_testset(name, expr)
    quote
        @warn "Skipping test set: $($name)"
        return :(
            begin end
        )
    end
end

@info "Let's goooooooooooooooooooo make sure this code works"

@testset "PropertyGraphs" begin

    john = Node(1, "person", "human"; name="John Doe", age=25)
    jane = Node(2, "person", "human"; name="Jane Doe", age=26)

    john_jane = Edge(1, 2, "friend of"; since=2010)
    jane_john = Edge(2, 1, "friend of"; since=2011)
    husband = Edge(1, 2, "husband of"; since=1992)
    wife = Edge(2, 1, "wife of"; since=1992)

    @testset "Node" begin
        @info "Testing Node"

        # Test signed integer id
        node = Node(1, "person", "human"; name="John Doe", age=25)
        @test node.id == 1
        @test node.labels == ["person", "human"]
        @test node.properties.name == "John Doe"
        @test node.properties.age == 25

        # Now test unsigned integer id
        unode = Node(UInt(1), "person", "human"; name="John Doe", age=25)
        @test unode.id == 1
        @test unode.labels == ["person", "human"]
        @test unode.properties.name == "John Doe"
        @test unode.properties.age == 25

        # Test equivalence
        @test node == unode

        # Test getindex
        @test node["name"] == "John Doe"
        @test node["age"] == 25

        # Test setindex
        node["name"] = "Jane Doe"
        @test node["name"] == "Jane Doe"
    end

    @testset "Edge" begin
        @info "Testing Edge"

        edge = Edge(1, 2, "friend of"; since=2010)
        @test edge.source == 1
        @test edge.target == 2
        @test edge.type == "friend of"
        @test edge.properties.since == 2010

        # Test unsigned integer id
        uedge = Edge(UInt(1), UInt(2), "friend of"; since=2010)
        @test uedge.source == 1
        @test uedge.target == 2
        @test uedge.type == "friend of"
        @test uedge.properties.since == 2010

        # Test equivalence
        @test edge == uedge

        # Test getindex
        @test edge["since"] == 2010

        # Test setindex
        edge["since"] = 2011
        @test edge["since"] == 2011

        # Edge constructor from nodes
        @test Edge(john, jane, "friend of"; since=2010) == Edge(1, 2, "friend of"; since=2010)
    end

    @testset "EdgeReference" begin
        @info "Testing EdgeReference"

        edge = Edge(1, 2, "friend of"; since=2010)

        edge_references = EdgeReference(Edge[], Edge[])
        @test edge_references.in_edges == Edge[]
        @test edge_references.out_edges == Edge[]

        # Test equivalence
        @test edge_references == EdgeReference(Edge[], Edge[])
        @test edge_references == EdgeReference(in_edges=Edge[], out_edges=Edge[])

        # Test with values
        edge_references = EdgeReference(Edge[edge, edge], Edge[edge, edge])
        @test edge_references.in_edges == Edge[edge, edge]
        @test edge_references.out_edges == Edge[edge, edge]

        # Test equivalence
        @test edge_references == EdgeReference(in_edges=Edge[edge, edge], out_edges=Edge[edge, edge])
    end

    @testset "PropertyGraph" begin
        @info "Testing PropertyGraph"

        # Test empty graph
        graph = PropertyGraph()
        @test graph.nodes == Node[]
        @test graph.edges == Edge[]

        # Edge-only graph
        graph = PropertyGraph(Edge[john_jane, jane_john])
        @test graph.nodes == Node[]
        @test graph.edges == Edge[john_jane, jane_john]

        # Node-only graph
        graph = PropertyGraph(Node[john, jane])
        @test graph.nodes == Node[john, jane]
        @test graph.edges == Edge[]

        # Complete graph
        graph = PropertyGraph(Node[john, jane], Edge[john_jane, jane_john])
        @test graph.nodes == Node[john, jane]
        @test graph.edges == Edge[john_jane, jane_john]

        # Refresh edge references
        update_edge_references(graph)
        @test graph.nodes[1].edge_references == EdgeReference(Edge[john_jane], Edge[jane_john])
        @test graph.nodes[2].edge_references == EdgeReference(Edge[jane_john], Edge[john_jane])
    end

    @testset "match" begin
        @info "Testing match"

        @testset "Node match" begin
            @info "Testing Node match"

            # Test matching a single node
            graph = PropertyGraph(Node[john, jane], Edge[john_jane, jane_john])

            @test match(graph, Node(1, "person", "human"; name="John Doe", age=25)) == PropertyGraph(Node[john], Edge[])
            @test match(graph, Node(1)) == PropertyGraph(Node[john], Edge[])
            @test match(graph, NodePattern("person", "human")) == PropertyGraph(Node[john, jane], Edge[])
            @test match(graph, Node(1, "person")) == PropertyGraph(Node[john], Edge[])
            @test match(graph, NodePattern("person", "human"; name="John Doe", age=25)) == PropertyGraph(Node[john], Edge[])
            @test match(graph, NodePattern("person", "human"; name="Jane Doe", age=26)) == PropertyGraph(Node[jane], Edge[])
            @test match(graph, NodePattern("person", "human"; name="Jane Doe")) == PropertyGraph(Node[jane], Edge[])
        end

        @testset "Edge match" begin
            @info "Testing Edge match"

            # Testing matching a single edge
            graph = PropertyGraph(Node[john, jane], Edge[john_jane, jane_john])
            # Testing matching a single edge
            graph = PropertyGraph(Node[john, jane], Edge[john_jane, jane_john, husband, wife])

            # Match all friend edges
            match(graph, EdgePattern("friend of")) == PropertyGraph(Node[], Edge[john_jane, jane_john])

            # Match all edges from John
            match(graph, EdgePattern(1)) == PropertyGraph(Node[], Edge[john_jane, husband])

            # Match edges between john and jane
            match(
                graph,
                NodePattern("person", "human"; name="John Doe", age=25),
                EdgePattern(1, 2),
                EdgePattern(2, 1),
            ).edges == Edge[john_jane, husband, jane_john, wife]
        end
    end

    @testset "intersect" begin
        @info "Testing intersect"

        john = Node(1, "person", "human"; name="John Doe", age=25)
        jane = Node(2, "person", "human"; name="Jane Doe", age=26)
        john_jane = Edge(1, 2, "friend of"; since=2010)
        jane_john = Edge(2, 1, "friend of"; since=2011)

        graph = PropertyGraph(Node[john, jane], Edge[john_jane, jane_john])
        @test intersect(graph, PropertyGraph(Node[john], Edge[])) == PropertyGraph(Node[john], Edge[])
    end

    @testset "push!" begin
        @info "Testing push!"

        graph = PropertyGraph(Node[john], Edge[])
        @test push!(graph, jane) == PropertyGraph(Node[john, jane], Edge[])
        @test_throws PropertyGraphs.NodeAlreadyExists push!(graph, jane)

        # Test pushing an edge
        @test push!(graph, john_jane) == PropertyGraph(Node[john, jane], Edge[john_jane])

        # Test pushing a node that is non-sequential
        big_id_node = Node(1000000)
        graph = PropertyGraph(Node[john], Edge[])
        @test_throws PropertyGraphs.NonSequentialID push!(graph, big_id_node)

        # Test pushing an edge taht already exists
        graph = PropertyGraph(Node[john, jane], Edge[john_jane])
        @test_throws PropertyGraphs.EdgeAlreadyExists push!(graph, john_jane)
    end
end;

@info "Testing complete"


# john = Node(1, "person", "human"; name="John Doe", age=25)
# jane = Node(2, "person", "human"; name="Jane Doe", age=26)
# john_jane = Edge(1, 2, "friend of"; since=2010)
# jane_john = Edge(2, 1, "friend of"; since=2011)

# husband = Edge(1, 2, "husband of"; since=1992)
# wife = Edge(2, 1, "wife of"; since=1992)

# g = PropertyGraph(Node[john, jane], Edge[john_jane, jane_john, husband, wife])

# pretty_print(g)
