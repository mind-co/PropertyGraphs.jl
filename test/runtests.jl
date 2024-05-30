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
    end

    @testset "Remapping" begin
        @info "Testing remapping"

        nonsequential_node_a = Node(1000000)
        nonsequential_node_b = Node(1000001)
        nonsequential_edge = Edge(1000000, 1000001, "friend of"; since=2010)

        corrected_a = Node(1)
        corrected_b = Node(2)
        corrected_edge = Edge(1, 2, "friend of"; since=2010)

        g = PropertyGraph(Node[nonsequential_node_a, nonsequential_node_b], Edge[nonsequential_edge])
        remapped_g = remap_id(g)
        corrected_g = PropertyGraph(Node[corrected_a, corrected_b], Edge[corrected_edge])

        @test nodes(remapped_g) == nodes(corrected_g)
        @test edges(remapped_g) == edges(corrected_g)
        @test remapped_g == corrected_g
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
            @test match(graph, EdgePattern("friend of")) == PropertyGraph(Node[], Edge[john_jane, jane_john])

            # Match all edges from John
            @test match(graph, EdgePattern(1)) == PropertyGraph(Node[], Edge[john_jane, husband])

            # Match edges between john and jane
            @test match(
                graph,
                NodePattern("person", "human"; name="John Doe", age=25),
                EdgePattern(1, 2),
                EdgePattern(2, 1),
            ).edges == Edge[john_jane, husband, jane_john, wife]
        end

        @testset "Node pair match" begin
            @info "Testing Node pair match"

            alice = Node(1, "person", "human"; name="Alice Doe", age=27)
            bob = Node(2, "person", "human"; name="Bob Doe", age=28)
            charlie = Node(3, "person", "human"; name="Charlie Doe", age=29)
            doug = Node(4, "dog", "pet"; name="Doug", breed="Labrador", age=3)
            eve = Node(5, "cat", "pet"; name="Eve", breed="Persian", age=2)

            alice_bob = Edge(1, 2, "friend of"; since=2012)
            bob_alice = Edge(2, 1, "friend of"; since=2013)
            alice_doug = Edge(1, 4, "pet of"; since=2014)
            bob_eve = Edge(2, 5, "pet of"; since=2015)
            doug_eve = Edge(4, 5, "best friend of"; since=2016)
            doug_eve_2 = Edge(4, 5, "toy stealer of"; since=2017)
            eve_doug = Edge(5, 4, "mortal enemy of"; since=2017)

            nds = Node[alice, bob, charlie, doug, eve]
            edg = Edge[alice_bob, bob_alice, alice_doug, bob_eve, doug_eve, eve_doug, doug_eve_2]

            graph = PropertyGraph(nds, edg)

            gab = (remap_id ∘ PropertyGraph)(Node[alice, bob], Edge[alice_bob])
            gad = (remap_id ∘ PropertyGraph)(Node[alice, doug], Edge[alice_doug])
            gbe = (remap_id ∘ PropertyGraph)(Node[bob, eve], Edge[bob_eve])
            gde = (remap_id ∘ PropertyGraph)(Node[doug, eve], Edge[doug_eve, doug_eve_2])

            # Test matching a single node pair
            @test (remap_id ∘ match)(graph, alice => bob) == (remap_id ∘ PropertyGraph)(Node[alice, bob], Edge[alice_bob])
            @test (remap_id ∘ match)(graph, alice => doug) == (remap_id ∘ PropertyGraph)(Node[alice, doug], Edge[alice_doug])
            @test (remap_id ∘ match)(graph, bob => eve) == (remap_id ∘ PropertyGraph)(Node[bob, eve], Edge[bob_eve])
            @test (remap_id ∘ match)(graph, doug => eve) == (remap_id ∘ PropertyGraph)(Node[doug, eve], Edge[doug_eve, doug_eve_2])

            # Test matching by connection type
            @test match(graph, NodePattern() => "friend of" => NodePattern(), remap_ids=true) == remap_id(PropertyGraph(Node[alice, bob], Edge[alice_bob, bob_alice]))
            @test match(
                graph,
                NodePattern() => "pet of" => NodePattern(),
                remap_ids=true
            ) == remap_id(PropertyGraph(
                Node[alice, doug, bob, eve],
                Edge[alice_doug, bob_eve]
            ))
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

    @testset "union" begin
        @info "Testing union"

        alice = Node(1, "person", "human"; name="Alice Doe", age=27)
        bob = Node(2, "person", "human"; name="Bob Doe", age=28)
        charlie = Node(3, "person", "human"; name="Charlie Doe", age=29)
        alice_bob = Edge(1, 2, "friend of"; since=2012)
        bob_alice = Edge(2, 1, "friend of"; since=2013)
        charlie_alice = Edge(3, 1, "friend of"; since=2014)

        g1 = PropertyGraph(Node[alice, bob], Edge[alice_bob, bob_alice])
        g2 = PropertyGraph(Node[charlie], Edge[charlie_alice])

        @test union(g1, g2) == PropertyGraph(Node[alice, bob, charlie], Edge[alice_bob, bob_alice, charlie_alice])
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

