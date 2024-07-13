using PropertyGraphs

# Create a graph
g = PropertyGraph()

# Add a node
alice = Node(1, "person", "human"; name="Alice", age=30)
bob = Node(2, "person", "human"; name="Bob", age=35)
car = Node(3, "vehicle"; brand="Toyota", model="Prius", year=2024)

# Add nodes to the graph
push!(g, alice)
push!(g, bob)
push!(g, car)

# Create an edge
alice_bob = Edge(alice, bob, "friend"; since=2020)
alice_car = Edge(alice, car, "owns"; since=2020)
bob_car = Edge(bob, car, "hit"; on="2024-02-29")
alice_bob_angry = Edge(alice, bob, "mad at"; since=2020)

# Add the edge to the graph
push!(g, alice_bob)
push!(g, alice_car)
push!(g, bob_car)
push!(g, alice_bob_angry)

# Match nodes by label
people = match(g, NodePattern("person"))
println("Number of people: ", length(nodes(people)))

# Match edges by type
friendships = match(g, EdgePattern("friend"))
println("Number of friendships: ", length(edges(friendships)))

@time friendships = match(g, EdgePattern("friend"), NodePattern("person"))
@time friendships = match(g, NodePattern("person") => "hit" => NodePattern("vehicle"))

# Access node properties
println("Alice's age: ", alice["age"])

# Modify edge properties
alice_bob["since"] = 2021
println("Updated friendship year: ", alice_bob["since"])

