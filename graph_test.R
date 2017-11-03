source('graph.R')

# es crea el node
graph <- new("Graph")

# s'afegeix un node
addNode(graph, 1)

# s'afegeixen atributs al node
addNodeAttrs(graph, 1, list('name'='one'))

# s'afegeix un node directament amb atributs
addNodeWithAttrs(graph, 2, list('name'='two'))

# s'afegeixen nodes procedents d'una llista de valors
addNodesFrom(graph, c(3, 4))

# s'afegeixen atributs als nodes
addNodeAttrs(graph, 3, list('name'='three'))
addNodeAttrs(graph, 4, list('name'='four'))

# s'afegeix un node directament amb atributs
addNodeWithAttrs(graph, 5, list('name'='five'))

# s'eleimina el node '2'
removeNode(graph, 2)

# s'assigna a x el node 3
x <- getNode(graph, 3)

# s'accedeix a un atribut del node 'x'
x$name

# es comprova si existeixen nodes al graph
hasNode(graph, 2)
hasNode(graph, 4)

# s'afegeix una aresta al graph
addEdge(graph, c(1, 3))

# s'afegeixen dos atributs a l'aresta anterior
addEdgeAttributes(graph, c(1, 3), list('weight'=10, 'distance'=4))

# s'afegeixen dues arestes directament amb atributs
addEdgeWithAttributes(graph, c(3, 4), list('weight'=11, 'distance'=5))
addEdgeWithAttributes(graph, c(5, 4), list('weight'=12, 'distance'=6))

#' es comprova si el graf té una aresta
hasEdge(graph, 1, 2)
hasEdge(graph, 1, 3)
hasEdge(graph, 3, 1)

#' s'elimina un node (ha d'eliminar també les arestes involucrades)
removeNode(graph, 4)

#' s'elimina l'aresta 3, 1 del graf
removeEdge(graph, 3, 1)


# Es mostren els nodes i les arestes
graph@nodes
View(graph@nodes)

graph@edges
View(graph@edges)


