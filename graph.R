
library(pryr)

#' Tipus de dades abstracte per representar el graph
#' Conté un diccionari de nodes i un diccionari d'arestes
#' Cada node del graph és un diccionari d'atributs
#' Cada aresta del graph és un diccionari d'atributs
setClass(
    
    "Graph", 
    
    representation(
        nodes="list", 
        edges="list"
    ), 
    
    prototype(
        nodes=list(), 
        edges=list()
    )
)

#' Afegeix un node al graph
addNode <- function(G, node) {
    eval.parent(substitute(G@nodes[toString(node)] <- list('node')))
}

#' Afegeix un atribut a un node
addNodeAttr <- function(G, node, attr) {
    eval.parent(substitute(G@nodes[[toString(node)]][toString(names(attr))] <- attr))
}

#' Afegeix un node amb atributs
addNodeWithAttrs <- function(G, node, attrlist) {
    eval.parent(substitute(addNode(G, node)))
    keys <- names(attrlist)
    for (i in 1: length(attrlist)) {
        eval.parent(substitute(addNodeAttr(G, node, attrlist[i])))
    }
}

#' Afegeix nodes procedents d'una llista 'c' -no permet atributs-
addNodesFrom <- function(G, nodelist) {
    for (i in 1:length(nodelist)) {
        eval.parent(substitute(addNode(G, nodelist[i])))
    }
}

#' Elimina un node del graph
removeNode <- function(G, node) {
    eval.parent(substitute(G@nodes[toString(node)] <- NULL))
}

#' Retorna un node
getNode <- function(G, node) {
    return(G@nodes[[toString(node)]])
}

#' Retorna true si existeix un node, false en cas contrari
hasNode <- function(G, node) {
    return(!is.null(G@nodes[[toString(node)]]))
}


graph <- new("Graph")
print (pryr::address(graph))


addNodeWithAttrs(graph, 'node_1', list('attr_1'='value_1', 'attr_2'='value_2'))
addNodeWithAttrs(graph, 'node_2', list('attr_3'='value_3', 'attr_4'='value_4'))
graph@nodes$node_2$attr_3

addNode(graph, 'node_3')
#addNodeAttr(graph, 'node_3', 'attr_5', 'value_5')
addNodeAttr(graph, 'node_3', list('attr6'='value_7'))
graph@nodes$node_3

View(graph@nodes$node_3)

addNodesFrom(graph, c(1,2,3,4,5,'holaaaNode'))
graph@nodes

addNodeAttr(graph, 1, list('attr11111'='value_111111'))
View(graph@nodes)

