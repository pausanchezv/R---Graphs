#' Tipus de dades abstracte per representar el graph
#' Conté un diccionari de nodes i un diccionari d'arestes
#' Cada node del graph és un diccionari d'atributs
#' Cada aresta del graph és un diccionari d'atributs
setClass(
    
    "Graph", 
    
    representation(
        nodes="list", 
        edges="list",
        neighbors="list"
    ), 
    
    prototype(
        nodes=list(), 
        edges=list(),
        neighbors=list()
    )
)

#' Afegeix un node al graph
addNode <- function(G, node) {
    eval.parent(substitute(G@nodes[toString(node)] <- list('node')))
}

#' Afegeix un atribut a un node
addNodeAttrs <- function(G, node, attrs) {
    
    keys <- names(attrs)
    for (i in 1: length(attrs)) {
        eval.parent(substitute(G@nodes[[toString(node)]][toString(keys[i])] <- attrs[i]))
    }
}

#' Afegeix un node amb atributs
addNodeWithAttrs <- function(G, node, attrs) {
    
    eval.parent(substitute(addNode(G, node)))
    eval.parent(substitute(addNodeAttrs(G, node, attrs)))
}

#' Afegeix nodes procedents d'una llista 'c' -no permet atributs-
addNodesFrom <- function(G, nodelist) {
    
    for (i in 1:length(nodelist)) {
        eval.parent(substitute(addNode(G, nodelist[i])))
    }
}

#' Elimina un node del graph
#' Si el node estava connectat, eliminatotes les arestes de connexió al node
removeNode <- function(G, node) {
    
    node <- toString(node)
    keys <- names(graph@edges)
    
    for (i in 1:length(G@edges)) {
        
        edge <- unlist(strsplit(toString(keys[i]), split=" "))
        
        if (node %in% edge) {
            eval.parent(substitute(removeEdge(G, edge[1], edge[2])))
            eval.parent(substitute(removeEdge(G, edge[2], edge[1])))
        }
    }
    
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

#' Afegeix una aresta sense atributs al graph
#' 
#' 1. Comprova si els nodes existeixeen i si no és així, els crea
#' 2. Afegeix l'aresta com un diccionari on la clau és l'string concatenat dels dos nodes que formen l'aresta
#' i el valor és un diccionari d'atributs
#' 3. Afegeix els veïns en les dues direccions
addEdge <- function(G, edge) {
    
    blackNode <- toString(edge[1])
    redNode <- toString(edge[2])
    
    if (!hasNode(G, blackNode)) {
        eval.parent(substitute(addNode(G, blackNode)))
    }
    
    if (!hasNode(G, redNode)) {
        eval.parent(substitute(addNode(G, redNode)))
    }
    
    strEdge <- paste(blackNode, redNode)
    eval.parent(substitute(G@edges[strEdge] <- list('edge')))
    
    eval.parent(substitute(G@neighbors$blackNode <- c(G@neighbors$blackNode, redNode)))
    eval.parent(substitute(G@neighbors$redNode <- c(G@neighbors$redNode, blackNode)))

    eval.parent(substitute(G@neighbors$redNode <- unique(G@neighbors$redNode)))
    eval.parent(substitute(G@neighbors$blackNode <- unique(G@neighbors$blackNode)))
}

#' Afegeix atributs a l'aresta
addEdgeAttributes <- function(G, edge, attrs) {
    
    edge <- paste(toString(edge[1]), toString(edge[2]))
    keys <- names(attrs)
    
    for (i in 1: length(attrs)) {
        eval.parent(substitute(G@edges$edge[toString(keys[i])] <- attrs[i]))
    }
}

#' afegeix una aresta directament amb atributs
addEdgeWithAttributes <- function(G, edge, attrs) {
    
    eval.parent(substitute(addEdge(G, edge)))
    eval.parent(substitute(addEdgeAttributes(G, edge, attrs)))
}


#' Retorna true si existeix una aresta, false en cas contrari
hasEdge <- function(G, blackNode, redNode) {
    
    blackNode <- toString(blackNode)
    redNode <- toString(redNode)
    
    return(blackNode %in% G@neighbors[[redNode]] || redNode %in% G@neighbors[[blackNode]])
}


#' Elimina una aresta del graph,
#' I elimina la relació de veïnat de l'aresta a la llista d'adjacència de veïns
removeEdge <- function(G, blackNode, redNode) {
    
    blackNode <- toString(blackNode)
    redNode <- toString(redNode)
    
    eval.parent(substitute(G@edges[paste(blackNode, redNode)] <- NULL))
    eval.parent(substitute(G@edges[paste(redNode, blackNode)] <- NULL))
    
    blackIndex <- which(G@neighbors[[blackNode]] == redNode)
    redIndex <- which(G@neighbors[[redNode]] == blackNode)
    
    eval.parent(substitute(G@neighbors[[blackNode]] <- G@neighbors[[blackNode]][-blackIndex]))
    eval.parent(substitute(G@neighbors[[redNode]] <- G@neighbors[[redNode]][-redIndex]))
    
    if (length(eval.parent(substitute(G@neighbors$blackNode))) < 1) {
        eval.parent(substitute(G@neighbors$blackNode <- NULL))
    }
    
    if (length(eval.parent(substitute(G@neighbors$redNode))) < 1) {
        eval.parent(substitute(G@neighbors$redNode <- NULL))
    }
}


