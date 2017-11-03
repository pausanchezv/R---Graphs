source('graph.R')

#' Llegeix les paraules del fitxer de text
getWords <- function() {
    words <- scan("words.txt", what=character())
    return(words)
}

#' Retorna parelles de paraules per aquelles que són diferents
getDifferentPairs <- function(words) {
    
    pairs <- list()
    cont <- 1
    
    for (i in 1:length(words)) {
        for (j in 1:length(words)) {
            if (words[i] != words[j]) {
                pairs[[cont]] <- c(words[i], words[j])
                cont <- cont + 1
            }
        }
    }
    
    return(pairs)
}

#' incementa el comptador
incCounter <- function(x) {
    eval.parent(substitute(x <- x + 1))
}

#' Crea les arestes al graph tenint encompte aquelles a les que només canvia una lletra
addConnections <- function(graph, pairs) {
    
    for (i in 1:length(pairs)) {
        
        diff <- 0
        
        A <- strsplit(pairs[[i]][1], '')
        B <- strsplit(pairs[[i]][2], '')
        
        for (j in 1:length(A[[1]])) {
            if (A[[1]][j] != B[[1]][j]) {
                incCounter(diff)
            }
        }
        
        if (diff == 1) {
            eval.parent(substitute(addEdge(graph, c(pairs[[i]][1], pairs[[i]][2]))))
        }
    }
}


bfs_shortest_path <- function(graph, start, goal) {
    
    queue <- list(list(start, list(start)))
    
    while (length(queue)) {
        
        current <- queue[[1]]
        queue[[1]] <- NULL
        
        node <- current[[1]]
        path <- current[[2]]
        
        
        for (neighbor in graph@neighbors[[node]]) {
            
            if (!(neighbor %in% path)) {
                
                path <- c(path, neighbor)
                queue[[length(queue) + 1]] <- list(neighbor, path)
                
                if (neighbor == goal) {
                    return(path)
                }
                
            }
            
        }
    }
    
    return(list())
}



words <- getWords()
graph <- new('Graph')
pairs <- getDifferentPairs(words)
addConnections(graph, pairs)
bfs_shortest_path(graph, 'fool', 'sage')
