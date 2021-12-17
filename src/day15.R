#https://adventofcode.com/2021/day/15
input <- do.call(rbind,lapply(strsplit(readLines("data/day15.txt"),""), as.numeric))

#part1
chiton <- function(input){
        #get adjacent positions for each element in the matrix
        adj.l <- lapply(as.list(1:length(input)), function(x) raster::adjacent(raster::raster(input),x, directions=4, pairs=FALSE))
        #transform to a data frame
        df <- do.call(rbind,lapply(as.list(1:length(input)), function(x) data.frame('node1'=x,
                                                                                    'node2'=adj.l[[x]],
                                                                                    'value'=input[as.vector(adj.l[[x]])])))
        #create adjacent matrix
        adj <-matrix(0, length(input), length(input))
        for(i in 1:NROW(df)) adj[df[i,1], df[i,2]] <- df[i,3]  
        g <- igraph::graph_from_adjacency_matrix(adj, weighted=TRUE)
        sp <- get.shortest.paths(graph = g,from = 1, to = length(input), output = 'both')
        risks <- input[unlist(sp$vpath)]
        print(sum(risks[2:length(risks)]))
}
chiton(input=input)

#part2
dimensions<- function(input){
        x=input
        for(i in 1:4){
                df <- input+i
                df[df>9] <- (df[df>9]-9)
                x <- cbind(x,df)
        }
        y=x
        for(i in 1:4){
                df <- x+i
                df[df>9] <- (df[df>9]-9)
                y <- rbind(y,df)
        }
        return(y)
}
input2<-dimensions(input=input)
chiton(input=input2)


