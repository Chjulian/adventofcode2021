#https://adventofcode.com/2021/day/12
input <- read.table("data/day12.txt", sep = '-')
plot(igraph::graph_from_data_frame(input, directed = FALSE)) #just to visualize

names(input) <- c('from','to')
input <- rbind(input, setNames(input, c("to","from")))
input <- input[input$to!='start',]
input <- input[input$from!='end',]


#part 1
caves <- unique(c(input$to,input$from))
edges <- split(input$to, input$from)

cavesTracker <-setNames(rep(FALSE,length(caves)),caves)
cavesTracker[caves==toupper(caves)]<-NA #not affected by TRUE/FALSE statements ()

#run recursively
#e.g.the (next) cave to check is taken from the possible set of node it is connected to, and so on
ADDcaves <- function(cave='start',cavesVisited=cavesTracker){ #always start with start in edges
        if(isTRUE(cavesVisited[cave])) return(0) #if visited stop--does not apply to uppercase so can be visited more than once.
        if(cave=="end") return(1)
        cavesVisited[cave] <-!cavesVisited[cave]
        sum(sapply(edges[[cave]],ADDcaves,cavesVisited)) #recursion
}
ADDcaves()

#part2
ADDcaves2 <- function(cave='start',cavesVisited=cavesTracker, revisited=FALSE){ 
        if(isTRUE(cavesVisited[cave])) {
                if(revisited) return(0) #if second time, stop
                cavesVisited[cave]<-FALSE #to allow a second time
                revisited <-TRUE #track second time
        } 
        if(cave=="end") return(1)
        cavesVisited[cave] <-!cavesVisited[cave]
        sum(sapply(edges[[cave]],ADDcaves2,cavesVisited, revisited)) #recursion
}
ADDcaves2()

# #DEPRECATED:
# #get paths
# simplepaths <- igraph::all_simple_paths(input,'start','end')
# simplepaths <- allpaths <- lapply(simplepaths, function(x) x<-names(x))
# 
# #Isolated caves
# #if a big cave (A) is connected to a single cave (a) w/out edges, then add 'A->a->A' 
# ADDcaves <- function(v,bc,sc) unlist(lapply(as.list(v), function(x) if(x == bc) c(bc, sc, bc) else x))
# for(sc in smallcaves) {
#         if(length(E(input)[from(match(sc, V(input)$name))])==1){
#                 bc<- gsub("[^[:alnum:]]", "", unlist(strsplit(attributes(E(input)[from(match(sc, V(input)$name))])[[1]],'\\|')))
#                 bc<- bc[bc!=sc]
#                 if(bc==toupper(bc)){
#                         for(path in 1:length(simplepaths)){
#                                 simplepaths[[path]] <- ADDcaves(v=simplepaths[[path]],bc=bc,sc=sc)
#                         }
#                 }
#         }
#         allpaths<-c(allpaths,simplepaths)
#         allpaths<-allpaths[!duplicated(allpaths)]
#         simplepaths <- allpaths
# }







