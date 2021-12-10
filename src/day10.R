#https://adventofcode.com/2021/day/10
input <- scan("data/day10.txt", what = 'character')
input <- strsplit(input,"")

'%!in%' <- function(x, y) ! ('%in%'(x, y))

#part1
inspector <-#return score for a vector of values
        function(x){
                dictionary<- setNames(c("(","[","{","<"),c(")","]","}",">"))
                scoring<- setNames(c(3,57,1197,25137),c(")","]","}",">"))
                runsignal<-TRUE
                while(runsignal) {
                        toremove <- integer()
                        for(pos in 1:(length(x)-1)){
                                if(x[pos]%in%dictionary){
                                        if(x[pos+1]==names(which(x[pos]==dictionary))) toremove <- c(toremove,pos,pos+1)
                                }
                        }
                        if(length(toremove)==0) runsignal <- FALSE
                        x<-x[1:length(x)%!in%toremove] #remove chunks
                }
                if(any(x%in%names(dictionary))) {
                        return(as.integer(scoring[x[which(x%in%names(dictionary))][[1]]]))
                } else return(0)
        }
scores <- lapply(input, inspector)
sum(unlist(scores))

#part2
input<-input[which(unlist(scores)==0)] #get incomplete lines

scoring<- function(y){
        dictionary <- setNames(c(1,2,3,4),c(")","]","}",">"))
        timer<-length(y)
        score=0
        for(pos in 1:length(y)){
                score<-(score*5)+as.integer(dictionary[y[pos]])
        }
        return(score)
}

inspector2 <- #return sequence instead of scores
        function(x){
                dictionary<- setNames(c("(","[","{","<"),c(")","]","}",">"))
                rev.dictionary<- setNames(c(")","]","}",">"),c("(","[","{","<"))
                runsignal<-TRUE
                while(runsignal) {
                        toremove <- integer()
                        for(pos in 1:(length(x)-1)){
                                if(x[pos]%in%dictionary){
                                        if(x[pos+1]==names(which(x[pos]==dictionary))) toremove <- c(toremove,pos,pos+1)
                                }
                        }
                        if(length(toremove)==0) runsignal <- FALSE
                        x<-x[1:length(x)%!in%toremove] #remove chunks
                }
                return(scoring(as.vector(rev(rev.dictionary[x]))))
        }
finalscores <- sort(unlist(lapply(input, inspector2)))
finalscores[ceiling(length(finalscores)/2)]

