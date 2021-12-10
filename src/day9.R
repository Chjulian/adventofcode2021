#https://adventofcode.com/2021/day/9

input<- scan("data/day9.txt", what = 'character')
input<- matrix(as.integer(unlist(strsplit(input,""))),nrow=length(input), byrow = T)

#part1
adjacentradar <- function(x,y,data=input){ #get value of any low point
        myvalues <- integer()
        N<-try(data[x,y-1], silent = TRUE)
        S<-try(data[x,y+1], silent = TRUE)
        E<-try(data[x+1,y], silent = TRUE)
        W<-try(data[x-1,y], silent = TRUE)
        if(class(N) != "try-error") myvalues <- c(myvalues,N)
        if(class(S) != "try-error") myvalues <- c(myvalues,S)
        if(class(E) != "try-error") myvalues <- c(myvalues,E)
        if(class(W) != "try-error") myvalues <- c(myvalues,W)
        return(myvalues)
}

risklevels <- function(data=input){ #return scores as in the guidelines
        myvalues<-c()
        for(i in 1:nrow(input)){
                for(j in 1:ncol(input)){
                        adjacent <- adjacentradar(i,j)
                        if(all(data[i,j] < adjacent)) myvalues <- c(myvalues,data[i,j])
                }
        }
        return(sum(myvalues+1))
}
risklevels()

#part2
risklevels2 <- function(data=input){ #return matrix indexes of any low point
        myvalues<-c()
        for(i in 1:nrow(input)){
                for(j in 1:ncol(input)){
                        adjacent <- adjacentradar(i,j)
                        if(all(data[i,j] < adjacent)) myvalues <- c(myvalues,paste0(i,',',j))
                }
        }
        return(myvalues)
}
coordinates<-risklevels2()

vector.is.not.empty <- function(x) return(length(x) !=0 )

adjacentradar2 <- function(x,y,data=input){ #return matrix indexes of locations != 9
        myvalues <- integer()
        N<-try(data[x,y-1], silent = TRUE)
        S<-try(data[x,y+1], silent = TRUE)
        E<-try(data[x+1,y], silent = TRUE)
        W<-try(data[x-1,y], silent = TRUE)
        if(class(N) != "try-error" & vector.is.not.empty(N)) if(N!=9) myvalues <- c(myvalues,paste0(x,',',y-1))
        if(class(S) != "try-error" & vector.is.not.empty(S)) if(S!=9) myvalues <- c(myvalues,paste0(x,',',y+1))
        if(class(E) != "try-error" & vector.is.not.empty(E)) if(E!=9) myvalues <- c(myvalues,paste0(x+1,',',y))
        if(class(W) != "try-error" & vector.is.not.empty(W)) if(W!=9) myvalues <- c(myvalues,paste0(x-1,',',y))
        return(myvalues)
}

'%!in%' <- function(x, y) ! ('%in%'(x, y))

basinsizes<- integer() #loop to check locations:
for(coordinate in coordinates){
        tocheck <- coordinate
        checked <- character()
        while(length(tocheck)!=0){
                for(loc in tocheck){
                        checked <- c(loc, unique(checked))
                        locx <- as.integer(strsplit(loc,",")[[1]][1])
                        locy <- as.integer(strsplit(loc,",")[[1]][2]) 
                        tocheck <- c(tocheck,adjacentradar2(locx,locy))
                }
                tocheck <- tocheck[tocheck%!in%checked]
                tocheck <- unique(tocheck)
        }
        basinsizes<-c(basinsizes,length(unique(checked)))
}
basinsizes <- sort(basinsizes,decreasing = TRUE)
basinsizes[1]*basinsizes[2]*basinsizes[3]

