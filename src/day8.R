#https://adventofcode.com/2021/day/8
input<- read.table("data/day8_test1.txt", header = FALSE, sep = " ")
input<- read.table("data/day8_test2.txt", header = FALSE, sep = " ")
input<- read.table("data/day8.txt", header = FALSE, sep = " ")

#part1
sum(apply(input[,rev(names(input))[1:4]], 2,nchar)%in%c(2,3,4,7))

#part2

sevensegment <- function(data=input){
        '%!in%' <- function(x, y) ! ('%in%'(x, y))
        uniques <- function(x,y) unlist(strsplit(y[,names(y)[which(segmentsizes==x)]],""))
        nonUniques <- function(x,y) as.character(unlist(apply(y[,names(which(segmentsizes==x))],1, function(z) strsplit(z,""))))
        myvalues <- numeric()
        for(i in 1:nrow(data)){
                entry <- data[i,]
                #rules:
                segmentsizes <- apply(entry[,names(entry)[1:10]], 2, nchar)
                pos1 <- uniques(3,entry)[which(uniques(3,entry)%!in%uniques(2,entry))]
                pos2_4 <- uniques(4,entry)[which(uniques(4,entry)%!in%uniques(3,entry))]
                pos2_5 <- names(which(table(nonUniques(5,entry))==1))
                pos2 <- intersect(pos2_4,pos2_5)
                pos4<-setdiff(pos2_4,pos2)
                pos5 <- setdiff(pos2_5,pos2)
                pos1_4_7 <- names(which(table(nonUniques(5, entry))==3))
                pos7 <- setdiff(pos1_4_7,c(pos1,pos4))
                pos3_4_5 <- names(which(table(nonUniques(6, entry))==2))
                pos3 <-setdiff(pos3_4_5,c(pos4,pos5))
                pos6<- setdiff(c('a','b','c','d','e','f','g'),c(pos1,pos2,pos3,pos4,pos5,pos7))
                #signals decoded
                sig0 <- paste0(sort(c(pos1,pos2,pos3,pos5,pos6,pos7)),collapse="")
                sig1 <- paste0(sort(c(pos3,pos6)),collapse="")
                sig2 <- paste0(sort(c(pos1,pos3,pos4,pos5,pos7)),collapse="")
                sig3 <- paste0(sort(c(pos1,pos3,pos4,pos6,pos7)),collapse="")
                sig4 <- paste0(sort(c(pos2,pos3,pos4,pos6)),collapse="")
                sig5 <- paste0(sort(c(pos1,pos2,pos4,pos6,pos7)),collapse="")
                sig6 <- paste0(sort(c(pos1,pos2,pos4,pos5,pos6,pos7)),collapse="")
                sig7 <- paste0(sort(c(pos1,pos3,pos6)),collapse="")
                sig8 <- paste0(sort(c(pos1,pos2,pos3,pos4,pos5,pos6,pos7)),collapse="")
                sig9 <- paste0(sort(c(pos1,pos2,pos3,pos4,pos6,pos7)),collapse="")
                signals <- c(sig0,sig1,sig2,sig3,sig4,sig5,sig6,sig7,sig8,sig9)
                #match
                myvalues <- c(myvalues,as.numeric(paste(apply(entry[,sort(rev(names(entry))[1:4])], 2,function(x) which(signals==paste(sort(unlist(strsplit(x,""))),collapse=""))-1),collapse="")))
        }
        return(myvalues)
}
myvalues <- sevensegment(input)
sum(myvalues)

      