#https://adventofcode.com/2021/day/4
day4file<- "data/day4.txt"
input <- readr::read_lines(day4file,skip=2)

#operator
'%!in%' <- function(x, y) ! ('%in%'(x, y))
#two functions, one to parse data and another to play bingo!
boarding <- function(data=input){
        data <- data[1:length(data)%!in%seq(6,length(data),by=6)]
        boardsnumber<- length(data)/5
        boardstrack <- c(1:5)
        boardsnames <- 1
        boards <- list()
        while(boardsnumber!=0){
                myboard <- matrix(as.integer(unlist(lapply(strsplit(data[boardstrack], " "), function(x){x[!x ==""]}))),5,5, byrow = T)
                boards[[boardsnames]] <- myboard
                boardstrack <- boardstrack+5
                boardsnames <- boardsnames+1
                boardsnumber <- boardsnumber-1
        }
        return(boards)
}
 bingo<- function(bingonumbers=inputnumbers,boards=inputboards){
         boards.logical <- boards
         bingo.vector <- integer()
         for(w in 1:length(boards)) boards.logical[[w]] <- matrix(rep(FALSE, 25),5,5)
         bingomatch <- function(x,y,z) {
                 y[x==z] <- TRUE
                 return(y)
                 }
         for(i in bingonumbers){
                 for(j in 1:length(boards)){
                         boards.logical[[j]] <- bingomatch(boards[[j]], boards.logical[[j]], i)
                         if(any(rowSums(boards.logical[[j]])==5) | any(colSums(boards.logical[[j]])==5)) {
                                 unmarked <- sum(boards[[j]][!boards.logical[[j]]])
                                 score <- unmarked*i
                                 return(score)
                         }
                 }
         }
 }

#part1
#play bingo
inputnumbers <- as.integer(unlist(strsplit(readLines(day4file, n = 1),",")))
inputboards <- boarding()
bingo()

#part2
#change the function to return the order of boards
bingo2<- function(bingonumbers=inputnumbers,boards=inputboards){
        boards.logical <- boards
        bingo.vector <- integer()
        score.table <- data.frame('board'=integer(),'score'=integer())
        for(w in 1:length(boards)) boards.logical[[w]] <- matrix(rep(FALSE, 25),5,5)
        bingomatch <- function(x,y,z) {
                y[x==z] <- TRUE
                return(y)
        }
        for(i in bingonumbers){
                for(j in 1:length(boards)){
                        boards.logical[[j]] <- bingomatch(boards[[j]], boards.logical[[j]], i)
                        if(any(rowSums(boards.logical[[j]])==5) | any(colSums(boards.logical[[j]])==5)) {
                                score.table<- rbind(score.table, data.frame('board'=j,
                                                                            'score'=sum(boards[[j]][!boards.logical[[j]]])*i))
                        }
                }
        }
        return(score.table)
}
score.table<-bingo2()
head(score.table[score.table$board==tail(unique(score.table$board), n=1),],1) #board + score
