#https://adventofcode.com/2021/day/3
input <- read.table("data/day3.txt", header = F, colClasses = c("character"))
input<- stringr::str_split_fixed(input$V1, "", length(unlist(strsplit(input[1,1], ""))))

#part1
#get the most common value of a column
#function to table and sort
count2<- function(x, increasing=TRUE) ifelse(increasing==TRUE, 
                                         as.numeric(tail(names(sort(table(x))), 1)),
                                         as.numeric(head(names(sort(table(x))), 1)))

#use strtoi() for binary conversion 
gamma <- strtoi(paste(apply(input, 2, count2, increasing=T), collapse = ""), base=2)
epsilon <- strtoi(paste(apply(input, 2, count2, increasing=F), collapse = ""), base=2)
gamma*epsilon

#part2
#get the most common value of a column and iteratively subset rows until one remains
input<- apply(input, 2, as.integer)
#count2 is modified to return a vector of two when there is a tie
count3<- function(x, increasing=TRUE) {
        if(table(x)[[1]]==table(x)[[2]]) c(0,1) else ifelse(increasing==TRUE, 
                                                            as.numeric(tail(names(sort(table(x))), 1)),
                                                            as.numeric(head(names(sort(table(x))), 1)))
}
#main function. Loop until one row remains
lifesupport <- function(mydata=input, condition='oxygen'){
        colTracker<-1
        while(!is.vector(mydata)){
                criteria <- if(condition=='oxygen') count3(mydata[,colTracker], increasing=T) else count3(mydata[,colTracker], increasing=F)
                if(length(criteria)==1){
                        mydata<-mydata[mydata[,colTracker]==criteria,]
                } else {
                        mydata<-if(condition=='oxygen') mydata[mydata[,colTracker]==1,] else mydata[mydata[,colTracker]==0,]
                }
                colTracker <- colTracker+1
        }
        return(strtoi(paste(mydata, collapse = ""), base=2))
}
oxygen<-lifesupport()
CO2<-lifesupport(condition = 'CO2')
oxygen*CO2

