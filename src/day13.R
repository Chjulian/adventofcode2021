#https://adventofcode.com/2021/day/13
input <- scan("data/day13.txt", what = character())

instructions <- input[grep("=", input)]
input <- input[grep(",", input)]

instructions<- as.data.frame(t(as.data.frame(strsplit(instructions, "="))))
rownames(instructions)<- NULL
names(instructions) <- c('axis', 'value')
instructions$value <- as.integer(instructions$value)

input<- t(as.data.frame(strsplit(input, ",")))
input <- as.data.frame(apply(input, 2, as.integer))
rownames(input)<- NULL
names(input) <- c('x', 'y')

#part1
folder <- function(axis,value,data=input){
        rangey <- 0:max(data$y) #rows
        rangex <- 0:max(data$x) #columns
        folding <- function(v,value, rangev) unlist(lapply(as.list(v), function(z) if(z > value) rev(rangev)[z]-1 else z))
        if(axis=='x') {
                data$x <- folding(data$x, value=value, rangex)
                mm<- matrix(0, nrow = max(data$y)+1,ncol = value)
                }
        if(axis=='y') {
                data$y <- folding(data$y, value=value, rangey)
                mm<- matrix(0, nrow = value,ncol = max(data$x)+1)
                }
        for(i in 1:nrow(data)) mm[data$y[i]+1,data$x[i]+1] <- 1
        return(list(mm,data))
}

# mm<-folder('y',7) #test data
mm <- folder('x',655)
sum(mm[[1]]==1)

#part2
#to be completed

#and apply function across the element instructions
mm<-folder('y',6, mm[[2]])

#save matrix
write.table(mm[[1]], file="data/output_day13.txt", row.names=FALSE, col.names=FALSE)

# 1  1  11   11    11 111  1111 1  1  11 
# 1  1 1  1 1  1    1 1  1 1    1  1 1  1
# 1111 1    1  1    1 111  111  1111 1   
# 1  1 1 11 1111    1 1  1 1    1  1 1   
# 1  1 1  1 1  1 1  1 1  1 1    1  1 1  1
# 1  1  111 1  1  11  111  1111 1  1  11 



