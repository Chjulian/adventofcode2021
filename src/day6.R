#https://adventofcode.com/2021/day/6
input<- c(3,4,3,1,2) #test
input<-c(2,5,3,4,4,5,3,2,3,3,2,2,4,2,5,4,1,1,4,4,5,1,2,1,5,2,1,5,1,1,1,2,4,3,3,1,4,2,3,4,5,1,2,5,1,2,2,5,2,4,4,1,4,5,4,2,1,5,5,3,2,1,3,2,1,4,2,5,5,5,2,3,3,5,1,1,5,3,4,2,1,4,4,5,4,5,3,1,4,5,1,5,3,5,4,4,4,1,4,2,2,2,5,4,3,1,4,4,3,4,2,1,1,5,3,3,2,5,3,1,2,2,4,1,4,1,5,1,1,2,5,2,2,5,2,4,4,3,4,1,3,3,5,4,5,4,5,5,5,5,5,4,4,5,3,4,3,3,1,1,5,2,4,5,5,1,5,2,4,5,4,2,4,4,4,2,2,2,2,2,3,5,3,1,1,2,1,1,5,1,4,3,4,2,5,3,4,4,3,5,5,5,4,1,3,4,4,2,2,1,4,1,2,1,2,1,5,5,3,4,1,3,2,1,4,5,1,5,5,1,2,3,4,2,1,4,1,4,2,3,3,2,4,1,4,1,4,4,1,5,3,1,5,2,1,1,2,3,3,2,4,1,2,1,5,1,1,2,1,2,1,2,4,5,3,5,5,1,3,4,1,1,3,3,2,2,4,3,1,1,2,4,1,1,1,5,4,2,4,3)

#part1
fishpopulation <- function(end.time=90, init.fish=input){
        fish.counts <- data.frame('s0'=0,'s1'=0,'s2'=0,'s3'=0,
                                  's4'=0,'s5'=0,'s6'=0,'s7'=0,
                                  's8'=0)
        fishage <- function(x,y=fish.counts) fish.counts[,paste0('s',x)] <<- fish.counts[,paste0('s',x)]+1
        for(fish in input) fishage(fish)
        previous.gen <- 0
        for(day  in 1:end.time){
                fish.develop <- fish.counts[,c(paste0(rep('s',8),1:8))] #normal develop
                names(fish.develop) <- paste0(rep('s',7),0:7)
                fish.develop[,'s6'] <- fish.develop[,'s6'] + fish.counts[,'s0'] #reset develop
                if(previous.gen>0) fish.develop$s8 <- previous.gen else fish.develop$s8 <- 0
                previous.gen <- fish.develop[,'s0']
                fish.counts <- fish.develop
        }
        return(fish.counts)
}
myfish<- fishpopulation(end.time=256, init.fish=input)
totalfish <- sum(myfish)


