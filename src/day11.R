#https://adventofcode.com/2021/day/11
input <- scan("data/day11.txt", what = 'character')
input<- matrix(as.integer(unlist(strsplit(input,""))),nrow=length(input), byrow = T)

'%!in%' <- function(x, y) ! ('%in%'(x, y))
#part1 (and part2)
flashingcount <- function(data=input, steps=300){
        octocount = 0
        for(step in 1:steps){
                data <- data+1
                flashed <- integer()
                flashing <- which(data==10) 
                while(length(flashing)!=0){
                        for(octopus in flashing){
                                adjacentOctopus<-raster::adjacent(raster::raster(data),octopus, directions=8, pairs=FALSE)
                                data[adjacentOctopus] <- data[adjacentOctopus]+1
                                flashed <- c(flashed,octopus)
                                octocount <<- octocount+1
                        }
                        flashing <- which(data>9)[which(data>9) %!in% flashed]
                }
                data[which(data>9)] <- 0
                if(all(data==0)) print(paste0('all are flashing in step: ',step)) #part2
        }
        return(octocount)
}

octo<-flashingcount()

