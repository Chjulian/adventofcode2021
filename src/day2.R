#https://adventofcode.com/2021/day/2
input <- read.table("data/day2.txt", header = F, sep = " ")

#part1
#perform an operation as a function of a column value
#using a loop
horizontal <- depth <- 0
for(i in 1:nrow(input)){
        if(input[i,'V1']=="forward"){
                horizontal <- horizontal + input[i,'V2']
        } else{
                if(input[i,'V1']=="down") {
                        depth <- depth + input[i,'V2']
                } else{
                        depth <- depth - input[i,'V2']
                }
        }
};print(horizontal*depth);rm(i,horizontal,depth)

#part2
#perform an operation as a function of a column value
#using a loop
horizontal <- depth <- aim <- 0
for(i in 1:nrow(input)){
        if(input[i,'V1']=="forward"){
                horizontal <- horizontal + input[i,'V2']
                depth <- depth + (aim * input[i,'V2'])
        } else{
                if(input[i,'V1']=="down") {
                        aim <- aim + input[i,'V2']
                } else{
                        aim <- aim - input[i,'V2']
                }
        }
};print(horizontal*depth);rm(i,horizontal,depth)

