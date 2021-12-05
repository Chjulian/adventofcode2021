#https://adventofcode.com/2021/day/5
input<- read.csv("data/day5_test1.txt", header = F)
#ugly code, is Sunday :P

#part1
#parsing function
parser <- function(data=input){
        V1<-data[,'V1']; V4<-data[,'V3'] 
        V2<-as.data.frame(data$V2); names(V2) <- 'V2'
        V2 <- as.data.frame(stringr::str_split_fixed(V2$V2, " ", 3))
        V2<-V2[,c('V1','V3')]
        V2$V1<- as.integer(V2$V1);V2$V3<- as.integer(V2$V3)
        Vf<-cbind(V1,V2)
        Vf<-cbind(Vf,V4)
        names(Vf) <- c('V1','V2','V3','V4')
        return(Vf)
}
input <- parser(input)

#function to get points
pointer<-function(x1,y1,x2,y2){
        points<-integer()
        for(i in x1:x2){
                for(j in y1:y2){
                        points<-c(points, paste(i,j,sep=','))
                }
        }
        return(points)
}

vents <- function(data=input){
        all.points <- character()
        for(i in 1:nrow(data)){
                #only consider vertical/horizontal lines
                if(data[i,'V1']==data[i,'V3']|data[i,'V2']==data[i,'V4']){
                        points <- pointer(data[i,'V1'], data[i,'V2'],data[i,'V3'],data[i,'V4'])
                        all.points<- c(all.points, points)
                }
                
        }
        return(all.points)
} 
points <- vents()
sum(table(points)>=2)


##part2

pointer2<-function(x1,y1,x2,y2){
        xs <- x1:x2
        ys <- y1:y2
        return(paste(xs,ys,sep=","))
}

vents <- function(data=input){
        all.points <- character()
        for(i in 1:nrow(data)){
                #consider vertical/horizontal lines and 45o diagonals
                if(data[i,'V1']==data[i,'V3']|data[i,'V2']==data[i,'V4']){
                        points <- pointer(data[i,'V1'], data[i,'V2'],data[i,'V3'],data[i,'V4'])
                        all.points<- c(all.points, points)
                } else { 
                        if(length(data[i,'V1']:data[i,'V3'])==length(data[i,'V2']:data[i,'V4'])){
                                points <- pointer2(data[i,'V1'], data[i,'V2'],data[i,'V3'],data[i,'V4'])
                                all.points<- c(all.points, points)
                        }
                }
                
                
        }
        return(all.points)
} 
points <- vents()
sum(table(points)>=2)
