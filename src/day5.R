#https://adventofcode.com/2021/day/5
input<- read.csv("data/day5.txt", header = F)
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


#function to add +1 in a matrix
# mapper <- function(m,x,y){
#         m[y+1,x+1]<-m[y+1,x+1]+1
#         return(m)
# }

#function to get points
pointer<-function(x,y,z,w){
        points<-integer()
        for(i in x:z){
                for(j in y:w){
                        points<-c(points, paste(i,j,sep=','))
                }
        }
        return(points)
}



vents <- function(data=input){
        # mygrid <- matrix(rep(0,(max(data)+1)*(max(data)+1)),max(data)+1,max(data)+1)
        # row.names(mygrid)<- 0:max(data); colnames(mygrid)<- 0:max(data)
        all.points <- character()
        for(i in 1:nrow(data)){
                #only consider vertical/horizontal lines
                if(data[i,'V1']==data[i,'V3']|data[i,'V2']==data[i,'V4']){
                        points <- pointer(data[i,'V1'], data[i,'V2'],data[i,'V3'],data[i,'V4'])
                        all.points<- c(all.points, points)
                        # for(j in points){
                        #         mygrid <- mapper(mygrid, as.integer(strsplit(j,',')[[1]][1]),
                        #                          as.integer(strsplit(j,',')[[1]][2])) 
                        # }
                }
                
        }
        return(all.points)
        # return(mygrid)
} 
# mygrid <- vents()
# sum(mygrid==2)

points <- vents()
sum(table(points)>=2)

