input <- read.csv("data/day1.txt", header = F)

sonarSweep<-0;i<-1
while(i<nrow(input)){
        if(input[i+1,]>input[i,]) sonarSweep <- sonarSweep+1
        i<-i+1
};rm(i)
sonarSweep
