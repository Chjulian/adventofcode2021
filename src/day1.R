input <- read.csv("data/day1.txt", header = F)

#part1
sonarSweep<-0;i<-1
while(i<nrow(input)){
        if(input[i+1,]>input[i,]) sonarSweep <- sonarSweep+1
        i<-i+1
};rm(i)
sonarSweep

#part2
sonarSweep<-0
codons <- c(1,2,3)
while(codons[3]<nrow(input)){
        x <- input[codons[1],] + input[codons[2],] + input[codons[3],]
        codons <- codons+1
        y <- input[codons[1],] + input[codons[2],] + input[codons[3],]
        if(y>x) sonarSweep <- sonarSweep+1
};rm(x,ycodons)
sonarSweep
