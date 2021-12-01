#https://adventofcode.com/2021/day/1
input <- read.csv("data/day1.txt", header = F)

#part1
#count the number of times a depth measurement increases 
#from the previous measurement.
#(There is no measurement before the first measurement.)
#using a loop
sonarSweep<-0;i<-1
while(i<nrow(input)){
        if(input[i+1,]>input[i,]) sonarSweep <- sonarSweep+1
        i<-i+1
        if(i==nrow(input)) print(sonarSweep)
};rm(i, sonarSweep)

#using diff() #input is a vector
sum(diff(input$V1) > 0)


#part2
#count the number of times the sum of measurements in 
#å sliding window of 3 increases from the previous sum
#using a loop
sonarSweep<-0
codons <- c(1,2,3)
while(codons[3]<nrow(input)){
        x <- input[codons[1],] + input[codons[2],] + input[codons[3],]
        codons <- codons+1
        y <- input[codons[1],] + input[codons[2],] + input[codons[3],]
        if(y>x) sonarSweep <- sonarSweep+1
        if(codons[3]==nrow(input)) print(sonarSweep)
        
};rm(x,y,codons,sonarSweep)

#using diff() as above
input <-input$V1 #create vector
x<-input[1:c(length(input)-2)]
y<-input[2:c(length(input)-1)]
z<-input[3:length(input)]
input<- x + y + z
sum(diff(input) > 0)

