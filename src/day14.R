#https://adventofcode.com/2021/day/14
input <- df<-readLines("data/day14.txt")
input <- unlist(strsplit(input[1], ''))
input <- table(unlist(lapply(as.list(1:(length(input)-1)), function(x) paste0(input[x],input[x+1]))))
input <- data.frame('base'=names(input), 'count'=as.integer(input))

df <- as.data.frame(t(as.data.frame(strsplit(df[grep("->",df)], " -> "))))
rownames(df)<- NULL; names(df) <- c('base', 'poly')
df <-  tidyr::separate(data=df, col=base, into=c("base1", "base2"), sep=1)

#part1 and 2
#tabulate rules as pairs
rules<- data.frame('base'=character(), 'poly'=character())
for(i in 1:nrow(df)){
        rules<-rbind(rules, data.frame('base'=paste0(df$base1[i],df$base2[i]), 'poly'=paste0(df$base1[i],df$poly[i])))
        rules<-rbind(rules, data.frame('base'=paste0(df$base1[i],df$base2[i]), 'poly'=paste0(df$poly[i],df$base2[i])))
};rm(i,df)

template=input
steps=40
for(step in 1:steps){
        template <- merge(template,rules,by.x = "base")
        template <- dplyr::count(template,poly,wt=count)
        names(template) <- c('base','count')
};rm(step,steps)
#count outcomes
template <- tidyr::separate(data=template, col=base, into=c("base1", "base2"), sep=1)
counts <- dplyr::count(template,base2,wt=count)
format(max(counts$n)-min(counts$n), scientific = F)
