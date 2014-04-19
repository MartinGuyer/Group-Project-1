setwd("C:/Users/Martin/Documents")
higher.ed <- read.csv("EdSalary.csv", header=TRUE)
dim(higher.ed)
head(higher.ed[,4:9])
Ed.Sal_max <- apply(higher.ed[,4:9], 1, which.max)
#All of the states have people with professional
#degrees receiving the highest income.

#Check for all degrees except professional
Ed.Sal_max.nopro <- apply(higher.ed[,c(4:7, 9)], 1, which.max)
Ed.Sal_max.nodocOrPro <- apply(higher.ed[,c(4:6)], 1, which.max)

hist(Ed.Sal_max, breaks=c(0, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5))
ed.names <- c("AA", "BA/BS", "MA/MS", "PE", "PhD")
Max.Sal <- c(0, 0, 0, 51, 0)
barplot(Max.Sal, names.arg=ed.names, main="Histogram of Educational
        level with highest income", ylab= "Frequency", xlab="Educational
        level")
ed.names2 <- c("AA", "BA/BS", "MA/MS", "PhD")
Max.Sal2 <- c(0, 0, 0, 51)
barplot(Max.Sal2, names.arg=ed.names2, main="Histogram of Educational
        level with highest income", ylab= "Frequency", xlab="Educational
        level")
apply(higher.ed[,c(4:9)], 2, mean)
