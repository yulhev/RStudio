#TASK1----
let1 <- c("M","A","T","H","E","M","A","T","I","C","S") #MATHEMATICS
table(let1)
let2<-c("A","C","E","H","I","M","S","T")
pr<-c(2,1,1,1,1,2,1,2)/11 # Probabilities before exp.
#table(letters) #prob of each letter
n <- 550
#prop.table(table(sample(let1, size=n, replace=TRUE)))
#all letters can repeat k times;

x <- sample(let2,n,prob=pr,replace = TRUE);x #sample with repetitions

tab=table(x)/length(x)  #probabilities after exp.
print(tab)
par(mfrow=c(1,2))
barplot(pr,names.arg=let2,main="Before experiment")#histogram for probabilities before experiment
barplot(tab,main="After experiment") #histogram for probabilities after experiment
dev.copy(png, file = "Hevchuk2.pdf")
dev.off()