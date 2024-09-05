#7.1
#a
set.seed(3579)
#b
b = 10000
n=200
mu=4  #population mean
alpha = 0.1
#c
CI90 <- function() {ds = rexp(n, rate=1/mu)
xbar = mean(ds)
sd = sd(ds)
lbd = xbar - qnorm(1-alpha/2)*sd/sqrt(n)
ubd = xbar + qnorm(1-alpha/2)*sd/sqrt(n)
return(c(lbd,ubd))
}

#d
confint=NULL
for (i in c(1:b)) {confint = rbind(confint,CI90())}
colnames(confint)=c("Lower Bound", "Upper Bound")

#e
proportion<- mean(confint[, 1] <= mu & confint[, 2] >= mu)
proportion


#7.2
smoker=read.table("/Users/pp16/Downloads/smoker.txt",header=TRUE)
colnames(smoker)=c("Smoke","SES")
smoker
summary(smoker)
smoketable <- table(smoker$Smoke,smoker$SES)
smoketable

margin.table(smoketable)
margin.table(smoketable, 1)
margin.table(smoketable, 2)

barplot(smoketable, legend=T, beside=T, main='Smoking Status by SES') 

barplot(prop.table(smoketable,2), legend=F, beside=T, main='Smoking
Status by SES')

barplot(t(prop.table(smoketable,1)), legend=T, beside=T, main='Smoking status as of now')

expected_freq <- margin.table(smoketable, 1) %*% t(margin.table(smoketable, 2)) / sum(smoketable)
expected_freq

chisq_test <- chisq.test(smoketable)
chisq_test

summary(smoketable)


#7.3
shifttable=matrix(c(16,24,654,306,670,330),nrow=3,byrow=T)
chisq.test(shifttable)

#pvalue is lesser than 0.05, hence accept null hypothesis, there is no significant association



 