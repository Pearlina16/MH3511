#5.1
data5 = read.table("/Users/pp16/Downloads/rivers.txt", header = FALSE)
data = data5[,1]
hist(data)
boxplot(data)

qqnorm(data)
qqline(data,col="red")
#Data does not look normal as there is both long left and right tail

mean5 <- numeric(1000)
for(i in 1:1000) {mean5[i] <- mean(sample(rivers, size=5, replace=FALSE))}
qqnorm(mean5)
qqline(mean5,col="blue")
#Data does not look normal as there is both long left and right tail

mean30 <- numeric(1000)
for(i in 1:1000){mean30[i]<- mean(sample(rivers, size=30, replace=FALSE))}
qqnorm(mean30)
qqline(mean30,col="blue")
#Data is closer to normal distribution

#5.2
install.packages("HSAUR")
library("HSAUR")
str(roomwidth)
par(mfrow=c(1,2))
boxplot(roomwidth$width ~ roomwidth$unit) # 2 sequence tgt
#The plot gives the wrong suggestion that the estimates using "metres" would be lower than 

room_metres = roomwidth$width
cfactor= ifelse(roomwidth$unit=="feet", 1, 3.28) #if feet dn transform (1) else tranform factor 3.28
roomwidth$convert= roomwidth$width*cfactor
roomwidth

mean(roomwidth$convert[roomwidth$unit=="metres"])
sd(roomwidth$convert[roomwidth$unit=="metres"])
mean(roomwidth$convert[roomwidth$unit == "feet"])
sd(roomwidth$convert[roomwidth$unit == "feet"])
#Since we are not given the true width of the lecture hall, we cannot compare the two ways of estimation based on mean. however, the SD suggests that estimates in feet are more accurate than estimates in metres.

t.test(roomwidth$convert[roomwidth$unit=="metres"],conf.level = 0.95)
t.test(roomwidth$convert[roomwidth$unit == "feet"],conf.level = 0.95)

#5.3
shots = c(1.95, 1.78, 2.10, 1.82, 1.73, 2.01, 1.83, 1.90, 2.05, 1.85, 1.96, 1.98, 1.79)
qqnorm(shots)
qqline(shots,col="red")
#Approximately normal 

shapiro.test(shots)
#since p value is more than 0.05, we do not reject H0, data is normal

#b
coffee <- c(1.95, 1.78, 2.10, 1.82, 1.73, 2.01, 1.83, 1.90, 2.05, 1.85, 1.96, 1.98, 1.79)
n <- length(coffee)
xbar <- mean(coffee)
s <- sd(coffee)

alpha <- 0.10
z<- qnorm(1-alpha/2)
t <- qt(1-alpha/2, df=n-1)

zCI90p<- c(xbar-z*s/sqrt(n), xbar+z*s/sqrt(n))
tCI90p <- c(xbar-t*s/sqrt(n), xbar+t*s/sqrt(n))

print(paste("n=",n, "; xbar=", xbar, "; s=", s ))
print(paste("alpha=",alpha, "; z=", z, "; t=", t ))
print(paste("z90% CI= [", zCI90p[1], zCI90p[2], "]"))
print(paste("t90% CI= [", tCI90p[1], tCI90p[2], "]"))     
t.test(coffee, conf.level=0.9)


#c
alpha <- 0.10
z1<- qnorm(1-alpha) #one side dn /2
t1 <- qt(1-alpha, df=n-1)

z1CI90p<- c("-infty", xbar+z1*s/sqrt(n))
t1CI90p <- c("-infty", xbar+t1*s/sqrt(n))

print(paste("n=",n, "; xbar=", xbar, "; s=", s ))
print(paste("alpha=",alpha, "; z=", z1, "; t=", t1 ))
print(paste("1-sided z90% CI= [-infty, ", z1CI90p[2], "]"))
print(paste("1-sided t90% CI= [-infty," , t1CI90p[2], "]"))  
t.test(coffee, conf.level=0.9, alt="less")
