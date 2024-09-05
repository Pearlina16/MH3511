#3.1a
data = read.table("/Users/pp16/Downloads/Lab03wip.txt", sep = "", header=T)

#3.1b
# plant 1
plant_1=data[data$plant==1,1]
plant_1
mean(plant_1)
median(plant_1)
Q1 = quantile(plant_1,0.25)
Q3 = quantile(plant_1,0.75)
max(plant_1)
min(plant_1)
range(plant_1)
IQR = Q3-Q1
var(plant_1)
sd(plant_1)
summary(plant_1)

#plant2
plant2 = data[data$plant==2,]
plant_2 = plant2[,-2]
summary(plant_2)
mean(plant_2)
median(plant_2)
Q1 = quantile(plant_2,0.25)
Q3 = quantile(plant_2,0.75)
max(plant_2)
min(plant_2)
range(plant_2)
IQR = Q3 -Q1
var(plant_2)
sd(plant_2)

#c
par(mfrow = c(1,2))
hist(plant_1)
boxplot(plant_1)

hist(plant_2)
boxplot(plant_2)

#Both appear to skew to the right and have a possible outlier at the right tail

#d

#The mean and median of plant 2 is higher 
#The spread for plant 2 is greater than plant 1


#3.2a

babies = read.table("/Users/pp16/Downloads/Lab03babiesl.data",sep = "", header = T)

baby=babies[babies$smoke ==0 | babies$smoke ==1 , ]
str(baby)

nonsmoker = babies[babies$smoke==0,1]
str(nonsmoker)

smoker = babies[babies$smoke==1,1]
str(smoker)

summary(nonsmoker)
summary(smoker)
#non-smoker babies are heavier as the mean is greater

par(mfrow=c(1,2))
hist(smoker)
hist(nonsmoker)
#very difficult to compare the mean as both graphs looks equally distributed
boxplot(smoker,nonsmoker)
#Boxplot will be a better plot for comparison as the median and mean of nonsmoker is higher than smoker babies

par(mfrow=c(1,2))
qqnorm(smoker,main="smoker")
qqline(smoker,col="blue")

qqnorm(nonsmoker,main="nonsmoker")
qqline(nonsmoker,col="red")
#The smoking group appears to be normally distributed, whilst the nonsmoking group appears to have longer tails on both sides than normal

#3.3
height = read.table("/Users/pp16/Downloads/Lab03skull_height.txt",sep="",header=F)
height
stem(height[,1])
#it appears that 122 and 126 is outlier

boxplot(height)
Q1 = quantile(height[,1],0.25)
Q3 = quantile(height[,1],0.75)
IQR = Q3-Q1
IQR
outlier1 = Q1 - 1.5*IQR
outlier1
#since 121 is smaller than 124.125, it is an outlier
summary(height[,1])
par(mfrow=c(1,1))
hist(height[,1])
xpt=seq(120,145,by=0.1)
n_den = dnorm(xpt,mean(height[,1]),sd(height[,1]))
ypt = n_den*length(height[,1])*5
lines(xpt,ypt,col="blue")

par(mfrow=c(1,1))
qqnorm(height[,1],main="heights")
qqline(height[,1],col="blue")
#The data is not normally distributed as there is a long left tail as compared to normal distribution

sqrt = sqrt(height[,1])
qqnorm(sqrt)
qqline(sqrt,col="red")
#no, it does not make the distribution closer to normal

#3.4
IQR = 41.5 - 14.0
IQR

#As the mean is greater than the median, it is right skewed

#The 40th percentile should be in the range (14.0, 27.0)

sum(exec.pay > 100)/ length(exec.pay) 

mean(exec.pay[ exec.pay <= quantile(exec.pay, 0.1)] )

#3.5
#The vector y has a longer left tail, while x has a slightly longer right tail

#So, if quantile(x, 0.6)=2, then we see from the plot that quantile(y, 0.6) is approximately 0.
