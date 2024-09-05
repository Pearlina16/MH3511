
#5.2
install.packages("HSAUR")
library("HSAUR")
str(roomwidth)

cfactor= ifelse(roomwidth$unit=="feet", 1, 3.28)
roomwidth$convert= roomwidth$width*cfactor
roomwidth
par(mfrow=c(1,1))
boxplot(roomwidth$width ~ roomwidth$unit)
# This plot gives the wrong suggestion that the estimates using “metres” would be lower than those using “feet”. The fact is that one metre is about 3.28 feet.

cfactor = ifelse(roomwidth$unit=="feet", 1, 3.28)
roomwidth$convert = roomwidth$width*cfactor
roomwidth

boxplot(roomwidth$convert ~roomwidth$unit)
mean(roomwidth$convert[roomwidth$unit=="metres"])
mean(roomwidth$convert[roomwidth$unit=="feet"])
sd(roomwidth$convert[roomwidth$unit=="metres"])
sd(roomwidth$convert[roomwidth$unit=="feet"])
#Since we are not given the true width of the lecture hall, we cannot compare the two ways of
#estimation based on mean. However, the SD suggests that estimates in feet are more accurate
#than estimates in metres.

t.test(roomwidth$convert[roomwidth$unit=="metres"], conf.level=0.95)
t.test(roomwidth$convert[roomwidth$unit=="feet"], conf.level=0.95)


#5.3
coffee = c(1.95,1.78,2.1,1.82,1.73,2.01,1.83,1.90,2.05,1.85,1.96,1.98,1.79)
qqnorm(coffee)
qqline(coffee, col="red")
shapiro.test(coffee)
n <- length(coffee)
xbar <- mean(coffee)
s <- sd(coffee)
alpha <- 0.10
z<- qnorm(1-alpha/2)  	
zCI90p<- c(xbar-z*s/sqrt(n), xbar+z*s/sqrt(n))
print(paste("z90% CI= [", zCI90p[1], zCI90p[2], "]"))

t = qt(1-alpha/2, df=n-1)
tCI90p<- c(xbar-t*s/sqrt(n), xbar+t*s/sqrt(n))
print(paste("n=",n, "; xbar=", xbar, "; s=", s ))
print(paste("alpha=",alpha, "; z=", z, "; t=", t ))
print(paste("t90% CI= [", tCI90p[1], tCI90p[2], "]"))
          
t.test(coffee, conf.level = 0.9)

z1<- qnorm(1-alpha)
z1CI90p <- c(xbar-z1*s/sqrt(n), xbar+z1*s/sqrt(n))
print(paste("z190% CI= [ -infinity, ", z1CI90p[2], "]"))
t1 = qt(1-alpha/2, df=n-1)
t1CI90p<- c(xbar-t1*s/sqrt(n), xbar+t1*s/sqrt(n))
print(paste("t190% CI= [ -infinity, ", t1CI90p[2], "]"))
t.test(coffee, conf.level = 0.9,alternative = "less")


#6.1
prop.test(17, 23, conf.level=0.95)
#The width is  0.3760442

prop.test(17, 23, conf.level=0.8)
#The width is 0.269207

prop.test(170, 230, conf.level=0.95)

prop.test(11, 23, conf.level=0.95)

prop.test(1, 23, conf.level=0.95)


#6.2
wind = subset(airquality, (Month==8|Month==9))
wind89 = wind$Wind
qqnorm(wind89)
qqline(wind89)

#H0: u = 10
#H1: u not = 10

n <- length(wind89)
xbar <- mean(wind89)
sd <- sd(wind89)
mu0 = 10
z = (xbar - mu0)/ sd * sqrt(n)
z_pvalue = 2*pnorm(z) 
print(paste("p-value for normal approximation = ", z_pvalue))

t=(xbar-mu0)/sd * sqrt(n)
t_pvalue= 2*pt(z,df=n-1)
print(paste("p-value for t approximation = ", t_pvalue))

t.test(wind89,mu=10)


#6.3
n= 50000
p0= 0.113
phat= 0.117
pvalue = 1-pnorm((phat-p0)/sqrt(p0*(1-p0)/n))
pvalue

prop.test(x=5850, n=50000, p=0.113, alt="greater")


#6.4
n = c(10,11,20,30)
type1_error = 1-pnorm(0.5*sqrt(n))
type1_error 

mu1 = c(1,1.1,1.2,1.3)
n=11
power=1-pnorm((0.5-mu1)*sqrt(n))
power


#7.1
set.seed(3579)
b=10000
n=200
mu=4
alpha= 0.10

CI90 <- function() { ds = rexp(n, rate=1/mu)
  xbar = mean(ds)
  sd = sd(ds)
  lbd = xbar - qnorm(1-alpha/2)*sd/sqrt(n)
  ubd = xbar + qnorm(1-alpha/2)*sd/sqrt(n)
  return(c(lbd,ubd))
}

confint = NULL
for (i in c(1:b)) {confint = rbind(confint, CI90())}
colnames(confint) = c("Lower Bound", "Upper Bound")

coverage = sum(confint[,1]<mu & confint[,2]>mu)/b
coverage

sum(confint[,1]>mu)
sum(confint[,2]<mu)



#7.2
smoke = read.table("/Users/pp16/Downloads/smoker (1).txt", header=TRUE)
colnames(smoke) = c("Smoke", "SES")
summary(smoke)
smoketable = table(smoke$Smoke, smoke$SES)
smoketable
margin.table(smoketable)
margin.table(smoketable,1)
margin.table(smoketable, 2)

barplot(smoketable, legend=T, beside=T, main='Smoking Status by SES')
barplot(prop.table(smoketable,2), legend=F, beside=T, main='Smoking Status by SES')
barplot(t(prop.table(smoketable,1)), legend=T, beside=T, main='Put an appropriate title here')

smoketable
colsum = matrix(colSums(smoketable), ncol=3)
rowsum = matrix(rowSums(smoketable), ncol=1)
exp_freq= rowsum %*% colsum / sum(colsum)
exp_freq

chisq= sum((smoketable-exp_freq)^2/ exp_freq)
chisq

v = (nrow(smoketable)-1)*(ncol(smoketable)-1)
v

pvalue = 1-pchisq(chisq,df=v)
pvalue

summary(smoketable)


#7.3
test = matrix(c(16,24,654,306), byrow = TRUE, ncol=2)
rownames(test) = c("No","Yes")
colnames(test) = c("Day", "Evening")
test

colsum = matrix(colSums(test), ncol=2)
rowsum = matrix(rowSums(test), ncol=1)
exp_freq= rowsum %*% colsum / sum(colsum)
exp_freq

chisq= sum((test-exp_freq)^2/ exp_freq)
chisq

v = (nrow(test)-1)*(ncol(test)-1)
pvalue = 1-pchisq(chisq, df=v)
pvalue

summary(test)

prop.table(test,2)
#by column is more informative as it shows the percentages of non conformances of the 2 shifts. this can provide info to determine whether percentages are different.


#7.4
data = matrix(c(510,17,5,90), byrow=T, ncol=2)
str(data)
colnames(data) = c("Survived", "Died")
rownames(data) = c("Survived", "Died")
data
mcnemar.test(data)


#8.1

#H0: uA = uB
#H1: uA < uB

a = c(102,86,98,109,92)
n1=length(a)
a_xbar = mean(a)
a_var=var(a)
b = c(81,165,97,134,92,87,114)
n2=length(b)
b_xbar = mean(b)
b_var = var(b)
n2;b_xbar;b_var;sqrt(b_var)
n1;a_xbar;a_var;sqrt(a_var)

var.test(a,b)

v=((a_var/n1)+(b_var/n2))^2/ ((a_var/n1)^2/(n1-1)+(b_var/n2)^2/(n2-1))
v
t_stat = (a_xbar - b_xbar)/sqrt(a_var/n1+b_var/n2)
pvalue = pt(t_stat,df=v)
print(paste("t-statistics = ", t_stat,"; pvalue = ", pvalue, "; DF = ",v))

t.test(a,b,var.equal=FALSE)

#pvalue >0.05 hence we do not reject H0 as there are not enough evidence to prove that the average running time of B exceed A


#8.2
faculty = read.table("/Users/pp16/Downloads/Lab08faculty (1).txt", header=TRUE)
str(faculty)
head(faculty)

aggregate(faculty$salary, list(faculty$male), FUN=length)

attach(faculty)
boxplot(salary~male, main="All Professors")

#H0: male salary = female salary
#H1: male salary > female salary

t.test(salary[male=="Men"], salary[male=="Women"], var.equal = FALSE, alternative =  "greater")

boxplot(salary~male, subset = rank=="Assistant", main= "All Assistant")
var.test(salary[male=="Men" & rank == "Assistant"] , salary[male=="Women" & rank =="Assistant"])

t.test(salary[male=="Men" & rank == "Assistant"], salary[male=="Women" & rank=="Assistant"], alternative = "greater", var.equal = FALSE)

#8.3
new = c(2250,2410,2260,2200,2360,2320,2240,2300,2090)
old = c(1920,2020,2060,1960,1960,2140,1980,1940,1790)

#H0: u1 - u0 <= 250
#H1: u1 - u0 >250

t.test(new-old, mu=250, alternative = "greater")
#p > 0.05 we do not reject H0 


#8.4
a = c(4, 5, 4, 3, 2, 4, 3, 4, 4 )
b = c( 6, 8, 4, 5, 4, 6, 5, 8, 6 )
c = c(6, 7, 6, 6, 7, 5, 6, 5, 5 )
pain = c(a,b,c)
drug = c(rep("a", 9), rep("b",9), rep("c",9))
rbind(pain,drug)

boxplot(pain~ drug, ylab="Pain Score")

aggregate(pain, list(drug), FUN=summary)
aggregate(pain, list(drug), FUN = length)
aggregate(pain, list(drug), FUN=var)

#null hypothesis 
#H0: u1 = u2 = u3
#H1: not all u are equal 

summary(aov(pain~drug))
#pvalue <0.05 reject H0
pairwise.t.test(pain, drug, p.adjust.method = "none")
#From the pairwise t-test, we see that drug A is significantly different from the other two drugs, and drug B and drug C are quite similar in pain treatment. 

#quiz

score = c(69,70,71,71,77,78,82,83,85,88,93,96)
n = length(score)
xbar = mean(score)
sd = 9  
alpha = 0.05
z = qnorm(1-alpha/2)
zCI95p = c(xbar-z*s/sqrt(n), xbar+z*s/sqrt(n))
print(paste("z95% CI= [", zCI95p[1], zCI95p[2], "]"))

t.test(score, conf.level = 0.95)


morethan80 <- sum(score > 80) / n
morethan80 
alpha = 0.1
z_value <- qnorm(1-alpha / 2)

standard_error <- sqrt((proportion_above_80 * (1 - proportion_above_80)) / n)
lower_bound <- proportion_above_80 - z_value * standard_error
upper_bound <- proportion_above_80 + z_value * standard_error
cat("90% Confidence Interval for the proportion above 80 marks: [", lower_bound, ", ", upper_bound, "]\n")

score = c(69,70,71,71,77,78,82,83,85,88,93,96)
n = length(score)
xbar = mean(score)
s = 9  # Known standard deviation
alpha = 0.05
z = qnorm(1-alpha/2)
zCI95p = c(xbar-z*s/sqrt(n), xbar+z*s/sqrt(n))
print(paste("z95% CI= [", zCI95p[1], zCI95p[2], "]"))


n <- length(score)
xbar <- mean(score)
s <- sd(score)
alpha <- 0.10
z1<- qnorm(1-alpha) 
z1CI90p <- c(xbar-z1*s/sqrt(n), xbar+z1*s/sqrt(n))
print(paste("z190% CI= [ 80, ", z1CI90p[2], "]"))

t.test(score, conf.level = 0.95)

higher = c(82, 83, 85, 88, 93, 96.)
n <- length(higher)
xbar <- mean(higher)
s <- sd(higher)
alpha <- 0.10
z1CI90p <- c(xbar-z1*s/sqrt(n), xbar+z1*s/sqrt(n))
print(paste("z190% CI= [", z1CI90p[1], z1CI90p[2], "]"))


exercise <- matrix(c(65, 60, 75), nrow = 1, byrow =T)
rownames(exercise)= c("students")
colnames(exercise)= c("Regular","Sporadic","Regular")
exercise
colsum = matrix(colSums(exercise), ncol=3)
colsum
rowsum = matrix(rowSums(exercise),nrow=1)
rowsum
exp = (rowsum %*% colsum )/sum(colsum)
exp
chisq= sum((exercise-exp)^2/exp)
chisq

exercise <- matrix(c(65, 60, 75), nrow = 1, byrow =T)
expected <- c(400 * 0.60, 400 * 0.25, 400 * 0.15)

chi_sq_test <- chisq.test(observed, p = c(0.60, 0.25, 0.15))
chi_sq_test




#q3

fertilizer <- read.table("/Users/pp16/Downloads/Quiz02_fertilizer_2024.txt", header = TRUE)
boxplot(height ~ fertilizer, data = fertilizer, xlab = "Fertilizer", ylab = "Height")

fertilizer

shapiro_test <- shapiro.test(fertilizer$height)
shapiro_test

var.test()


leveneTest(height ~ fertilizer, data = fertilizer)


# Two-sample t-test assuming equal variances
t_test <- t.test(Height ~ Fertilizer, data = data, var.equal = TRUE)
t_test


# Load the dataset
data <- read.table("Quiz02_fertilizer_2024.txt", header = TRUE)

var.test(height ~ fertilizer, data = fertilizer)

t.test(height ~ fertilizer, data = fertilizer, var.equal = FALSE)
t_test


# Data
scores <- c(69, 70, 71, 73, 77, 78, 82, 83, 85, 88, 93, 96)
n <- length(scores)
xbar <- mean(scores)
std_dev <- 9  # Known standard deviation
alpha <- 0.05

# Confidence interval
z <- qnorm(1 - alpha / 2)
CI <- c(xbar - z * (std_dev / sqrt(n)), xbar + z * (std_dev / sqrt(n)))

# Print confidence interval
cat("95% Confidence Interval for the mean exam score (known std dev): [", CI[1], ", ", CI[2], "]\n")


# Proportion of students achieving more than 80 marks
prop_above_80 <- sum(scores > 80) / n

# Confidence interval
z <- qnorm(0.95 + (1 - 0.90) / 2)
SE <- sqrt((prop_above_80 * (1 - prop_above_80)) / n)
CI_prop_above_80 <- c(prop_above_80 - z * SE, prop_above_80 + z * SE)

# Print confidence interval
cat("90% Confidence Interval for the proportion of students above 80 marks: [", CI_prop_above_80[1], ", ", CI_prop_above_80[2], "]\n")

