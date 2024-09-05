#Lab1

205+106
2*6+9/3
2*(6+9)/3
3^4-4^3
28%%6

red_apples<-4
green_apples<- 5
basket_apples<- green_apples + red_apples
basket_apples
18*basket_apples

my_course = "MH1234"
my_score = 76
my_grade = "A-"
my_core=T

class(my_course)
class(my_score)
class(my_grade)
class(my_core)

seq(10)
seq(0,10,length=10)
seq(0,10,by=1)
rep(2,3)
rep(-1,4)
rep(1:3.2)
rep("A",4)
c(rep("A",3),rep("B",3))
rep(c("A","B"),3)

vector1=seq(3,9,by=2)
vector2 = c(rep(2,2),rep(3,3),rep(4,4))
vector3= c(rep(T,2),rep(F,3))
vector4 = rep(c("High","Medium","Low"),2)


poker_winnings = c(140,-50,20,-120,240)
roulette_winnings = c(-24,-50,100,-350,10)
days = c("Monday","Tuesday","Wednesday","Thursday","Friday")
names(poker_winnings ) = days
names(roulette_winnings)=days
poker_winnings[poker_winnings>0]
daily_winnings = poker_winnings+roulette_winnings
daily_winnings[daily_winnings>0]
poker_sum = sum(poker_winnings)
roulette_sum= sum(roulette_winnings)
poker_sum>roulette_sum
overall_winning = poker_sum + roulette_sum


matrix_4x2=matrix(1:8,byrow=F,nrow=4)
matrix_2x4 = matrix(1:8,byrow=T,nrow=2)
matrix_4x2 %*% matrix_2x4
matrix_2x4 %*% matrix_4x2 
matrix_2x2 = matrix_4x2[1:2,]
solve(matrix_2x2)
diag(3)
diag(c(2,3,5))


apple <- c(350, 150) 
orange <- c(400, 230) 
watermelon <- c(180, 90)
fruit_matrix = matrix(c(apple,orange,watermelon), byrow=T, nrow=3)
rownames(fruit_matrix) <- c("Apple", "Orange", "Watermelon")
colnames(fruit_matrix) <-c("Weekday","Weekend")
rowSums(fruit_matrix)
colSums(fruit_matrix)
fruit_matrix
cbind(fruit_matrix,rowSums(fruit_matrix))
price = c(1,1.5,4)
sum(rowSums(fruit_matrix)*price)



#Lab2
car1= read.table("/Users/pp16/Downloads/Lab02_Ex1.csv",sep=",",header=TRUE)
car1[order(car1$Weight,decreasing = T),]
car1[car1$Weight>3000,]
car1[car1$Weight>3000 & car1$Horsepower>200,]
car1[(car1$Weight>3000),"MPG"]


lab2 = read.fwf("/Users/pp16/Downloads/Lab02fixed.txt",width=c(3,1,3,2,1))
names=c("id","gender","height","weight","siblings")
colnames(lab2) = names         
str(lab2)
lab2[1:12,]
lab2test = read.table("/Users/pp16/Downloads/Lab02test.txt",sep="",header = TRUE)
lab2merged = merge.data.frame(lab2,lab2test,by.x="id",by.y ="id")
lab2merged[1:12,]
lab2merged[lab2merged$id==211,"weight"]=77
lab2merged
lab2merged[lab2merged$height>182,]
females = lab2merged[lab2merged$gender=="F",]
tallest_female = females[females$height==max(females$height),]
tallest_female


number <-999
if (number < 100)
  { if (number < 10) {result <- "single digit"}
    else {result <- "double digits"}
  } else { if(number<1000){result <- "triple digits"}
    else { result <- "more than 3 digits" }}
print(paste(number, "is", result))



speed <- 140
  while (speed > 60) {
    if(speed>120){
      print(paste("Your speed is",speed,"km/h! Slow down BIG time"))
      speed <- speed -12
    }
    else{print(paste("Your speed is",speed," km/h! Slow down time!"))
    speed <- speed -7
    }
  }



statquote <- "There are three types of lies -- lies, damn lies, and statistics"
chars <- strsplit(statquote, split = "")[[1]]

ectr <- 0
for (char in chars) {if (char == "e") {ectr <- ectr +1}}
print(ectr)

sctr <- 0
  for (char in chars) {if (char == "s") {sctr <- sctr +1}
    if (char == "m") {break}
     }
print(sctr)


m2 <- function(x){y=(x-mean(x))^2
return(mean(y)) }

x=c(1:10)
m4 = function(x){y=(x-mean(x))^4
return(mean(y))}



#Lab3
wip=read.table("/Users/pp16/Downloads/Lab03wip.txt",sep = "",header=TRUE)
plant1 = wip[wip$plant==1,1]
summary(plant1)
IQR(as.numeric(plant1))
plant2 = wip[wip$plant==2,1]
summary(plant2)
IQR(as.numeric(plant2))
par(mfrow=c(1,2))
hist(plant1)
hist(plant2)
#Both are right-skewed
boxplot(plant1)
boxplot(plant2)
#Both have a possible outlier at the right tail
#Plant2 has higher mean and median as compared to plant 1
#Plant 2 has a greater spread than plant 1


babies=read.table("/Users/pp16/Downloads/Lab03babiesl.data",sep = "",header=TRUE)
new = babies[babies$smoke==0|babies$smoke==1,]
str(new)
smoker=babies[babies$smoke==1,]
dim(smoker)
nonsmoker=babies[babies$smoke==0,]
dim(nonsmoker)
summary(smoker$bwt)
summary(nonsmoker$bwt)
#Babies with nonsmoker appeares to be heavier
hist(smoker$bwt)
hist(nonsmoker$bwt)
boxplot(smoker$bwt)
boxplot(nonsmoker$bwt)
#boxplot is better, it can be seen clearly that there is a shift in median, 1st quartile and 3rd quartile
qqnorm(nonsmoker$bwt,main="nonsmoker")
qqline(nonsmoker$bwt,col="blue")
qqnorm(smoker$bwt,main="smoker")
qqline(smoker$bwt,col="red")
#nonsmoker data is not normal as there is long right and left tail
#smoker data is approximately normal

heights=read.table("/Users/pp16/Downloads/Lab03skull_height.txt",sep="",header=FALSE)
heights
stem=stem(heights[,1])
#121 is an outlier
boxplot(heights)
summary(heights)
Q1 = quantile(height[,1],0.25)
Q3 = quantile(height[,1],0.75)
IQR = Q3-Q1
IQR
121<Q1-1.5*IQR
h = heights[,1]
par(mfrow=c(1,1))
hist(h)
xpt=seq(120,145,by=1)
n_den = dnorm(xpt,mean(h),sd(h))
ypt=n_den*length(h)*5
lines(xpt,ypt,col="blue")
qqnorm(h)
qqline(h,col="blue")
#No, as there is a long left tail



IQR = 41.5-14.0
IQR
#right skewed as mean is greater median
#range(14.0,27.0)
sum(exec.pay>100)/length(exec.pay)
mean(exec.pay[ exec.pay <= quantile(exec.pay, 0.1)] )


#x have longer right tail and y have longer left tail
#0


m = matrix(1:12,byrow=T,ncol=3)
sum(m[c(2),c(1,2)])


