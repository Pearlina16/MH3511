#1.1
205+106
2*6+9/3
2*(6+9)/3
3^4 - 4^3
28 %% 6

#1.2
red_apples <- 4
green_apples <- 5

basket_apples <- red_apples +green_apples
basket_apples

18*basket_apples

#1.3
my_course <- "MH1234"
my_score <- 76
my_grade <- "A-"
my_core <- T
class(my_course)
class(my_score)
class(my_grade)
class(my_core)

#1.4
seq(10) 
seq(0, 10, length=10)
seq(0, 10, by = 1)
rep(2, 3)
rep(-1, 4)
rep(1:3, 2)
rep("A", 4)
c(rep("A",3), rep("B",3))
rep(c("A", "B"), 3) 

vector1 <- seq(3,9,by=2)
vector1
vector2 <- c(rep(2,2), rep(3,3), rep(4,4))
vector2
vector3 <- c(rep(T,2), rep(F,2))
vector3
vector4 <- rep(c("High","Medium","Low"),2)
vector4

#?
vector <- c('!@#',123,T) #cannot have diff types of values in one vector

#1.5
poker_winnings <- c(140,-50,20,-120,240)
roulette_winnings <- c(-24,-50,100,-350,10)
days<- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")
names(poker_winnings)<-days.    #number of elements in both poker and days must be the same
names(roulette_winnings) <-days

poker_winnings[poker_winnings>0]
poker_winnings>0

daily_winnings <- poker_winnings +roulette_winnings

daily_winnings[daily_winnings>0]
days[daily_winnings>0]

sum(poker_winnings)

sum(roulette_winnings)

sum(poker_winnings) > sum(roulette_winnings)

sum(poker_winnings) +sum(roulette_winnings)

#1.6
matrix_4x2 <- matrix(c(1:8), byrow=F, nrow =4)
matrix_4x2

matrix_2x4 <- matrix(c(1:4, 11:14), byrow=T, nrow=2)
matrix_2x4

matrix_4x2 %*% matrix_2x4
matrix_2x4 %*% matrix_4x2

matrix_2x2 <- matrix_4x2[1:2,]
matrix_2x2

solve(matrix_2x2)

diag(3)
diag(c(2,3,5))

#1.7
apple <- c(350, 150)
orange <- c(400, 230)
watermelon <- c(180, 90)

fruit_matrix <- matrix(c(apple,orange,watermelon),nrow=3,byrow=T)
fruit_matrix

rownames(fruit_matrix) <- c("Apples","Orange","Watermelon")
colnames(fruit_matrix) <- c("Weekday","Weekend")
fruit_matrix

rowSums(fruit_matrix)
colSums(fruit_matrix)

cbind(fruit_matrix, rowSums(fruit_matrix)) 

price <- matrix(c(1,1.5,4),nrow=3)

fruit_rowsum <- matrix(rowSums(fruit_matrix),nrow =1)

fruit_rowsum %*% price
