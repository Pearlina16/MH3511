#T.1
rep(seq(1,9, by=2),3)

#T.2

exp(-2)*2^1/1

exp(-4)* (4^6)/factorial(6)

#T.3
set.seed(3511)
datanorm <- rnorm(50, 2, 4)
ds <- 1.1^(datanorm)

hist(ds, breaks=7, main="Grouping")
xpt = seq (0.4,2.2,by=0.1)
n_den = dnorm(xpt,mean(ds),sd(ds))
ypt= n_den * length(ds) * 0.2
lines(xpt,ypt,col='blue')

#T.4
boxplot(ds)

#i)
outliers <- numeric(0)

# Find outliers using your approach
for (elem in ds) {
  if (abs(elem - mean(ds)) > 2 * sd(ds)) {
    outliers <- c(outliers, elem)
  }
}
outliers
# Print the number of outliers found
num_outliers <- length(outliers)
print(paste("Number of possible outliers (using classical method):", num_outliers))

#Part ii
# Calculate quartiles and IQR
Q1 <- quantile(ds, 0.25)
Q3 <- quantile(ds, 0.75)
IQR <- Q3 - Q1

# Initialize an empty vector to store outliers
outlier <- numeric(0)

# Find outliers using the boxplot method
for(elem in ds) {
  if(elem < Q1 - 1.5 * IQR | elem > Q3 + 1.5 * IQR) {
    outlier <- c(outlier, elem)
  }
}

# Print the number of outliers found
num_outlier <- length(outlier)
print(paste("Number of possible outliers (using boxplot method):", num_outlier))

#MidTerm 1

n <- 7
k=2
value = 2^3
while(n>k){
  value = value + (n^3)
  n = n-1
}
print(value)
n=2
C <- function(n) {
  value = 0
  while(n>=k) {
    value <- value + n^3
    n = n-1
  }
  return(value)
}
C(7)
C(2)
C(3)
C(4)
C(10)

C <- function(n) {
  value = 0
  for (k in 2:n) {
    value <- value + k^3
  }
  return(value)
}
C(10)

# Calculate and print C(7) and C(10)
C_7 <- C(7)
C_10 <- C(10)

print(paste("C(7) =", C_7))
print(paste("C(10) =", C_10))

data = read.table("/Users/pp16/Downloads/Quiz01_score_2024(1).txt",sep="", header=TRUE)
MHScore = data.frame(data)

MHFinal = if (MHScore$Test2 == -1,0.3* subset(MHScore,Test1) 
Final
MHFinal = cbind(MHScore, )

MHFinal <- MHScore
MHFinal$Final <- ifelse(MHScore$Test2 == -1, 0.4 * MHScore$Test1 + 0.6 * MHScore$Exam, 0.3 * MHScore$Test1 + 0.3 * MHScore$Test2 + 0.4 * MHScore$Exam)
MHFinal

MHFinal <- MHScore
MHFinal$Final <- if(MHScore$Test2 == -1) {0.4 * MHScore$Test1 + 0.6 * MHScore$Exam} else{0.3 * MHScore$Test1 + 0.3 * MHScore$Test2 + 0.4 * MHScore$Exam}
MHFinal

MHFinal$Final <- ifelse(MHScore$Test2 == -1, (0.4 * MHScore$Test1 + 0.6 * MHScore$Exam), 0.3 * MHScore$Test1 + 0.3 * MHScore$Test2 + 0.4 * MHScore$Exam)
MHFinal
MHFinal[order(MHFinal$Final,decreasing=T),]

MHFinal[MHFinal$Final>=75,1]

boxplot(MHFinal$Final)


data = read.table("/Users/pp16/Downloads/Quiz01_score_2024(1).txt",sep="", header=TRUE)
MHScore = data.frame(data)
MHScore

MHFinal <- MHScore
MHFinal$final <- ifelse(MHScore$Test2 == -1,(0.4*MHScore$Test1) + (0.6*MHScore*Exam), (0.3 * MHScore$Test1) + (0.3 * MHScore$Test2) + (0.4 * MHScore$Exam))
MHFinal



MHFinal <- MHScore
MHFinal$Final <- ifelse(MHScore$Test2 == -1, 0.4 * MHScore$Test1 + 0.6 * MHScore$Exam, 0.3 * MHScore$Test1 + 0.3 * MHScore$Test2 + 0.4 * MHScore$Exam)
MHFinal

0.3*69 + 0.3*42 + 0.4*78 
MHFinal[order(MHFinal$Final,decreasing=T),]

2^3 + 3^3 + 4^3 +5^3 +6^3 + 7^3
