#2.1
library (readr);
#data1 <- read_csv("/Users/pp16/Downloads/Lab02_Ex1.csv")
car1<-read.table("/Users/pp16/Downloads/Lab02_Ex1.csv", sep = ",", header = TRUE, fill=TRUE)
car1

#setwd("/Users/pp16/Downloads/")
#data <- read.csv("Lab02_Ex1.csv")

car1_ordered <- car1[order(car1$Weight, decreasing = TRUE), ]
car1_ordered

car1[car1$Weight>3000,]

car1[(car1$Weight>3000) & (car1$Horsepower>200),]

car1[(car1$Weight > 3000), "MPG"]

subset(car1, car1$Weight>3000 )
subset(car1, Weight>3000)

subset(car1, Weight>3000 & Horsepower>200)

#2.2
lab2 <- read.fwf("/Users/pp16/Downloads/Lab02fixed.txt", width=c(3,1,3,2,1))

colnames(lab2)=c("id","gender","height","weight","siblings");

str(lab2)
lab2[1:12,];

lab2test <- read.table("/Users/pp16/Downloads/Lab02test.txt", sep=" " , header=TRUE)
lab2test

lab2merged <- merge.data.frame(lab2, lab2test, by.x="id", by.y="id")
lab2merged

lab2merged[lab2merged$id==211, "weight"] <- 77
lab2merged

lab2height<-lab2merged[lab2merged$height>182,]

females <- lab2merged[lab2merged$gender == "F",]
tallest_females <- females[which.max(females$height),]
tallest_females

tallest = max(lab2merged$height[lab2merged$gender=='F'])
lab2merged[lab2merged$height == tallest & lab2merged$gender=='F',]

#2.3
number <-8

if (number < 100)
  { if (number < 10) {result <- "single digit"}
    else {result <- "double digits"}
      } else { result <- "more than 2 digits" }
print(paste(number, "is", result))

number <-999
if (number < 100)
{ if (number < 10) {result <- "single digit"}
  else {result <- "double digits"}
} else { if(number<1000) {result <- "triple digits" }
  else{result<-'more than 3 digits'}
}
print(paste(number, "is", result))

#2.4
speed <- 90
  while (speed > 60) {
    print(paste("Your speed is",speed," km/h! Slow down time!"))
    speed <- speed -7
  }

speed <- 150
while (speed > 60) {
  if(speed>120){print(paste('speed is', speed,'KM/h! slow down BIG time!'))
    soeed<- speed -12
  }
  else print(paste("Your speed is",speed," km/h! Slow down time!"))
  speed <- speed -7
}

#2.5
statquote <- "There are three types of lies -- lies, damn lies,
and statistics"
chars <- strsplit(statquote, split = "")[[1]]

ectr <- 0
for (char in chars) {if (char == "e") {ectr <- ectr +1}}
print(ectr)

sctr <- 0
  for (char in chars) {if (char == "s") {sctr <- sctr +1}
    if (char == "m") {break}
    }
  print(sctr)
  
#2.6
help(mean)
store1 = c(15,9,13,5,2,17,14)  
store2 = c(16,7,5,16,8,13,14)

average = mean(store1+store2)
average

mean(store1+store2,trim=0.20)

#2.7
m2 = function(x){
  y = (x-mean(x))^2
    return (mean(y))
}

m4 = function(x){
  y = (x-mean(x))^4
    return(mean(y))
}
