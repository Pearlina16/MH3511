#4.1
X=matrix(rep(0, 8*4), nrow=8, ncol=4)
for (i in c(1:8)) {for (j in c(1:4)) {X[i,j] = ((i==2*j)|(i==2*j-1))}}
A = diag(c(1:4))
matrix_M = X %*% A %*% t(X)
matrix_M

#4.2
painlevel <- function(score)
{if (score < 5) {
  if (score < 2) {result <- "no pain"}
  else {result <- "minor pain"} }
  else if (score < 8) {
    result <- "moderate pain"}
  else {
    result <- "severe pain"}
  return(result)}

painlevel(2)
painlevel(7)
painlevel(9)

painscore <- c(0:10, 0:8)
painvector <- NULL
for( i in c(1:length(painscore)))
{
  painvector [i] = painlevel(painscore[i])
}
  pain_level = cbind(painscore,painvector)
  print(pain_level) 

  
#4.4
  Student_ID=c(101, 102, 103, 104, 105, 106, 107, 108,
               109, 110)
  Test1=c(78, 87, 67, 53, 89, 57, 83, 92, 74, 82)
  Test2=c(68, 53, 47, 56, 75, 69, 48, 84, 83, 68)
  Test3=c(73, 67, 75, 89, 46, 75, 67, 88, 80, 75)
  Test4=c(93, 73, 67, 73, 52, 86, 86, 87, 53, 65)
  Test5=c(67, 70, 91, 56, 75, 56, 81, 81, 54, 77)
  CA_result=data.frame(Student_ID, Test1, Test2, Test3,
                         Test4, Test5)
  CA_result
  
  best3_mean <- function(score)
  {
    new = score[order(score, decreasing = TRUE)]
    return (mean(new[1:3]))
  }
  best_average = apply(CA_result[,2:6], 1, best3_mean)
  best_average  

  
#4.5
  f <- function(i){ if(i==floor(i) & i>0)
  {value=1; while(i>0){value=value*i; i=i-2};
  return(value)}
    else{"undefined"}
  } 