#Confidence Interval
x = c(1.95,1.80,2.10,1.82,1.75,2.01,1.83,1.90)
n = length(x)
xbar = mean(x)
s = sd(x)

alpha = 0.1
z =qnorm(1-alpha/2)
zCI90p<- c(xbar-z*s/sqrt(n), xbar+z*s/sqrt(n))

z1 = qnorm(1-alpha)
z1CI90p<- c(xbar-z1*s/sqrt(n), xbar+z1*s/sqrt(n))
z1CI90p = c("-infinty", xbar+z1*s/sqrt(n))
print(paste("1-sided z90% CI= [-infinity, ",  z1CI90p[2], "]"))

#Goodness-of-Fit-Test X~B(5,0.52)
x=0:5
b_pdf = dbinom(x,5,0.52)
b_pdf
exp_freq = 500*b_pdf
exp_freq
f <- c(20, 75, 145, 140, 85, 35)
rbind(exp_freq,f)
barplot (rbind(exp_freq,f), names.arg=x, beside=TRUE, legend.text=TRUE)
cbind(x,f,round(exp_freq),exp_freq)


chisq= sum((f-exp_freq)^2/exp_freq)
chisq
pvalue = 1-pchisq(chisq,4)
pvalue
chisq.test(f,p=b_pdf)

#FINDING P-VALUE:

#Normal Approximations
n= 25000
p0 = 0.1
phat= 2700/25000
pvalue = 1-pnorm((phat-p0)/sqrt(p0*(1-p0)/n))
pvalue

#chi-square
chisq = sum((exp_freq-f)^2/exp_freq)
chisq
pvalue = 1-pchisq(chisq, df=4)
pvalue

table = matrix(c(49,50,69,24,36,38,19,22,28), nrow=3, byrow=TRUE)
colnames(table) = c("forth", "fifth", "sixth")
rownames(table) = c("Grades", "Popular", "Sports")
table
colsum = matrix(colSums(table),ncol=3)
rowsum = matrix(rowSums(table),ncol=1)
exp = (rowsum %*% colsum / sum(colsum))
exp

chisq = sum((table-exp)^2/exp)
chisq

v = (nrow(table)-1)*(ncol(table)-1)
v

pvalue = 1-pchisq(chisq,df=v)
pvalue

#Example
students = matrix(c(57,87,24,50,42,6,42,22,5),nrow=3,byrow=TRUE)
rownames(students) = c("Grades","Popular","Sports")
colnames(students) = c("Rural","Suburban","Urban")
students

colsum = matrix(colSums(students), ncol=3)
rowsum = matrix(rowSums(students),ncol=1)

chisq.test(students)


#2-way contingency table
goal = matrix(c(49,50,69,24,36,38,19,22,28), nrow=3, ncol=3, byrow=T)
rownames(goal)= c("Grades","Popular","Sports")
colnames(goal)= c("4th","5th","6th")
goal
colsum = matrix(colSums(goal), ncol=3)
rowsum = matrix(rowSums(goal),nrow=3)
exp_goal = (rowsum %*% colsum )/sum(colsum)
chisq= sum((goal-exp_goal)^2/exp_goal)
chisq
v = (3-1)*(3-1)
v
pvalue = pvalue = 1-pchisq(chisq,4)
pvalue #greater than 0.05, do not reject H0


goals= matrix(c(57,87,24,50,42,6,42,22,5), nrow=3, ncol=3, byrow=T)
colnames(goals)=c("Rural","Suburban","Urban")
rownames(goals)=c("Grades","Popular","Sports")
colsums= matrix(colSums(goals), ncol=3)
rowsums = matrix(rowSums(goals),nrow=3)
exp_goals= (rowsums %*% colsums)/sum(rowsums)
chisq = sum((goals-exp_goals)^2/exp_goals)
chisq
pvalue = 1-pchisq(chisq, 4)
pvalue
chisq.test(goals)


#Paired 2-way contingency table
patient = read.table("/Users/pp16/Downloads/ex4_4.txt")
str(patient)
names(patient)= c("ID","before","after")
table(patient$before)
table(patient$after)
pattable=table(patient$before,patient$after)
mcnemar.test(pattable)


#Two samples normally distributed, first use F test
control = c(80,93,83,89,98)
n1=length(control)
c_xbar = mean(control)
c_var=var(control)
treatment = c(100,103,104,99,102)
n2=length(treatment)
t_xbar = mean(treatment)
t_var = var(treatment)
n1;c_xbar;c_var;sqrt(c_var)
n2;t_xbar;t_var;sqrt(t_var)

var.test(control, treatment)

v=((c_var/n1)+(t_var/n2))^2/ ((c_var/n1)^2/(n1-1)+(t_var/n2)^2/(n2-1))
t_stat = (c_xbar - t_xbar)/sqrt(c_var/n1+t_var/n2)
pvalue = 2*pt(t_stat,df=v)
print(paste("t-statistics = ", t_stat,"; pvalue = ", pvalue, "; DF = ",v))

t.test(control,treatment,var.equal=FALSE)


v= n1+n2-2
pool_var = ((n1-1)*c_var + (n2-1)*t_var)/(n1+n2-2)
t_stat = (c_xbar-t_xbar)/sqrt(pool_var/n1+pool_var/n2)
pvalue = 2*pt(t_stat,df=v)
print(paste("t-statistics = ", t_stat ,"; p-value= ", pvalue, "; DF=" , v))

t.test(control,treatment, var.equal = TRUE)


#for large samples 
runtime = read.table("/Users/pp16/Downloads/LT5_runtime.txt", header = TRUE)
str(runtime)
table(runtime$Athlete)

boxplot(runtime$MileMinDur ~ runtime$Athlete, name=c("Non-Athlete (0)", "Athlete (1)", main = "Time to Rune a Mile"))

var.test(runtime$MileMinDur ~ runtime$Athlete)

t.test(runtime$MileMinDur ~ runtime$Athlete, var.equal=FALSE)

NA_var = var(runtime[runtime$Athlete==0, "MileMinDur"])
A_var = var(runtime[runtime$Athlete==1, "MileMinDur"])
v = ((NA_var/221)+(A_var/162)^2/ ((NA_var/221)^2/(220)+(A_var/162)^2/161))
v     


#more than two sample
#Test if there is diff between 3 types of diet for losing weight
diet = read.csv("/Users/pp16/Downloads/stcp-Rdataset-Diet.csv",header=TRUE)
diet$wloss <- diet$pre.weight - diet$weight6weeks
boxplot(diet$wloss ~ diet$Diet, xlab = "Diet Type", ylab = "Weight Loss", main = "Weight Loss per Diet Type")
abline(h=mean(diet$wloss),col="red")
gpn = NULL
gpxbar = NULL
gpvar = NULL
for(i in c(1:3)) {gpn[i]=length(diet$wloss[diet$Diet==i])}
for(i in c(1:3)) {gpxbar[i]= mean(diet$wloss[diet$Diet ==i])}
for (i in c(1:3)) {gpvar[i] = var(diet$wloss[diet$Diet==i])}
gpn
gpxbar 
gpvar 
xbar = mean(diet$wloss)
w_var=sum((gpn-1)*gpvar)/(sum(gpn)-length(gpn))
b_var=sum(gpn*(gpxbar-xbar)^2)/(length(gpn)-1)
fstat = b_var/w_var
pvalue = 1-pf(fstat,df1=length(gpn)-1, df2= sum(gpn)-length(gpn))
b_var;w_var;fstat;pvalue

summary(aov(diet$wloss~factor(diet$Diet)))

pairwise.t.test(diet$wloss, diet$Diet, p.adjust.method = "none")
