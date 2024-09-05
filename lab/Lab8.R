#8.1
A= c(102,86,98,109,92)
B = c(81,165,97,134,92,87,114)

#null hypothesis: avgB-avgA =0
#alt hypothesis: avgB-abgA < 0

n1=length(A)
A_xbar = mean(A)
A_var=var(A)

n2=length(B)
B_xbar = mean(B)
B_var = var(B)
n2;B_xbar;B_var;sqrt(B_var)
n1;A_xbar;A_var;sqrt(A_var)
var.test(avgA,avgB)
v=((A_var/n1)+(B_var/n2))^2/ ((A_var/n1)^2/(n1-1)+(B_var/n2)^2/(n2-1))
v
t_stat = (A_xbar - B_xbar)/sqrt(A_var/n1+B_var/n2)
t_stat
pvalue = pt(t_stat,df=v)
pvalue
#greater than 0.05 hence we do not reject H0. 


t.test(A,B, var.equal = F, mu=0, alternative = "less")

#pvalue > 0.05 hence we do not reject H0. there is not enough evidence to show that average running time of films produced by Company B exceeds the average running time of films provided by Company A.


#8.2
faculty = read.table("/Users/pp16/Downloads/Lab08faculty.txt", header=T )
str(faculty)
head(faculty)

aggregate(faculty$salary, list(faculty$male), FUN=length)

Msalary = subset(faculty$salary, faculty$male == "Men")
Fsalary = subset(faculty$salary, faculty$male == "Women")
boxplot(Msalary, Fsalary)
#Yes as the median of male is higher than that of female 

#null: male > female
#alt: male < female
var.test(Msalary, Fsalary)
t.test(Msalary,Fsalary, var.equal = FALSE)

#The t-test shows a p-value of 2.2e-16. We reject  and conclude that the mean salary of male professors is higher than that of female professors. 


#8.3
#null: new result in an increase of <=250 crop yield
#alt: new result in an increase of >250 crop yield

new = c(2250,2410,2260,2200,2360,2320,2240,2300,2090)
old = c(1920,2020,2060,1960,1960,2140,1980,1940,1790)
diff = new-old

n = length(diff)
xbar = mean(diff)
s = sd(diff)
t.test(diff, conf.level = 0.90, mu=250, alternative = "greater")
#since p value is greater than 0.05, we do not reject null hypothesis 


#8.4
a = c(4, 5, 4, 3, 2, 4, 3, 4, 4 )
b = c( 6, 8, 4, 5, 4, 6, 5, 8, 6 )
c = c(6, 7, 6, 6, 7, 5, 6, 5, 5 )
pain = c(a,b,c)
drug = c(rep("a", 9), rep("b",9), rep("c",9))
rbind(pain,drug)

boxplot(pain~ drug, ylab="Pain Score")

aggregate(pain, list(drug), FUN=summary)


#null hypothesis 
#H0: u1 = u2 = u3
#H1: not all u are equal 

summary(aov(pain~drug))
#pvalue <0.05 reject H0
pairwise.t.test(pain, drug, p.adjust.method = "none")
#From the pairwise t-test, we see that drug A is significantly different from the other two drugs, and drug B and drug C are quite similar in pain treatment. 