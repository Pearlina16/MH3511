#6.1

prop.test(17,23,conf.level = 0.95)
0.889-0.513

prop.test(17,23,conf.level = 0.80)
0.854-0.585

prop.test(170,230,0.95)
0.793-0.676

prop.test(11,23,conf.level = 0.95)
0.689-0.274

prop.test(1,23,conf.level=0.95)
0.239-0.002

#6.2
str(airquality)
as=subset(airquality,(Month==8|Month==9))
wind= as[,"Wind"]
qqnorm(wind)
qqline(wind)

#null: mean wind speed is 10m/s during August and September
#alternative: mean wind speed is not 10m/s during August and September

n = length(wind)
xbar=mean(wind)
s = sd(wind)

mu0 = 10
z=(xbar-mu0)/(s/sqrt(n))
z_pvalue= 2*pnorm(z)
print(paste("p-value for normal approc = ", z_pvalue))
qnorm(z_pvalue/2)

t=(xbar-mu0)/sqrt(n)*s
t_pvalue= 2*pt(z,df=n-1)
print(paste("p-value for t approc = ", t_pvalue))

t.test(wind,mu=10)


#6.3
#null= 10% chance 
#alt= more than 10% chance

n= 25000
phat = 2700/25000
p0=0.1
z_pvalue = 1-pnorm((phat-p0)/sqrt(p0*(1-p0)/n)) 
z_pvalue
prop.test(2700,25000, p=0.1, alt="greater")


#6.4
n = c(10,11,20,30)
typeI= 1-pnorm(0.5*sqrt(n))
typeI

mul=c(1.0,1.1,1.2,1.3)
power = 1-pnorm((0.5-mul)*sqrt(n))
power
