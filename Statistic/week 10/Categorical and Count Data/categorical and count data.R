#### 7.1 
### b) What is the critical values for the χ2 -test of the hypothesis H0 : p1 = p2 with significance level α = 0.01?
## This test has degrees of freedom (2 − 1)(2 − 1) = 1, with the critical value χ2 =0.99
qchisq(0.99, 1)


#### 7.2
### b) 
##(row - 1)(column - 1) = 2
qchisq(0.95, 2)


#### 7.3
### b) Calculate a 95%-confidence interval for p based on the exit poll
0.227 + c(-1, 1)*1.96*sqrt(0.227*0.773/740)

#### 7.4
### a) p-value
2 * (1 - pnorm(1.5617)) 
## we cannot reject our hypothesis 

### b) 
p = 36/200
p + c(-1, 1)*qnorm(0.995)*sqrt(p*(1-p)/200)

prop.test(x=36, n=200, correct=FALSE, conf.level=0.99)

### c) degrees of freedom is (r − 1) · (c − 1) = (3 − 1) · (3 − 1) = 4
1 - pchisq(10.9849, 4)
