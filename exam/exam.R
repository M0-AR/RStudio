x <- c(38, 35, 47, 38, 42, 41, 48, 35)
y <- c(25, 21, 26, 23, 28, 27, 29, 18)

cor(x, y)


pill.study <- matrix(c(15, 24, 25, 239,
                       7,14,8,273), ncol = 4, byrow = TRUE)
rownames(pill.study) <- c("Diet A", "Diet B")
colnames(pill.study) <- c("I", "II", "III", "IV")
pill.study


chisq.test(pill.study, correct = FALSE)

k <- 10
X_a <- rnorm(k, 12, 2)
X_b <- rnorm(k, 25, 3)
X_c <- rnorm(k, 42, 4)
Y <-X_a + X_b + X_c
var(Y)

1 - pnorm(q=85, mean=79, sd=sqrt(29))




sqrt(412.7042)


qchisq(c(0.025, 0.975), 95)
       
       
       
x <- c(90.6, 90.3, 88.9, 87.5, 87.6, 88.1, 87.5, 88, 88, 89.6)
n <- length(x)
tobs <- (mean(x) - 90) / (sd(x) / sqrt(n))

p <- 2* (1-pt(abs(tobs), df=n-1))

t.test(x, conf.level = 0.90)



a = 30
b = 33
pnorm(b, mean = 30, sd=sqrt(162/2)) - pnorm(a, mean=30, sd=sqrt(162/2))


power.t.test(delta = 5, sd=10, sig.level = 0.05, power = 0.8)


x1 <- c(63.5, 66.7, 59.2, 57.4, 63.9, 63.2, 60.7, 62.6, 63.3)
x2 <- c(51.3, 51.9, 57.8, 50.2, 54.6, 43.3, 51.2, 40.4, 52.2)
m <- x2-x1
t.test(m)



#using the t-distribution with n − (p + 1) =  degrees of freedom to find the quantile t1−α/2:
qt(p=0.975, df=)

#So we use the t-distribution with n − k = 32 − 4 = 28 degrees of freedom


###########
# May 2018

# 8 
set.seed(7643)
k <- 10000
R1 <- rnorm(k, mean = 2, sd = 0.2)
R2 <- rnorm(k, mean = 3, sd = 0.5)
R <- 1/(1/R1 + 1/R2)
quantile(R, c(0.025,0.975)) # result 0.9647361 1.4016874 


# 13 
chisq.test(matrix(c(7966, 7297,6691,10173,10253,10006,8551,10130,10339), ncol = 3), correct = FALSE)

# 16 
# p = 0.2 and size n = 20.
dbinom(0, size = 20, p = 0.2)
## [1] 0.01152922
pbinom(0, size = 20, p = 0.2)
## [1] 0.01152922


# 18
# Use Method 7.13 with p = 0.2, remembering that the ME is half the expected width of the
# confidence interval, and using the information that 20 components are produced per day


# 19 
#Use Method 3.9 / (3-11) with n = 20, ¯x = 793.1/20, s =
#√0.01099. The 0.975 quantile for the t-distribution with 19 degrees of freedom is:
qt(p=0.975, df=19)

# 20
tobs <- 2.38
p <- 2* (1-pt(abs(tobs), df=19))


# 21 
power.t.test(delta=0.05, sd=0.11,sig.level = 0.01,power=0.9, type = "one.sample")

#23 See definition 1.11 
#s1 =sqrt(131606104/(304 − 1)) = 659.0475
#s2 =sqrt(12775276/(252 − 1)) = 225.6048

#26
#Set the significance level to α = 0.05. Give the critical value for the usual test used to investigate
#whether the expected crop yield differs between years.
# Theorem 8.6 (k − 1, n − k) = (4 − 1, 20 − 4) = (3, 16)
qf(0.95, df1 = 3, df2 = 16)


#28
x <- c(-6.5, -6.5, -5.2, -4.9, -4.8, -2.9, -2.6, -2.0, -1.9, -1.8, -1.5,
       -0.1, 1.9, 2.1, 2.6, 5.2, 8.0)
median(x)
t.test(x)
mean(x)


###########
#2018 dec

#21 
pill.study <- matrix(c(54, 58, 1280, 966,
                       144,135), ncol = 2, byrow = TRUE)



chisq.test(pill.study, correct = FALSE)



# 23
x <-c(13, 12, 9, 7, 12, 15, 12, 10, 6, 13, 7, 13, 19, 12, 6, 4, 15, 16, 11, 18)
mean(x)
var(x)


# 24
x = c(9,9.65,9.65,9.80,9.0,9.95,10,10,10,10.05,12.95,12.95,12.95,13,13.05,13.10,13.10,13.10,13.15,13.40)
m = apply(replicate(200, sample(x, replace = TRUE)), 2, mean)
quantile(m, c(0.025, 0.975))


# 25 
#Let X represent the number of guests arriving at the toilets in a randomly selected hour,
#then X ∼ Pois(150). The capacity is 10 · 20 = 200 per hour, hence we need to calculate
#P(X > 200) = 1 − P(X ≤ 200):
1 - ppois(200, lambda=150)
## [1] 4.205886e-05




####3#######
# 2019 may 
# 12 
#Method 5.15 with β1 = 5031, ˆσβ1 = 945.5 (both from the R-output) and t0.975 found using
qt(0.975, df = 10-2)
## [1] 2.306004
# answer is : 5031 ± 2.306 · 945.5


# 21
x1 = c(10.5,9.3,10.7,10.8,11.2)
x2 = c(8.9,9.5,10.2,9.8,10.3)


# 23 
t.test(x1, conf.level=0.9)
#24
n1 <- length(x1)
n2 <- length(x2)
varp <- ((n1-1)*var(x1)+(n2-1)*var(x2)) / (n1+n2-2)
power.t.test(power=0.99,delta=1,sd=sqrt(varp),sig.level=0.01)


#25
F<-(SSTr/(3-1)/(0.44147/(12-3)))

#30
x = c(10, 25, 25, 36, 37, 41, 54, 64, 68, 83)
var(x)


###########
#2019 aug 
#1
# Simply the 15% quantile in the distribution, hence N(5, 2^2)
?qnorm(0.15, mean=5, sd=2)

#2 
1 - pnorm(4.5, mean=5, sd=2/sqrt(25))

#4
rnorm()

#5
?dnorm()

#15
1-pf(q=4.3, df1=4, df2=25)

#16
((1.19)/2) / (4.53/9)

#23
109.4+qt(p=0.975, df=7) * (6.2/sqrt(8))
109.4-qt(p=0.975, df=7) * (6.2/sqrt(8))

#24
qchisq(p=0.995, df=7)
qchisq(p=0.005, df=7)

#25
(109.4-107.0)/sqrt((6.2^2/8) + (4.1^2/12))

#26
power.t.test(power=0.8,delta=4,sd=5,sig.level=0.01)



#######
# 2019 dec
#12
(1.99-1.14)/sqrt((0.58^2/5) + (0.84^2/10))

#15
prop.test(x=123,n=756,correct=FALSE)

#16
prop.test(x=85,n=756,correct=FALSE)

p1 = 26/85
p2 = 59/85
sqrt((p1*(1-p1)))

#18
# See Method 7.22. Here, r = 2 and c = 4, so the χ2-distribution with (r−1)·(c−1) = 3 degrees
#of freedom should be used. The p-value may be computed “by hand” as1-pch
1 - pchisq(9.6127, df=3)

#21
# Residual standard error: 2.496 on 17 degrees of freedom
# variance = 2.496^2


#22
x <- c(6.5, 5.7, 1.2, 0.2, 7.0, 3.3)
t.test(x)
##RRRRRRRRRRRRRRRRRRrMMMMMMMMBBBBBAAAAAAAIR
##### Bracets 
(mean(x) - 3.0) / (sd(x) /sqrt(6))

#23
qchisq(0.05,df=6)
(5*var(x))/qchisq(0.95,df=5)
(5*var(x))/qchisq(0.05,df=5)

#See Method 3.19. In R:
(6-1)*var(x)/qchisq(c(0.95, 0.05), df = 6-1)
## [1] 3.695257 35.712948

#25 
x = c(10.8,8.2,8.7,12,6.2,11.2,8.6,5.5)
median(x)


#28
x <- c(39.5, 59.7, 42.1, 13, 3.6, 10.9, 61.6, 1, 17.8, 5,
       24.3, 21, 4.2, 21.1, 78.9, 11.1, 6.6, 0.3, 9.2, 10.4)
quantile(x, type=2)
?IQR(x,type=2) #### becarefull about type=2
summary(x)
quantile(x, 0.75, type = 2) - quantile(x, 0.25, type = 2)


##########
#2020 may 
#1
dnorm(3,4,sqrt(0.2))
#2
#Its a non-linear function, so use simulation
k <- 1000000
R1 <- rnorm(k, mean=4, sd=sqrt(0.2))
R2 <- rnorm(k, mean=2, sd=sqrt(0.2))
R <- 1 / (1/R1 + 1/R2)
sd(R)

#4
x <- c(1.6, 2, 3.4, 4, 2.1, 0.6, 0.4, 0.4, 6, 0.4, 4.9, 2, 2, 4.6, 0.5,
       3.4, 7.2, 10.5, 3.2, 1.3, 5.7, 1.9, 2.6, 2.5, 4.4, 1.8, 3.9, 6, 0.9)
mean(x)
sd(x)

#6 It’s a binomial setup, so discrete, and the probability is
#P(X > 5) = 1 − P(X ≤ 5)
#which is found in R by
n <- 120
p <- 0.07
1 - pbinom(5, n, p)
## [1] 0.8522782

# 13
prop.test(x=c(18),n=c(56),correct=FALSE,conf.level=0.95)
# We use the CI formula in Method 7.3:
p <- 18/(18+38)
## With the formula of the book
p - qnorm(0.975) * sqrt(p*(1-p)/(18+38))
## [1] 0.1991
p + qnorm(0.975) * sqrt(p*(1-p)/(18+38))
## [1] 0.4437


#15
#If we don’t want to do all the calculations, then we can put in R (we could have done that already) by:
M <- as.table(rbind(c(18,11,22,9),c(38,36,15,12)))
M

## Chi^2 test
(Xsq <- chisq.test(M))
##
## Pearson's Chi-squared test
##
## data: M
## X-squared = 13, df = 3, p-value = 0.006
## Observed statistic
Xsq$statistic
## X-squared
## 12.57
## p-value
1 - pchisq(Xsq$statistic, df=ncol(M)-1)
## X-squared
## 0.005671
## Critical value
qchisq(0.99, df=length(x)-1)
## [1] 11.34
# A difference between the schools can be detected, since the relevant test statistic is above
# the critical level of 11.3. *********************

# 18 
power.t.test(delta=2, sd=3.5,sig.level = 0.05,power=0.9,type="one.sample")


# 21
#Based on the above R output, what is the critical value for the test
#H0 : β1 = 0
# using a 1% significance level? ## Residual standard error: 124.7 on 28***** degrees of freedom
qt(0.995, df=28)
## [1] 2.763262


#27
# What is the 99% confidence interval for the standard deviation of the diameter of the tubes?
qchisq(0.995, df=29)
## [1] 52.3
qchisq(0.005, df=29)
## [1] 13.1

#(29 · 531) / 52.3 = 17.2
#(29 · 531) / 13.1 = 34.3
# = [17.2, 34.3]


#29 
#Delta is 11.8. To obtain a power of at least 90% we need to be at least as far away from the
#hypotesized mean as given by delta. This corresponds to detecting a true mean of 88.2 or below
#or 111.8 and above. Therefore only answer 2 can be correct.
# H0: mean= 100 delta = 11.8 -> 100 - 11.8 = 88.2
# If the true mean is 88.2 or lower, we have more than 90% chance of rejecting H0.


# 30 
#X ∼ B(10, 0.2). The Probability in question is P(X ≤ 3). It can be found using the following
#R code:
pbinom(3,size=10,prob=0.2)
## [1] 0.8791261


############
# 2017aug
# 1
## Sandsynligheden for ikke at bliver trukket alle 7 dage
dbinom(7, 7, 1-4/35)
## [1] 0.4276176

# 8
power.t.test(delta = 40, sd=10,sig.level = 0.05,power=0.99)


#12 
# Sxx = (n − 1)s^2(small below)x

#14 



#######
#dec2020

#2
Bloodtype <- matrix(c(62,255,113,433,37,153),nrow=3,byrow=T)
colnames(Bloodtype) <- c("ICU","Admitted")
rownames(Bloodtype) <- c("30","10","20")
## Answer:
prop.test(Bloodtype)
chisq.test(Bloodtype)

mat <- Bloodtype
Exp <- rowSums(mat) %o% colSums(mat) / sum(mat)
Chisq.val <- sum((mat - Exp)^2 / Exp)
df <- prod(dim(mat) - 1)
pchisq(Chisq.val, df, lower=FALSE)
## [1] 0.0008443032
#3
?prop.test(x=c(62,37), n=c(212,212), correct = TRUE)


#5



Bloodtype <- matrix(c(13,33,12,144,53,21,54,35,183,60,32,77,85,251,86,10,16,12,64,27),nrow=4,byrow=T)
chisq.test(Bloodtype)



#10 
# https://02323.compute.dtu.dk/assets/pdfjs-viewer/web/viewer.html?file=/filemanager/02323/sharelatex-public/book-IntroStatistics.pdf#page.264
qt(0.975,df=48)
#if tobs>t1a/2 we rejectH0, otherwise we acceptH0

#13
rnorm
x <- rnorm(100)^2 + rnorm(100)^2 + rnorm(100)^2
mean(x)
t.test(x)

#17 
dbinom(10,20,3/20)
pbinom(2,20,3/20)

p1 <- 0.15

n1 <- 10
p1*(1-p1)^(n1-1)
#19
?pbinom()

#21
qchisq(0.995, 90-1)
## [1] 20.28
qchisq(0.005, 90-1)
## [1] 0.9893


1-pnorm(q = 10, mean = 1, sd = 0.7)



  
(12.341  - 10.84) / qt(0.975, df = 89)

10.84+qt(0.995, df = 89) * ((12.341  - 10.84) / qt(0.975, df = 89))
10.84-qt(0.995, df = 89) * ((12.341  - 10.84) / qt(0.975, df = 89))

#22 
?t.test()

#26
qf(0.95, df1 = 6, df2 = 357)


#29 
x <- c(1.11, 0.94, -2.43, -0.90,  0.29, -1.41,  0.38,  0.99, -0.50)
median(x)

par(mfrow=c(1,2))
hist(x,freq=FALSE)
curve(dchisq(xseq,df=n),xname="xseq",add=TRUE,col="red")



pnorm(10, 1, 0.7)

