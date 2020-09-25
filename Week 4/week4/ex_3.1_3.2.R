#E3.1
items <- c(3003, 3005, 2997, 2999, 2998, 3007, 3005, 3001)
##a
m <- mean(items)
s <- sd(items)
n <- 9
se <- s/sqrt(9)

##  c) Find the 95% confidence interval for the mean µ
## 97.5% fraktilen af t-fordelingen for n=10:
f <- qt(p=0.975, df=8)
 m + f * s/sqrt(n)
 m - f * s/sqrt(n)
t.test(items) 
#One Sample t-test
#data:  items
#t = 2306.3, df = 7, p-value <
#  2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  2998.797 3004.953
# sample estimates: mean of x 3001.875 

#d) Find the 99% confidence interval for µ. Compare with the 95% one from
# above and explain why it is smaller/larger! 
f_d <- qt(p=0.995, df=8)
m + f_d * s/sqrt(n)
m - f_d * s/sqrt(n)
t.test(items, conf.level = 0.99) 
# It makes good sense that the 99% confidence interval becomes larger than the 95%
# one, as the consequence of wanting to be more confident on capturing the true mean
# µ will make us having to state a larger interval.

#e) Find the 95% confidence intervals for the variance σ
# 2 and the standard deviation σ.
qchisq(c(0.025, 0.975), df = 8)
s^2
c(8*s^2/qchisq(0.975,8),8*s^2/qchisq(0.025,8) )
sqrt(c(8*s^2/qchisq(0.975,8),8*s^2/qchisq(0.025,8) ))

#)  Find the 99% confidence intervals for the variances2and the standarddeviations
qchisq(c(0.005, 0.995), df = 8)
s^2
c(8*s^2/qchisq(0.995,8),8*s^2/qchisq(0.005,8) )
sqrt(c(8*s^2/qchisq(0.995,8),8*s^2/qchisq(0.005,8) ))





################################ 

# E3.2
# a) A 90%-confidence interval for µ becomes?
x <- c(180.02, 180.00, 180.01, 179.97, 179.92, 180.05, 179.94, 180.10,
       180.24, 180.12, 180.13, 180.22, 179.96, 180.10, 179.96, 180.06)
n <- 16

m <- mean(x)

s <- sd(x)

q_t <- qt(0.95, 15)

c(m - q_t*s/sqrt(n),m + q_t*s/sqrt(n)  )

t.test(x, conf.level=0.9)

# b) A 99%-confidence interval for σ becomes?


c((n-1) * s^2 / qchisq( 0.995, df = 15),(n-1) * s^2 / qchisq( 0.005, df = 15))
sqrt(c((n-1) * s^2 / qchisq( 0.995, df = 15),(n-1) * s^2 / qchisq( 0.005, df = 15)))
t.test(x, conf.level=0.99)