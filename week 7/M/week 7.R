#### 4.1
### a
## Number of simulations 
k <- 10000
## Generating k component A liftetime
xA <- rexp(k, 1/2)
## Checking the mean of these 
mean(xA)


## Generating k component B liftetime
xB <- rexp(k, 1/3)
## Checking the mean of these 
mean(xB)


## Generating k component C liftetime
xC <- rexp(k, 1/5)
## Checking the mean of these 
mean(xC)

# Putting these three sets of k lifetime togethere into a
# single k-by-3 matrix:
x <- cbind(xA,xB,xC)

# Finding the minimum value of the three components 
# in each of the k situtions 
lifetimes <- apply(x, 1, min)

## Histogram of the simulated lifetimes 
hist(lifetimes, col = "blue", nclass = 30)

### b
## The estimated mean lifetime 
mean(lifetimes)

### c 
## The estimated std. dev. of the lifetime
sd(lifetimes)

### d 
## The fraction of times the simulated lifetimes was below or equal 1
mean(lifetimes <= 1)

### e 
## The estimated median lifetime
median(lifetimes)

### f 
## The estimated 10% quantile
quantile(lifetimes, 0.10)

#############################################
#### 4.3
###a
## To achieve a particular result 
set.seed(6287)

x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
       38.43, 38.31, 38.32, 38.48, 38.50)

## Number of simulations 
k <- 10000
simsamples <- replicate(k, sample(x, replace = TRUE))
simmeans <- apply(simsamples, 2, mean)
quantile(simmeans, c(0.025, 0.975))

hist(simmeans, col="blue", nclass=30)

### b
k <- 10000
n <- length(x)
simsamples <- replicate(k, rnorm(n, mean(x), sd(x)))
simmeans <- apply(simsamples, 2, mean)
quantile(simmeans, c(0.025, 0.975))

hist(simmeans, col="red", nclass=30)

t.test(x)

### c 
k <- 10000
simsamples <- replicate(k, rlnorm(n, mean(log(x)), sd(log(x))))
simmeans <- apply(simsamples, 2, mean)
quantile(simmeans, c(0.025, 0.975))

hist(simmeans, col="orange", nclass=30)

### d Find the 95% confidence interval for the lower quartile Q1 by the parametric bootstrap assuming the normal distribution for the observations.
Q1 <- function(x) { quantile(x, 0.25) }
k <- 10000
simsamples <- replicate(k, rnorm(n, mean(x), sd(x)))
simQ1s <- apply(simsamples, 2, Q1)
quantile(simQ1s, c(0.025, 0.975))

hist(simQ1s, col="blue", nclass=30)

### e Find the 95% confidence interval for the lower quartile Q1 by the nonparametric bootstrap (so without any distributional assumptions)
k <- 10000
simsamples <- replicate(k, sample(x,replace = TRUE))
simQ1s <- apply(simsamples, 2, Q1)
quantile(simQ1s, c(0.025, 0.975))


#### 4.4
### a
## Might to use the seed to achieve a particular result 
set.seed(98273)

x1 <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
x2 <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)
## Number of simulated (bootstrapped) samples 
k <- 10000
## Simultaed samples of TV1 group
simx1samples = replicate(k, sample(x1, replace = TRUE))
## Simultae samples of TV2 group 
simx2samples = replicate(k, sample(x2, replace = TRUE))
simmeandifs = apply(simx1samples, 2, mean) - apply(simx2samples, 2, mean)
##The quantiles giving the 95% CI
quantile(simmeandifs, c(0.025, 0.975))

# We reject the null hypothesis of µ1 = µ2, since zero is not included in the CI of the
# differences.

### b)  Compare the two means assuming normal distributions for the two samples - without using simulations (or rather: assuming/hoping that the
### sample sizes are large enough to make the results approximately valid).
t.test(x1, x2)
# We reject the null hypothesis of µ1 = µ2.

### c) Compare the two means assuming normal distributions for the two samples - simulation based (parametric bootstrap confidence interval and relevant hypothesis test interpretation – in spite of the obviously wrong assumption).
simx1samples <- replicate(k, rnorm(n, mean(x1), sd(x1)))
simx2samples <- replicate(k, rnorm(n, mean(x2, sd(x2))))
simmeandifs = apply(simx1samples, 2, mean) - apply(simx2samples, 2, mean)
quantile(simmeandifs, c(0.025, 0.975)) # percentiles
# We reject the null hypothesis of  µ1 = µ2.


