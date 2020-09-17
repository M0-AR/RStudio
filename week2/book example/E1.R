#E1.1
x <- c(2474, 2547, 2830, 3219, 3429, 3448, 3677, 3872, 4001, 4116)
mean(x)    
var(x)
sd(x)
summary(x)
median(x)
boxplot(c(x, 235), col="red", main="Females")
y <- c(2844, 2863, 2963, 3239,3379, 3449, 3582, 3926, 4151, 4356)
mean(y)
var(y)
sd(y)
summary(y)
boxplot(c(y, 235), col="red", main="Males")
#E1.2
Total <- c(34, 28, 43, 42, 39, 33, 32)
median(Total)
summary(Total
        )
#The textbook might have misconstrued John Tukey's method of computing "hinges" (aka "fourths"). The difference is that when splitting the dataset around the median, he includes the median in both halves. That would produce 9.5 and 28 for the example dataset
#E1.3
Before <- c(9.1, 8.0, 7.7, 10, 9.6,7.9,9, 7.1, 8.3, 9.6, 8.2, 9.2, 7.3, 8.5, 9.5)
After <- c(8.2, 6.4, 6.6, 8.5, 8, 5.8, 7.8, 7.2, 6.7, 9.8, 7.1, 7.7, 6, 6.6, 8.4)
median(Before)
median(After)
sd(Before)
sd(After)
# Use cov() with DAX_logreturns and FTSE_logreturns
cov(Before, After)

# Use cor() with DAX_logreturns and FTSE_logreturns
cor(Before, After)

Before <- c(sort(Before))
After <- c(sort(After))

Dif <- Before - After

summary(Dif)
sd(Dif)
boxplot(c(Dif, 1.5), col="red", main="Dif")

#E2.1
#b
## P(X <= 5)
pbinom(5,10,0.6)

## P(X < 5)
pbinom(4,10,0.6)

## P(X > 4)
1 - pbinom(4,10,0.6)

## P(X = 5)
pbinom(5,10,0.6) - pbinom(4,10,0.6)

#d
## P(X <= 5))
ppois(5,3)

## P(X < 5)
ppois(4,3)

## P(X > 4)
1 - ppois(4,3)

## P(X = 5)
ppois(5,3) - ppois(4,3)


#2.4 
#a

#2.5
#a
1 - phyper(0,3,2,30) # ? 

#b 
#We can use the pdf or cdf in R. For p1:
dbinom(x=0, size=10, prob=0.01)

pbinom(q=0, size=10, prob=0.01)

#2.7
#a 
1 - ppois(q=5, lambda=1.6)

#b
ppois(q=8, lambda=8)

#2.8
#a 
1- ppois(q=19, lambda=15)

#b
ppois(q=22:26, lambda=15)