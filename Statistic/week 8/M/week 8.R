####5.2
###a) Calculate the 95% confidence interval for the slope in the usual linear regression model, which expresses the life time as a linear function of the temperature
D <- data.frame(t=c(10,20,30,40,50,60,70,80,90),
                y=c(420,365,285,220,176,117,69,34,5))
fit <- lm(y ~ t, data=D)
summary(fit)

t <- qt(.975, 7)
-5.31+c(-1,1)*t*0.2558

####5.3
###b) Can a significant relationship between yield and temperature be documented on the usual significance level α = 0.05?
D <- data.frame(x=c(0,25,50,75,100),
                y=c(14,38,54,76,95))
fit <- lm(y ~ x, data=D)
summary(fit)
qt(.975, 3)
predict(fit, newdata=data.frame(x=80), interval="confidence",
        level=0.95)

#### 5.4 
### a) What is the 95% confidence interval for the slope of the regression model,
### expressing the impact strength as a linear function of the cooling time?
D <- data.frame(x=c(15,25,35,40),
                y=c(42.1, 36.0,31.8,28.7))
fit <- lm(y ~ x, data=D)
summary(fit)

# t = c(qt(0.025, df=2), qt(0.975, df=2)) both are same
t <- qt(.975, 2)
-0.52136+c(-1,1)*t*0.0281

#### 5.5 
### a) What are the parameter estimates for the three unknown parameters in
### the usual linear regression model: 1) The intercept (β0), 2) the slope (β1)
### and 3) error standard deviation (σ)?
D <- data.frame(concentration=c(11.5, 10.2, 10.3, 9.68, 9.32),
                distance=c(2, 4, 6, 8, 10))
fit <- lm(concentration ~ distance, data=D)
summary(fit)


### c)
predict(fit, newdata=data.frame(distance=7), interval="confidence", level=0.95)


#### 5.6
D <- data.frame(
  pressure=c(1.02,2.08,2.89,4.01,5.32,5.83,7.26,7.96,9.11,9.99),
  flux=c(1.15,0.85,1.56,1.72,4.32,5.07,5.00,5.31,6.17,7.04)
)
### a) What is the empirical correlation between pressure and flux estimated to?
### Give also an interpretation of the correlation.
fit <- lm(flux ~ pressure, data=D)
summary(fit)

### b) 
c(qt(0.05, df=8), qt(0.95, df=8))
t <- qt(0.95, 8)
0.72248+c(-1,1)*t*0.07064


predict(fit, newdata=data.frame(pressure=3.5), interval="confidence", level=0.95)
