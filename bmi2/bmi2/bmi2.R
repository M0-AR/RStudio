# Read the dataset ’bmi2_data.csv’ into R
D <- read.table("bmi2_data.csv", header = TRUE, sep = ";")
# Add log-BMI to the dataset
D$logbmi <- log(D$bmi)


##############################
###a 
cor(D[,c("logbmi","age","fastfood")], use="pairwise.complete.obs")
df <- data.frame(logbmi = rnorm(D$logbmi), age = rnorm(D$age), fastfood = rnorm(D$fastfood))
plot(df)

# Subset containing the first 840 observations (for model estimation)
D_model <- subset(D, id <= 840)
# Subset containing the last 7 observations (for validation)
D_test <- subset(D, id >= 841)

## Using histograms
par(mfrow=c(1,3), mar=c(4,3,1,1))
hist(D$logbmi, xlab="logbmi")
hist(D$age, xlab="age")
hist(D$fastfood, xlab="fastfood")

##Box plot with three groups 

par(mfrow=c(1,1), mar=c(4,3,1,1))
boxplot(D$logbmi ,main="logbmi")
boxplot(D$age ,main="age")
boxplot(D$fastfood ,main="fastfood")

## Summary of data
summary(D)

length(D$logbmi)
length(D$age)
length(D$fastfood)
mean(D$logbmi, na.rm=TRUE)
mean(D$age, na.rm=TRUE)
mean(D$fastfood, na.rm=TRUE)
sd(D$logbmi)
sd(D$age)
sd(D$fastfood)


### b
par(mfrow=c(1,2))
plot(D$age, D$logbmi, pch=19, cex=0.5, xlab=expression("Age"))
plot(D$fastfood, D$logbmi, pch=19, cex=0.5, xlab=expression("Fastfood"))

### c 
# Estimate multiple linear regression model
fit <- lm(logbmi ~ age + fastfood, data = D_model)
# Show parameter estimates etc.
summary(fit)

### d
# Plots for model validation
# Observations against fitted values
par(mfrow=c(1,1))
plot(fit$fitted.values, D_model$logbmi, xlab = "Log(BMI) against Fitted values",
     ylab = "log(BMI)")
# Residuals against each of the explanatory variables
par(mfrow=c(1,2))
plot(D_model$age, fit$residuals,
     xlab = "Residuals against Age", ylab = "Residuals")
plot(D_model$fastfood, fit$residuals,
     xlab = "Residuals against Fast food ", ylab = "Residuals")

# Residuals against fitted values
par(mfrow=c(1,2))
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values",
     ylab = "Residuals")
# Normal QQ-plot of the residuals
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores",
       main = "")
qqline(fit$residuals)


### e) 
# t1−α/2
d <- qt(.975,df=837)
3.1124298+c(-1,1)*d*0.0193517
0.0023744+c(-1,1)*d*0.0003890
0.0005404+c(-1,1)*d* 0.0001732
# Confidence intervals for the model coefficients
confint(fit, level = 0.95)

### f) 
t_obs <- 0.0023831+0.001 / 0.0003876
p.v1 <- 2 * (1 - pt(abs(t_obs), df=844))

### g) 
# Estimate linear regression model
fit <- lm(logbmi ~ age, data = D_model)
# Show parameter estimates etc.
summary(fit)

# Estimate linear regression model
fit <- lm(logbmi ~ fastfood, data = D_model)
# Show parameter estimates etc.
summary(fit)

# Estimate linear regression model
fit <- lm(logbmi ~ age, data = D_test)
# Show parameter estimates etc.
summary(fit)

# Estimate linear regression model
fit <- lm(logbmi ~ fastfood, data = D_test)
# Show parameter estimates etc.
summary(fit)


### h) 
# Predictions and 95% prediction intervals
pred_00 <- predict(fit, newdata = D_test, se = TRUE)
pred <- predict(fit, newdata = D_test, interval = "prediction", level = 0.95)

# Observed values and predictions
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)