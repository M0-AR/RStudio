#### 8.1 
### d) 

-1.50+c(-1,1)* qt(0.95, 15)*sqrt(4.1060/15*(1/3))

#### 8.2 
nitrogen <- c(5.01, 5.59, 3.02,
              6.23, 5.13, 4.76,
              5.98, 5.33, 3.46,
              5.31, 4.65, 4.12,
              5.13, 5.52, 4.51,
              5.65, 4.92, 4.42)
year <- factor(rep(c("1998", "2003", "2011"), 6))
tapply(nitrogen, year, mean)
tapply(nitrogen, year, var)

mean(nitrogen)

### b) 
D <- data.frame(
  nitrogen=c(5.01, 5.59, 3.02,
             6.23, 5.13, 4.76,
             5.98, 5.33, 3.46,
             5.31, 4.65, 4.12,
             5.13, 5.52, 4.51,
             5.65, 4.92, 4.42),
  year=factor(rep(c("1998", "2003", "2011"), 6))
)
tapply(D$nitrogen, D$year, mean)
tapply(D$nitrogen, D$year, var)
mean(D$nitrogen)
(18 - 1) * var(D$nitrogen)

### c) Run the ANOVA in R and produce the ANOVA table in R.
fit = lm(nitrogen ~ year, data = D)
anova(fit)



### d) 
LSD_0.01667 <- qt(1-(0.05/3)/2, 15) * sqrt(2*0.2737/6)

plot(D$year, D$nitrogen, xlab="Nitrogen", ylab="Year")
text(1:3, c(5.7, 5.4, 4), c("a", "a", "b"), cex=2)

qqnorm(fit$residuals)
qqline(fit$residuals)
