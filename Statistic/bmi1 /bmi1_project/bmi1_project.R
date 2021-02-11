## Read data from bmi1_data.csv
D <- read.table("bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)
#D <- read.csv("bmi1_data.csv")

## Dimensions of bmi1 (number of rows and columns)
dim(D)
## Column/variable names
names(D)
## The first rows/observations 
head(D)
## The last rows/observations
tail(D)
## Selected summary statistics
summary(D)
## Another type of summary of the dataset str(bmi1)
str(D)



#------------------------------------
#b
## Calculate BMI scores and add new variable to D
D$bmi <- D$weight/(D$height/100)^2

## Histogram describing the empirical density of the BMI scores 
## (histogram of the BMI scores normalized to have an area of 1) 
hist(D$bmi, xlab="BMI", prob=TRUE)

# Is there much variation to be seen in the observations? 
var(D$bmi)

sd(D$bmi)

#------------------------------------
#c 
## Divide data into two subsets according to gender
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)

## Density histograms describing the emprical density 
## of the BMI scroes of women and men, respectively

hist(Dmale$bmi, xlab="BMI (male)", prob=TRUE)

#------------------------------
#d
## Box plot of BMI scores by gender 
boxplot(Dfemale$bmi, Dmale$bmi, names=c("Female", "Male"), xlab="Gender", ylab="BMI")

#-------------------------------
#e
## The argument 'na.rm=TRUE' ensures that the statistic is 
## computed even in cases where there are missing values.

## Total number of observations 
## (doesn't include missing values if there are any)
sum(!is.na(D$bmi))
## Sample mean (both genders combined)
mean(D$bmi, na.rm=TRUE)
## Sample variance (both genders combined)
var(D$bmi, na.rm=TRUE)
## Sample std (both genders combined)
sqrt(var(D$bmi, na.rm=TRUE))
## Lower quartile median and upper quartile (both genders combined)
quantile(D$bmi, na.rm=TRUE)

# For Men: 
## Total number of observations 
## (doesn't include missing values if there are any)
sum(!is.na(Dmale$bmi))
## Sample mean 
mean(Dmale$bmi, na.rm=TRUE)
## Sample variance 
var(Dmale$bmi, na.rm=TRUE)
## Sample std 
sqrt(var(Dmale$bmi, na.rm=TRUE))
## Lower quartile median and upper quartile
quantile(Dmale$bmi, na.rm=TRUE)

# For Women: 
## Total number of observations 
## (doesn't include missing values if there are any)
sum(!is.na(Dfemale$bmi))
## Sample mean 
mean(Dfemale$bmi, na.rm=TRUE)
## Sample variance 
var(Dfemale$bmi, na.rm=TRUE)
## Sample std 
sqrt(var(Dfemale$bmi, na.rm=TRUE))
## Lower quartile median and upper quartile 
quantile(Dfemale$bmi, na.rm=TRUE)

#------------------------------------
## New variable 'logbmi' with log-trasformed BMI
D$logbmi <- log(D$bmi)
## qq-plot of log-transformed BMI
qqnorm(D$logbmi)
qqline(D$logbmi)
#f
## Statistical Model 
## Sample mean (both genders combined)
mean(D$logbmi, na.rm=TRUE)
## Sample variance (both genders combined)
var(D$logbmi, na.rm=TRUE)

sd(D$logbmi, na.rm=TRUE)


## Using histograms
par(mfrow=c(1,2), mar=c(4,3,1,1))
hist(D$logbmi, xlab="Height", main="", breaks=8)
hist(D$logbmi, xlab="Height", main="", breaks=2)

## The expected quantiles in a 0 to 1 uniform distribution
n <- length(D$logbmi)
## They have equal distance
pseq <- (1:n-0.5)/n
## Plot the expected normal distribution quantiles
plot(x=qnorm(p=pseq), y=sort(D$logbmi), xlab="Normal quantiles",
     ylab="Sample quantiles")
## Mark the 1st and 3rd quantiles with crosses
points(x=qnorm(p=c(0.25,0.75)), y=quantile(D$logbmi,probs=c(0.25,0.75)),
       pch=3, col="red")
## Add a straight line through the 1st and 3rd quantiles
qqline(D$logbmi)

## Let's use the rnorm command to draw 500 numbers at random from a normal distribution having mean 100 and standard deviation 10.
x <- rnorm(1,mean=mean(D$logbmi, na.rm=TRUE),sd=sd(D$logbmi, na.rm=TRUE))
z <- (x - mean(D$logbmi, na.rm=TRUE)) / sd(D$logbmi, na.rm=TRUE) / sqrt(length(D$logbmi))
#---------
#g
## Sample mean (both genders combined)
meanOfLogbmi <- mean(D$logbmi, na.rm=TRUE)
lengthOfLogbmi <- length(D$logbmi)
s <- sd(D$logbmi, na.rm=TRUE)

## The t-quantiles for n=10: which n - 1 = degrees of freedom
qt(0.975, lengthOfLogbmi - 1)

## The 95% confidence interval for the mean
meanOfLogbmi - qt(0.975, df = lengthOfLogbmi - 1) * s / sqrt(lengthOfLogbmi)

meanOfLogbmi + qt(0.975, df = lengthOfLogbmi - 1) * s / sqrt(lengthOfLogbmi)

## Test for previous 
## The 99% confidence interval for the mean
t.test(D$logbmi, conf.level = 0.95)

## Check if BMI score  coming from a normal distribution 
hist(D$bmi)
qqnorm(D$bmi, ylab = "Sample quantiles", xlab = "Normal quantiles")
qqline(D$bmi)

## As I had expected BMI score closer to a normal distribution
## Back transform to original scale, now we get the median! 
exp( 3.217641 )

#------------------------
#h
t.test(D$bmi, mu=25)

t_obs <- ( 25.24795  - 25) / sd(D$logbmi) / length(D$logbmi)

2 * (1 - pt(t_obs, length(D$logbmi) -1))
### Specify the significance level α
# sample standard deviationt
sampleSD <- sd(D$logbmi)
sampleSize <-  length(D$logbmi)
# standard deviation of our sampling distribution
sdOfDistribution <- sampleSD / sampleSize
## Z-statistics: how far we away from mean 
#mean 
meanOfLogbmi <- mean(D$logbmi)
z <- meanOfLogbmi / sdOfDistribution

## Testing hypothesis mu=log(25) for log-trasformed BMI
t.test(D$logbmi, mu=log(25))

hist(D$logbmi)


### investigate whether a difference can be detected between the BMI of women and men.
## New variable 'logbmi' with log-trasformed BMI
Dmale$logbmi <- log(Dmale$bmi)
Dfemale$logbmi <- log(Dfemale$bmi)
##  Keep the summary statistics
ms <- c(mean(Dmale$bmi), mean(Dfemale$bmi))
vs <- c(var(Dmale$bmi), var(Dfemale$bmi))
ns <- c(length(Dmale$bmi), length(Dfemale$bmi))
## The observed statistic 
t_obs <- (ms[1]-ms[2])/sqrt(vs[1]/ns[1]+vs[2]/ns[2])
## The degrees of freedom 
nu <- ((vs[1]/ns[1]+vs[2]/ns[2])^2)/((vs[1]/ns[1])^2/(ns[1]-1)+(vs[2]/ns[2])^2/(ns[2]-1))

## Print the result 
t_obs

nu

## p-value
pv <- 2 * pt(t_obs, df = 145)

t.test(Dmale$bmi, Dfemale$bmi)

### i 
## New variable 'logbmi' with log-trasformed BMI
Dmale$logbmi <- log(Dmale$bmi)
Dfemale$logbmi <- log(Dfemale$bmi)
##  Keep the summary statistics
ms <- c(mean(Dmale$logbmi), mean(Dfemale$logbmi))
vs <- c(var(Dmale$logbmi), var(Dfemale$logbmi))
ns <- c(length(Dmale$logbmi), length(Dfemale$logbmi))


### j
## The t-quantiles for Nmen=73 and Nwomen=73:

qt(0.975, ns[2]-1)

## Calculate 95% confidence intervals 
sd <- c(sd(Dmale$logbmi), sd(Dfemale$logbmi))
# Men
ms[1] -  qt(0.975, ns[1]-1) * sd[1] / sqrt(ns[1])
ms[1] +  qt(0.975, ns[1]-1) * sd[1] / sqrt(ns[1])

# Women
ms[2] -  qt(0.975, ns[2]-1) * sd[2] / sqrt(ns[2])
ms[2] +  qt(0.975, ns[2]-1) * sd[2] / sqrt(ns[2])

## Consider data for women only
Dfemale <- subset(D, gender == 0)
## Compute CI for mean log-BMI score of a woman
KI <- t.test(Dfemale$logbmi, conf.level=0.95)$conf.int
KI
## "Back-transform" to get a CI for median BMI score of a woman 
exp(KI)

## Consider data for men only
Dmale <- subset(D, gender == 0)
## Compute CI for mean log-BMI score of a men
KI <- t.test(Dmale$logbmi, conf.level=0.95)$conf.int
KI
## "Back-transform" to get a CI for median BMI score of a mea 
exp(KI)



#------------------------------
### k 
## Comparison of mean logBMI for women and men
t.test(D$logbmi[D$gender == 0], D$logbmi[D$gender == 1])

#------------------------------
### i 
## The confidence intervals and joining the lower and upper limits
CIMen <- t.test(Dmale$logbmi)$conf.int
CIWomen <- t.test(Dfemale$logbmi)$conf.int
lower <- c(CIMen[1], CIWomen[1])
upper <- c(CIMen[2], CIWomen[2])
## First install the package with: 
install.packages("gplots")
library(gplots)
barplot2(c(mean(Dmale$logbmi),mean(Dfemale$logbmi)), plot.ci=TRUE, ci.l=lower, ci.u=upper,
         col = 2:3)

## The confidence intervals
CIMen



CIWomen
#---------------------------
## Computing correlations between selected variables
cor(D[,c("weight","fastfood","bmi")], use="pairwise.complete.obs")

df <- data.frame(weight = rnorm(D$weight), fastfood = rnorm(D$fastfood), bmi = rnorm(D$bmi))
plot(df)


cor(D$bmi, D$weight)^2