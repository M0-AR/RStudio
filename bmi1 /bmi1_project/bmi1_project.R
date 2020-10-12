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

#------------------------------------
#c 
## Divide data into two subsets according to gender
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)

## Density histograms describing the emprical density 
## of the BMI scroes of women and men, respectively
hist(Dfemale$bmi, xlab="BMI (female)", prob=TRUE)
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