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
