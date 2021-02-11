############################################################
############################################################
### Problem 1 
# Prepare an R script file which can solve this 3 x 3 linear equation system A x = b, where the:
# - data matrix A columns contain normal distributed random numbers and the right hand side,
# - vector b contains normal distributed random numbers.
# In both the matrix and vector case the numbers are from a mean value 0 and spread 1 normal distribution.
# This latter property is the so called standardized normal distribution.

# Create and display a 3x3 data matrix A
N <- 3
A <- matrix(rnorm(N^2), nrow=N, ncol=N, byrow =FALSE); A

# Create and display Nx1 right hand side vector b
b <- matrix(rnorm(N), nrow=N, ncol=1, byrow=FALSE);b

# Create and display the inverse of matrix A
A.inv <- solve(A); A.inv

# Create and display the solution vector x
x <- A.inv%*% b;x

# Repeat the experiment for N <- 1000 
# What size of data matrix might the laptop allow? Try this matrix inversion up to the limit of your laptop.

# Create and display a 3x3 data matrix A
N <- 1000
time_start <- Sys.time()
A <- matrix(rnorm(N^2), nrow=N, ncol=N, byrow =FALSE); A

# Create and display Nx1 right hand side vector b
b <- matrix(rnorm(N), nrow=N, ncol=1, byrow=FALSE);b

# Create and display the inverse of matrix A
A.inv <- solve(A); A.inv

# Create and display the solution vector x
x <- A.inv%*% b;x
time_stop <- Sys.time()
time_duration <- time_stop - time_start
time_duration

############################################################
############################################################
### Problem 2
# Prepare an R script file for demonstrating an example of customer assessments of a selection of products
# p1, p2, .... from a company. The products are organized into NP=4 subgroups (notice that R is case sensi-
# tive), where each subgroup contains highly related products (products with the “same function”).




Prod_no <- c(3,1,3,2,4,2,1,2,2,4,2,3,4,1)# The products assessed by the 
Gender <- c("F","F","M","F","M","M","F","M","F","F","M","F","M","M")
Age <- c(37,81,57,79,17,18,67,45,57,79,17,18,67,45)
Geographical_location <- c("Copenhagen", "CopenhagenEast", "CopenhagenNorth","Copenhagen", "CopenhagenEast", "CopenhagenNorth","Copenhagen", "CopenhagenEast", "CopenhagenNorth","Copenhagen", "CopenhagenEast", "CopenhagenNorth","Copenhagen", "CopenhagenEast")
Likert_like <- c('Agree','Neither Agree','Disagree','Disagree','Agree','Neither Agree','Agree','Neither Agree','Agree','Neither Agree','Agree','Neither Agree','Disagree','NA')

Assessments <- data.frame(Prod_no, Gender, Age, Geographical_location, Likert_like) # Create d. frame.

# Need more work 



#Assessments$AgeCat[Assessments$Age >= 13 & Assessments$Age <= 19] <- "Teen"
#Assessments$AgeCat[Assessments$Age >= 20 & Assessments$Age <= 39] <- "Young"
#Assessments$AgeCat[Assessments$Age >= 40 & Assessments$Age <= 69] <- "MidAge"
#Assessments$AgeCat[Assessments$Age >= 70 & Assessments$Age <= 79] <- "MidAgeP"
#Assessments$AgeCat[Assessments$Age >= 80]  <- "Old"
#Assessments       # Display Assessments with age categories.

