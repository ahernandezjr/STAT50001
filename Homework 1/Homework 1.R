#' ---
#' title: "Homework 1"
#' author: "Alexander Hernandez"
#' date: "09/08/2022"
#' ---


#' # 1) Calculate the following numerical results to the three decimal places
#' ## a)
log(3, base=exp(1)) + sqrt(2)*sin(pi) - exp(3)
# The natural log is used here instead of the log function, which requires base e to be used.

#' ## b)
2 * (5+3) - sqrt(6) + 9^2
# Simple functions are automatically done in order of operations by R, resulting in the results

#' ## c)
log(5, base=exp(1)) - exp(2) + 2^3
# Another example of the natural log with an e value used

#' ## d)
(9/2) * 4 - sqrt(10) + log(6, base=exp(1)) - exp(2)
# Similar to the last problem with more complex PEMDAS

#' ## e)
log(14,base=10) + log(14,base=exp(1)) + 47%%5
# log base 10 and natural log are used here, which require specifying in their method



#' # 2) Create the following vectors using rep function:
#' ## v1)  V1 = 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5
V1 = rep(c(1,2,3,4,5), 5)
V1
# default argument following the vector replicated the vector the specified amount of times

#' ## v2) V2= 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6
V2 = rep(c(1,2,3,4,5,6), each=4)
V2
# the each function allows the separate values repeated, instead of the entire vector

#' ## v3) V3= MATH, MATH, STAT, STAT, STAT, STAT, STAT, ECE, ECE, ECE, BIO, BIO
V3 = rep(c("MATH", "STAT", "ECE", "BIO"), times=c(2,5,3,2))
V3
# the times function allows choosing which variables to repeat



#' # 3) Use data from "What Does it Take to Heat a New Room?"
#' http://jse.amstat.org/datasets/utility.dat.txt
#' ## a) Import the data in R
utility_data = read.table('http://jse.amstat.org/datasets/utility.dat.txt')
# Can import webdata directly into R

#' ## b) How many variables are included in this dataset?
ncol(utility_data)
# Variables are sorted by the number of columns

#' ## c) The missing values in this dataset are denoted by *. Remove them.
new_utility_data = utility_data[
  (utility_data$V1 != '*') &
  (utility_data$V2 != '*') & 
  (utility_data$V3 != '*') & 
  (utility_data$V4 != '*') & 
  (utility_data$V5 != '*') & 
  (utility_data$V6 != '*') & 
  (utility_data$V7 != '*') & 
  (utility_data$V8 != '*') & 
  (utility_data$V9 != '*') & 
  (utility_data$V10 != '*') & 
  (utility_data$V11 != '*') & 
  (utility_data$V12 != '*') & 
  (utility_data$V13 != '*'),]

new_utility_data
# This looks cumbersome, but it goes through every variable and, if it sees an 'asterisk/*',
# removes that line from the data.



#' # 4) Extract 2004 (2nd tab) from the CPSS.XLS data and determine dimension
library('readxl')
pop_survey_file = file.choose()
pop_survey = read_excel(pop_survey_file, sheet='CPSSW4')
dim(pop_survey)
# since there are two cases, the sheet must be specified in the read_excel function after
# specifying path



#' # 5) Use R to solve the following system of equations:
C = matrix(c(2,1,2,1,1, 1,-1,1,-3,2, 1,2,-1,1,-1, -3,1,2,2,3, 1,-1,1,-1,-1), nrow=5, ncol=5)
Y = matrix(c(12,1,-2,-9,0), nrow=5, ncol=1)
C
Y

D = solve(C,Y)
D
# Taking the values of the variables in the system of equations forms a matrix.
# Matrices are created in a top to bottom, left to right fashion, with rows and cols specified.
# Another matrix takes the right side of the system and solve takes both to solve it.



#' # 6) Print the first 50 numbers of the fibbonochi sequence
Fibonacci <- numeric(50)
Fibonacci[1] <- Fibonacci[2] <- 1
for (i in 3:length(Fibonacci)) Fibonacci[i] <- Fibonacci[i-2] + Fibonacci[i-1]
Fibonacci
# The function used was provided in the hint to question 6. Simple changing the vector to 50
# length and repeating the process until 50 gets all 50 numbers.



#' # 7) Test scores of Fifteen students in Test 1 and Test 2:
sn = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
test1 = c(56,78,87,89,95,98,NA,78,87,98,54,89,78,98,97)
test2 = c(86,67,78,89,87,67,94,78,81,83,78,NA,93,98,100)

df1 = data.frame(sn, test1,test2)
df1
#' ## a) How many students have their test 1 score greater than 80 ?
nrow(df1[df1$test1>80 & !is.na(df1$test1),])
# This function gets all test1 scores above 80 and removes instances of 'NA'.
# nrow counts how many students

#' ## b) How many students have their test 2 score greater than 85 ?
nrow(df1[df1$test2>85 & !is.na(df1$test2),])
# This function gets all test2 scores above 85 and removes instances of 'NA'.
# nrow counts how many students

#' ## c) Did all fifteen students take both tests?
print("Which student(s) did not take test 1:")
which(is.na(df1$test1))
print("Which student(s) did not take test 2:")
which(is.na(df1$test2))
# student 7 did not take test 1 and student 12 did not take test 2

#' ## d) How many students did better in the second test than the first test?
nrow(df1[(df1$test2 > df1$test1) & !is.na(df1$test2) ,])
# Conditional that selects results with greater test2 than test 1 and
# remove 'NA' results of test2
# Any score is higher than not taking test1 so student 7 is included.

#' ## e) How many students have the same score in the first and second test?
nrow(df1[(df1$test2==df1$test1) & !is.na(df1$test1) & !is.na(df1$test2) ,])
# Checks if the first and second scores are the same, while ignoring 'NA'



#' # 8) Create the following matrix with column and row names
M = matrix(c(1:20), nrow=4, ncol=5)
rownames(M) = c('Experiment.1','Experiment.2','Experiment.3','Experiment.4')
colnames(M) = c('column-1','column-2','column-3','column-4','column-5')
M
# A spread of numbers 1:20 works can make this matrix. Rownames and
# colnames can be used to set them

#' ## a) Determine the dimension of the matrix M
dim(M)
# dim function tells the number of rows and cols

#' ## b) Select the first two row of the matrix M
M[0:2,]
# Slice takes the entered number of rows first (and an optional columns)

#' ## c) Calculate the sum of all columns of the matrix M
colSums(M)
# colSums sums the vertical numbers of a column

#' ## d) Calculate the sum of all rows of the matrix M
rowSums(M)
# rowSums calculates the horizontal sum of a row

#' ## e) Use “sample” to shuffle the elements of each row of the matrix M
shuffled_M = apply(M, 1, sample)
shuffled_M
#This successfully applies the shuffling to the rows in the matrix and
# is correct, but apply rotates the matrix for some reason
