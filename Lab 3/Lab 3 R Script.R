#' ---
#' title: "Lab 3"
#' author: "Alexander Hernandez"
#' date: "09/01/22"
#' ---

# -----------------------
#' # 1) Create the following matrix using R.
matrix(c(3,3,2,1,-2,20,12,2,3,6,-17,8,-1,8,12,-9,0,9,5,10), nrow=4, ncol=5)


# -----------------------
#' # 2) The data below contains missing values.
# 7,4,5,6,23,8,NA,34,23,56,NA,6,4,58,12,17,23, -10
list_data = c(7,4,5,6,23,8,NA,34,23,56,NA,6,4,58,12,17,23, -10)

#' ## a) Remove the missing values
new_data = list_data[!is.na(list_data)]
new_data

#' ## b) How many observations are less than 10 
new_data[new_data<10]
length(new_data[new_data<10])

 
# -----------------------
#' # 3) Create a sequence of numbers from 1 to 10 and insert comma using the r code >paste(data,collapse=",")
paste(seq(1:10), collapse=",")

 
# -----------------------
#' # 4) Consider the following two data sets:
df1 = data.frame("Name"=c("Tony", "Drew", "Nancy"), Age=c(21,25,27), "Major"=c("Math","Math", "STAT"), "Gender"=c("Male", "Male", "Female"))
df2 = data.frame("Name"=c("Jaw", "Amanda", "George"), Age=c(23,28,27), "Major"=c("CS","Math", "STAT"), "Gender"=c("Male", "Female", "Male"))

#' ## a) Create two different data frames from the above observations and convert them to a single data frame.
df_combined = rbind(df1, df2)
df_combined

#' ## b) Sort the new data frame using Age.
df_combined = df_combined[order(df_combined$Age), ]
df_combined

# -----------------------
#' # 5) If A = [] and B = [] then calculate A+B and A-B
A = matrix(c(3,2,6,2,-4,0,1,3,-1,-3,0,5), nrow=3, ncol=4)
B = matrix(c(2,-4,2,-3,-5,4,7,0,-3,6,-2,5), nrow=3, ncol=4)

A
B

added = A+B
subtracted = A-B

added
subtracted

# -----------------------
#' # 6) using the matrix method [solve], solve this: 3x - y = 5, -4x + 2y = -9.

C = matrix(c(3,-4,-1,2), nrow=2, ncol=2)
Y = matrix(c(5,-9), nrow=2, ncol=1)

D = solve(C,Y)
D
