#' ---
#' title: "Lab 2"
#' author: "Alexander Hernandez"
#' date: "08/30/22"
#' ---

#' ## 1) Create the following vectors using rep function in R:
V1 = rep(c(1,2,3,4,5), 5)
V2 = rep(c(1,2,3,4,5,6), each=4)
V3 = rep(c(5,10,15,20,25), 1:5)
V4 = rep(c("Math", "CS", "STAT", "PHY"), times=c(2,2,3,3))

#' a) V1
V1
#' b) V2
V2
#' c) V3
V3
#' d) V4
noquote(V4)


#' ***
#' ## 2) Import the data below in R using scan function
# Scan-ed from file (for report)
# Data in file: 2 4 5 6 7 8 9 2 3 4 5 6 77 89 45 67 8 9 0 12 
scan_import = scan(file="C:/repos/STAT 50001/Lab 2/scan_data.txt", nmax=20)
scan_import

#' ***
#' ## 3) Generate the following sequence of numbers
a = seq(1,50)
b = seq(2,50,2)
c = LETTERS[seq(from=1, to=8)]
d = letters[seq(from=5, to=12)]

#' a) a
a
#' b) b
b
#' c) c
c
#' d) d
noquote(d)

#' ***
#' ## 4) Suppose we have the data below
data = c(2,5,7,8,9,3,5,8,67,45, 1,NA, 34,23,12,90)
data

#' a) How many observations are there in the data set?
length(data)
#' b) Is there any missing value?
any(is.na(data))
#' c) Identify the location of the missing value
which(is.na(data))
#' d.1) Identify the smallest value
data[which.min(data)]
#' d.2) Identify the smallest value position
which.min(data)
#' d.3) Identify the largest value
data[which.max(data)]
#' d.4) Identify the largest value position
which.max(data)