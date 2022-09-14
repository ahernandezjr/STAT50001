#' ---
#' title: "Lab 4"
#' author: "Alexander Hernandez"
#' date: "09/06/22"
#' ---

#' # 1) Babyboomer data
#' ## Time of birth (24-HOUR), Sex (1-GIRL,2-BOY), Birth Weight (GRAMS), Number of minutes after midnight
#' ## a) Import the 'babyboom.dat.txt'
babyboom_data = read.table('http://jse.amstat.org/datasets/babyboom.dat.txt')

#' ## b) How many observations are recorded?
nrow(babyboom_data)

#' ## c) Print first 5 observations
head(babyboom_data, 5)

#' ## d) Print last 5 observations
tail(babyboom_data, 5)


#' # 2) Car Crashes with harm (people or property) and at least 1 vehicle towed
#' ## a) Import the data in R using appropriate R code
file_car_crashes = file.choose()
car_crashes = read.csv(file_car_crashes)

#' ## b) Print the first 5 observations
head(car_crashes, 5)


#' # 3) consider a data set where the columns are separated by $
#' ## Data in problem-3-data.csv
file_problem3 = file.choose()
test_data = read.table(file_problem3, sep="$", header=T)
test_data


#' # 4) Baby Weight
library(readstata13)
file_bweight = file.choose()
baby_weight = read.dta13(file_bweight)

#' ## a) Identify dimensions of data
dim(baby_weight)

#' ## b) Extract the variables included in the datasets
colnames(baby_weight)


#' # 5) Biochemist publications - unable to complete
#' ## Data: http://www.stata-press.com/data/lf2/couart2.dta
#' ## Neither the 'readstat13' or 'foreign' packages can read this file
#' ## Claims the 20kb file is 8.1 GB
#' ## a)	Import the data in R - Showing how you would do this without errors
# webdata_publications = read.dta("https://www.stata-press.com/data/lf2/couart2.dta")

#' Unable to use direct read from url due to error:
#' the 'wininet' method is deprecated for http:// and https:// URLs
#' No alternative argument in read.dta or read.dta13 method

#' ## b)	List the variables included in the data
# colnames(webdata_publications)

#' ## c)	State the dimension of the data
# dim(webdata_publications)