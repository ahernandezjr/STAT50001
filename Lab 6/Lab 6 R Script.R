#' ---
#' title: "Lab 6 "
#' author: "Alexander Hernandez"
#' date: "09/13/2022"
#' ---


#' # 1) The 'mtcars' data is provided in the Base package in R
#' ## a) Identify the dimension of the mtcars data
dim(mtcars)

#' ## b) Create a .csv file of the mtcars dataset
write.csv(mtcars, 'C:/repos/STAT 50001/Lab 6/mtcars.csv')

#' # 2) The Weight of the Euro Coins
# http://jse.amstat.org/datasets/euroweight.dat.txt
#' ## a) Import the euroweight.dat.txt data in R
euro_data = read.table('http://jse.amstat.org/datasets/euroweight.dat.txt')

#' ## b) Select the third column batch of the coins
euro_data$V3

#' ## c) Create the frequency table of the batch of the coins
prob.cut = cut(euro_data$V3, breaks=seq(0,8))
freqtable = table(prob.cut)
freqtable

#' ## d) calculate the aggregate means of each batch
euro_mean <- aggregate(euro_data$V2, list(euro_data$V3), mean)
euro_mean

#' # 3) Create a pie chart displaying the information below and save it
students = c(20, 15, 9, 5)
names(students) = c("Apartment", "Dorm", "House", "Sorority/Fraternity")
pie(students, main="Student Housing")

#' # 4) Go to http://jse.amstat.org/datasets/babyboom.dat.txt
#' ## a) Import the babyboom.dat.txt data
babyboom = read.table('http://jse.amstat.org/datasets/babyboom.dat.txt')

#' ## b) Select the column with the birth weight of the new born babies
babyboom$V3

#' ## c) Create a histogram of the subject data
hist(x=babyboom$V3,
     main="Histogram of Baby Weights",
     xlab="Baby Weight",
     ylab="Frequency", ylim=c(0,20))


#' # 5) Link below provides data file 'homes'
# http://www.principlesofeconometrics.com/poe4/data/stata/homes.dta
#' ## a) Import the data in R
library(readstata13)
homes_file = file.choose()
homes_data = read.dta13(homes_file)

#' ## b) Calculate the five number sum of homes and irate
summary(homes_data)

#' ## c) Draw a scatterplot to display the data
plot(homes_data, main="Homes vs Irate")
