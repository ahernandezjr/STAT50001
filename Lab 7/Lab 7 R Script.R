#' ---
#' title: "Lab 7 "
#' author: "Alexander Hernandez"
#' date: "09/15/2022"
#' ---


#' # 1) National HighWay System Designation Act
speed_file = file.choose()
speed_data = read.table(speed_file, sep='\t', header=TRUE)

#' ## a) Print the first 5 lines of the data
speed_data[1:5,]

#' ## b) Draw the histogram of the %changes fatalities from 95-96
hist(speed_data$FATALITIESCHANGE,
     main="States with Percentage Change of Interstate Fatlities from 1995-1996",
     xlab="Percentage Change", ylab="Frequency",
     xlim=range(-80,80))

#' ## c) Compare speed limit and fatalities with side by side boxplots
boxplot(speed_data$FATALITIESCHANGE ~ speed_data$INCREASe,
        col=c("red", "green"),
        main="Speed limit and Traffic Fatalities",
        xlab="Increase", ylab="Fatalities Change")


#' # 2) Crime Rates for 50 States in 2005
# http://datasets.flowingdata.com/crimeRatesByState2005.tsv

#' ## a) Import the dataset in R and name it 'crime'
crime = read.table('http://datasets.flowingdata.com/crimeRatesByState2005.tsv', sep='\t', header=TRUE)

#' ## b) How many variables are included in the data
ncol(crime)

#' ## c + d) Use code below to draw bubble plots + Add the name of the states using code
bubble_plot = symbols(crime$murder, crime$burglary, circles=crime$population)
text(crime$murder, crime$burglary, crime$state, cex=0.5, bg="red")


#' # 3) Hepatitis
#  https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data
#' ## a) Import the data in R
hepatitis = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", sep=',')

#' ## b) Replace missing values ('?') with 'NA'
new_hepatitis = hepatitis
new_hepatitis[new_hepatitis == '?'] = NA

#' ## c) How many observations contain missing information?
sum(is.na(new_hepatitis))


#' # 4) Generate 100 random numbers from a normal distribution with mean 40 and SD 5
rnorm(100, mean=40, sd=5)


#' # 5) Manuscript Data - Import to R without saving and determine dims
dim(read.table("http://lib.stat.cmu.edu/jcgs/tu",skip=3, header=TRUE))

