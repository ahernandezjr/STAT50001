#' ---
#' title: "Test 1"
#' author: "Name: Alexander Hernandez | ID: 029531239"
#' date: "09/29/2022"
#' ---


library(UsingR)
library(VIM)


#' # 1) Short Answer Questions
#' ## a) Create the sequence: 15, 20, 20, 25, 25, 25, 30, 30, 30, 30, 35, 35, 35, 35, 35
rep(seq(15,35,5), times=c(1,2,3,4,5))

#' ## b) Generate 100 random numbers from a norm dist with mean=10, var=9. Only print first 5
head(rnorm(100, mean=5, sd=sqrt(9)))

#' ## c) 'brightness' dataset in 'UsingR'. How many obs and print first 5
length(brightness)
head(brightness)

#' ## d) 'CO2' dataset. Draw a side-by-side boxplot of CO2 uptake by location (quebec vs mississippi)
boxplot(CO2$uptake ~ CO2$Type, col=c(2,3),
        main= "CO2 Uptake by Location",
        xlab="Location", ylab="CO2 Uptake")

#' ## e) Import 'urine.txt' url into R and calculate average conductivity of urine
urine = read.table("https://www.stat.auckland.ac.nz/~wild/data/Rdatasets/txt/boot/urine.txt",
                   header=TRUE)
mean(urine$cond, na.rm=TRUE)


#' # 2) 'chickwts' dataset
#' ## a) How many variables are in the database?
length(names(chickwts))

#' ## b) What is the dimension of the dataset?
dim(chickwts)

#' ## c) Display in side-by-side boxplot using appropriate variable
boxplot(chickwts$weight ~ chickwts$feed, col=c(2,3,4,5,6,7),
        main= "Chicken Weights by Feed Supplement",
        xlab="Weight", ylab="Feed")
#' ## d) Construct a 95% confidence interval for the horsebean weight
t.test(chickwts$weight[chickwts$feed == "horsebean"],
       conf.level = .95)$conf.int


#' # 3) 'VIM' package, dataset 'tao'
#' ## a) How many variables and print their names
length(names(tao))
names(tao)

#' ## b) Remove missing values of NA's to create dataset, 'Clean'
Clean = na.omit(tao)

#' ## c) How many observations are in 'Clean'
nrow(Clean)

#' ## d) Test test the hypothesis that the mean Air.Temp is greater than 25
# Null: u = 25
# Alt:  u > 25
t.test(Clean$Air.Temp, alt="greater", mu=25)
# As the p-value is 1.741e-05,
# there is enough evidence to reject the null hypothesis.


#' # 4) Insurance Quotes
#' ## a) Calculate the summary statistics for both local and online quotes
quotes = read.csv("C:\\repos\\STAT 50001\\Test 1\\quotes.tsv",
                  sep='\t', header=TRUE)
summary(quotes$Local)
summary(quotes$Online)

#' ## b) Display the information with side-by-side boxplots
boxplot(quotes, col=c(2,3), main="Insurance Quotes: Local vs Online",
        ylab="Price Quote", xlab="Origin")

#' ## c) Does the data support that the online quotes are cheaper than the one provided by a local agent?
# Null: u(O) - u(L) = 0
# Alt:  u(O) - u(L) < 0
t.test(quotes$Local, quotes$Online, alt="less", mu=0)
# With a p-value of 0.6592,
# we do not have enough evidence to reject the null.


#' # 5) Chicago DOBA and Consumer Protection Taxi Data
#' ## a) Import the data in R
taxi = read.table("C:\\repos\\STAT 50001\\Test 1\\chicago_taxi.txt",
                  sep='\t', header=TRUE)
   
#' ## b) Draw a pie chart
taxi_payments = table(taxi$payment_type)

total = taxi_payments[1] + taxi_payments[2]
cash_percent = round(100 * taxi_payments[1] / total,
                     digits = 2)
credit_percent = round(100 * taxi_payments[2] / total,
                     digits = 2)
taxi_labels = c(paste("Cash", cash_percent, "%"),
                paste("Credit", credit_percent, "%"))
pie(taxi_payments, labels = taxi_labels, col=c(2,3),
    main="Taxi Payment Method Percents in Chicago")

#' ## c) Construct a 95% confidence interval for the tip amounts based on method of payment
t.test(taxi$tips ~ taxi$payment_type)$conf.int

#' ## d) Is there a significant difference in the tips amount by daytype?
# Null: u(d) - u(e)  = 0
# Alt:  u(d) - u(e) != 0
t.test(taxi$tips[taxi$daytype=="weekday"],
       taxi$tips[taxi$daytype=="weekend"], mu=0)
# With a p-value of 2.537e-08,
# we have enough evidence to reject the null hypothesis.

