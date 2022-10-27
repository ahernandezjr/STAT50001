#' ---
#' title: "Lab 8"
#' author: "Alexander Hernandez"
#' date: "09/20/2022"
#' ---


#' # 1) Generate 500 random numbers from a standard normal distribution and display them using a histogram.
q1 = rnorm(500)
hist(q1,
     main='500 Random Numbers of Normal Distribution',
     xlab='Random Numbers',
     col=c(1,2,3,4))


#' # 2) The National Highway System designation Act
speed = read.table('C:/repos/STAT 50001/Lab 8/speed.txt',
                   header=TRUE, sep='\t')
x = factor(speed$INCREASe, c("Yes", "No"))
boxplot(speed$FATALITIESCHANGE ~ x,
        col=c("red", "green"),
        main="Speed limit and Traffic Fatalities",
        xlab="Increase", ylab="Fatalities Change")


#' # 3) All registered elevators in New York City are provided in the  link below
# https://www.kaggle.com/new-york-city/nyc-elevators/discussion/39528
#' ## a) Import the data in R.
nyc = read.csv('C:/repos/STAT 50001/Lab 8/NYC.csv', header=TRUE)

#' ## b) How many elevators are active?
nrow(nyc[nyc$Device.Status == "A",])

#' ## c) How many elavators are active in Manhattan borough?
s = subset(nyc, Borough=="Manhattan")
nrow(s[s$Device.Status == "A",])


#' # 4) Plot pdf of a standard normal distribution by generating data in (-4.4)
curve(dnorm, -4, 4, xlab= "Probability", ylab="Value")
axis(1, at=-4:4, labels=c(-4,-3,-2,-1,0,1,2,3,4))


#' # 5) Generate 100 random numbers from a normal dist with mean=5 and var=64
rnorm(100, mean=5, sd=sqrt(64))

#' # 6) Generate 100 random sample from each of the following distributino and draw their normal qq plots
#' ## a) Normal
qqnorm(rnorm(100), main="Normal Distribution QQ Plot")

#' ## b) Student's t (df=20)
qqnorm(rt(100, 20), main="T Distribution QQ Plot")

#' ## c) Exponential (rate=1)
qqnorm(rexp(100), main="Exponential Distribution QQ Plot")

#' ## d) Uniform
qqnorm(runif(100), main="Uniform Distribution QQ Plot")
