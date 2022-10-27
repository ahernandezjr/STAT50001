#' ---
#' title: "Lab 17 R Script"
#' author: "Alexander Hernandez"
#' date: "10/27/2022"
#' output: pdf_document
#' ---


#' # 1) Beer vs BAL
#' ## a) Make a scatterplot with a regression line
beers = c(5 ,    2     ,   9 ,    8,   3 ,   7 ,    3,     5,    3 ,   5)
BAL = c(0.10 , 0.03 , 0.19 , 0.12 , 0.04 , 0.095,  0.07,  0.06 , 0.02 , 0.05)

plot(beers, BAL,
     main="Beers vs MAL")
model = lm(BAL ~ beers)
model
# BAL =- 0.0185 + 0.0192*beers
abline(model)

#' ## b) Calculate 95% confidence interval for the model parameters
confint(model)

#' ## c) State the estimated linear regression model
summary(model)
# multiple R-squared: 0.789


#' # 2) Annual sales
#' ## a) Prepare a scatterplot of the data
year = c(0 , 1,  2 , 3,  4,  5 , 6 , 7,  8,  9)
sales = c(98 , 135 , 162 , 178 , 221 , 232 , 283 , 300 , 374,  395)
plot(year, sales, main="Year vs Sales (by thousands of units)")

#' ## b) no question 2.b.

#' ## c) State the regression line and add to scatterplot
model=lm(sales ~ year)
model
# sales = 91.56 + 32.50*year
plot(year, sales, main="Year vs Sales (by thousands of units)")
abline(91.56, 32.5)

#' ## d) Use the model to predict sales in the 10th years. 90 and 95 confidence
predict(model, data.frame(x=10), interval="conf", level=0.95)
# 95% confidence interval: (392.9089, 440.1578)
predict(model, data.frame(x=10), interval="conf", level=0.9)
# 90% confidence interval: (397.4827, 435.5839)


#' # 3) Age by Liver Volume per Unit of Body Weight
#' ## a) Prepare a scatter plot of the data
age = c(0.5, 0.7, 2.5, 4.1, 5.9, 6.1, 7, 8.2, 10, 10.1, 10.9, 11.5, 12.1, 14.1, 15)
vol = c(41, 55, 41, 39, 50, 32, 41, 42, 26, 35, 25, 31, 31, 29, 23)
plot(age, vol, main="Age vs Liver Volume Per Unit of Body Weight")

#' ## b) no question for 3.b.

#' ## c) 
model=lm(vol ~ age)
model
# vol = 48.540 - 1.576*age
abline(model)

#' ## d) Use the model to predict the liver volume of an 8 year old child
predict(model, data.frame(age=8))

#' ## e) Construct a 90% confidence interval for this prediction
predict(model, data.frame(age=8), interval="conf", level=0.9)
# 90% confidence interval: (33.24692, 38.61321)

#' ## f) Construct a 90% prediction interval for this prediction
predict(model, data.frame(age=8), interval="pred",  level=0.9)
# 90% prediction interval: (25.1994, 46.66072)

