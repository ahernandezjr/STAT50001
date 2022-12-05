#' ---
#' title: "Lab 18 R Script"
#' author: "Alexander Hernandez"
#' date: "11/01/2022"
#' output: pdf_document
#' ---

library(UsingR)
library(ggplot2)
library(MASS)

#' \newpage
#' # 1) For the given values of x and y, fit a linear model and display it
#' ## a) Linear Model:
x = c(1, 2, 2, 3, 4, 4, 5, 6, 6, 8, 9, 9, 11, 12, 12)
y = c(6, 7, 7, 9, 12, 13, 13, 15, 16, 19, 22, 23, 23, 25, 26)
values = data.frame(x, y)

model = lm(values$y ~ values$x)
model

#' ## b) ggplot:
ggplot(values, aes(x, y)) +
  geom_point() +
  geom_smooth(method='lm')


#' \newpage
#' # 2) GDP of the US from 1950-2013
#' ## a) Display the data using a scatterplot
gdp = read.table("https://media.pearsoncmg.com/aw/aw_sharpe_business_3/datasets/txt/GDP_2013.txt",
                 skip=1)
colnames(gdp) = c("Year", "GDP")

plot(gdp,
     main = "Year versus GPD of United States from years 1950-2013")

#' ## b) Fit a simple linear regression model
model = lm(gdp$GDP ~ gdp$Year)
model

#' \newpage
#' ## c) Add the fitted line to the scatter plot
plot(gdp,
     main = "Year versus GPD of United States from years 1950-2013")
abline(model)

#' ## d) Determine the coefficient of determination
summary(model)$r.squared

#' \newpage
#' ## e) Analyze the residual plots. Is your model questionable?
par(mfrow=c(2,2))
plot(model)
# The models are questionable.

#' \newpage
#' ## f) Use Box-Cox Transformation to see whether this model can be improved
boxcox(model)
