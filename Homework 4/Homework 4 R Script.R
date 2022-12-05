#' ---
#' title: "Homework 4 R Script"
#' author: "Alexander Hernandez"
#' date: "11/17/2022"
#' output: pdf_document
#' ---


library(PASWR)
library(MASS)
library(ISLR)
library(UsingR)


#' \newpage
#' # 1) SimDataST
#' ## a) Import the data in R and draw a scatterplot using x1 and Y1
attach(SimDataST)
plot(x1, Y1,
     main = "SimDataST variables Y1 vs x1")

#' ## b) Fit a simple linear regression model using Y1 as response and x1 as regressor. Assess the residual plots of the model
model1b = lm(Y1 ~ x1)
summary(model1b)
# Y1 = -12.5720 + 10.9452(x1)

par(mfrow=c(2,2))
plot(model1b)
# The residual plots do not look valid.

#' ## c) Determine a lambda value using Box-Cox transformation to improve the model
boxcox(model1b)
# A lambda value around 0.1 would improve the model

#' ## d) Fit a simple regression model after transformation
model1d = lm(Y1**0.1 ~ x1)
summary(model1d)
# Y1 = -12.5720 + 10.9452(x1)

#' ## e) Compare the results in b and d. Was the transformation worth it?
par(mfrow=c(2,2))
plot(model1d)
# The plots have improved and thus the transformation is worth it


#' \newpage
#' # 2) Cars93
#' ## a) How many variables are included in the dataset
names(Cars93)
length(names(Cars93))

#' ## b) Fit a regression model for MPG.city using the numerical variables EngineZie, Weight, Passengers, and Price
attach(Cars93)
model2b = lm(MPG.city ~ EngineSize +
                        Weight +
                        Passengers +
                        Price)

model2b
# MPG.city = 46.389 + 0.196(EngineSize) - 0.008(Weight) +
#            0.270(Passengers) - 0.036(Price)

#' ## c) Which variables are marked as statistically significant by the marginal t-test?
summary(model2b)
# Weight is the only variable marked as statistically significant

#' ## d) Which model is selected by AIC criteria?
model2d = lm(MPG.city ~ Weight)

AIC(model2b, k=5)
AIC(model2d, k=2)
# 496.7923 and 474.6028
# Given two models, one using EngineSize, Weight, Passengers, and Price and
# another using only the statistically significant variable Weight,
# the model containing only Weight has a lower AIC and thus is chosen.


#' \newpage
#' # 3) Home Ownership to Family Income
#' ## a) Fit a simple logistic regression model for the subject data and display with the scatterplot
homedata = read.csv("C:\\repos\\STAT 50001\\Homework 4\\Homedata.csv")
homedata
attach(homedata)
model3 = glm(homeownership ~ Income,
                     family = binomial(logit))
model3
# homeownership = [1 + exp( 8.7395139 - 0.0002009(Income) )]^-1

plot(homedata, main = "Homeownership vs Income",
     col=ifelse(homeownership==0, "red", "blue"),
)
curve(predict(model3, data.frame(Income=x), type="resp"), add=TRUE)
points(Income, fitted(model3), pch=20)

#' ## b) What is the estimated porbability that a family with an income of $45,000 owns a house?
predict(model3, data.frame(Income=45000), type="resp")
# There is a 0.5747 chance a family of income $45,000 owns a house.


#' \newpage
#' # 4) Defaulting on a Credit Card Versus Annual Income and Balance
attach(Default)
model4 = glm(default ~ balance,
             family = binomial(logit))
model4
# default = [1 + exp( 10.651331 - 0.005499(balance) )]^-1


#' \newpage
#' # 5) KeepKidsHealthy - fetal smoking and malnutrition on premature births
#' ## a) Extract the variables of interest: gestation, smoking status, mother's height and weight, and birth weight of the babies
b = babies[ , c("gestation", "smoke", "ht", "wt1", "wt")]
attach(b)

#' ## b) Clean the data set as there are some missing values coded as 9, 99, or 999
bedit = b
is.na(bedit) = bedit == 9 | bedit == 99 | bedit == 999
bedit = na.omit(bedit)

#' ## c) Calculate the BMI of mothers
bedit["BMI"] = (bedit["wt1"] * 0.453592) / (bedit["ht"] * 0.0254)
head(bedit["BMI"], 5)

#' ## d) Create indicator variable (1 for premature and 0 for not premature) babies
bedit["premature"] = with(bedit, ifelse(bedit["gestation"] < 259, 1, 0))

#' ## e) Fit a logistic regression model with smoke and BMI as a predictor variable and premature as a response variable
attach(bedit)
model5 = glm(premature ~ smoke + BMI,
             family = binomial(logit))
model5
# homeownership = [1 + exp( 3.53570 - 0.09376(smoke) - 0.02491(BMI) )]^-1
