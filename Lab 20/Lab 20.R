#' ---
#' title: "Lab 20 R Script"
#' author: "Alexander Hernandez"
#' date: "11/10/2022"
#' output: pdf_document
#' ---





#' # 1) Mutual and Stock Firms
#' ## a) Draw a Scatterplot of "Size of Firm vs Number of month elapsed"
stock = read.table("C:\\repos\\STAT 50001\\Lab 20\\stock.txt",
                   header=TRUE)
attach(stock)
plot(y, X1,
     col=ifelse(X2=="Mutual", "red", "blue"),
     pch=ifelse(X2=="Mutual", "M", "S"),
     main="Size of the firm vs. Number of Months Elapsed",
     ylab="Size of the firm", xlab="Number of Months")

#' ## b) Fit a reression model with indicator variable and write out model
model1 = lm(X1~y+X2)
model1
# y = 314.079 - 8.699(y)  if "Firm" is Mutual
# y = 387.155 - 8.699(y)  if "Firm" is Stock
plot(y, X1,
     col=ifelse(X2=="Mutual", "red", "blue"),
     pch=ifelse(X2=="Mutual", "M", "S"),
     main="Size of the firm vs. Number of Months Elapsed",
     ylab="Size of the firm", xlab="Number of Months")
abline(314.079, -8.699, col="red", lwd=2)
abline(387.155, -8.699, col="blue", lwd=2)


#' # 2) Weight, Health, and Fitness in "BodyFat"
#' ## a) Import "BodyFat" and access Variable Names
library(Lock5withR)
names(BodyFat)
attach(BodyFat)

#' ## b) Fit a model to predict Bodyfat using Height and Weight. Find predictors
model2b = lm(Bodyfat ~ Height + Weight)
summary(model2b)
# All predictors are valid

#' ## c) Add Abdomen as a third predictor
model2b = lm(Bodyfat ~ Height + Weight + Abdomen)
summary(model2b)
# With abdomen included, Height has a lower p-value and thus less relevant

#' ## d) Interpret the coefficient of Abdomen
# Abdomen's coefficient value shows that it affects bodyfat
# by 1.0747 per unit when predicting a value


#' # 3) Children Measurements
children = read.table("C:\\repos\\STAT 50001\\Lab 20\\pediatrician.txt",
                   header=TRUE)
attach(children)

#' ## a) Construct a correlation matrix. Multicollinearity?
cor(children)
# Yes, we do not want a relationship between Height and Weight

#' ## b) Find the LEast-squares regression equation with response variable = head circumference
model3 = lm(Head_Circumference ~ Height + Weight)
model3
# Head Circumference = 18.82425 + 0.78634(Height) + 0.01281(Weight)

#' ## c) Construct 95% confidence and prediction intervals for 27.5 height and 285 weight
predict(model3, data.frame(Height=27.5, Weight=285), interval = "conf")
predict(model3, data.frame(Height=27.5, Weight=285), interval = "pred")

#' ## d) Perform the residual analysis of the model
library(MASS)
b=boxcox(model3, lambda=seq(-10,10))

par(mfrow=c(2,2))
plot(model3)
# No transformation is needed
# There are further contributing factors that affect head circumference
