#' ---
#' title: "Lab 19 R Script"
#' author: "Alexander Hernandez"
#' date: "11/03/2022"
#' output: pdf_document
#' ---

library(PASWR)
library(missMDA)

#' # 1) Biochemical Oxygen Demand from Holston River
#' ## a) Fit the regression model relating BOD to time
bod = read.table("C:\\repos\\STAT 50001\\Lab 19\\BOD.txt",
                 header=TRUE)
attach(bod)

model1 = lm(BOD ~ Days)
model1
# BOD = 0.6578 + 0.1781(Days)

#' ## b) What is the variance?
summary(model1)$sigma**2

#' \newpage
#' ## c) What is the expected BOD level at 15 days? 90% conf+pred int
predict(model1, data.frame(Days=15), interval="conf", level=0.9)
predict(model1, data.frame(Days=15), interval="pred",  level=0.9)
# With a 90% confidence interval (3.125933, 3.531346)
# and a 90% prediction interval of (2.764355, 3.892924),
# the predicted, fit value is 3.328639.

#' ## d) What change in mean BOD is expected when time changes by 3 days?
0.1781 * 3 
# Days * 3


#' \newpage
#' # 2) missMDA for Air Polution
#' ## a) Import the dataset
data(ozone)

#' ## b) Generate the list of variables included in the data
colnames(ozone)

#' ## c) Create a subset of the data with only the first 11 variables
oz = subset(ozone[0:11])

#' ## d) Fit a multiple linear regression model for maxO3 as a response var
attach(oz)
model2 = lm(maxO3 ~   T9 +  T12 +  T15 +  Ne9 +   Ne12 +
                    Ne15 +  Vx9 + Vx12 + Vx15 + maxO3v)
summary(model2)
# Only 'maxO3v' is significant with a 95% confidence.


#' # 3) Realtor
attach(vit2005)
plot(totalprice ~ area,
     col=ifelse(elevator=="1", "red", "black"),
     main= "Total Price of Apartments versus Area (Colored for Elevator)",
     pch=19)

model3 = lm(totalprice ~ area + elevator)
model3
# totalprice = 36174 + 2405(area) + 39091(elevator)

abline(36174,       2405, col="black", lwd=2)
abline(36174+39091, 2405, col="red", lwd=2)

summary(model3)
 # Both 'area' and presence of 'elevator' have a significant impact
# on apartment pricing in Victoria, Spain.
