#' ---
#' title: "Homework 3"
#' author: "Alexander Hernandez"
#' date: "11/03/2022"
#' output:
#'    pdf_document:
#'       latex_engine: xelatex
#' ---

library(pwr)
library(MASS)


#' # 1) "HairEyeColor" of 592 students
#' ## a) Is hair color independent of eye color for men?
# H0: Hair and eye color are independent
# Ha: Hair and eye color are   dependent
chisq.test(HairEyeColor[, , "Male"])
# With a p-value of 4.447e-06, there is enough evidence to reject the null,
# and it can be claimed that hair color is dependent on eye color for males.

#' ## b) Is hair color independent of eye color for women?
# H0: Hair and eye color are independent
# Ha: Hair and eye color are   dependent
chisq.test(HairEyeColor[, , "Female"])
# With a p-value of 2.2e-16, there is enough evidence to reject the null,
# and it can be claimed that hair color is dependent on eye color for females.


#' # 2) Diets A and B. How many subjects are needed in each group, assuming equal sized groups (a=0.05, Power=0.8)?
pwr.t.test(d=(0-10)/16.03, power=.8, sig.level=0.05, type="two.sample", alt="two.sided")
# Subjects required for each group is at least 21.


#' \newpage
#' # 3) Fire Damage versus distance of fire from Fire Station
fires = read.table("C:\\repos\\STAT 50001\\Homework 3\\q2.txt",
                   header=TRUE)
attach(fires)

#' ## a) Fit a simple linear regression model and analyze the residual plots.
model_3a = lm(Damage ~ Distance)
model_3a

par(mfrow=c(2,2))
plot(model_3a)
# Plot 4 demonstrates that the residuals may not exhibit a linear pattern.

#' \newpage
par(mfrow=c(1,1))
plot(fires,
     main="Damage versus Distance (in thousand of dollars)")
abline(model_3a)

#' ## b) What is the expected Damage if the fire station is 4 miles away?
predict(model_3a, data.frame(Distance = 4), interval="conf", level=0.95)
# Expected damage interval with 95% confidence: (28.56955, 31.36365)

#' \newpage
#' ## c) Use the Box-Cox transformation to choose an appropriate value of λ to improve the model.
boxcox(model_3a)
# The graph shows a lambda value of 0.5 should be chosen.

#' ## d) Fit a simple linear regression model after transformation.
model_3b = lm(Damage**0.5 ~ Distance)
model_3b

#' \newpage
#' ## e) Compare and contrast models in (a) and (d).
summary(model_3a)$r.squared
summary(model_3b)$r.squared
# The new model in part d has a lambda=0.5 with a higher R**2 of 0.9315393
# compared to the part a model which has a R**2 of 0.9265571.
# This shows that the transformed model may be a better fit.


#' \newpage
#' # 4) Website weeks versus hits/visits
#' ## a) Display the data using a scatterplot.
website = read.table("C:\\repos\\STAT 50001\\Homework 3\\q4.txt",
                   header=TRUE)
plot(website,
     main= "Website Hits/Visits versus Weeks")

#' ## b) Calculate the correlation coefficient to measure the association between the week and the number of hits on the website. Check whether rank correlation is more appropriate than Pearson correlation
# Pearson Test
cor.test(website$Week, website$Hits)

# Spearman Rank Test
cor.test(website$Week, website$Hits,
         method="spearman", exact=FALSE)

#' ## c) Test for the significance of the correlation at 0.05 level.
# Ho: Correlation coefficient is not significantly different from 0 (p = 0)
# Ha: Correlation coefficient is significantly different from 0     (p != 0)
# Given the values are not ranked/ordinal, the Pearson Correlation Test
# is used, which results in using the p-value, 0.357. This means
# the null hypothesis is rejected, claiming there is a significant
# linear relationship between website weeks and website hits.


#' \newpage
#' # 5) Cars: speed versus distance
#' ## a) Display the data using scatter plot.
plot(cars, main="Car Distance versus Speed")

#' ## b) Fit a simple regression model using speed as a predictor variable.
attach(cars)
model_5 = lm(dist ~ speed)
model_5
# dist = -17.579 + 3.932(speed)

#' \newpage
#' ## c) Add the fitted line to the scatter plot.
plot(cars, main="Car Distance versus Speed")
abline(model_5)

#' ## d) Calculate the residuals and fitted values and print only first five observations of the residuals and fitted values.
head(resid(model_5),  5)
head(fitted(model_5), 5)

#' \newpage
#' ## e) Create a scatter plot of the residuals and fitted values.
plot(fitted(model_5), resid(model_5),
     col  = c(2,3),
     main = "Fitted and Residual Values of Car Distance versus Speed")

#' \newpage
#' ## f) Assuming that no intercept model is appropriate fit a simple linear regression model.
model_5 = lm(dist ~ -1 + speed)
plot(cars, main="Car Distance vs Speed")
abline(model_5)

#' ## g) Calculate and compare the coefficient of determination for both with intercept and no-intercept models.
summary(lm(dist ~ speed))$r.squared
summary(lm(dist ~ -1 + speed))$r.squared
# Compared to the intercept R**2 of 0.65, the no-intercept
# R**2 is significantly stronger at 0.90 (rounded),
# which indicates a valid strength of data.

#' \newpage
#' ## h) Using your fitted model predict the stopping distance for a car with an speed of 21 mph.
predict(lm(dist ~ speed), data.frame(speed=21), interval="conf", level=0.9)
# Intercept model with a 90% confidence interval of (59.65934, 70.34364),
# predicts a fit value of 65.00149.
predict(lm(dist ~ -1 + speed), data.frame(speed=21), interval="conf", level=0.9)
# No-intercept model with a 90% confidence interval of (56.11453, 66.06902),
# predicts a fit value of 61.09178.
