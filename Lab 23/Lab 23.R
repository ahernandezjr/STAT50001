#' ---
#' title: "Lab 23 R Script"
#' author: "Alexander Hernandez"
#' date: "11/17/2022"
#' output: pdf_document
#' ---


#' # 1) Clinical Trial Drop Outs
#' ## a)Import the data and determine its dimension
trials = read.table("https://media.pearsoncmg.com/aw/aw_sharpe_business_3/datasets/txt/Clinical%20Trials.txt",
                    sep="\t", header=TRUE)
dim(trials)

#' ## b) The Missing values are left blank. Clean the data by removing them
trials_new = na.omit(trials)
attach(trials_new)

#' ## c) Fit a multiple logistic regression model using Age and HDRS as predictor variables
model1 = glm(DRP ~ AGE + HD2114,
            family = binomial(logit))
summary(model1)
# DRP = [1 + exp( 0.44197 + 0.03790(AGE) -0.04682(HD2114) )]^-1

#' ## d) What is the predicted dropout probability of a 30 year old patient with HDRS score of 30?
predict(model1, data.frame(AGE=30, HD2114=30), type="resp")
# Dropout Probability of 30 year-old with HDRS score of 30: 0.4564631


#' \newpage
#' # 2) Respiratory Fucnction and Smoking
#' ## a) Import the data and identify its dimension
resp = read.table("http://jse.amstat.org/datasets/fev.dat.txt")
colnames(resp) = c("age", "fev", "height", "sex", "smoke")
attach(resp)

dim(resp)

#' ## b) Test whether smoking status differ by gender (2-sample t-test)
# H0: There is no significant difference of smoking status between sexes
# Ha: There is a significant different of smoking status between sexes
table(sex)
xtabs(~sex + smoke)
prop.test(c(39, 26), n=c(318, 336), correct=F)
# With a p-value of 0.05316 and a standard confidence of 95%,
# there is not enough evidence to reject the null
# and claim there is no difference in smoking status between sexes.

#' ## c) Fit a multiple linear regression model to study fev, using age, height, sex, and smoking status as predictor variables
model2 = lm(fev ~ age + height + sex + smoke)
summary(model2)
# fev = -4.457 + 0.066(age) + 0.104(height) + 0.157(sex) - (0.087(smoke))

#' ## d) Use model to predict the FEV of a 50 inches tall, 12 year-old girl who is not a smoker. Construct a 95% confidence interval
predict(model2, data.frame(age=12, height=50, sex=0, smoke=0), interval="conf", level=0.95)
# Predicted FEV is: 1.539109
# Confidence Interval: (1.399775, 1.678443)


#' \newpage
#' # 3) Real Estate on 1115 Houses
homes = read.csv("C:\\repos\\STAT 50001\\Lab 23\\home.csv")
attach(homes)

model3 = glm(Sold ~ .,
             family = binomial(logit),
             data = homes)
summary(model3)
# Sold = [1 + exp(  3.222                  +
#                   0.001444(Living.Area)  -
#                   0.0049(Age)            -
#                   0.00001693(Price)      -
#                   0.4805(Bedrooms)       +
#                   0.1813(Bathrooms)      +
#                   0.1253(Fireplaces)      )]^-1


#' \newpage
#' # 4) Health Clinic for Flu Shots
#' ## a) Fit a multiple logistic regression model and check for significance of each variable (X1, X2, and X3)
flu = read.table("C:\\repos\\STAT 50001\\Lab 23\\flu.txt", header=TRUE)
model4 = glm(y ~ .,
             family = binomial(logit),
             data = flu)
summary(model4)
# y = [1 + exp( 1.17716     -
#               0.07279(x1) +
#               0.09899(x2) -
#               0.43397(x3)   ]^-1

# Only x1 and x2 are significant.

#' ## b) What is the estimate probability that a male client aged 55 with a health awareness index 60 will receive a flu shot?
predict(model4, data.frame(x1=55, x2=60, x3=1), interval="conf", level=0.95, type="resp")
# The chance that the male client will get the flu shot:
# 0.0642




