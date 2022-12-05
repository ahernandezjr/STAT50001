#' ---
#' title: "Lab 21 R Script"
#' author: "Alexander Hernandez"
#' date: "11/15/2022"
#' output: pdf_document
#' ---


#' # 1) American Football
placekick = read.csv("C:\\repos\\STAT 50001\\Lab 21\\Placekick.csv")
attach(placekick)

model = glm(good ~ distance,
            family = binomial(logit),
            data   = placekick)
summary(model)
# Simple Logistic Regression Model:
# good = [1 + exp( -5.812080 + .115027(distance) )]^-1

par(mfrow=c(2,2))
plot(model)

model = glm(good ~ distance + PAT,
            family = binomial(logit),
            data   = placekick)
summary(model)
# Using all significant regressor variables with 95% confidence, the model is:
# good = [1 + exp( -5.812080 + 0.08587(distance) - 1.33827(PAT) )]^-1

