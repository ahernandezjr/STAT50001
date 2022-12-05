#' ---
#' title: "Lab 22 R Script"
#' author: "Alexander Hernandez"
#' date: "11/17/2022"
#' output: pdf_document
#' ---


#' # 1) Insulating Fluids
fluids = read.table("C:\\repos\\STAT 50001\\Lab 22\\fluids.txt", header = TRUE)
attach(fluids)

types = factor(rep(c("Type_I", "Type_II", "Type_III", "Type_IV"), c(6,6,6,6)))
y = c(Type_I, Type_II, Type_III, Type_IV)

plot(types, y, names=c("Type_I", "Type_II", "Type_III", "Type_IV"),
     xlab="Types", ylab="Effective Life",
     main="Effective Life of Insulating Fluid at 35 KV",
     col=c(2,3,4,5))

#' ## a) Is there any indication that the fluids differ at a = 0.1? What about at a = 0.05?
summary(aov(y ~ types))
# Based on the summary, the effective life between types based on the summary:
# is significantly different at 0.1 confidence,
# is not significantly different at .05 confidence.

#' ## b) Use the Tukey's HSD test to identify the fluid types that are different (if any)
TukeyHSD(aov(y ~ types), conf.level = 0.90)
TukeyHSD(aov(y ~ types), conf.level = 0.95)
# At both a 90% and 95% confidence level, a p-value of 0.044 shows
# there is a significant difference between Type_III and Type_II.

plot(TukeyHSD(aov(y ~ types), conf.level = 0.95), las = 2)
plot(TukeyHSD(aov(y ~ types), conf.level = 0.90), las = 2)
