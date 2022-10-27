#' ---
#' title: "Lab 13 R Script"
#' author: "Alexander Hernandez"
#' date: "10/13/2022"
#' output: pdf_document
#' ---

library(BSDA)
library(pwr)

#' 1) Shrinkage Percentage of Plastic Clay
nsize(b=0.2, sigma=1.2, conf.level=0.98, type="mu")


#' 2) New Product
nsize(b=9/40, conf.level=0.90, type="pi")


#' 3) Sample Size based on Power of Two-sided t-test
sample1 = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,100)
power = cbind(NULL, NULL)
for (i in sample1) {
  p1 = power.t.test(d=0.7, n=i, sig.level=0.05,
               alt="two.sided", type="two.sample")
  power = rbind(power, cbind(p1$n, p1$power))
}

power

plot(power, xlab="Sample Size", ylab="Power", main="Sample Size Vs. Power ",type="b",col=2)

#' 4) Sample Size based on Effect Size of Two-sided t-test
sample2 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5)
effect = cbind(NULL, NULL)
for (i in sample2) {
  p2 = power.t.test(d=i, power=0.8, sig.level=0.05,
               alt="two.sided", type="two.sample")
  effect = rbind(effect, cbind(p2$d, p2$n))
}

effect

plot(effect, xlab="Effect Size", ylab="Sample Size", main="Effect Size Vs. Sample Size ",type="b",col=2)
