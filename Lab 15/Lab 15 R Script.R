#' ---
#' title: "Lab 15 R Script"
#' author: "Alexander Hernandez"
#' date: "10/20/2022"
#' output: pdf_document
#' ---

library(PASWR)
library(UsingR)

#' 1) Honda Accord Gas Mileage
# Ho: The mileage data is not significantly different than a normal population
# Ha: The mileage data is significantly different than a normal population
gas = c(27,  26,  31,  30,  30,  28,  26,  24,  30,  30,  23,  30,  23)
shapiro.test(gas)
# With a p-value of 0.04207, we can reject the null hypothesis
# and can claim with evidence that the Honda Accord mileage collected
# differs from a normal population.


#' 2) Credit-card Balance from Mobilize.org
# Ho: Student credit card significantly debt does not differ from 1770 dollars
# Ha: Student credit card significantly debt differs from 1770 dollars
balance = c(6000, 870, 1530, 1660,
            1060, 1790, 1630, 3180,
            2180, 2370, 1800, 2170,
            1210, 410, 1720, 1270,
            570, 1050, 2320, 1120)
SIGN.test(balance, md=1770, conf.level=0.9)
# With a p-value of 0.5034, we fail to reject the null hypothesis
# and cannot claim that the debt of students at the college differs from $1770.


#' 3) CEO Salaries at Top 199 US Companies
# Ho: CEO salary average is not significantly more than $220,000
# Ha: CEO salary average is significantly more than $220,000
SIGN.test(exec.pay, md=22, alt="greater")
# With a p-value of 0.00851, we have enough evidence to reject the null,
# and can claim that the median CEO salary is significantly greater than
# $220,000.
