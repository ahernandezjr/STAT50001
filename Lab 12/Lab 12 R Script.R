#' ---
#' title: "Lab 12"
#' author: "Alexander Hernandez"
#' date: "10/6/2022"
#' ---


library(PASWR)
library(MASS)
library(readxl)


#' # 1) Internet Traffic Data

#' ## a) Identify variables and dims
Q1 = read.table("http://jse.amstat.org/datasets/packetdata.dat.txt",
                header=TRUE)

#' ## b) Print frist five observations
head(Q1, 5)

#' ## c) Is there any missing value?
# For count: sum(is.na(Q1))
any(is.na(Q1))

#' ## d) Construct a 90% conf int for the average timestamp
t.test(Q1$time, conf.leve=0.9)$conf.int


#' # 2) Maternal Smoking Impact on Infant Health
#' ## a) Import the data
smoking = read.table("http://www.stat.berkeley.edu/~statlabs/data/babiesI.data",
                     header=TRUE)

#' ## b) How many observations have smoking status unknown
nrow(smoking[smoking$smoke == "9",])
# Option 2 :table(smoking$smoke)

#' ## c) Clean dataset by removing subjects with unknown smoking status
new_smoking = subset(smoking, smoking$smoke!="9")
dim(smoking)
dim(new_smoking)

#' ## d) Is there evidence that the newborn baby will have significantly low weight for a smoker mom than for a non-smoker mom?
# Null: u1 - u2 = 0
# Alt:  u1 - u2 > 0
t.test(new_smoking$bwt ~ new_smoking$smoke, alt = "greater")
# With p-value of 2.2e-16,
# we have enough evidence to reject the null hypothesis and
# can claim that newborn babies have lower weights with smoking mothers.


#' # 3) Bottle Water
# Null: u1 - u2 = 0
# Alt:  u1 - u2 < 0
t.test(Water$Sodium ~ Water$Source, alt="less")
# With a p-value of 0.03822,
# we have enough evidence to reject the null hypothesis and
# can claim that there is less sodium in source X than Y.


#' # 4) Foot Measurements of Fourth Graders
# Null: u(b) - u(g) = 0
# Alt:  u(b) - u(g) > 0
feet = read.table("http://ww2.amstat.org/publications/jse/datasets/kidsfeet.dat.txt",
                  header=FALSE)
t.test(feet$V2 ~ feet$V5)
# With a p-value of 0.247,
# we do not have enough evidence to reject the null hypothesis
# and cannot claim there is a difference in feet length of
# fourth graders by gender.


#' # 5) Capital Punishment under 18 Poll
# Null: u(c) - u(s)  = 0
# Alt:  u(c) - u(s) != 0
prop.test(c(180,238), c(580,600), correct=F)
# With a p-value of 0.0023 (or 0.0019 without correction),
# we have enough evidence to reject the null hypothesis
# and claim that the proportion between Catholics and seculars is different


#' # 6) Female Hurricanes vs Male Hurricanes
# Null: u(f) - u(m) = 0
# Alt:  u(f) - u(m) > 0
hurricanes1 = read_xlsx("C:\\repos\\STAT 50001\\Lab 12\\Hurricane.xlsx",
                       range="A1:D47")
hurricanes2 = read_xlsx("C:\\repos\\STAT 50001\\Lab 12\\Hurricane.xlsx",
                        range="E1:H47")
hurricanes = rbind(hurricanes1, hurricanes2)

t.test(hurricanes$Death[hurricanes$Gender=="Female"],
       hurricanes$Death[hurricanes$Gender=="Male"],
       alt="greater")
# With a p-value of 0.03024,
# we have enough evidence to reject the null hypothesis
# and claim can claim that female hurricanes cause more deaths than male ones.

#' # 7) birthwt data and smoking
# Null: u(s) - u(ns) = 0
# Alt:  u(s) - u(ns) > 1

attach(birthwt)
table(low)
xtabs(~low+smoke)
prop.test(c(29,30), c(115,74))

# With a p-value of 0.0396,
# we have enough evidence to reject the null hypothesis
# and can claim that there is a higher fraction of low-birth weights
# in smoking mothers versus non-smoking mothers.