#' ---
#' title: "Lab 11"
#' author: "Alexander Hernandez"
#' date: "09/29/2022"
#' ---


#' # 1) Test Scores, Major, and Class
#' ## a) merge() both into a single table
test_scores1 = read.csv("C:\\repos\\STAT 50001\\Homework 2\\test-scores1.tsv",
         sep=" ", header=TRUE)

test_scores2 = read.csv("C:\\repos\\STAT 50001\\Homework 2\\test-scores2.tsv",
                        sep=" ", header=TRUE)

test_scores_all = merge(test_scores1, test_scores2)
test_scores_all

#' ## b) How many students did better in the second test?
nrow(test_scores_all[test_scores_all$Test2 > test_scores_all$Test1,])

#' ## c) How many did better in the first test?
nrow(test_scores_all[test_scores_all$Test1 > test_scores_all$Test2,])

#' ## d) how many have the same score in both tests?
nrow(test_scores_all[test_scores_all$Test2 == test_scores_all$Test1,])

#' ## e) Calculate the average and SD of both tests
mean(test_scores_all$Test1)
sd(test_scores_all$Test1)
mean(test_scores_all$Test2)
sd(test_scores_all$Test2)


#' # 2) Health Insurance from 'custdata.tsv'
#' ## a) Import the data
cust_data = read.csv("C:\\repos\\STAT 50001\\Homework 2\\custdata.tsv",
                     sep='\t', header=TRUE)
                        
#' ## b) Display the age dist of customers using a histogram
hist(cust_data$age,
     main="Age Distribution of Health Insurance Customers",
     xlab="Age")

#' ## c) Display marital status using bar graph
barplot(table(cust_data$marital.stat), col=c(2,3,4,5))

#' ## d) How many customers are from Indiana?
table(cust_data$state.of.res)["Indiana"]


#' ## 3) 1988 Stockton PRrimary Exit Poll
# http://www.stat.berkeley.edu/users/statlabs/data/vote.data
#' ## a) How many variables are included? Print them.
vote = read.table("http://www.stat.berkeley.edu/users/statlabs/data/vote.data",
                 header=TRUE)
length(names(vote))
names(vote)

#' ## b) Display distribution of the voter's race
pie(table(vote$race), main="Distribution of Voters by Race",
    c("Missing", "White", "Hispanic", "Black", "Asian", "Other"))


#' # 4) YouthRisk
#' ## a) Import data and determine dimension
youth_risk = read.csv("C:\\repos\\STAT 50001\\Homework 2\\YouthRisk.csv", header=TRUE)
dim(youth_risk)

#' ## b) Is there any missing value? Remove if so
sum(is.na(youth_risk))
new_youth_risk = na.omit(youth_risk)

#' ## c) Display the age distribution based on gender using Parallel boxplot
boxplot(new_youth_risk$age4 ~ new_youth_risk$female,
        col= c(3,2),
        xlab="Gender (0-Male,1-Female", ylab="Age")

#' ## d) Display the grade distribution using a pie chart
pie(table(new_youth_risk$grade))


#' # 5) Generate 500 random numbers with rnorm with mean=10, var=25.
norm_dist = rnorm(500, mean=10, sd=sqrt(25))

#' ## a) How many observations are within one SD from the mean? (68%)
length(norm_dist[5 < norm_dist & norm_dist< 15])
100 * length(norm_dist[5 < norm_dist & norm_dist< 15]) / 500
# ~ 68%

#' ## b) Two?
length(norm_dist[0 < norm_dist & norm_dist< 20])
100 * length(norm_dist[0 < norm_dist & norm_dist< 20]) / 500
# ~ 95%

#' ## c) Three?
length(norm_dist[-5 < norm_dist & norm_dist< 25])
100 * length(norm_dist[-5 < norm_dist & norm_dist< 25]) / 500
# ~ 99.7%

#' # 6) FEV
#' ## a) Import data. How many children are included in the study?
FEV = read.table("http://www.statsci.org/data/general/fev.txt",
                 header=TRUE)
nrow(FEV)

#' ## b) Display the FEV of male and female children
mean(FEV$Sex=="Male")
mean(FEV$Sex=="Female")

#' ## c) Test the hypothesis whether there is a difference in FEV of the sexes
# Null:         u(M) - u(F)  = 0
# Alternative:  u(M) - u(F) != 0
t.test(FEV$Sex == "Male", FEV$Sex=="Female")
# With a p-value of 0.3199, we do not have enough evidence to reject the null.

#' ## d) Construct a 95% confidence interval for the difference in the mean for male and female students
t.test(FEV$Sex == "Male", FEV$Sex=="Female")$conf.int


#' # 7) Employee Satisfaction
#' ## a) Import the data in R
employee = read.csv("C:\\repos\\STAT 50001\\Homework 2\\employee.csv",
                    header=TRUE, skip=3)
#' ## b) Display the satisfaction scores for low, medium, and high salary employees
low = round(100 * mean(employee$satisfaction_level[employee$salary == "low"]),
            digits = 2)
med = round(100 * mean(employee$satisfaction_level[employee$salary == "medium"]),
            digits = 2)
high = round(100 * mean(employee$satisfaction_level[employee$salary == "high"]),
             digits = 2)

employee_labels = c(paste("Low: ", low, "%"),
                    paste("Medium: ", med, "%"),
                    paste("High: ", high, "%"))
  
pie(c(low,med,high),
    main = "Job Satisfaction by Salary Level",
    labels = employee_labels)


#' ## c) Test job satisfact level for high earners is different from low earners
# Null:         u(high) - u(low)  = 0
# Alternative:  u(high) - u(low) != 0
t.test(employee$satisfaction_level[employee$salary == "high"],
       employee$satisfaction_level[employee$salary == "low"])
# With a p-value of 2.58e-07,
# we have enough evidence to reject the null hypothesis.


#' # 8) PlantGrowth
#' ## a) How many observation are recorded in the data set?
nrow(PlantGrowth)

#' ## b) What is the mean of each of the control and treatment conditions?
mean(PlantGrowth$weight[PlantGrowth$group=="ctrl"])
mean(PlantGrowth$weight[PlantGrowth$group!="trt1"])
mean(PlantGrowth$weight[PlantGrowth$group!="trt2"])

#' ## c) Test whether there is a significant difference between t1 and t2
# Null:         u(t1) - u(t2)  = 0
# Alternative:  u(t1) - u(t2) != 0
t.test(PlantGrowth$weight[PlantGrowth$group!="trt1"],
       PlantGrowth$weight[PlantGrowth$group!="trt2"])
# With a p-value of 0.039,
# we have enough evidence to reject the null hypothesis.


#' # 9) Child and Health Development Study: babies
# Null:         u(dage) - u(age) = 0
# Alternative:  u(dage) - u(age) > 0
library(UsingR)
t.test(babies$dage, babies$age, alt="greater")
# With a p-value of 2.2e-16,
# we have enough evidence to reject the null hypothesis.


#' # 10) Doctor noshows
#' ## a) Import the data in R and identify its dims
noshows = read.csv("C:\\repos\\STAT 50001\\Homework 2\\noshow.csv", header=TRUE)
dim(noshows)

#' ## b) Print the variables included in the dataset
names(noshows)

#' ## c) Display the Age distribution by gender creating parallel box plot
boxplot(noshows$Age ~ noshows$Gender, col=c(2,3),
        xlab="Gender", ylab="Age", main="No Shows by Age per Gender")

#' ## d) Test whether females are more likely to miss the appointment than males
# Null:         u(female) - u(male) = 0
# Alternative:  u(female) - u(male) > 0
t.test(noshows$Gender=="F", noshows$Gender=="M", alt="greater")
# With a p-value of 2.2e-16,
# we have enough evidence to reject the null hypothesis.


#' ## e)Are females older than males. Perform the test.
# Null:         u(fage) - u(dage) = 0
# Alternative:  u(fage) - u(dage) > 0
t.test(noshows$Age[noshows$Gender=="F"],
       noshows$Age[noshows$Gender=="M"], alt="greater")
# With a p-value of 2.2e-16,
# we have enough evidence to reject the null hypothesis.