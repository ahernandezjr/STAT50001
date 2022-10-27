#' ---
#' title: "Lab 16 R Script"
#' author: "Alexander Hernandez"
#' date: "10/24/2022"
#' output: pdf_document
#' ---

library(PASWR)

#' # 1) Study Hours and Test Score
students = read.table("C:\\repos\\STAT 50001\\Lab 15\\hours_scores.txt",
           header=TRUE)

#' ## a) Pearson Correlation Coefficient
cor.test(students$Score, students$hours)

#' ## b) Spearman Correlation Coefficient
cor.test(students$Score, students$hours,
         method="spearman", exact=FALSE)

#' ## c) Kendall's Tau
cor.test(students$Score, students$hours,
         method="kendall", exact=FALSE)

#' ## d) For each method, test the hypothesis that the correlation is nonzero
# Ho: p == 0
# Ha: p != 0
cor.test(students$Score, students$hours)$p.value
cor.test(students$Score, students$hours,
         method="spearman", exact=FALSE)$p.value
cor.test(students$Score, students$hours,
         method="kendall", exact=FALSE)$p.value
# With p-values of:
#  0.0056 for Pearson's method,
#  0.0009 for Spearman,s method,
#  0.0012 for Kendall's method,
# we have enough evidence for all three cases to reject the null hypothesis
# and claim that the Correlation Coefficient is non-zero
# and that there is a linear correlation between student study hours and scores.


#' # 2) GPA and SAT scores in Grades
#' ## a) Create scatterplot of the data for GPA and SAT scores
plot(Grades$gpa, Grades$sat,
     main="Student GPA versus SAT Scores",
     xlab="GPA", ylab="SAT Score")

#' ## b)  Obtain the least squares estimates for B0 and B1
model = lm(Grades$sat ~ Grades$gpa)
model
# B0 = 714.1
# B1 = 181.4

#' ## c) Display the regression model along with the scatterplot
plot(Grades$gpa, Grades$sat,
     main="Student GPA versus SAT Scores (with regression line)",
     xlab="GPA", ylab="SAT Score")
abline(model)
