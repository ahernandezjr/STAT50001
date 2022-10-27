#' ---
#' title: "Lab 9"
#' author: "Alexander Hernandez"
#' date: "09/22/2022"
#' ---


#' # 1) data “vacation”  provided in the link below describe a sample of 200 Chicago households regarding their vacation. The data includes the following variables: miles, income, age, kids
# http://www.principlesofeconometrics.com/poe4/data/stata/vacation.dta
#' ## a) a)	Import the data in R 
library(haven)
vacation = read_dta('C:\\repos\\STAT 50001\\Lab 9\\vacation.dta')

#' ## b) Display the miles distribution based on the number of kids by drawing parallel  box-plot
boxplot(vacation$miles ~ vacation$kids, col=c(2,3,4,5,6))

#' ## c) Draw histogram along with boxplot of the income data
library(UsingR)
simple.hist.and.boxplot(vacation$miles)


#' # 2) The following are the head circumferences (centimeters) at birth of 15 infants. Construct 95% CI for head circumferences (cm) at birth of all infants born at the local hospital
# 33.38 32.15 33.99 34.10 33.97 34.34 33.95 33.85 34.23 32.73 33.46 34.13 34.45 34.19 34.05
infants = scan('C:\\repos\\STAT 50001\\Lab 9\\data.txt')
t.test(infants, conf.level=0.95)$conf.int

#' # 3) Hurricane Data
# https://dasl.datadescription.com/datafile/tracking-hurricanes-2016/
#' ## a) Import the data in R
hurricane = read.table('C:\\repos\\STAT 50001\\Lab 9\\tracking-hurricanes-2016.txt',
                       sep="\t", header=TRUE)

#' ## b) Display the 24-, 48- and 72-hours errors creating appropriate graph
names(hurricane) = c("Year", "E1", "E2", "E3")
boxplot(hurricane$E1, hurricane$E2, hurricane$E3,
        names=c("E1 (24)", "E2 (48)", "E3 (72)"), col=c(3,4,5),
        main="Mean Area in Nautical Miles of 24, 48, and 72 Hour Predictions",
        xlab=" Prediction Separated by Times Span (Hours)")

#' ## c) Construct 90% CI for 72 hours prediction errors
t.test(hurricane$E3, conf.level=0.9)$conf.int


#' 4) Fatal Encounters 
# https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit#gid=0
#' ## a) Import the data in R and display age distribution using histogram
# Unable to get googlesheets4 working
library(readxl)

fatalities = read_excel("C:\\repos\\STAT 50001\\Lab 9\\FATAL ENCOUNTERS DOT ORG SPREADSHEET (See Read me tab).xlsx",
                        sheet=1,
                        range = cell_cols("A:G"))

hist(as.numeric(na.omit(fatalities$Age)),
     main = "Fatal Encounters with Police Percentages by Age",
     xlab = "Age",
     ylab = "Fatal Encounters")

#' ## b) Display the gender distribution using a pie chart
library(dplyr)
male = length(which(fatalities$Gender == "Male"))
female = length(which(fatalities$Gender == "Female"))
total = male + female

male_percent = round((male / total), digits=4) * 100
female_percent = round((female / total), digits=4) * 100

pie_labels = c(paste("Male: ", male_percent, "%"),
               paste("Female: ", female_percent, "%"))

pie(rbind(male, female),
    col=c(2,3),
    labels = pie_labels,
    main = "Fatal Encounters with Police by Gender")

#' ## c) Display race distribution using a pie chart
race_data = fatalities %>% count(fatalities$`Race with imputations`,
                                 sort = TRUE)
names(race_data) = c("Races", "Frequency")
race_data

pie(race_data$Frequency,
    labels=(race_data$Races),
    main = "Fatal Encounters with Police by Race")