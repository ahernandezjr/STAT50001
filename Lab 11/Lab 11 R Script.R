#' ---
#' title: "Lab 11"
#' author: "Alexander Hernandez"
#' date: "09/29/2022"
#' ---

library(UsingR)


#' # 1) Does the data in normtemp support that average body temp is 98.6? Hypothesis test
# Null:         u  = 98.6
# Alternative:  u != 98.6
t.test(normtemp$temperature,
       mu=98.6,
       conf.level = 0.9)
# The p-value is 2.411e^-07,
# we have enough evidence to reject the null hypothesis.


#' # 2) Waiting time of 100 bank customers
waiting_data = c(0.8, 0.8, 1.3, 1.5, 1.8, 1.9, 1.9,
                 2.1, 2.6, 2.7, 2.9, 3.1, 3.2, 3.3,
                 3.5, 3.6, 4.0, 4.1, 4.2, 4.2, 4.3,
                 4.3, 4.4, 4.4, 4.6, 4.7, 4.7, 4.8,
                 4.9, 4.9, 5, 5.3, 5.5, 5.7, 5.7, 6.1,
                 6.2, 6.2, 6.2, 6.3, 6.7, 6.9, 7.1, 7.1,
                 7.1, 7.1, 7.4, 7.6, 7.7, 8, 8.2, 8.6,
                 8.6, 8.6, 8.8, 8.8,  8.9,  8.9,  9.5,
                 9.6,  9.7,  9.8,  10.7,  10.9,  11,  11,
                 11.1,  11.2,  11.2, 11.5, 11.9, 12.4,
                 12.5, 12.9, 13, 13.1, 13.3, 13.6, 13.7,
                 13.9, 14.1, 15.4, 15.4, 17.3, 17.3, 18.1,
                 18.2, 18.4, 18.9, 19,  19.9, 20.6, 21.3,
                 21.4, 21.9, 23.0, 27, 31.6, 33.1, 38.5)
#' ## a) construct a 95% confidence interval
t.test(waiting_data, conf.level=0.95)$conf.int

#' ## b) construct a 99% confidence interval
t.test(waiting_data, conf.level=0.99)$conf.int

#' ## c) Is there enough evidence to conclude that it takes on average more than 8 minutes to be served
# Null:         u  = 8
# Alternative:  u >= 8
t.test(waiting_data, mu=8,
       alternative="greater")
# With a p-value of 0.0055,
# there is enough evidence to reject the null hypothesis.


#' # 3) Smokers and Non-Smokers. Is there evidence that the nonsmokers has a higher score than smokers?
# Null:         u(ns) - u(s) = 0
# Alternative:  u(ns) - u(s) > 0
smokers = c(16,20,14,21,20,18,13,15,17,21 )
nonsmokers = c(18,22,21,17,20,17,23,20,22,21)

t.test(nonsmokers, smokers, alt="greater")
# With a p-value of 0.019,
# we have enough evidence to reject the null hypothesis.


#' # 4) Pulse rate Experiment
pulse_data = read.table("http://www.statsci.org/data/oz/ms212.txt", header=TRUE)
 
#' ## a) Test the hypothesis of whether there is a difference in pulse rate if the students were sitting
#' # Null:      u(norm) - u(sitting)  = 0
# Alternative:  u(norm) - u(sitting) != 0

# First version:
t.test(pulse_data$Pulse2~pulse_data$Ran, alt="greater")

# Second version:
R = subset(pulse_data, pulse_data$Ran=="1")
S = subset(pulse_data, pulse_data$Ran=="2")
t.test(R$Pulse2, S$Pulse2, alt="greater")
# With a p-value of 2.2e^-16, we have enough evidence to reject the null hypothesis.


#' ## b) Test the hypothesis whether the average pulse rate for running students increased by 10 after they ran
# Null:         u(norm) - u(ran) = 10
# Alternative:  u(norm) - u(ran) > 10
length(R$Age)
t.test(R$Pulse2, R$Pulse1,
       alt="greater",
       mu=10,
       paired=T)
# With a p-value of 2.2e^-16,
# we have evidence to reject the null hypothesis.


#' # 5) Furness and Bryant Metabolic rates of male and female breeding fulmars
#' ## a) Display the metabolic rate of female and male group using side-by-side boxplot
species_data = read.csv('C:\\repos\\STAT 50001\\Lab 11\\species.csv', sep='\t')

boxplot(species_data$Metabolic.rate ~ species_data$Sex, col=c(2,3))

#' ## b) Test the hypothesis whether there is a difference in metabolic rate based on gender
# Null:         u(f) - u(m)  = 0
# Alternative:  u(f) - u(m) != 0
t.test(species_data$Metabolic.rate ~ species_data$Sex)
# with a p-value of 0.456,
# we do not have enough evidence to reject the null.
