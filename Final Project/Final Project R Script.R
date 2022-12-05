#' ---
#' title: "Final Project R Script"
#' author: "Alexander Hernandez, Tony Ortiz, Diego Ramirez"
#' date: "12/7/2022"
#' output: pdf_document
#' ---


#' # 0.1) Source
#' ## "The dataset is publically available on the Kaggle website, and it is from an ongoing cardiovascular study on residents of the town of Framingham, Massachusetts. The classification goal is to predict whether the patient has 10-year risk of future coronary heart disease (CHD).The dataset provides the patients’ information. It includes over 4,000 records and 15 attributes."
#' ## https://www.kaggle.com/datasets/dileep070/heart-disease-prediction-using-logistic-regression?resource=download


#' # 0.2) Importing data:
heart = read.csv("C:\\repos\\STAT 50001\\Final Project\\framingham.csv")

head(heart)
attach(heart)


#' \newpage
#' # 1) Introduction
#' Relevancy and Data/Variable Information
#' ## Relevancy
#' ### Demographics and Risk Factors
#' sex, age, smoking status, cholesterol level, blood pressure, etc...
#' ### Research Topic
#' Trends and Changes in patient demographics, risk factors, and medical statistics may be used to predict heart disease.

#' ## Data Information
#' ### Demographic:
#' Sex: male or female(Nominal)
#' Age: Age of the patient;(Continuous - Although the recorded ages have been truncated to whole numbers, the concept of age is continuous) 

#' ### Behavioral
#' Current Smoker: whether or not the patient is a current smoker (Nominal)
#' Cigs Per Day: the number of cigarettes that the person smoked on average in one day.(can be considered continuous as one can have any number of cigarettes, even half a cigarette.) 

#' ### Medical( history)
#' BP Meds: whether or not the patient was on blood pressure medication (Nominal)
#' Prevalent Stroke: whether or not the patient had previously had a stroke (Nominal)
#' Prevalent Hyp: whether or not the patient was hypertensive (Nominal)
#' Diabetes: whether or not the patient had diabetes (Nominal)

#' ### Medical(current)
#' Tot Chol: total cholesterol level (Continuous)
#' Sys BP: systolic blood pressure (Continuous)
#' Dia BP: diastolic blood pressure (Continuous)
#' BMI: Body Mass Index (Continuous)
#' Heart Rate: heart rate (Continuous - In medical research, variables such as heart rate though in fact discrete, yet are considered continuous because of large number of possible values.)
#' Glucose: glucose level (Continuous)

#' ### Predict variable (desired target)
#' 10 year risk of coronary heart disease CHD (binary: “1”, means “Yes”, “0” means “No”)


#' \newpage
#' # 2) Model and Logistic Equation Creation
#' ## Model Creation
model = glm(TenYearCHD ~ .,
            family = binomial(logit),
            data = heart)

#' ## Significant Variables:
summary(model)
#' ## Significant Variables:
#' variables:       significance:
#' male             (0.001) ***
#' age              (0.001) ***
#' cigsPerDay       (0.01)  **
#' prevalentHyp     (0.1)   .
#' totChol          (0.05)  *
#' sysBP            (0.001) ***
#' glucose          (0.01)  **

#' ## Logistic Equation from Model
significant_model = glm(TenYearCHD ~ male         +
                                     age          +
                                     cigsPerDay   +
                                     prevalentHyp +
                                     totChol      +
                                     sysBP        +
                                     glucose,
                        family = binomial(logit),
                        data = heart)
#' With confidence: a = 0.05
#' TenYearCHD = [1 + exp( 8.322206231 - 0.555097538(male) - 0.063453347(age) - 0.017929305(cigsPerDay) - 0.002324(totChol) - 0.015398(sysBP) - 0.007124(glucose) )]^-1


#' PERFORM AIC STEP FUNCTION TO TO GET IMPROVED MODEL WITH LOWER AIC
#' TEST RESIDUALS 
#' MODEL SELECTION CRITERIA TABLE?


#' \newpage
#' # 3) Data Distribution and Hypothesis Testing

# Binary vs Binary: boxplot(male ~ TenYearCHD)
# USE DIFFERENT PLOT

# Binary vs Binary: boxplot(prevalentStroke ~ TenYearCHD)
# USE DIFFERENT PLOT

#' ### Ten Year Risk of Coronoary Heart Disease corresponding to Age
boxplot(age ~ TenYearCHD,
        main="Ten Year Risk of Coronoary Heart Disease corresponding to Age",
        col=c(2,3))
# PERFORM TEST

# Ten Year Risk of Coronoary Heart Disease corresponding to Cigarettes per Day
boxplot(cigsPerDay ~ TenYearCHD,
        main="Ten Year Risk of Coronoary Heart Disease corresponding to Cigarettes per Day",
        col=c(2,3))
# PERFORM TEST

# Ten Year Risk of Coronoary Heart Disease corresponding to Total Cholesterol
boxplot(totChol ~ TenYearCHD,
        main="Ten Year Risk of Coronoary Heart Disease corresponding to Total Cholesterol",
        col=c(2,3))
# PERFORM TEST

# Ten Year Risk of Coronoary Heart Disease corresponding to Systolic BP
boxplot(sysBP ~ TenYearCHD,
        main="Ten Year Risk of Coronoary Heart Disease corresponding to log(Systolic BP)",
        col=c(2,3))
# PERFORM TEST

# Ten Year Risk of Coronoary Heart Disease corresponding to Glucose level
boxplot(glucose ~ TenYearCHD,
        main="Ten Year Risk of Coronoary Heart Disease corresponding to log(Glucose level)",
        col=c(2,3))
# PERFORM TEST


#' Plot with Different Models
# FIND OUT WHICH GRAPHS TO PLOT AND FIT MODEL TO
# plot(TenYearCHD ~ LI, main="Leukemia Remmisions (REMISS) vs LI",
#      col=ifelse(leukemia["REMISS"] == 1, 3, 2))
# curve(predict(model3, data.frame(LI=x), type="resp"), add=TRUE)
# points(LI, fitted(model3), pch=20)


