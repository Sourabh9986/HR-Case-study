install.packages("MASS")
install.packages("car")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("devtools")
install.packages("cowplot")
install.packages("GGally")

library(MASS)
library(car)
library(caret)
library(dypl)
library(ggplot2)
library(cowplot)





employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
in_time<- read.csv("in_time.csv", stringsAsFactors = F)
manager_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time<- read.csv("out_time.csv", stringsAsFactors = F)
View(out_time_dt)

str(employee_survey_data)    # 4410 obs of 4 variables 
str(general_data) # 4410 obs of 24 variables including the target variable
str(in_time) # 4410 obs of 262 variables
str(manager_survey_data) #4410 obs of 3 variables
str(out_timeact) ##4410 obs of 262 variables



for (x in 3) {
  print(as.POSIXct(out_time[c(1:4410),x],format = "%Y-%m-%d %H:%M:%S",tz=Sys.timezone()))
  
}


# Collate the data together in one single file
length(unique(tolower(employee_survey_data$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(general_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410, confirming EmployeeID is key

setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets

hr<- merge(general_data,employee_survey_data, by="EmployeeID", all = F)
hr<- merge(general_data,manager_survey_data, by="EmployeeID", all = F)

View(hr) #master file



### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(factor(hr$BusinessTravel)) #4410 obs. of 26 variables;
summary(hr)



# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



ggplot(hr, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()
          ggplot(hr, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1
          ggplot(hr, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1
          ggplot(hr, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1
          ggplot(hr, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1





# Histogram and Boxplots for numeric variables 


ggplot(hr, aes(DistanceFromHome))+ geom_histogram(binwidth = 10)
          ggplot(hr, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()


ggplot(hr, aes(Education))+ geom_histogram(binwidth = 1)
          ggplot(hr, aes(x="",y=Education))+ geom_boxplot(width=0.1)+coord_flip()

ggplot(hr, aes(JobLevel))+ geom_histogram()
          ggplot(hr, aes(x="",y=JobLevel))+ geom_boxplot(width=0.1)+coord_flip()
          
          
ggplot(hr, aes(MonthlyIncome))+ geom_histogram()
          ggplot(hr, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()#out
          
ggplot(hr, aes(NumCompaniesWorked))+ geom_histogram()
          ggplot(hr, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()#out
          
ggplot(hr, aes(PercentSalaryHike))+ geom_histogram()
          ggplot(hr, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()
          
ggplot(hr, aes(StandardHours))+ geom_histogram()
          ggplot(hr, aes(x="",y=StandardHours))+ geom_boxplot(width=0.1)+coord_flip()
          
ggplot(hr, aes(StockOptionLevel))+ geom_histogram()
          ggplot(hr, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()#out
          
          ggplot(hr, aes(TotalWorkingYears))+ geom_histogram()
          ggplot(hr, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip() #out         

          ggplot(hr, aes(TrainingTimesLastYear))+ geom_histogram()
          ggplot(hr, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip() #out
          
          ggplot(hr, aes(TotalWorkingYears))+ geom_histogram()
          ggplot(hr, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()#out 
          
          ggplot(hr, aes(YearsAtCompany))+ geom_histogram()
          ggplot(hr, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()#out 
          
          ggplot(hr, aes(YearsSinceLastPromotion))+ geom_histogram()
          ggplot(hr, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()#out 
          
          ggplot(hr, aes(YearsWithCurrManager))+ geom_histogram()#out
          ggplot(hr, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip() 
          
          ggplot(hr, aes(JobInvolvement))+ geom_histogram()
          ggplot(hr, aes(x="",y=JobInvolvement))+ geom_boxplot(width=0.1)+coord_flip() 
          
          ggplot(hr, aes(PerformanceRating))+ geom_histogram()
          ggplot(hr, aes(x="",y=PerformanceRating))+ geom_boxplot(width=0.1)+coord_flip() 
          
          
          
          
          
#No outliers in numeric variables

# Boxplots of numeric variables relative to Attrition status
ggplot(hr, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none")
          ggplot(hr, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip()#var 
          ggplot(hr, aes(x=Attrition,y=EmployeeCount, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() 
          ggplot(hr, aes(x=Attrition,y=JobLevel, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() #var
          ggplot(hr, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip()#var 
          ggplot(hr, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() #var
          ggplot(hr, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() 
          ggplot(hr, aes(x=Attrition,y=StockOptionLevel, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() 
          ggplot(hr, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip()#var 
          ggplot(hr, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() 
          ggplot(hr, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip()#var 
          ggplot(hr, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip()#var 
          ggplot(hr, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip()#var
          ggplot(hr, aes(x=Attrition,y=JobInvolvement, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip()
          ggplot(hr, aes(x=Attrition,y=PerformanceRating, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip()
          




# Correlation between numeric variables
library(GGally)
ggpairs(hr[, c("Age", "MonthlyIncome", "NumCompaniesWorked", "TotalWorkingYears","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager")])

#YearsAtCompany and YearsWithCurrManager are correlated(corr 0.76)



# Missing value
sapply(hr, function(x) sum(is.na(x))) # shows all 19 NAs are in NumCompaniesWorked and 9 NA's in TotalWorkingYears column

#best is to remove these observations from the analysis
hr <- hr[!is.na(hr$NumCompaniesWorked),]
hr <- hr[!is.na(hr$TotalWorkingYears),]


################################################################
# Feature standardisation

# Normalising continuous features 
hr$Age<- scale(hr$Age) 
hr$MonthlyIncome<- scale(hr$MonthlyIncome)
hr$NumCompaniesWorked<- scale(hr$NumCompaniesWorked)
hr$TotalWorkingYears<- scale(hr$TotalWorkingYears)
hr$TrainingTimesLastYear<- scale(hr$TrainingTimesLastYear)
hr$YearsAtCompany<- scale(hr$YearsAtCompany)
hr$YearsSinceLastPromotion<- scale(hr$YearsSinceLastPromotion)
hr$YearsWithCurrManager<- scale(hr$YearsWithCurrManager)


# converting target variable telecom from No/Yes character to factorwith levels 0/1 
hr$Attrition<- factor(hr$Attrition)
hr$Attrition<- ifelse(hr$Attrition=="Yes",1,0)

# Checking Attrition rate of prospect customer

Attrition <- sum(hr$Attrition)/nrow(hr)
Attrition # 16.08854% Attrition rate. 

# creating a dataframe of categorical features
hr_Attrition<- hr[,-c(1,2,6,9,12,14,15,16,17,18,20,21,22,23,24)]
str(hr)
# converting categorical attributes to factor
hr_fact<- data.frame(sapply(hr_Attrition, function(x) factor(x)))
str(hr_fact)

# creating dummy variables for factor attributes
levels(hr_fact$Attrition)<-c(1,0)
hr_fact$Attrition <- as.numeric(levels(hr_fact$Attrition))[hr_fact$Attrition]

levels(hr_fact$Gender)<-c(1,0)
hr_fact$Gender <- as.numeric(levels(hr_fact$Gender))[hr_fact$Gender]

levels(hr_fact$PerformanceRating)<-c(1,0)
hr_fact$PerformanceRating <- as.numeric(levels(hr_fact$PerformanceRating))[hr_fact$PerformanceRating]


hr_fact$BusinessTravel <- factor(hr_fact$BusinessTravel, levels = c("A",levels(hr_fact$BusinessTravel)))
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = hr_fact))
dummy_1 <- dummy_1[,-1]
hr_1 <- cbind(hr_fact[,-2], dummy_1)

hr_fact$Department <- factor(hr_fact$Department, levels = c("A",levels(hr_fact$Department)))
dummy_2 <- data.frame(model.matrix( ~Department, data = hr_fact))
dummy_2 <- dummy_2[,-1]
hr_1 <- cbind(hr_1[,-2], dummy_2)

hr_fact$Education <- factor(hr_fact$Education, levels = c("A",levels(hr_fact$Education)))
dummy_3 <- data.frame(model.matrix( ~Education, data = hr_fact))
dummy_3 <- dummy_3[,-1]
hr_1 <- cbind(hr_1[,-2], dummy_3)

hr_fact$EducationField <- factor(hr_fact$EducationField, levels = c("A",levels(hr_fact$EducationField)))
dummy_4 <- data.frame(model.matrix( ~EducationField, data = hr_fact))
dummy_4 <- dummy_4[,-1]
hr_1 <- cbind(hr_1[,-2], dummy_4)

hr_fact$JobLevel <- factor(hr_fact$JobLevel, levels = c("A",levels(hr_fact$JobLevel)))
dummy_6 <- data.frame(model.matrix( ~JobLevel, data = hr_fact))
dummy_6 <- dummy_6[,-1]
hr_1 <- cbind(hr_1[,-3], dummy_6)

hr_fact$MaritalStatus <- factor(hr_fact$MaritalStatus, levels = c("A",levels(hr_fact$MaritalStatus)))
dummy_7 <- data.frame(model.matrix( ~MaritalStatus, data = hr_fact))
dummy_7 <- dummy_7[,-1]
hr_1 <- cbind(hr_1[,-3], dummy_7)


hr_fact$StockOptionLevel <- factor(hr_fact$StockOptionLevel, levels = c("A",levels(hr_fact$StockOptionLevel)))
dummy_8 <- data.frame(model.matrix( ~StockOptionLevel, data = hr_fact))
dummy_8 <- dummy_8[,-1]
hr_1 <- cbind(hr_1[,-3], dummy_8)

hr_fact$JobInvolvement <- factor(hr_fact$JobInvolvement, levels = c("A",levels(hr_fact$JobInvolvement)))
dummy_9 <- data.frame(model.matrix( ~JobInvolvement, data = hr_fact))
dummy_9 <- dummy_9[,-1]
hr_1 <- cbind(hr_1[,-3], dummy_9)

View(hr_1)
str(hr_fact)
# Final dataset
hr_1

########################################################################
# splitting the data between train and test
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(hr_1), 0.7*nrow(hr_1))
# generate the train data set
train = hr_1[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = hr_1[-trainindices,]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2539.7....31 coeff..nullDev 2671.2...resDev 2483.7

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)


# Removing multicollinearity through VIF check
library(car)
vif(model_2)


#exclude EducationFieldMarketing

model_3<- glm(formula = Attrition ~ Gender + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                DepartmentHuman.Resources + DepartmentResearch...Development + 
                Education2 + EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
                EducationFieldMarketing + EducationFieldMedical + MaritalStatusDivorced + 
                MaritalStatusMarried + StockOptionLevel0 + StockOptionLevel2, family = "binomial", 
      data = train)
summary(model_3) 
vif(model_3)


#exclude DepartmentResearch...Development
model_4<- glm(formula = Attrition ~ Gender + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                DepartmentHuman.Resources + 
                Education2 + EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
                EducationFieldMarketing + EducationFieldMedical + MaritalStatusDivorced + 
                MaritalStatusMarried + StockOptionLevel0 + StockOptionLevel2, family = "binomial", data = train)

summary(model_4)
vif(model_4)

#exclude DepartmentHuman.Resources

model_5<- glm(formula = Attrition ~ Gender + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                Education2 + EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
                EducationFieldMarketing + EducationFieldMedical + MaritalStatusDivorced + 
                MaritalStatusMarried + StockOptionLevel0 + StockOptionLevel2, 
              family = "binomial", data = train)


summary(model_5)
vif(model_5)


#exclude StockOptionLevel2 

model_6 <- glm(formula = Attrition ~ Gender + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                 Education2 + EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + MaritalStatusDivorced + 
                 MaritalStatusMarried + StockOptionLevel0, 
               family = "binomial", data = train)

summary(model_6)
vif(model_6)

#exclude StockOptionLevel0


model_7<- glm(formula = Attrition ~ Gender + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                Education2 + EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
                EducationFieldMarketing + EducationFieldMedical + MaritalStatusDivorced + 
                MaritalStatusMarried, 
              family = "binomial", data = train)


summary(model_7)
vif(model_7)


# exclude gender


model_8<- glm(formula = Attrition ~ BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                Education2 + EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
                EducationFieldMarketing + EducationFieldMedical + MaritalStatusDivorced + 
                MaritalStatusMarried, 
              family = "binomial", data = train)
summary(model_8)
vif(model_8)


# exclude EducationFieldMedical
model_9<- glm(formula = Attrition ~ BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                Education2 + EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
                EducationFieldMarketing + MaritalStatusDivorced + 
                MaritalStatusMarried, 
              family = "binomial", data = train)
summary(model_9)
vif(model_9)


#exclude EducationFieldMarketing

model_10<- glm(formula = Attrition ~ BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                Education2 + EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
                MaritalStatusDivorced + 
                MaritalStatusMarried, 
              family = "binomial", data = train)
summary(model_10)
vif(model_10)

#exclude EducationFieldLife.Sciences

model_11<- glm(formula = Attrition ~ BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
                 Education2 + EducationFieldHuman.Resources +
                 MaritalStatusDivorced + 
                 MaritalStatusMarried, 
               family = "binomial", data = train)
summary(model_11)
vif(model_11)

#############################################################
###########
# With 10 significant variables in the model

final_model<- model_11

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_at <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_at <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_at,test_pred_at)

#######################################################################
test_pred_at <- factor(ifelse(test_pred >= 0.70, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_at, test_actual_at, positive = "Yes")
test_conf


#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_at <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_at, test_actual_at, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.78 for final model

test_cutoff_at <- factor(ifelse(test_pred >=0.78, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_at, test_actual_at, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
