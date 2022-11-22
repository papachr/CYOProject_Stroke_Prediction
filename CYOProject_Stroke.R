### Import data
# Packages needed

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(nortest)) install.packages("nortest", repos = "http://cran.us.r-project.org")
if(!require(mice)) install.packages("mice", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(performanceEstimation)) install.packages("performanceEstimation", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(GGally)
library(nortest)
library(mice)
library(reshape2)
library(rpart.plot)
library(randomForest)
library(performanceEstimation)

#Load data
stroke_temp<-  read.csv("healthcare-dataset-stroke-data.csv")

#Look at the structure of the data
str(stroke_temp)

#Convert stroke variable to a factor
stroke_temp$stroke<-factor(stroke_temp$stroke, levels=c(0,1),labels=c("neg","pos"))

#Check for duplicated values in our dataset
sum(duplicated(stroke_temp))

### Features Selection
#### gender variable

#Table of frequencies for the gender variable 
tab_gender<-table(stroke_temp$gender)
tab_gender

#Table of relative frequencies for the gender variable 
prop_gender <-prop.table(tab_gender)
round(prop_gender,3)

#Barplot of stroke versus gender variable
plot_gender_stroke<- ggplot(stroke_temp, aes(x = gender, fill = stroke)) +
  geom_bar(position = "dodge") + stat_count(geom = "text", colour = "black", size = 4,
                                            aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)

plot_gender_stroke

#Chi square test for stoke - gender variables
chitest_gender <- chisq.test(stroke_temp$gender, stroke_temp$stroke)
chitest_gender

# Fisher test for stroke - gender variables
fishertest_gender <- fisher.test(stroke_temp$gender, stroke_temp$stroke)
fishertest_gender


#### age variable
#Histogram for age variable
plot_age<-stroke_temp %>%  ggplot(aes(age)) + 
  geom_histogram(color="black",fill="navy",binwidth = 7) +
  ggtitle("Histogram of age")

plot_age

#Boxplot for age variable
plot_age_box<-stroke_temp %>%  ggplot(aes(x = 1, y = age)) +  geom_boxplot(fill="skyblue")
plot_age_box

#Age outliers
boxplot(stroke_temp$age, plot=FALSE)$out

#boxplot of stroke versus the age
plot_age_stroke <- stroke_temp %>% ggplot(aes(stroke,age))+geom_boxplot(fill="skyblue")
plot_age_stroke

#Histogram of stroke versus the age
plot_age_stroke_histo<-ggplot(stroke_temp, aes(x = age)) +  
  geom_histogram(color="black",fill="navy",binwidth = 7) +
  facet_wrap(~ stroke)

plot_age_stroke_histo

#Kolmogorov-Smirnov test for age variable
normtest_age <- lillie.test(stroke_temp$age)
normtest_age

# Mann Whitney U - Wilcoxon test age - stroke variables
wilcoxtest_stroke_age <- wilcox.test(x = stroke_temp[stroke_temp$stroke=="pos","age"],
                                     y = stroke_temp[stroke_temp$stroke=="neg","age"],
                                     alternative = "two.sided",
                                     paired = FALSE,
                                     conf.level = 0.95)
wilcoxtest_stroke_age

#### Hypertension variable
#Table of frequencies for the hypertension variable 
tab_hypertension<-table(stroke_temp$hypertension)
tab_hypertension

#Table of relative frequencies for the hypertension variable 
prop_hypertension <-prop.table(tab_hypertension)
round(prop_hypertension,3)

#Barplot of stroke versus hypertension variable
plot_hypertension_stroke<- ggplot(stroke_temp, aes(x = hypertension, fill = stroke)) +
  geom_bar(position = "dodge") + stat_count(geom = "text", colour = "black", size = 4,
                                            aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)

plot_hypertension_stroke

#Chi square test for stoke - hypertension variables
chitest_hypertension <- chisq.test(stroke_temp$hypertension, stroke_temp$stroke)
chitest_hypertension

#### Heart disease
#Table of frequencies for the heart_disease variable 
tab_heart_disease<-table(stroke_temp$heart_disease)
tab_heart_disease

#Table of relative frequencies for the heart_disease variable 
prop_heart_disease <-prop.table(tab_heart_disease)
round(prop_heart_disease,3)

#Barplot of stroke versus heart_disease variable
plot_heart_disease_stroke<- ggplot(stroke_temp, aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = "dodge") + stat_count(geom = "text", colour = "black", size = 4,
                                            aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)

plot_heart_disease_stroke

#Chi square test for stoke - heart_disease variables
chitest_heart_disease <- chisq.test(stroke_temp$heart_disease, stroke_temp$stroke)
chitest_heart_disease

#### Ever-married variable
#Table of frequencies for the ever_married variable 
tab_ever_married<-table(stroke_temp$ever_married)
tab_ever_married

#Table of relative frequencies for the ever_married variable 
prop_ever_married <-prop.table(tab_ever_married)
round(prop_ever_married,3)

#Barplot of stroke versus ever_married variable
plot_ever_married_stroke<- ggplot(stroke_temp, aes(x = ever_married, fill = stroke)) +
  geom_bar(position = "dodge") + stat_count(geom = "text", colour = "black", size = 4,
                                            aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)

plot_ever_married_stroke

#Chi square test for stoke - ever_married variables
chitest_ever_married <- chisq.test(stroke_temp$ever_married, stroke_temp$stroke)
chitest_ever_married


#### Work-type variable
#Table of frequencies for the work_type variable 
tab_work_type<-table(stroke_temp$work_type)
tab_work_type

#Table of relative frequencies for the work_type variable 
prop_work_type <-prop.table(tab_work_type)
round(prop_work_type,3)


#Barplot of stroke versus work_type variable
plot_work_type_stroke<- ggplot(stroke_temp, aes(x = work_type, fill = stroke)) +
  geom_bar(position = "dodge") + stat_count(geom = "text", colour = "black", size = 4,
                                            aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)

plot_work_type_stroke

#Chi square test for stoke - work_type variables
chitest_work_type <- chisq.test(stroke_temp$work_type, stroke_temp$stroke)
chitest_work_type

#Fisher test for stoke - work_type variables
fishertest_work_type <- fisher.test(stroke_temp$work_type, stroke_temp$stroke,simulate.p.value=TRUE)
fishertest_work_type

#### Residence type variable
#Table of frequencies for the Residence_type variable 
tab_Residence_type<-table(stroke_temp$Residence_type)
tab_Residence_type


#Table of relative frequencies for the Residence_type variable 
prop_Residence_type <-prop.table(tab_Residence_type)
round(prop_Residence_type,3)


#Barplot of stroke versus Residence_type variable
plot_Residence_type_stroke<- ggplot(stroke_temp, aes(x = Residence_type, fill = stroke)) +
  geom_bar(position = "dodge") + stat_count(geom = "text", colour = "black", size = 4,
            aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)

plot_Residence_type_stroke

#Chi square test for stoke - Residence_type variables
chitest_Residence_type <- chisq.test(stroke_temp$Residence_type, stroke_temp$stroke)
chitest_Residence_type


#### Average glucose level variable
#Histogram of average glucose level
plot_avg_glucose_level<-stroke_temp %>%  ggplot(aes(avg_glucose_level)) + 
  geom_histogram(color="black",fill="navy",binwidth = 7) +
  ggtitle("Histogram of average glucose level")

plot_avg_glucose_level

#Boxplot of average glucose level
plot_avg_glucose_level_box<-stroke_temp %>%  ggplot(aes(x = 1, y = avg_glucose_level)) +  geom_boxplot(fill="skyblue")
plot_avg_glucose_level_box

#Outliers - the first 15 and the maximum
boxplot(stroke_temp$avg_glucose_level, plot=FALSE)$out%>%head(15)
max(stroke_temp$avg_glucose_level)

#Boxplot of stroke versus the avg_glucose_level
plot_avg_glucose_level_stroke <- stroke_temp %>% ggplot(aes(stroke,avg_glucose_level))+
  geom_boxplot(fill="skyblue")
plot_avg_glucose_level_stroke

#Histogram of stroke versus the avg_glucose_level
plot_avg_glucose_level_stroke_histo<-ggplot(stroke_temp, aes(x = avg_glucose_level)) +  
  geom_histogram(color="black",fill="navy",binwidth = 7) +
  facet_wrap(~ stroke)

plot_avg_glucose_level_stroke_histo

#Kolmogorov-Smirnov test for avg_glucose_level variable
normtest_avg_glucose_level <- lillie.test(stroke_temp$avg_glucose_level)
normtest_avg_glucose_level


# Mann Whitney U - Wilcoxon test avg_glucose_level - stroke variables
wilcoxtest_stroke_avg_glucose_level <- wilcox.test(x = stroke_temp[stroke_temp$stroke=="pos",
                                                                   "avg_glucose_level"],
                                                   y = stroke_temp[stroke_temp$stroke=="neg","avg_glucose_level"],
                                                   alternative = "two.sided",
                                                   paired = FALSE,
                                                   conf.level = 0.95)
wilcoxtest_stroke_avg_glucose_level


#### BMI variable
# Convert bmi to numerical
stroke_temp$bmi<-as.numeric(stroke_temp$bmi)

#Histogram of bmi variable
plot_bmi<-stroke_temp %>%  ggplot(aes(bmi)) + 
  geom_histogram(color="black",fill="navy",binwidth = 5) +
  ggtitle("Histogram of bmi")

plot_bmi

#Boxplot of bmi variable
plot_bmi_box<-stroke_temp %>%  ggplot(aes(x = 1, y = bmi)) +  geom_boxplot(fill="skyblue")
plot_bmi_box

#Outliers for bmi
boxplot(stroke_temp$bmi, plot=FALSE)$out%>%sort(decreasing=TRUE)

#Boxplot of stroke versus the bmi variable
plot_bmi_stroke <- stroke_temp %>% ggplot(aes(stroke,bmi))+geom_boxplot(fill="skyblue")
plot_bmi_stroke

#Histogram of stroke versus the bmi variable
plot_bmi_stroke_histo<-ggplot(stroke_temp, aes(x = bmi)) +  
  geom_histogram(color="black",fill="navy",binwidth = 7) +
  facet_wrap(~ stroke)

plot_bmi_stroke_histo

#Kolmogorov-Smirnov test for bmi variable
normtest_bmi <- lillie.test(stroke_temp$bmi)
normtest_bmi

# Mann Whitney U - Wilcoxon test bmi - stroke variables
wilcoxtest_stroke_bmi <- wilcox.test(x = stroke_temp[stroke_temp$stroke=="pos","bmi"],
                                     y = stroke_temp[stroke_temp$stroke=="neg","bmi"],
                                     alternative = "two.sided",
                                     paired = FALSE,
                                     conf.level = 0.95)
wilcoxtest_stroke_bmi


#### Smoking status variable
#Table of frequencies for the smoking_status variable 
tab_smoking_status<-table(stroke_temp$smoking_status)
tab_smoking_status


#Table of relative frequencies for the smoking_status variable 
prop_smoking_status <-prop.table(tab_smoking_status)
round(prop_smoking_status,3)


#Barplot of stroke versus smoking_status variable
plot_smoking_status_stroke<- ggplot(stroke_temp, aes(x = smoking_status, fill = stroke)) +
  geom_bar(position = "dodge") + stat_count(geom = "text", colour = "black", size = 4,
                                            aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)

plot_smoking_status_stroke

#Chi square test for stoke - smoking_status variables
chitest_smoking_status <- chisq.test(stroke_temp$smoking_status, stroke_temp$stroke)
chitest_smoking_status



#### Multicollinearity analysis
#Correlation among numerical features
ggpairs(stroke_temp[,c(3:5,9,10,12)], aes(color = stroke, alpha=0.5))

#### Final selected features
#Dataset with the selected features
stroke_final <- stroke_temp[,c(3:7, 9:12)]


# Convert ever_married as integer
stroke_final$ever_married<-as.integer(factor(stroke_final$ever_married, 
                                             levels=c("No","Yes")))

#Most machine learning algorithms only work with numeric values
#Convert  work_type as integer
stroke_final$work_type<-as.integer(factor(stroke_final$work_type, 
                                          levels=c("children","Govt_job",
                                                   "Never_worked","Private",
                                                   "Self-employed")))


# Convert smoking status as integer
stroke_final$smoking_status<-as.integer(factor(stroke_final$smoking_status, 
                                               levels=c("formerly smoked", "never smoked",
                                                        "smokes","Unknown")))

#Convert the smoking status class 4: "Unknown" as NAs
stroke_final$smoking_status[stroke_final$smoking_status==4] <-NA

#Look at the structure of our data
str(stroke_final)

#Check again for correlation among features now that they are all numerical
ggpairs(stroke_final, aes(color = stroke, alpha=0.5))

### Normalization of the predictors since features with larger scales will dominate 
### over the machine learning algorithm
stroke_final[,c(1:8)] <- as.data.frame(scale(stroke_final[,c(1:8)]))

### Data splitting
#### Train_temp and validation set partition
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = stroke_final$stroke, times = 1, p = 0.2, list = FALSE)
train_temp <- stroke_final[-test_index,]
validation_set <- stroke_final[test_index,]

#### Train set and test set partition
set.seed(123, sample.kind="Rounding")
test_index1 <- createDataPartition(y = train_temp$stroke, times = 1, p = 0.2, list = FALSE)
train_set <- train_temp[-test_index1,]
test_set <- train_temp[test_index1,]

### Data exploration
# Check the structure of the train_set
str(train_set)

# Check the summary of the train_set
summary(train_set)

# Boxplots of the features
df.m <- melt(train_set, id.var = "stroke")
head(df.m)

p1<-ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes())
p1 + facet_wrap( ~ variable, scales="free")

#### Missing values
# Check the pattern of the missing values
md.pattern(train_set,rotate.names=T)

# Imputing the missing values
ini <- mice(train_set, maxit = 0,seed=123)
meth <- ini$meth
meth["smoking_status"] <- "rf"
imp1 <- mice(train_set, maxit = 50, meth = meth, seed = 123)

# Density plots for the imputed values
densityplot(imp1)

# Stripplots for the imputed values
stripplot(imp1, pch=20, cex=2)

# Dataset completed with the imputed values
train_set1<- complete(imp1,2)

#### Response variable stroke
# Barplot of stroke
plot_stroke<- stroke_temp%>%ggplot(aes(stroke))+geom_bar(fill="navy") + 
  stat_count(geom = "text", colour = "black", size = 4,
             aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)

plot_stroke

# Relative frequencies of variable stroke
prop_stroke <-prop.table(table(train_set1$stroke))
round(prop_stroke,3)

# Since we have highly imbalanced dataset we will use SMOTE technique to 
# balance it
smote_data <- smote(stroke~., train_set1, 
                    k = 5, # indicates the number of nearest neighbours used to generate
                    # the new examples of the minority class.
                    perc.over = 19, # A number of how many extra cases from the minority
                    # class are generated (known as over-sampling).
                    perc.under = 1.1) # A number of how many extra cases from the majority
# classes are selected for each case generated from 
#the minority class (known as under-sampling) 

table(smote_data$stroke)

prop_stroke_smote <-prop.table(table(smote_data$stroke))
round(prop_stroke_smote,3)

### Modelling approach
#### Logistic regression approach
# Logistic regression - fit the model
model_glm_stroke <- glm(stroke ~ ., family ="binomial", data = smote_data)


# Logistic regression - predict
predict_model_glm_stroke <- predict(model_glm_stroke, newdata = test_set, type = "response")

# Logistic regression - ifelse to denote all the probabilities above 0.5 as 
# positives and the remaining as negatives.
y_model_glm_stroke <- ifelse(predict_model_glm_stroke >= 0.5, "pos", "neg")


# Logistic regression - confusion matrix
tab_model_glm_stroke <- table(test_set$stroke, y_model_glm_stroke)

confusionMatrix_model_glm_stroke <- confusionMatrix(tab_model_glm_stroke,positive ="pos")

confusionMatrix_model_glm_stroke

# Logistic regression - Accuracy
glm <-confusionMatrix_model_glm_stroke$overall[["Accuracy"]]
glm


#### Desicion Trees
# Classification tree
set.seed(123, sample.kind="Rounding")
model_tree <- rpart(stroke ~ ., data = smote_data, method = "class",
                    control = rpart.control(xval=20, cp=0.009))

# Tree plot
rpart.plot(model_tree, cex=0.85)

# Classification tree - predict
predict_model_tree <- predict(model_tree, newdata = test_set, type = "class")

# Classification tree - confusion matrix
confusionMatrix_model_tree <- confusionMatrix(predict_model_tree,test_set$stroke, positive ="pos",mode="everything")
confusionMatrix_model_tree

# Classification tree - Accuracy
classification_tree <- confusionMatrix_model_tree$overall[["Accuracy"]]
classification_tree

#### Random Forest
# Select the best mtry parameter

set.seed(12, sample.kind="Rounding")

control <- trainControl(method="cv", number = 10)

grid <- data.frame(mtry = c(1:8))

model_rf <- train(stroke~.,smote_data,method = "rf",
                  trControl=control, tuneGrid = grid)

model_rf$bestTune

# Random Forest - fit model
fit_rf <- randomForest(stroke~., smote_data, minNode = model_rf$bestTune$mtry)

# Random Forest - predict
predict_rf<- predict(fit_rf, test_set)

# Random Forest - confusion matrix
confusionMatrix_rf <- confusionMatrix(predict_rf,test_set$stroke, positive ="pos",
                                      mode="everything")
confusionMatrix_rf

# Random Forest - Accuracy
rf<-confusionMatrix_rf$overall[["Accuracy"]]
rf

## Results
# Results table
accuracy_results <- tibble(method = c("Logistic Regression Model",
                                      "Classification Trees", "Random Forest"),
                           Accuracy = c(glm, classification_tree, rf))

knitr::kable(accuracy_results,digits=5)


# Best model Random Forest
# We will check the performance of the model with the validation set and the train_temp set

# We will impute the missing values for the train_temp set
ini2 <- mice(train_temp, maxit = 0,seed=123)
meth <- ini2$meth
meth["smoking_status"] <- "rf"
imp2 <- mice(train_temp, maxit = 50, meth = meth, seed = 123)

# Create the density plots and the stripplots
densityplot(imp2)
stripplot(imp2, pch=20, cex=2)

# We will complete our dataset with the imputed values
train_temp1<- complete(imp2,2)

# Table of relative frequencies for the response variable
prop_stroke_final <-prop.table(table(train_temp1$stroke))
round(prop_stroke_final,3)

# Balance our dataset
smote_data1 <- smote(stroke~., 
                     train_temp1, 
                     k = 5,              
                     perc.over = 19, 
                     perc.under = 1.1)

table(smote_data1$stroke)

prop_stroke_smote_final <-prop.table(table(smote_data1$stroke))
round(prop_stroke_smote_final,3)

# Select the best mtry parameter for the smote_data1

set.seed(321, sample.kind="Rounding")

control <- trainControl(method="cv", number = 10)

grid <- data.frame(mtry = c(1:8))

model_rf_final <- train(stroke~.,smote_data1,method = "rf",
                        trControl=control, tuneGrid = grid)

model_rf_final$bestTune

# Random Forest - fit model final
fit_rf_final <- randomForest(stroke~., smote_data1, minNode = model_rf_final$bestTune$mtry)

# Random Forest - predict final
predict_rf_final<- predict(fit_rf_final, validation_set)

# Random Forest - confusion matrix final
confusionMatrix_rf_final <- confusionMatrix(predict_rf_final,validation_set$stroke, 
                                            positive ="pos", mode="everything")
confusionMatrix_rf_final

# Random Forest - Accuracy final
rf_final<-confusionMatrix_rf_final$overall[["Accuracy"]]
rf_final

