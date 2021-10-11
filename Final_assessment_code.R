#install required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("corrplot")) install.packages("corrplot")
if (!require("caret")) install.packages("caret")
if (!require("plyr")) install.packages("plyr")
if (!require("randomForest")) install.packages("randomForest")

library(tidyverse)
library(readr)
library(dplyr)
library(corrplot)


# load the dataset. Data was downloaded from Kaggle on 13.10.2021 (https://www.kaggle.com/fedesoriano/heart-failure-prediction)
#"heart.csv" file must be in the same folder as this code!!!
data_heart <- read_csv("heart.csv")

# preview of the heart failure data
str(data_heart)
#number of rows
nrow(data_heart)
#data preview
options(dplyr.width = Inf)
data_heart %>% head() 

#data cleaning, exploration and visualization

#variables distribution, to understand our data
#age distribution
min(data_heart$Age)
max(data_heart$Age)
mean(data_heart$Age)
median(data_heart$Age)

data_heart %>%
  ggplot() + aes(Age) +
  geom_histogram(colour="black", fill="#add8e6", bins=11) +
  scale_x_continuous(labels = comma)

#sex distribution
data_heart %>%
  group_by(Sex) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = Sex, y = n, fill = Sex)) +
  geom_bar(stat = "identity")

#chest pain type distribution
data_heart %>%
  group_by(ChestPainType) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = ChestPainType, y = n, fill = ChestPainType)) +
  geom_bar(stat = "identity")

#resting BP distribution
data_heart %>%
  ggplot() + aes(RestingBP) +
  geom_histogram(colour="black", fill="#add8e6", bins=20) +
  scale_x_continuous(labels = comma)

mean(data_heart$RestingBP)
median(data_heart$RestingBP)
sd(data_heart$RestingBP)

#cholesterol distribution
data_heart %>%
  ggplot() + aes(Cholesterol) +
  geom_histogram(colour="black", fill="#add8e6", bins=20) +
  scale_x_continuous(labels = comma)

mean(data_heart$Cholesterol)
median(data_heart$Cholesterol)
sd(data_heart$Cholesterol)

#calculating number of patients with cholesterol=0
data_heart %>% filter(Cholesterol == 0) %>% count()

#172/918 #% of patients with cholesterol=0
#mean adn median if we ignore healthy patients
data_heart %>% filter(Cholesterol > 0) %>%
  summarize(mean_cholesterol = mean(Cholesterol), median_cholesterol = median(Cholesterol), sd_cholesterol = sd(Cholesterol))


#fasting BS distribution
data_heart %>%
  group_by(FastingBS) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = FastingBS, y = n, fill = FastingBS)) +
  geom_bar(stat = "identity")


#resting ECG distribution
data_heart %>%
  group_by(RestingECG) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = RestingECG, y = n, fill = RestingECG)) +
  geom_bar(stat = "identity")

#max HR distribution
data_heart %>%
  ggplot() + aes(MaxHR) +
  geom_histogram(colour="black", fill="#add8e6", bins=13) +
  scale_x_continuous(labels = comma)

mean(data_heart$MaxHR)
median(data_heart$MaxHR)
sd(data_heart$MaxHR)

#exercise angina distribution
data_heart %>%
  group_by(ExerciseAngina) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = ExerciseAngina, y = n, fill = ExerciseAngina)) +
  geom_bar(stat = "identity")


#oldpeak distribution
data_heart %>%
  ggplot() + aes(Oldpeak) +
  geom_histogram(colour="black", fill="#add8e6", bins=9) +
  scale_x_continuous(labels = comma)

mean(data_heart$Oldpeak)
median(data_heart$Oldpeak)
sd(data_heart$Oldpeak)
min(data_heart$Oldpeak)
max(data_heart$Oldpeak)

#ST slope distribution
data_heart %>%
  group_by(ST_Slope) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = ST_Slope, y = n, fill = ST_Slope)) +
  geom_bar(stat = "identity")

#heart disease distribution
data_heart %>%
  group_by(HeartDisease) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = HeartDisease, y = n, fill = HeartDisease)) +
  geom_bar(stat = "identity")

#correlations between continious variables 
data_heart %>% select(-Sex, -ChestPainType, -RestingECG, -ExerciseAngina, -ST_Slope) %>%
  cor() %>% corrplot()

#testing different assumptions
#chest pain type by age
data_heart %>% 
  group_by(Age, ChestPainType) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = Age, y = n, fill = ChestPainType)) +
  geom_bar(stat = "identity")

#resting bp vs age
data_heart %>% 
  group_by(Age, Sex) %>%
  summarize(RestingBP = mean(RestingBP)) %>%
  ggplot(aes(x = Age, y = RestingBP, color = Sex)) +
  geom_line()

#cholesterol vs age
data_heart %>% 
  group_by(Age, Sex) %>%
  summarize(Cholesterol = mean(Cholesterol)) %>%
  ggplot(aes(x = Age, y = Cholesterol, color = Sex)) +
  geom_line()
  
#max HR vs age
data_heart %>% 
  group_by(Age, Sex) %>%
  summarize(MaxHR = mean(MaxHR)) %>%
  ggplot(aes(x = Age, y = MaxHR, color = Sex)) +
  geom_line()  

#st slope vs age
data_heart %>% 
  group_by(Age, ST_Slope) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = Age, y = n, fill = ST_Slope)) +
  geom_bar(stat = "identity")

#heart disease vs age
data_heart %>% 
  group_by(Age, HeartDisease) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = Age, y = n, fill = HeartDisease)) +
  geom_bar(stat = "identity")  
  
#heart disease vs sex
data_heart %>% 
  group_by(Sex, HeartDisease) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = Sex, y = n, fill = HeartDisease)) +
  geom_bar(stat = "identity")  

#chest pain type vs resting BP
data_heart %>% 
  group_by(ChestPainType) %>%
  summarize(RestingBP = mean(RestingBP)) %>%
  ggplot(aes(x = ChestPainType, y = RestingBP, fill = ChestPainType)) +
  geom_bar(stat = "identity")

#chest pain type vs cholesterol - healthy is under 200. Still question of causation
data_heart %>% 
  group_by(ChestPainType) %>%
  summarize(Cholesterol = mean(Cholesterol)) %>%
  ggplot(aes(x = ChestPainType, y = Cholesterol, fill = ChestPainType)) +
  geom_bar(stat = "identity")

#max hr vs heart disease
data_heart %>% 
  group_by(MaxHR = MaxHR %/% 10 *10, HeartDisease) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = MaxHR, y = n, fill = HeartDisease)) +
  geom_bar(stat = "identity")  

  
# MODELING
library(caret)
library(plyr)
library(randomForest)

#standardization
preprocAge <- preProcess(data_heart[,1], method=c("center", "scale"))
ageNorm <- predict(preprocAge, data_heart[,1])

preprocRestingBP <- preProcess(data_heart[,4], method=c("center", "scale"))
RestingBPNorm <- predict(preprocRestingBP, data_heart[,4])

preprocCholesterol <- preProcess(data_heart[,5], method=c("center", "scale"))
CholesterolNorm <- predict(preprocCholesterol, data_heart[,5])

preprocMaxHR <- preProcess(data_heart[,8], method=c("center", "scale"))
MaxHRNorm <- predict(preprocMaxHR, data_heart[,8])

#updating dataset
data_heart <- data_heart %>% mutate(Age = ageNorm$Age, RestingBP = RestingBPNorm$RestingBP, 
                      Cholesterol = CholesterolNorm$Cholesterol, MaxHR = MaxHRNorm$MaxHR)

#splitting data into test and validation datasets
# test set will be 10% of our heart failure data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = data_heart$HeartDisease, times = 1, p = 0.1, list = FALSE)
train_set <- data_heart[-test_index,]
test_set <- data_heart[test_index,]

#generating function for cm visualization
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  


#logistic regression - starting point
#model training
fit_glm <- glm(HeartDisease ~ ., data=train_set, family="binomial")
#making predictions
p_hat_logistic <- predict(fit_glm, test_set)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 1, 0))
#generating confusion matrix
cm_lr <- confusionMatrix(table(y_hat_logistic, test_set$HeartDisease))

#calling visualization function we defined earlier
draw_confusion_matrix(cm_lr)

#knn model
#to fit knn model, we need to adjust our data in a way, that we will have just the numerical data. To achieve this,
#we will code our categorical data into numeric one when it is possible (when values could be arranged like "Low"s and "High"s). 
#But for example, in case of ChestPainType, it would not make sense to code values into 1, 2, 3 and 4, since they cannot be 
#really arranged. The problem could be solved with addition of N-dimensional space such as (1,0,...),(0,1,...) but for know,
#we will just skip this column since it is the only "problematic" one and it would complicated the model. We will rather 
#focus on improving other aspects of the model
data_heart <- data_heart %>% mutate(SexNum = unclass(as.factor(Sex)), ExerciseAnginaNum = unclass(as.factor(ExerciseAngina)), ST_SlopeNum = unclass(as.factor(ST_Slope)))
#since unclass do not arrange RestingECG as we want, we will manually map its values with mapvalues function. We waint it in order Normal, ST, LVH (description in report)
data_heart <- data_heart %>% mutate(RestingECGNum = as.numeric(mapvalues(data_heart$RestingECG, 
          from=c("Normal","ST","LVH"), 
          to=c(1,2,3))))

#again splitting the data, we have new columns now. We could also apply steps above to train and test data separately for the same result
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = data_heart$HeartDisease, times = 1, p = 0.1, list = FALSE)
train_set <- data_heart[-test_index,]
test_set <- data_heart[test_index,]

#training model with default settings
knn_fit <- knn3(as.matrix(train_set[,c(1,4,5,6,8,10,13:16)]), factor(train_set$HeartDisease))
#making predictions
y_hat_knn <- predict(knn_fit, test_set[,c(1,4,5,6,8,10,13:16)], type = "class")
cm_knn_default <- confusionMatrix(table(y_hat_knn, test_set$HeartDisease))

draw_confusion_matrix(cm_knn_default)

#pick the best performing k, using similar steps but within the function for diffrent values
ks <- seq(1, 100, 1)
knn_results <- map_df(ks, function(k){
  knn_fit <- knn3(as.matrix(train_set[,c(1,4,5,6,8,10,13:16)]), factor(train_set$HeartDisease), k = k)
  
  y_hat <- predict(knn_fit, test_set[,c(1,4,5,6,8,10,13:16)], type = "class")
  cm <- confusionMatrix(table(y_hat, test_set$HeartDisease))
  
  tibble(k = k, accuracy = cm$overall["Accuracy"])
  
})

#show the results
knn_results[which.max(knn_results$accuracy), 1]
max(knn_results$accuracy)

#plot results
knn_results %>% ggplot(aes(x = k, y = accuracy)) +
  geom_line(colour = "blue")

#training model with the best settings
knn_fit <- knn3(as.matrix(train_set[,c(1,4,5,6,8,10,13:16)]), factor(train_set$HeartDisease), k=9)
#making prediction
y_hat_knn <- predict(knn_fit, test_set[,c(1,4,5,6,8,10,13:16)], type = "class")
cm_knn <- confusionMatrix(table(y_hat_knn, test_set$HeartDisease))

draw_confusion_matrix(cm_knn)

#random forest
rf_fit <- randomForest(as.matrix(train_set[,c(1,4,5,6,8,10,13:16)]), factor(train_set$HeartDisease), data=train_set, ntree=500, mtry=100)

y_hat_rf = predict(rf_fit, newdata=test_set[,c(1,4,5,6,8,10,13:16)])
cm_rf <- confusionMatrix(table(y_hat_rf, test_set$HeartDisease))

draw_confusion_matrix(cm_rf)

#finding the best mtry and ntree
#inicializing "empty" values which we are later gonna change
ntree_best = 0
mtry_best = 0
accuracy_best = 0

#assigning max values for parameters which we are going to optimize
maxNtree <- 100
maxMtry = 15

#creating double loop to try different parameter values. Loops are not the most optimal, but in our case they are fast enough.
for (ntree in seq(from=1, to=maxNtree, by=2)) {
  for (mtry in seq(from=1, to=maxMtry, by=1)) {
    #setting seed, so we are going to be able to repeat the best parameters again with the same results. Probably could use better approach, but
    #in this case it is good enough
    set.seed(1, sample.kind="Rounding")
    rf_fit <- randomForest(as.matrix(train_set[,c(1,4,5,6,8,10,13:16)]), factor(train_set$HeartDisease), data=train_set, ntree=ntree, mtry=mtry)
    
    y_hat_rf = predict(rf_fit, newdata=test_set[,c(1,4,5,6,8,10,13:16)])
    cm_rf <- confusionMatrix(table(y_hat_rf, test_set$HeartDisease))
    
    #in case of better result, we change values stored in our variables
    if (cm_rf$overall["Accuracy"] > accuracy_best) {
      ntree_best = ntree
      mtry_best = mtry
      accuracy_best = cm_rf$overall["Accuracy"]
    }
  }
}

#show the results
ntree_best
mtry_best
accuracy_best

set.seed(1, sample.kind="Rounding")
#random forest with best parameters repeated
rf_fit <- randomForest(as.matrix(train_set[,c(1,4,5,6,8,10,13:16)]), factor(train_set$HeartDisease), data=train_set, ntree=ntree_best, mtry=mtry_best)

y_hat_rf = predict(rf_fit, newdata=test_set[,c(1,4,5,6,8,10,13:16)])
cm_rf <- confusionMatrix(table(y_hat_rf, test_set$HeartDisease))

draw_confusion_matrix(cm_rf)
