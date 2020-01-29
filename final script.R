library(rstudioapi)
library(ggplot2)
library(caret)
library(dplyr)
library(readr)


# Load the data
setwd(dirname(getActiveDocumentContext()$path))
training <- read_csv("~/Desktop/Datathon_wine-master/data/training.csv")


dim(training)

str(training)

names(training)

sum(is.na(training$quality))

training$quality <- as.factor(training$quality)

ggplot(training, aes(quality)) +
  geom_bar()  +
  labs(title=" Dependent variable distribution",
       x ="Quality of the wine",
       y = "")

set.seed(42)
index <- createDataPartition(training$quality, p = 0.5, list = FALSE)
train_data <- training[index, ] 
test_data  <- training[-index, ]


          # SMOTE by Caret
          ctrl <- trainControl(method = "repeatedcv", number = 10, sampling = "smote")
          set.seed(42)
          model_rf_smote <- train(quality ~ ., data = train_data, method = "rf", trControl = ctrl)
          final_smote <- data.frame(actual = test_data$quality, predict(model_rf_smote, test_data))
          
          cm_smote <- confusionMatrix(final_smote$predict, test_data$quality)
          
          # comparing performance
          models <- list(original = model_rf,
                         under = model_rf_under,
                         smote = model_rf_smote)
          
          resampling <- resamples(models)
          bwplot(resampling)

#####Smote in Caret was not working ######
###smotefamily too###
#library(smotefamily)
#SMOTE(training,quality, K = 2, dup_size = 100)
#training$quality <- factor(ifelse(training$quality == "3","4")) 
#table(training$quality)


library(DMwR)
train_data <- as.data.frame(train_data)
training <- as.data.frame(training)
test_data <- as.data.frame(test_data)
set.seed(42)
tdsmote <- SMOTE(quality~.,train_data, perc.over = 1000, k = 4, perc.under = 11000)

 table(tdsmote$quality)

 tdsmote <- as.data.frame(tdsmote) 
 na.exclude(tdsmote)
 
 tdsmote$quality <- as.factor(tdsmote$quality)
 
 ggplot(training, aes(quality)) +
   geom_bar()  +
   labs(title=" Dependent variable distribution",
        x ="Quality of the wine",
        y = "")
 
 tdsmote <- tdsmote[complete.cases(tdsmote), ]
 
 
 # baseline by rf in Caret
   set.seed(42)
 model_rf <- train(quality ~ ., data = tdsmote, method = "rf", 
                   trControl = trainControl(method = "cv", number = 2))
 
 final <- data.frame(actual = train_data$quality, predict(model_rf, validation))
 finalresults <- predict(model_rf, validation)
 finalresults
 
 # Once you have your predicsion, load the validation and fill the quality feature
 validation <- read.csv("data/validation.csv", sep = ",", header = TRUE)
 validation <- as.data.frame(validation)
 
 validation$quality <- finalresults
 
 print(validation$quality)
 
write.csv(validation, file = "~/Desktop/Datathon_wine-master/data/predictionsGHERA.csv")
