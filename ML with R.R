#MACHINE LEARNING PROJECT (SEX PREDICTION USING HEIGHT)
library(tidyverse)
library(caret)
library(dslabs)
data(heights)
#define the outcomes and predictors
y <- heights$sex
x <- heights$height
is.atomic(y)
#view(heights)

#randomly generating the training and test dataset from the heights dataset

#Defining createDataPartition function
set.seed(2007) # USED WITH random number generator. it is used to reproduce same sample again and again instead of generating
               #different samples at different times of execution
test_index <-createDataPartition(y, times = 1, p = 0.5, list = FALSE)

#calling the function
test_set <-heights[test_index, ]# the comma and space converts test_set from character to a data.frame
train_set <-heights[-test_index, ] # the comma and space converts train_set from character to a data.frame
class(test_set)
#view(test_set)
#view(train_set)

#Determining the overall accuracy
#step 1: create an algorithm that guesses the outcomes


y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))
view(y_hat)

mean(y_hat == test_set$sex)#comparing the guessed values with the actual test_set known outcomes
#accuracy is about 49.14%

#USING EDA to improve our accuracy
 heights%>%group_by(sex)%>%summarize(mean(height),sd(height))
 #we find that avg height of men is 69.3 and ladies is 64.9 using this insight we can then make a better prediction
 y_hat <- ifelse(x>62,"Male","Female")%>%factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
#accuracy shoots up from 49 to 74 %

#examining the accuracy for 10 different cutoff points to get the optimum cutoff point
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male","Female")%>%factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
plot(cutoff,accuracy)

max(accuracy)
#max accuracy is 85%. the corresponding cutoff point is determined as shown below
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff #best cutoff is fpund to be 64

#to ensure the results are not overly optimistic
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
  
#THE CONFUSION MATRIX
#AS NOTED, THE AVERAGE HEIGHT OF GIRLS IS 64 BUT A CUTOFF OF 64 YIELDS THE BEST RESULT WHICH SEEMS ODD AND INCORRECT
#TABULATING PREDECTED AND ACTUAL VALUE
table(predicted = y_hat, actual = test_set$sex)
#calculating accuracy separately for each sex we have
view(y_hat)
test_set %>%  mutate(y_hat = y_hat) %>% group_by(sex) %>% summarize(accuracy = mean(y_hat == sex))
view(test_set)
#prevalence of men is high in the dataset thus the overall accuracy is high yet individual sex prediction in slow for females
prevalence <- mean(y == "Male")
prevalence
#confusion matrix to determine ppv,accuracy,sensitivity and specificity
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm$overall["Accuracy"]
cm$byClass[c("Sensitivity","Specificity","Prevalence")]
#finding cutoff using F score (i.e maximizing the F score instead of overall accuracy)
cut_off <- seq(61, 70)
f_1 <- map_dbl(cut_off, function(x){
   y_hat = ifelse(train_set$height > x,"Male","Female") %>% 
     factor(levels = levels(test_set$sex))
   F_meas(data = y_hat, reference = factor(train_set$sex))
})
plot(cut_off, f_1, type = "l" , col = "green", main = "Graph of F scores")
max(f_1)
#max value is found to be 0.67 thus best cut off is found to be 
best_cutoff = cutoff[which.max(f_1)]
best_cutoff
#best_cutoff is found to be 66
#checking sensitivity and specificity
y_hat = ifelse(test_set$height > best_cutoff, "Male","Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex )
#we have a sensitivity of 0.63
specificity(data = y_hat, reference = test_set$sex)
#we have a sensitivity of o.83
#using the guessing technique but guessing male with a higher probability
p = 0.9
n = length(test_index)
y_hat = sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
#generating the ROC Curve
probs = seq(0,1, length.out = 10)
guessing = map_df(probs, function(p){
  y_hat = sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p)) %>% 
    factor(levels = c("Male","Female"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat,test_set$sex))
})
