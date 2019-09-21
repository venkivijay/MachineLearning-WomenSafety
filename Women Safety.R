library(caTools)
library(ggplot2)

#DATA-PREPROCESSING
#The dataset consists of categorical data which cannot be fed into any mathematical equations
#So, the categorical data are given unique numerical value using the factor function
datasets$Area=factor(datasets$Area)
datasets$Zone = factor(datasets$Zone)
datasets$Time = factor(datasets$Time,levels = c("Morning","Afternoon","Evening","Night"))
datasets$People.Frequency = factor(datasets$People.Frequency,levels = c("Low","Medium","High"))
datasets$Is.Police_Station = factor(datasets$Is.Police_Station,levels = c("Yes","No"))
datasets$Is.Bar = factor(datasets$Is.Bar,levels = c("Yes","No"))
datasets$Tier = factor(datasets$Tier,levels = c("Inner","Middle","Outer"))
datasets$Residence.Level = factor(datasets$Residence.Level,levels = c("Low","Medium","High"))
datasets$Class = factor(datasets$Class,levels = c("Safe","Unsafe"))

#DATA-SEPARATION
#The whole dataset is splited in a ratio of 0.75 for training and test sets
#The set.seed function generates random numbers based on the given parameter which helps to split the dataset
set.seed(123)
#sample.split function actually provides TRUE/FALSE values for all data in the dataset based on the seed
split = sample.split(datasets$Class,SplitRatio = 0.75)
#Then using the split table, TRUE are seperated into training sets and FLASE are seperated into test sets
training_set = subset(datasets,split==TRUE)
test_set = subset(datasets,split==FALSE)

#BINOMIAL-REGRESSION
binomial_resgressor = glm(Class~Zone+Time+People.Frequency+Is.Police_Station+Is.Bar+Residence.Level,family = "binomial",data = training_set)
predict_set = predict.glm(resgressor,type = "response",test_set)

#SVM-CLASSIFICATION
#e1070 is a library required to implement Support Vector Machine
library(e1071)
svm_model=svm(Class~.,data = datasets)

#MULTIVARIENT-REGRESSION
multivarient_regressor = lm(formula = Class~.,data = training_set)