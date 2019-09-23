library(caTools)
library(ggplot2)
library(caret)

#DATA-PREPROCESSING
#The dataset consists of categorical data which cannot be fed into any mathematical equations
#So, the categorical data are given unique numerical value using the factor function
datasets$Area=factor(datasets$Area,levels = c("Ramapuram","West Mambalam","Adyar"),labels = c(1,2,3))
datasets$Zone = factor(datasets$Zone,levels = c("Aanandam Nagar","Ambal Nagar","Amman Nagar","Chidambaram Nagar","Easwaran Nagar","Gokulam Colony","Moogambigai Nagar","Mullai Nagar","Royala Nagar","Sakthi Nagar","Suresh Nagar","Tamil Nagar","Thiru Nagar","Venkateshwara Nagar","Postal Colony","RamaKrishnapuram","Vivekanandapuram","Moovendar Colony","Kasi Viswanathar Colony","Pannerselvam Nagar","Shastri Nagar","Subramaniam Nagar","Baktavatsalm Nagar","Venkateshwar Nagar","Teachers Colony","South Kesavaperumalpuram"),labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26))
datasets$Time = factor(datasets$Time,levels = c("Morning","Afternoon","Evening","Night"),labels = c(1,2,3,4))
datasets$People.Frequency = factor(datasets$People.Frequency,levels = c("Low","Medium","High"),labels = c(1,2,3))
datasets$Is.Police_Station = factor(datasets$Is.Police_Station,levels = c("Yes","No"),labels = c(1,2))
datasets$Is.Bar = factor(datasets$Is.Bar,levels = c("Yes","No"),labels = c(1,2))
datasets$Tier = factor(datasets$Tier,levels = c("Inner","Middle","Outer"),labels = c(1,2,3))
datasets$Residence.Level = factor(datasets$Residence.Level,levels = c("Low","Medium","High"),c(1,2,3))
datasets$Class = factor(datasets$Class,levels = c("Safe","Unsafe"),labels = c(1,2))

#DATA-SEPARATION
#The whole dataset is splited in a ratio of 0.75 for training and test sets
#The set.seed function generates random numbers based on the given parameter which helps to split the dataset
set.seed(123)
#sample.split function actually provides TRUE/FALSE values for all data in the dataset based on the seed
split = sample.split(datasets$Class,SplitRatio = 0.75)
#Then using the split table, TRUE are seperated into training sets and FLASE are seperated into test sets
training_set = subset(datasets,split==TRUE)
test_set = subset(datasets,split==FALSE)

#SCALING TO BRING DATA TO COMMON RANGE
training_set[-9]=scale(training_set[-9])
test_set[-9]=scale(test_set[-9])

#PCA MODEL GENERATION
pca = preProcess(x=training_set[-9],method = c("pca"),pcaComp = 2)
#PREDICTING THE TEST AND TRAINING SETS WITH THE GENERATED PCA MODEL
training_pca_set = predict(pca,training_set)
training_pca_set = training_pca_set[c(2,3,1)]
test_pca_set = predict(pca,test_set)
test_pca_set = test_pca_set[c(2,3,1)]

#BINOMIAL-REGRESSION
binomial_resgressor = glm(Class~.,family = "binomial",data = training_pca_set)
predict_set = predict.glm(binomial_resgressor,type = "response",test_pca_set)

#plot for binomial-regression
par(mfrow=c(1,3))
for(i in 1:3) {
  boxplot(predict_set[,i], main=names(predict_set)[i])
}


#SVM-CLASSIFICATION
#e1070 is a library required to implement Support Vector Machine
library(e1071)
svm_model=svm(Class~.,data = datasets)

#MULTIVARIENT-REGRESSION
multivarient_regressor = lm(formula = Class~.,data = training_set)
#plot(svm_model,data = datasets,dependentvariable~independent,slice=list(missvar=position,missvar=position))

type.convert(datasets)