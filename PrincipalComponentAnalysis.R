library(caTools)
library(caret)
library(e1071)
#DATA-PREPROCESSING
#DATA-SPLITING
set.seed(123)
split = sample.split(datasets$Class,SplitRatio = 0.75)
training_set = subset(datasets,split==TRUE)
test_set = subset(datasets,split==FALSE)
#SCALING TO BRING DATA TO COMMON RANGE
training_set[-9]=scale(training_set[-9])
test_set[-9]=scale(test_set[-9])
#PCA MODEL GENERATION
pca = preProcess(x=training_set[-9],method = "pca",pcaComp = 2)
#PREDICTING THE TEST AND TRAINING SETS WITH THE GENERATED PCA MODEL
training_pca_set = predict(pca,training_set)
training_pca_set = training_pca_set[c(2,3,1)]
test_pca_set = predict(pca,test_set)
test_pca_set = test_pca_set[c(2,3,1)]