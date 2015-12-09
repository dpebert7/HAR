# reduction.R
# Updated 5 December 2015
# David Ebert

################################################
# Dimensionality reduction applied to HAR data #
################################################

###############
# Import data #
###############
X_test <- read.table("C:/Users/000678922/Desktop/HAR/X_test.txt", 
                     quote="\"")
X_train <- read.table("C:/Users/000678922/Desktop/HAR/X_train.txt", 
                      quote="\"")
y_test <- read.table("C:/Users/000678922/Desktop/HAR/y_test.txt", 
                     quote="\"")
y_train <- read.table("C:/Users/000678922/Desktop/HAR/y_train.txt", 
                      quote="\"")

y_test = as.factor(y_test$V1)
y_train = as.factor(y_train$V1)


################################################
# Reduce dimensions using PCA using preProcess #
# function from caret package                  #
################################################
library(caret)
trans = preProcess(X_train, method = c("BoxCox", 
                                       "center", 
                                       "scale", 
                                       "pca"))
head(trans)
pcaTrain = predict(trans, X_train)
pcaTest = predict(trans, X_test)
# Data reduced to "only" 102 columns!
# Preserves 95% of variance

plot(pcaTrain$PC1, 
     pcaTrain$PC2, 
     col = y_train, 
     pch = c(1:6)[as.numeric(y_train)])
# Plot of first 2 components of training data 
# shows some clustring possibilities!

################################
# Tree applied to reduced data #
################################
library(rpart)
pcaTree = rpart(as.factor(y_train$V1)~., data = pcaTrain)
treepred = predict(pcaTree, newdata = pcaTest, type = "class")
confusionMatrix(data = treepred, reference = y_test)

# New accuracy is a fairly bad  73.1%


##############################
#SVM applied to reduced data #
##############################
library(e1071)
pcaSVM = svm(y_train~., data = pcaTrain)
svmpred = predict(pcaSVM, 
                  newdata = pcaTest, 
                  type = "class")
confusionMatrix(data = svmpred, reference = y_test)
# New accuracy is a still-good 93.7%

pcaSVMtuned = svm(y_train~., 
                  data = pcaTrain, 
                  cost = 10000, 
                  gamma = 1e-04)
svmtunedpred = predict(pcaSVMtuned, 
                       newdata = pcaTest, 
                       type = "class")
confusionMatrix(data = svmtunedpred, reference = y_test)
# Tuned SVM accuracy is a now-WORSE 91.7%


##############################
#SVM applied to reduced data #
##############################

traindata = cbind(y_train, pcaTrain)

tunespecs <- tune(svm, y_train~., data = traindata, 
                  ranges = list(gamma = 2^(-1:3), cost = 2^(1:6)))

pcaSVMtuned = svm(y_train~., 
                  data = pcaTrain, 
                  cost = 4, 
                  gamma = 0.5)
svmtunedpred = predict(pcaSVMtuned, 
                       newdata = pcaTest, 
                       type = "class")
confusionMatrix(data = svmtunedpred, reference = y_test)
# Tuned SVM accuracy is a now-WORSE 91.7%