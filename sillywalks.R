# Given walking data for 2 people, can we determine who the person is?

#######################
#### Set up data ######
#######################

# Import subject factor data
subject_train <- read.table("~/Desktop/HAR/Data/subject_train.txt", quote="\"", comment.char="")
subject_test <- read.table("~/Desktop/HAR/Data/subject_test.txt", quote="\"", comment.char="")

# Combine data into one large data frame
walkerY = rbind(subject_train, subject_test)
walkerX = rbind(X_train, X_test)

# Reduce training data so it is only 1, 2, or 3: Walking, walking up, or walking down.
walkerX = walkerX[(as.numeric(y_train$V1) < 4),]
walkerY = walkerY[(as.numeric(y_train$V1) <4),]


#Select walker records for walkers 1,2, 3, and 7
walkerX.example = walkerX[walkerY %in% c(1:3, 7),]
walkerY.example = walkerY[walkerY %in% c(1:3, 7)]


########################################################################
### Use a tree to find training and test accuracy for n<= 30 walkers ###
########################################################################

library(rpart)
library(rattle)

n = 10
set.seed(1)
walkerID= sample(30,n)

walkerX.sample = walkerX[walkerY %in% walkerID,]
walkerY.sample = as.factor(walkerY[walkerY %in% walkerID])

# 70% training and 30% test data
train = sample(length(walkerY.sample), length(walkerY.sample)*.7)

# rpart tree
library(rpart)
tree = rpart(walkerY.sample[train] ~., data = walkerX.sample[train,])
train.treepred = predict(tree, newdata = walkerX.sample[train,], type = "class")
train.treeresult = confmatrix(walkerY.sample[train], train.treepred)

test.treepred = predict(tree, newdata = walkerX.sample[-train,], type = "class")
test.treeresult = confmatrix(walkerY.sample[-train], test.treepred)


# Accuracy with 3 walkers: ~97%
# Accuracy with 10 walkers: ~84%
# Accuracy with 15 walkers: ~70%
# Accuracy with 20 walkers: ~65%
# Accuracy with 25 walkers: ~63%
# Accuracy with all 30 walkers: 49.29%    <- THAT IS AWESOME!

#Pretty print the above table:
a = matrix(1:20, nrow = 5, ncol = 4)
library(gridExtra)
grid.table(a)
grid.table(test.treeresult$matrix)

write.table(test.treeresult$matrix, file = "tenWalkers.csv", sep = ",")

#######################################################################
### Find how tree accuracy changes with increased number of walkers ###
#######################################################################

maxWalkers = 29
nIterations = 8

bigAccVector = 1:maxWalkers

set.seed(1)
for(i in 2:maxWalkers){
  
  smallAccVector = 1:nIterations
  
  for(j in 1:nIterations){
    
    walkerID= sample(30,i)
    
    walkerX.sample = walkerX[walkerY %in% walkerID,]
    walkerY.sample = as.factor(walkerY[walkerY %in% walkerID])
    
    # 70% training and 30% test data
    train = sample(length(walkerY.sample), length(walkerY.sample)*.7)
    
    # rpart tree
    library(rpart)
    tree = rpart(walkerY.sample[train] ~., data = walkerX.sample[train,])
    test.treepred = predict(tree, newdata = walkerX.sample[-train,], type = "class")
    test.treeresult = confmatrix(walkerY.sample[-train], test.treepred)
    
    smallAccVector[j] = test.treeresult$accuracy
  }
  print(i)
  bigAccVector[i] = mean(smallAccVector)
}

bigAccVector

finalAccVectorTREE = c(bigAccVector, 0.4992) #Note that the case of 30 walkers is the same no matter which 30 are chosen...

plot(finalAccVectorTREE,
     xlab = "Number of Walkers Compared",
     ylab = "Average Accuracy of Model Over 8 Iterations",
     main = "SVM: Number of Walkers vs Model Accuracy")











#####################################################################
### Use SVM to find training and test accuracy for n<= 30 walkers ###
#####################################################################

library(e1071)

n = 30
set.seed(1)
walkerID= sample(30,n)

walkerX.sample = walkerX[walkerY %in% walkerID,]
walkerY.sample = as.factor(walkerY[walkerY %in% walkerID])

# 70% training and 30% test data
train = sample(length(walkerY.sample), length(walkerY.sample)*.7)

# svm model
svmModel = svm(walkerY.sample[train] ~., data = walkerX.sample[train,])
train.svmpred = predict(svmModel, newdata = walkerX.sample[train,], type = "class")
train.svmresult = confmatrix(walkerY.sample[train], train.svmpred)
train.svmresult

test.svmpred = predict(svmModel, newdata = walkerX.sample[-train,], type = "class")
test.svmresult = confmatrix(walkerY.sample[-train], test.svmpred)
test.svmresult

# Accuracy with 3 walkers: 
# Accuracy with 10 walkers: 
# Accuracy with 15 walkers: 
# Accuracy with 20 walkers: 
# Accuracy with 25 walkers: 
# Accuracy with all 30 walkers:     <- THAT IS AWESOME!

#Pretty print the above table:
a = matrix(1:20, nrow = 5, ncol = 4)
library(gridExtra)
grid.table(a)
grid.table(test.treeresult$matrix)

write.table(test.treeresult$matrix, file = "tenWalkers.csv", sep = ",")

#######################################################################
### Find how tree accuracy changes with increased number of walkers ###
#######################################################################

maxWalkers = 29
nIterations = 8

bigAccVector = 1:maxWalkers

set.seed(1)
for(i in 2:maxWalkers){
  
  smallAccVector = 1:nIterations
  
  for(j in 1:nIterations){
    
    walkerID= sample(30,i)
    
    walkerX.sample = walkerX[walkerY %in% walkerID,]
    walkerY.sample = as.factor(walkerY[walkerY %in% walkerID])
    
    # 70% training and 30% test data
    train = sample(length(walkerY.sample), length(walkerY.sample)*.7)
    
    # rpart tree
    svmModel = svm(walkerY.sample[train] ~., data = walkerX.sample[train,])
    test.svmpred = predict(svmModel, newdata = walkerX.sample[-train,], type = "class")
    test.svmresult = confmatrix(walkerY.sample[-train], test.svmpred)
    
    smallAccVector[j] = test.svmresult$accuracy
  }
  print(i)
  bigAccVector[i] = mean(smallAccVector)
}

bigAccVector

finalAccVectorSVM = c(bigAccVector, 0.9496) #Note that the case of 30 walkers is the same no matter which 30 are chosen...

plot(finalAccVectorTREE,
     xlab = "Number of Walkers Compared",
     ylab = "Average Accuracy of Model Over 8 Iterations",
     main = "SVM: Number of Walkers vs Model Accuracy",
     type = "l",
     col = "red")
lines(finalAccVectorSVM,
      type = "l",
      col = "black")

