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



n = 15
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
# Accuracy with 15 walkers: 
# Accuracy with 20 walkers: 
# Accuracy with 25 walkers: 
# Accuracy with all 30 walkers: 50%    <- THAT IS AWESOME!


#####################################################################
### Use SVM to find training and test accuracy for n<= 30 walkers ###
#####################################################################

