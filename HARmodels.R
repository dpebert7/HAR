# Updated 17 November 2015
# David Ebert and Parker Rider
# Models for HAR data

#Hey hey hey

###################
### Import data ###
###################


X_train <- read.table("~/Desktop/HAR/Data/X_train.txt", quote="\"", comment.char="")
X_test <- read.table("~/Desktop/HAR/Data/X_test.txt", quote="\"", comment.char="")

y_train <- read.table("~/Desktop/HAR/Data/y_train.txt", quote="\"", comment.char="")
y_test <- read.table("~/Desktop/HAR/Data/y_test.txt", quote="\"", comment.char="")








#################
####  TREES #####
#################

library(rpart)
library(rattle)

# Begin by importing X_test, X_train, y_test, and y_train from the UCI repository.
# http://archive.ics.uci.edu/ml/machine-learning-databases/00240/
# UCI HAR Dataset.zips

##########################################
# Case 1: Active and passive states Tree #
##########################################

#set new y vectors to be TRUE/FALSE levels instead of 6 levels
bin.y_train = as.factor(as.numeric(y_train$V1)>3)
bin.y_test = as.factor(as.numeric(y_test$V1)>3)

#fit tre
bin.HARtree = rpart(bin.y_train~., data = X_train)
print(bin.HARtree)
fancyRpartPlot(bin.HARtree)

#training data
bin.train.tree.HAR.pred = predict(bin.HARtree, newdata = X_train)
bin.train.tree.HAR.pred = apply(bin.train.tree.HAR.pred, 1, which.max)
confmatrix(bin.y_train, bin.train.tree.HAR.pred)
#Training accuracy: 99.9%

#test data
bin.test.tree.HAR.pred = predict(bin.HARtree, newdata = X_test)
bin.test.tree.HAR.pred = apply(bin.test.tree.HAR.pred, 1, which.max)
confmatrix(bin.y_test, bin.test.tree.HAR.pred)
#Test accuracy: 100%


# This turns out to be unnecessary, due to the results in case 2
# Nevertheless, the tree in case 1 is simpler than in case 2
# Note that the tree in case one is splitting on only ONE category, 
#     namely V390, which is fBodyAccJerk-bandsEnergy()-1,16,
#     which I don't completely understand, but it has something
#     to do with the 1) the accelerometer, 2) the jerk, and
#     3) range of energy band???



##############################
# Case 2: All 6 States: Tree #
##############################

HARtree = rpart(y_train$V1~., data = X_train)
print(HARtree)
fancyRpartPlot(HARtree)


#Training
train.tree.HAR.pred = predict(HARtree, newdata = X_train)
train.tree.HAR.pred = apply(train.tree.HAR.pred, 1, which.max)
confmatrix(y_train$V1, train.tree.HAR.pred)
#Training accuracy: 89.5%


#Test
test.tree.HAR.pred = predict(HARtree, newdata = X_test)
test.tree.HAR.pred = apply(test.tree.HAR.pred, 1, which.max)
confmatrix(y_test$V1, test.tree.HAR.pred)


# Details of HARtree:
# V53 -> 6
# V266 & V560 -> 4 & 5
# V509 -> 3
# V75 & V441 -> 1 & 2

# Double check to make sure HARtree categorizes active and passive states with 100% accuracy
bin.train.tree.HAR.pred = predict(HARtree, newdata = X_train)
bin.train.tree.HAR.pred = bin.train.tree.HAR.pred>3
confmatrix(bin.y_train, bin.train.tree.HAR.pred)
#Training accuracy: 100%










#######################
####  naiveBayes  #####
#######################

library(e1071)

#Build model
HARbayes = naiveBayes(y_train$V1~., data = X_train)

#Training data
train.bayes.HAR.pred = predict(HARbayes, newdata = X_train, type = "raw")
train.bayes.HAR.pred = apply(train.bayes.HAR.pred, 1, rand.which.max)
confmatrix(y_train$V1, train.bayes.HAR.pred)
#Training accuracy: 74.5%

OAR.pred=HAR.levels[apply(vote.matrix,
                          1,
                          rand.which.max)]


#Test data
test.bayes.HAR.pred = predict(HARbayes, newdata = X_test, type = "raw")
test.bayes.HAR.pred = apply(test.bayes.HAR.pred, 1, rand.which.max)
confmatrix(y_test$V1, test.bayes.HAR.pred)
#Test accuracy: 77.0% 








###################
#### HAR SVM  #####
###################

library(e1071)

# Begin by importing X_test, X_train, y_test, and y_train from the UCI repository.
# http://archive.ics.uci.edu/ml/machine-learning-databases/00240/
# UCI HAR Dataset.zips

###############################
# Case 1: basic SVM in 2-case #
###############################

#Fit SVM model to training data
bin.HARsvm = svm(bin.y_train~., data = X_train, kernel = "linear", cost = 1000)

bin.train.SVM.HAR.pred = predict(bin.HARsvm, newdata = X_train)
confmatrix(bin.y_train, train.SVM.HAR.pred)

bin.test.SVM.HAR.pred = predict(bin.HARsvm, newdata = X_test)
confmatrix(bin.y_test, test.SVM.HAR.pred)


# 100% Accuracy. Linearly Separable. Yay!
plot(X_train$V390, col = bin.y_train)
plot(X_train$V390, col = y_train$V1)
# Because the data are in so many dimensions, it's difficult to see that it is linearly separable,
# but column V390 gives a pretty good idea that the two classes are distinct.


##############################################################
# Case 2: basic SVM in 6-case using one against one approach #
##############################################################

plot(X_train$V390, col = y_train$V1)
# A quick look at the same plot from above indicates taht there is more difficulty in separating the other cases,
# though, again, we must remember that this is just one attribute out of 561

# Note also that the plot indicates that the "silly walks" concept is feasible! A lot of the active states look
# different from one another.






#Adding "other" back to iris data
levels(y_train$V1)=c(levels(y_train$V1),"other")
levels(y_test$V1) = levels(y_train$V1)

#Levels of Species and k
HAR.levels=levels(y_train$V1)
k=length(HAR.levels)-1


#Creating lists to store models and predictions
model.list=vector(length=k,mode="list")
pred.list=vector(length=k,mode="list")


for(i in 1:k){
  i.train = X_train
  i.classes = y_train
  i.classes$V1[(i.classes$V1!=HAR.levels[i])] = "other"
  i.model = svm(i.classes$V1~., data = X_train)
  i.pred = predict(i.model, newdata = X_test, type = "class")
  
  model.list[[i]] = i.model
  pred.list[[i]] = i.pred
}



#Checking model list
model.list
pred.list[[1]]


#Votes
vote.matrix = matrix(0, nrow = nrow(y_test), ncol = k)
dim(vote.matrix)

for(i in 1:k){
  vote.matrix[,i]=vote.matrix[,i] + (pred.list[[i]]==HAR.levels[i])*1
  vote.matrix[,-i]=vote.matrix[,-i] + (pred.list[[i]]=="other")*1
}





#Creating one-against-rest predictions with rand.which.max
OAR.pred=HAR.levels[apply(vote.matrix,
                          1,
                          rand.which.max)]


OAR.pred

confmatrix(y_test$V1, OAR.pred)

#Accuracy is 93.4%.
# Interestingly, this new model has the highest accuracy, but also makes the worst mistakes.
# I.e. the model does not do well in the active vs. passive predictions




autoSVMmodel = svm(y_train$V1~., data = X_train)
autoSVMmodel.pred = predict(autoSVMmodel, newdata = X_test)
confmatrix(y_test$V1, autoSVMmodel.pred)
# This code does the same thing as above, except it's better and is only 3 lines instead of lots.
# Accuracy: 95.1%


#############################################################
# Case 3: Tune SVM in 6-case using one against one approach #
#############################################################

#Try it by hand first. Autotune
SVMparams = expand.grid(gamma = 10^(-4:1), cost = 10^(3:8))
SVMmodels = vector(length = nrow(SVMparams), mode = "list")
SVMconfmatrices = vector(length = nrow(SVMparams), mode = "list")
SVMparams

for(i in 1:nrow(SVMparams)){
  #make models
  SVM.model = svm(y_train$V1~.,
                  data = X_train,
                  gamma = SVMparams$gamma[i],
                  cost = SVMparams$cost[i])
  SVMmodels[[i]]=SVM.model
  
  #store confusion matrices
  temp.pred = predict(SVMmodels[[i]], newdata = X_test)
  SVMconfmatrices[[i]] = confmatrix(y_test$V1, temp.pred)
  
  #fraction completed
  print(i/nrow(SVMparams))
}

SVMmodels
SVMconfmatrices

# Round 1 params: gamma = 10^(-5:-1), cost = 10^(0:3)
# Best results from round 1: gamma = 1e-04; cost = 1000; accuracy = 96.1

# Round 2 params: gamma = 10^(-6:0), cost = 10^(1:4)
# Best results from round 2: gamma = 1e-04; cost = 10000
# accuracy: 96.3%

# Round 3 params: gamma = 10^(-4,1), cost = 10^(3,8)
# 




