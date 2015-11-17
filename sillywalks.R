# Reduce training data so it is only 1, 2, or 3: Walking, walking up, or walking down.
active.X_train = X_train[(as.numeric(y_train$V1) < 4),]
active.X_test = X_test[(as.numeric(y_test$V1) < 4),]

active.y_train = y_train[(as.numeric(y_train$V1) < 4),]
active.y_test = y_test[(as.numeric(y_test$V1) < 4),]

active.subject_test = as.factor(subject_test[(as.numeric(y_test$V1) < 4),])
active.subject_train = as.factor(subject_train[(as.numeric(y_train$V1) < 4),])


# Combine data into one large data frame





# Select volunteers 1 and 3 from active.subject_train
table(active.subject_train)

# 1 3 5 6 7 8 11 15 16...


active.X_1 = active.X_train[active.subject_train == 6,]
active.X_3 = active.X_train[active.subject_train == 7,]
active.subject_1 = active.subject_train[active.subject_train == 6]
active.subject_3 = active.subject_train[active.subject_train == 7]


active.X = rbind(active.X_1, active.X_3)
active.y = as.factor(rbind(c(active.subject_1, active.subject_3)))


# Split active.X into 70% training data
set.seed(1)
train=sample(nrow(active.X),nrow(active.X)*.7)

# rpart tree
library(rpart)
tree = rpart(active.y[train]~., data = active.X[train,])
treepred = predict(tree, newdata = active.X[-train,], type = "class")
confmatrix(active.y[-train], treepred)





