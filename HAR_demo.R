source("http://faculty.tarleton.edu/crawford/documents/Math5364/MiscRFunctions.txt")

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}
set.seed(5364)

#Standardize X_train

xtrnbar<-apply(X_train,2,mean)
xtrnbarmat=cbind(rep(1,nrow(X_train)))%*%xtrnbar
sxtrn=apply(X_train,2,sd)
sxtrnmat=cbind(rep(1,nrow(X_train)))%*%sxtrn
stdxtrn=(X_train-xtrnbarmat)/sxtrnmat
#apply(stdxtrn,2,mean)
#apply(stdxtrn,2,sd)

#Standardize X_test

xtstbar<-apply(X_test,2,mean)
xtstbarmat=cbind(rep(1,nrow(X_test)))%*%xtstbar
sxtst=apply(X_test,2,sd)
sxtstmat=cbind(rep(1,nrow(X_test)))%*%sxtst
stdxtst=(X_test-xtstbarmat)/sxtstmat
#apply(stdxtst,2,mean)
#apply(stdxtst,2,sd)

#make std x set and y set

xtot<-rbind(X_train,X_test)
stdx<-rbind(stdxtrn,stdxtst)
ytot<-rbind(y_train,y_test)$V1


#predictions

library(class)
pred<-knn(train=stdxtrn,test=stdxtst,cl=y_train$V1,k=3)
confmat1<-confmatrix(y_test$V1,pred)
confmat1

predcv3=knn.cv(train=stdxtrn,cl=y_train$V1,k=3)
confmat2<-confmatrix(y_train$V1,predcv3)
confmat2

predcv3b=knn.cv(train=rbind(stdxtrn,stdxtst),cl=rbind(y_train,y_test)$V1,k=3)
confmat3<-confmatrix(rbind(y_train,y_test)$V1,predcv3b)
confmat3

accvect = rep(0,10)
for(k in 1:10){
  predcvk = knn.cv(train = rbind(stdxtrn,stdxtst),cl=rbind(y_train,y_test)$V1, k = k)
  accvect[k] = confmatrix(rbind(y_train,y_test)$V1, predcvk)$accuracy
}
k_0 = which.max(accvect)
k_0

accvector<-rep(0,10)
for(k in 1:10){
  predcvka = knn.cv(train =stdxtrn,cl=y_train$V1, k = k)
  accvector[k] = confmatrix(y_train$V1, predcvka)$accuracy
}
k_0a<-which.max(accvector)
k_0a

k<-10
createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}
folds=createfolds(nrow(stdx),10)
temperror10 = 0
for(i in 1:10){
  train <- stdx[folds!=i,]
  test <- stdx[folds==i,]
  tempknn = knn(train,test,ytot[folds!=i],k_0)
  
  temp_list = confmatrix(ytot[folds==i], tempknn)
  
  temperror10 = temperror10 + temp_list$error
}
temperror10 = temperror10 / 10
print(temperror10)

k<-100
folds=createfolds(nrow(stdx),100)
temperror100 = 0

for(i in 1:100){
  train <- stdx[folds!=i,]
  test <- stdx[folds==i,]
  tempknn = knn(train,test,ytot[folds!=i],k_0)
  
  temp_list = confmatrix(ytot[folds==i], tempknn)
  
  temperror100 = temperror100 + temp_list$error
}
temperror100 = temperror100 / 100
print(temperror100)

library(kknn)
fithar1<-train.kknn(ytot~.,data=xtot, kmax=15, kernel=
                     c('rectangular','triangular',
                       'epanechnikov','biweight',
                       'triweight','cos','inv',
                       'gaussian','optimal'),distance=2)
ktype1<-(fithar1$best.parameters)$kernel
k<-(fithar1$best.parameters)$k

fithar100<-train.kknn(ytot~.,data=xtot, kmax=100, kernel=
                      c('rectangular','triangular',
                        'epanechnikov','biweight',
                        'triweight','cos','inv',
                        'gaussian','optimal'),distance=2)
ktype100<-(fithar100$best.parameters)$kernel
k100<-(fithar100$best.parameters)$k

predkk1<-(kknn(as.factor(y_train$V1)~.,train=stdxtrn,test=stdxtst, k=k100,kernel=ktype100,distance=2))$fitted.values
confmkk1<-confmatrix(y_test$V1,predkk1)
confmkk1

library(pROC)
library(nnet)
library(reshape)

model1<-nnet(as.factor(y_train$V1)~.,data=X_train,size=1)
prednn1<-predict(model1,newdata=X_test,type="class")
confmnn1<-confmatrix(y_test$V1,prednn1)
confmnn1
plot(model1)
