X_train <- read.table("~/Desktop/HAR/Data/X_train.txt", quote="\"", comment.char="")
X_test <- read.table("~/Desktop/HAR/Data/X_test.txt", quote="\"", comment.char="")
y_train <- read.table("~/Desktop/HAR/Data/y_train.txt", quote="\"", comment.char="")
y_test <- read.table("~/Desktop/HAR/Data/y_test.txt", quote="\"", comment.char="")

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

names(y_train)<-c("class")
names(y_test)<-c("class")
ytot<-rbind(y_train,y_test)
xtot<-rbind(X_train,X_test)
HARdf<-data.frame(cbind(ytot,xtot))

#Standardize HAR

x<-HARdf[,2:562]
xbar<-apply(x,2,mean)
xbarmat<-cbind(rep(1,10299))%*%xbar
s<-apply(x,2,sd)
smat<-cbind(rep(1,10299))%*%s
z<-(x-xbarmat)/smat
mean(apply(z,2,mean))
mean(apply(z,2,sd))
z<-cbind(ytot,z)

#Train Standard

set.seed(5364)
ztrain=sample(nrow(z),nrow(z)*.7)
#z[train,]
#z[-train,]
class<-ytot$class

#Predictions

library(class)
stanpred1<-knn(train=z[ztrain,],test=z[-ztrain,],cl=as.factor(class)[ztrain],k=3)
stanconfmat1<-confmatrix(as.factor(class)[-ztrain],stanpred1)
stanconfmat1

stanpredcv3=knn.cv(train=z[2:562],cl=as.factor(class),k=3)
confmatstancv3<-confmatrix(as.factor(class),stanpredcv3)
confmatstancv3

accvect = rep(0,20)
for(k in 1:20) {
  tempred1 = knn.cv(train = z[,2:31], cl = as.factor(class), k = k)
  accvect[k] = confmatrix(as.factor(class), tempred1)$accuracy
}
k_0 = which.max(accvect)

createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}

folds=createfolds(nrow(z),10)
temperror10 = 0
for(i in 1:10) {                           
  train <- z[folds!=i,]
  test <- z[folds==i,]
  tempknn = knn(train[,2:31],test[,2:31],as.factor(class)[folds!=i],k_0)
  
  temp_list = confmatrix(as.factor(class)[folds==i], tempknn)
  
  temperror10 = temperror10 + temp_list$error
}
temperror10 = temperror10 / 10
print(temperror10)

k<-100
folds=createfolds(nrow(z),100)
temperror100 = 0

for(i in 1:100){
  train <- z[folds!=i,]
  test <- z[folds==i,]
  tempknn = knn(train[,2:31],test[,2:31],as.factor(class)[folds!=i],k_0)
  
  temp_list = confmatrix(as.factor(class)[folds==i], tempknn)
  
  temperror100 = temperror100 + temp_list$error
}
temperror100 = temperror100 / 100
print(temperror100)


library(kknn)
fithar1<-train.kknn(as.factor(class)~.,data=z, kmax=100, kernel=
                      c('rectangular','triangular',
                        'epanechnikov','biweight',
                        'triweight','cos','inv',
                        'gaussian','optimal'),distance=2)
ktype1<-(fithar1$best.parameters)$kernel
k<-(fithar1$best.parameters)$k

predkk1<-(kknn(as.factor(class)~.,train=z[ztrain,],test=z[-ztrain,], k=k,kernel=ktype1,distance=2))$fitted.values
confmkk1<-confmatrix(as.factor(class)[-ztrain],predkk1)
confmkk1

################## Neural Networks ################################

library(pROC)
library(nnet)
library(reshape)
library(scales)

train=sample(nrow(HARdf),nrow(HARdf)*.7)
model1<-nnet(as.factor(class)~.,data=HARdf[train,],size=1)
prednn1<-predict(model1,newdata=HARdf[-train,],type="class")
confmnn1<-confmatrix(as.factor(class)[-train],prednn1)
confmnn1
plot(model1)
phat1<-predict(model1,newdata=HARdf[-train,],type='class')
table(phat1>=0.5,prednn1)
plot(roc(response=as.factor(HARdf$class)[-train],predictor=as.numeric(phat1)))

model5<-nnet(as.factor(class)~.,data=HARdf[train,],size=5,MaxNWts=3000)
prednn5<-predict(model5,newdata=HARdf[-train,],type="class")
confmnn5<-confmatrix(as.factor(class)[-train],prednn5)
confmnn5
plot(model5)
phat5<-predict(model5,newdata=HARdf[-train,],type='class')
table(phat5>=0.5,prednn5)
plot(roc(response=as.factor(HARdf$class)[-train],predictor=as.numeric(phat5)))

nnfolds=createfolds(nrow(HARdf),10)

accmatrix=matrix(nrow=19,ncol=10)

for(netsize in 1:19){
  for(k in 1:10){
    temptest=HARdf[nnfolds==k,]
    temptrain=HARdf[nnfolds!=k,]
    
    tempnnet=nnet(as.factor(class)~.,data=temptrain,size=netsize,MaxNWts=20000)
    accmatrix[netsize,k]=confmatrix(as.factor(temptest$class),
                                    predict(tempnnet,newdata=temptest,type="class"))$accuracy
  }}

accmatrix
accvector=apply(accmatrix,1,mean)
plot(1:19,accvector)
which.max(accvector)           # 6, 9, 18

model6<-nnet(as.factor(class)~.,data=HARdf[train,],size=6,MaxNWts=3500)
prednn6<-predict(model6,newdata=HARdf[-train,],type="class")
confmnn6<-confmatrix(as.factor(class)[-train],prednn6)
confmnn6
plot(model6)
phat6<-predict(model6,newdata=HARdf[-train,],type='class')
table(phat6>=0.5,prednn6)
plot(roc(response=as.factor(HARdf$class)[-train],predictor=as.numeric(phat6)))

model9<-nnet(as.factor(class)~.,data=HARdf[train,],size=9,MaxNWts=7000)
prednn9<-predict(model9,newdata=HARdf[-train,],type="class")
confmnn9<-confmatrix(as.factor(class)[-train],prednn9)
confmnn9
plot(model9)
phat9<-predict(model9,newdata=HARdf[-train,],type='class')
table(phat9>=0.5,prednn9)
plot(roc(response=as.factor(HARdf$class)[-train],predictor=as.numeric(phat9)))
