# Functions required for

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}



rand.which.max=function(x){
  index=((1:length(x))[x==(max(x))])
  return(sample(c(index,index),1))
}