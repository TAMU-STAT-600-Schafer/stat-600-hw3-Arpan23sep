# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if(all(X[, 1] == 1) && all(Xt[, 1] == 1)){
   }
  else{
    stop("First column of X and Xt are not ones:Please check!")
  }
  # Check for compatibility of dimensions between X and Y
  if(dim(X)[1] == length(y)){
    }
  else{
    stop("Dimension of X and Y are not compatible,PLease check!")
  }
  # Check for compatibility of dimensions between Xt and Yt
  if(dim(Xt)[1] == length(yt)){
   }
  else{
    stop("Dimension of Xt and yt are not compatible,PLease check!")
  }
  # Check for compatibility of dimensions between X and Xt
  if(dim(X)[2] == dim(Xt)[2]){
   }
  else{
    stop("Dimension of X and Xt are not compatible,PLease check!")
  }
  # Check eta is positive
  if(eta >0){
  }else{
    stop("Eta is not positive:Check!")
  }
  # Check lambda is non-negative
  if(lambda >= 0){
  }else{
    stop("Lamda is negative:Check!")
  }
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  p<-ncol(X)
  K<-length(unique(y))
  
  if(is.null(beta_init)){
    beta_init<-matrix(0,nrow = p,ncol = K)
  }else{
    if(dim(beta_init)[1]!=p || dim(beta_init)[2]!=K){
      stop("dimensions of beta_initial are not suitable.Please check!")
    }
  }
  
  error_train<-array(0,dim=numIter + 1)
  error_test<-array(0,dim=numIter + 1)
  objective<-array(0,dim=numIter + 1)
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  error_train[1]<-objective_fun(beta_init,X,y)$error
  error_test[1]<-objective_fun(beta_init,Xt,yt)$error
  objective[1]<-objective_fun(beta_init,X,y)$objective_value
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  beta_old<-beta_init
  indicator_mat<-sapply(0:(K-1),function(j) as.integer(y==j))
  for(i in 1:numIter){
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
   beta_new<-beta_old-eta * (solve(t(X) %*% W %*% X +lambda * I)) %*% (t(X) %*% (t(P)-indicator_mat) + lamda* beta_old) 
   error_train[i+1]<-objective_fun(beta_new,X,y)$error
   error_test[i+1]<-objective_fun(beta_new,Xt,yt)$error
   objective[i+1]<-objective_fun(beta_new,X,y)$objective_value
   
   beta_old<-beta_new
   }
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  beta<-beta_new
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}

objective_fun<-function(beta,X,y){
K<-length(unique(y))
n<-nrow(X)
P<-matrix(0,nrow=K,ncol=n)
for(i in 1:K){
  for(j in 1:n){
    P[i,j]<-t(X[,i]) %*% beta[,j]
    P<-apply(P,2,colSums(P),FUN = "/")
  }
}
indicator_mat<-sapply(0:(K-1),function(j) as.integer(y==j))
indicator_mat<-t(indicator_mat)
objective1<-sum(indicator_mat * log(P)) 
beta_norm<-norm(beta, type = "F")
objective_value<-(-objective1)+(lambda/2) * beta_norm
y_fit<-apply(P,2,FUN= "which.max")-1
error<-100 *(sum(y!=y_fit)/n)
return(list(objective_value=objective_value,error=error))
}

