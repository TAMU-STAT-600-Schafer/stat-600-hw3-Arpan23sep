# Application of multi-class logistic to letters data

# Load the letter data
#########################
# Training data
letter_train <- read.table("Data/letter-train.txt", header = F, colClasses = "numeric")
Y <- letter_train[, 1]
X <- as.matrix(letter_train[, -1])

# Testing data
letter_test <- read.table("Data/letter-test.txt", header = F, colClasses = "numeric")
Yt <- letter_test[, 1]
Xt <- as.matrix(letter_test[, -1])

# [ToDo] Make sure to add column for an intercept to X and Xt
one_train<-array(1,dim = dim(X)[1])                #Defining array of 1's
one_test<-array(1,dim = dim(Xt)[1])
X<-cbind(one_train,X)                             #Making first column of 1's
Xt<-cbind(one_test,Xt)
# Source the LR function
source("FunctionsLR.R")

# [ToDo] Try the algorithm LRMultiClass with lambda = 1 and 50 iterations. Call the resulting object out, i.e. out <- LRMultiClass(...)
out<-LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL)

# The code below will draw pictures of objective function, as well as train/test error over the iterations
plot(out$objective, type = 'o')    #Train error 21.70
plot(out$error_train, type = 'o')   #Test eror 26.30
plot(out$error_test, type = 'o')

# Feel free to modify the code above for different lambda/eta/numIter values to see how it affects the convergence as well as train/test errors

#Differenet values of Lambda
#lambda_0.5<-LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 0.5, beta_init = NULL)     #Train error:21.60   Test error:26.16
#lambda_1.5<-LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1.5, beta_init = NULL)     #Train eror:21.95    Test eror:26.26

#Different values of eta
#eta_0.05<-LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.05, lambda = 0.5, beta_init = NULL)      #Train error:23.75   Test error:27.838
#eta_1.5<-LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 1.5, lambda = 0.5, beta_init = NULL)        #Train error:NA     Test error:NA

#Different values of numIter
#numIter_25<-LRMultiClass(X, Y, Xt, Yt, numIter = 25, eta = 0.05, lambda = 0.5, beta_init = NULL)      #Train error:26.35   Test error:29.92
#numIter_100<-LRMultiClass(X, Y, Xt, Yt, numIter = 100, eta = 1.5, lambda = 0.5, beta_init = NULL)     #Train error:NA     Test error:NA

# [ToDo] Use microbenchmark to time your code with lambda=1 and 50 iterations. To save time, only apply microbenchmark 5 times.
result<-microbenchmark(out<-LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL),times=5)
print(result)
# [ToDo] Report the median time of your code from microbenchmark above in the comments below
#Median time:1.38127(in sec)(Apple M1 chip)