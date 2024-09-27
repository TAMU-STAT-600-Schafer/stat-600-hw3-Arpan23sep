# Load the function
source("FunctionsLR.R")

# Load the dataset
data(iris)

# Preprocessing the data
X <- as.matrix(cbind(1, iris[, 1:4]))  # Add intercept
y <- as.integer(factor(iris$Species)) - 1  # Convert species to numeric 0, 1, 2

# Splitting the data 
set.seed(12345)
train_index <- sample(1:nrow(iris), 0.8 * nrow(iris))   # 80% train
test_index <- setdiff(1:nrow(iris), train_index)        #Remaining are test index

X_train <- X[train_index, ]  # Training data
y_train <- y[train_index]    # Training labels

X_test <- X[test_index,]    # Testing data
y_test <- y[test_index]     # Testing labels

# Running the multi-class logistic regression
iris_result <- LRMultiClass(X_train, y_train, X_test, y_test, numIter = 50, eta = 0.1, lambda = 0.5, beta_init = NULL)

# Print the results
plot(iris_result$objective, type = 'o')    
plot(iris_result$error_train, type = 'o')  #Train error 1.66 
plot(iris_result$error_test, type = 'o')   #Test eror  3.33


