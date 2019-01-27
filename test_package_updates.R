
## Test boosted_regression_forest
n = 2000; p = 10
X = matrix(runif(n * p), n, p)
mu = 2 * X[,1] * X[,2] + 3 * X[,3] + 4 * X[,4]
Y = mu + rnorm(n)

boost_reg_forest = boosted_regression_forest(X,Y)
mean((Y-predict(boost_reg_forest))^2) 

set.seed(1)
##Test implementation of causal forest that uses boosted forest 
n = 500; p = 10
X = matrix(rnorm(n * p), n, p)
TAU = 1 / (1 + exp(-X[, 3]))
E.W = 1 / (1 + exp(-X[, 1] - X[, 2]))
W = rbinom(n ,1, E.W)
E.Y = pmax(X[, 2] + X[, 3], 0) + rowMeans(X[, 4:6]) / 2 + W * TAU
Y = E.Y + rnorm(n)

tau.forest.boost = causal_forest(X, Y, W,
                           tune.parameters = TRUE,boosting=TRUE)

tau.forest = causal_forest(X, Y, W,
                          tune.parameters = TRUE,boosting=FALSE)

tau.hat.boost = predict(tau.forest.boost)$predictions
mse.tau.boost = mean((TAU - tau.hat.boost)^2)

tau.hat.forest = predict(tau.forest)$predictions
mse.tau.forest = mean((TAU - tau.hat.forest)^2)

print(mse.tau.boost)
print(mse.tau.forest)

