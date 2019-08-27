
## Test boosted_regression_forest
#compare_mse <-function(n=1000,p=6) { 

n=1000 
p=10 
  X = matrix(runif(n * p), n, p)
  mu = 2 * X[,1] * X[,2] + 3 * X[,3] + 4 * X[,4]
  Y = mu + rnorm(n)
  
  tic("train boosted forest")
  boost_reg_forest <- boosted_regression_forest(X,Y, tune.parameters=TRUE,boost.error.reduction=0.97)
  toc()
  mse.boost <- mean((Y-predict(boost_reg_forest)$predictions)^2) 
  print(length(boost_reg_forest$forests))
  print(boost_reg_forest$error)
  #40 seconds for 1 step, 170 seconds for regression_forest 
  tic("train regression forest")
  reg_forest <- regression_forest(X,Y,tune.parameters=TRUE) 
  mse.forest<- mean((Y-predict(reg_forest)$predictions)^2)
  toc()

  print(c(mse.boost,mse.forest))
  #return(c(mse.boost,mse.forest)) 
#} 




set.seed(1234)
##Test implementation of causal forest that uses boosted forest 
n = 1000; p = 8
X = matrix(rnorm(n * p), n, p)
TAU = 1 / (1 + exp(-X[, 3]))
E.W = 1 / (1 + exp(-X[, 1] - X[, 2]))
W = rbinom(n ,1, E.W)
E.Y = pmax(X[, 2] + X[, 3], 0) + rowMeans(X[, 4:6]) / 2 + W * TAU
Y = E.Y + rnorm(n)

tic("boost")
tau.forest.boost = causal_forest(X, Y, W,orthog.boosting=TRUE,
                           tune.parameters = TRUE) 

toc()
tic("forest")
tau.forest = causal_forest(X, Y, W,
                          tune.parameters = TRUE,orthog.boosting=FALSE)
toc()
tau.hat.boost = predict(tau.forest.boost)$predictions
mse.tau.boost = mean((TAU - tau.hat.boost)^2)

tau.hat.forest = predict(tau.forest)$predictions
mse.tau.forest = mean((TAU - tau.hat.forest)^2)

print(mse.tau.boost)
print(mse.tau.forest)

