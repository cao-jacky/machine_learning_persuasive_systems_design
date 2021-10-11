library(rpart)

##### FUNCTION TO GENERATE MULTIVARIATE DATA:
# Function to generate data as in Wu et al. (2007) /
# Pratola (2016):
gen_Wu_data <- function(n=300, sigma=.25, seed=1){
  #seed
  set.seed(seed)
  
  p <- 3 #number of x's
  
  # Gen x1-x3:
  x=matrix(runif(p*n),ncol=p) 
  x[1:200,1]=runif(200,0.1,0.4)
  x[201:300,1]=runif(100,0.6,0.9)
  x[1:100,2]=runif(100,0.1,0.4)
  x[101:200,2]=runif(100,0.6,0.9)
  x[201:300,2]=runif(100,0.1,0.9)
  x[1:200,3]=runif(200,0.6,0.9)
  x[201:300,3]=runif(100,0.1,0.4)
  
  # reshuffle the x's:
  x=x[sample(n),]
  
  # Generate y's
  f=rep(0,n)
  for(i in 1:n) {
    if(x[i,1]<0.5 && x[i,2]<=0.5) {
      f[i]=1
    }
    else if(x[i,1]<0.5 && x[i,2]>0.5) {
      f[i]=2
    }
    else if(x[i,1]>0.5) {
      f[i]=3
    }
  }
  
  # Add some noise
  y=f+rnorm(n,sd=sigma)
  
  return(data.frame(x=x, y=y))
  
}

## Generate a dataset
data <- gen_Wu_data()
head(data)

# Linear regression (NAIVE)
m1 <- lm(y ~ x.1 + x.2 + x.3, data=data)  ## y ~ b0 + b1+x.1 + b2*x.2 + b3*x.3 
pred <- predict(m1)

plot(data$y, pred)

# Create a model, with the smallest mean squared error:
# THE MEAN SQUARED ERROR TO BEAT:
mse <- sum((data$y-pred)^2)/300
print(cat("MSE: ", mse))

# Visualize the data set (well, sort off...)
plot(data$x.1, data$y, type="p")
plot(data$x.2, data$y, type="p")
plot(data$x.3, data$y, type="p")

### ASSIGNMENT:

# Create a model to predict y based on x.1, x.2, and x.3
# Compute the mean-squared prediction error on the 300 cases (within sample)
#   ... that is: sum((y - y.pred)^2) / n

# Pointers:
# Explore:
# 1. lm() https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm
# 2. rpart() https://www.statmethods.net/advstats/cart.html
#
# And/Or:
# https://cran.r-project.org/web/packages/randomForest/index.html
# https://cran.r-project.org/web/packages/glmnet/index.html
#
# Feel free to explore more [R] packages for for example NN, DNN, BART, etc.

# attempting logistic regression 

m2 <- glm(y ~ x.1 + x.2 + x.3, data=data)
pred <- predict(m2)

mse <- sum((data$y-pred)^2)/300
print(cat("MSE: ", mse))

# CART

m3 <- rpart(y ~ x.1 + x.2 + x.3, data=data)
pred <- predict(m3)
plot(data$y, pred)

mse3 <- sum((data$y-pred)^2)/300
print(cat("MSE: ", mse3))


# If stuck, start with a much simpler data generating mechanism:
n <- 100
x <- runif(n)
y <- 10 + 15*x + rnorm(n)
plot(x, y)

# And start with easy plotting and error predictions...
mod <- lm(y ~ x)
summary(mod)
y.pred <- predict(mod)
sum((y-y.pred)^2)/2

