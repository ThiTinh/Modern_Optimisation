# Use Simulated Annealing to select features for the model

# Split data into predictors and response
X <- as.matrix(numeric_data[, -4])
y <- numeric_data[, 4]

# Define function to fit linear model using selected features
sphere <- function(string, xx, yy) {
  # Extract the indices of selected features
  inc <- which(string == 1)
  
  if (length(inc) == 0) {
    # If no features are selected, return a terrible fitness value
    return(-10E20)
  }
  
  # Create matrix of selected features
  X <- cbind(1, xx[, inc])
  
  # Fit linear model and return negative AIC as fitness value
  mod <- lm.fit(X, yy)
  class(mod) <- "lm"
  return(-AIC(mod))
}


# sum a raw binary object x (evaluation function):
minsumbin=function(x) (length(x)-sum(x)) # optim only minimizes!


# slight random change of vector par:
#    par - initial solution
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    dist - random distribution function
#    round - use integer (TRUE) or continuous (FALSE) search
#    ... - extra parameters for dist
#    examples: dist=rnorm, mean=0, sd=1; dist=runif, min=0,max=1
hchange=function(par,lower,upper,dist,round=TRUE,...)
{ 
  D=length(par)           # dimension
  step=dist(D,...)        # slight step
  if(round) step=round(step) 
  par1=par+step
  # return par1 within [lower,upper]:
  return(ifelse(par1<lower,lower,ifelse(par1>upper,upper,par1)))
}

# SANN for sum of bits, one run:
D=ncol(numeric_data)-1 # dimension
s=rep(0,D) # c(0,0,0,0,...)
C=list(maxit=150,temp=10000,tmax=1,trace=TRUE,REPORT=1000)
bchange=function(par) # binary change
{ D=length(par)
hchange(par,lower=rep(0,D),upper=rep(1,D),rnorm,mean=0,sd=1)
}
s=optim(s,minsumbin,gr=bchange,method="SANN",control=C)
cat("best:",s$par,"f:",s$value,"(max: fs:",sum(s$par),")\n")


# Define function to fit linear model using selected features
featureFitness <- function(string, xx, yy) {
  # Extract the indices of selected features
  inc <- which(string == 1)
  
  if (length(inc) == 0) {
    # If no features are selected, return a terrible fitness value
    return(-10E20)
  }
  
  # Create matrix of selected features
  X <- cbind(1, xx[, inc])
  
  # Fit linear model and return negative AIC as fitness value
  mod <- lm.fit(X, yy)
  class(mod) <- "lm"
  return(-AIC(mod))
}


# lan 1:
# sann objective function values
# initial       value 18.000000
# final         value 3.000000
# sann stopped after 149 iterations
# best: 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 1 f: 3 (max: fs: 15 )
# string <- c(1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1)
# featureFitness(string, X,y)
# [1] -571525

# lan 2:
# sann objective function values
# initial       value 18.000000
# final         value 3.000000
# sann stopped after 149 iterations
# best: 1 1 1 1 1 1 1 0 1 1 1 1 0 1 0 1 1 1 f: 3 (max: fs: 15 )
# string <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1)
# featureFitness(string, X,y)
# [1] -571721.8

# lan 3:
# sann objective function values
# initial       value 18.000000
# final         value 3.000000
# sann stopped after 149 iterations
# best: 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 1 1 1 f: 3 (max: fs: 15 )
# string <- c(1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
# featureFitness(string, X,y)
# [1] -571932.9

# lan 4:
# sann objective function values
# initial       value 18.000000
# final         value 4.000000
# sann stopped after 149 iterations
# best: 1 0 0 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 f: 4 (max: fs: 14 )
# string <- c(1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1)
# featureFitness(string, X,y)
# [1] -572302

# lan 5:
# sann objective function values
# initial       value 18.000000
# final         value 3.000000
# sann stopped after 149 iterations
# best: 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 f: 3 (max: fs: 15 )
# string <- c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1)
# featureFitness(string, X,y)
# [1] -571712.9
