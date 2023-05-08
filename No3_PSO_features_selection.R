# Use PSO to select features for the model

library(pso)

# Split data into predictors and response
X <- as.matrix(numeric_data[, -4])
y <- numeric_data[, 4]

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


# Define function to run PSO feature selection
runPSO <- function() {
  # Set seed for replicability
  set.seed(12345)
  
  # Define problem parameters
  nvar <- ncol(X)  # Number of variables
  lower <- rep(0, nvar)  # Lower bound on variable values (0 = not selected)
  upper <- rep(1, nvar)  # Upper bound on variable values (1 = selected)
  control <- list(trace = 1, maxit = 100, REPORT = 1, REPORT = 1, trace.stats=1, s = 150)
  p <- list(D = nvar, lower = lower, upper = upper, control = control)
  
  # Run PSO optimization
  PSO <- psoptim(rep(NA, nvar), fn = featureFitness, xx = X, yy = y, 
                 lower = p$lower, upper = p$upper, control = p$control)
  
  # Extract the best solution (binary vector indicating which features were selected)
  best_solution <- ifelse(PSO$par > 0.5, 1, 0)
  
  # Print best solution and its fitness
  cat("Best solution:", best_solution, "\n")
  cat("Best fitness:", PSO$value, "\n")
  
  # Return PSO object and best solution
  return(list(PSO = PSO, best_solution = best_solution))
}


# Run PSO feature selection

# result <- runPSO()
# Best solution: 1 1 1 1 0 0 0 1 1 1 0 0 1 0 0 0 0 0 
# Best fitness: -1e+21

# result$PSO
# result$best_solution
# string <- c(1, 1, 1, 1, 0, 0 ,0, 1 ,1, 1, 0, 0, 1, 0, 0, 0, 0, 0)
# fitness <- featureFitness(string, X, y)
# fitness
# [1] -574668.8

