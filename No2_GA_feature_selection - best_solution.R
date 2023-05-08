# Use a Genetic Algorithm to select features for the model

library(GA)
library(dplyr)


#######===== Linear model =====#######

getBenchmark <- function(){
  #Fit a linear model.
  mod <- lm(Price ~ ., data = numeric_data)
  return (mod)
}

getData<-function(){
  #Fit a linear model.
  mod <- getBenchmark()
  
  #Extract the input data from the fitted model.
  #will have to explicitly mention all the variables used in the fitting above. 
  xx <- model.matrix(mod)[, -1]   
  yy <- numeric_data$Price          #the response variable
  data <- cbind(xx,yy)
  return (data)
}


#######===== RUN GA =====#######
#This function is used by the GA to compute or report the statistics of your interest after every generation.
#This function overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  # gaMonitor(obj)                      #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter                      #get the current iternation/generation number 
  if (iter <= maxGenerations){          #some error checking
    fitness <- obj@fitness              #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.    
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else{                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}

runGA <- function(noRuns = 5, problem = "feature"){
  #Specify GA parameter values; using the default values below. 
  if (problem == "feature"){
    maxGenerations <<- 20    #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    popSize = 150
    pcrossover = 0.8
    pmutation = 0.2
    type = "binary"
    crossover = 0.7
    data <- getData()
    xx <- data[,-ncol(data)]
    yy <- data[,ncol(data)]
    fitness = featureFitness              #fitness function defined in feature-selection.R
  }
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  
  #Set up what stats you wish to note.    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  names = c(colnames(xx), 'fitnessValue')
  selectedFeatures <- matrix(0, ncol = length(names))
  colnames(selectedFeatures) <- names
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    if (problem == "feature")
      GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx), 
               names = colnames(xx), seed=i, popSize = popSize, 
               pcrossover = pcrossover, pmutation = pmutation, 
               maxiter = maxGenerations, monitor= monitor)
    
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    
    selectedFeatures <- rbind(selectedFeatures, c(GA@solution, GA@fitnessValue))
    
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (list(resultsMatrix = resultsMatrix, selectedFeatures = selectedFeatures[-1,]))
}



getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}


featureFitness <- function(string,xx,yy) {
  #print(string)                         #uncomment this line if you want to print every single solution
  inc <- which(string == 1)              #'inc' includes those features/variables for which 'string' contains 1
  if (length(inc)==0) return (-10E20)    #if  no feature is selected then give a terrible fitness to this solution
  X <- cbind(1, xx[,inc])                #create a matrix of values for all the variables contained in 'inc'
  
  mod <- lm.fit(X, yy)                  #lm.fit computes faster than the 'lm'; because we have to fit thousands of models, use something efficient. 
  class(mod) <- "lm"
  -AIC(mod)                  #AIC should be minimised. But the GA package maximises. So let's turn it into a
  #maximisation problem. However, negative values will be a problem with roulette wheel
  #selection which requires positive values to build a roulette wheel. Therefore, consider
  #other ways of inverting the minimisation problem such as 1/(1+AIC); this normalises 
  #the values between 0 (the worst) and 1 (the best).
}


###################################

# r1 <- runGA() # pop:30
# > r1$selectedFeatures
# Suburb Rooms Type Method SellerG Distance Postcode Bedroom2 Bathroom Car Landsize BuildingArea
# [1,]      1     1    1      1       1        1        1        0        1   1        1            1
# [2,]      0     1    1      1       0        1        1        0        1   1        1            1
# [3,]      1     1    1      1       0        1        0        1        1   1        1            1
# [4,]      1     1    1      1       1        1        1        1        1   1        0            1
# [5,]      1     0    1      1       0        1        1        1        1   1        0            1
# YearBuilt CouncilArea Lattitude Longtitude Regionname Propertycount fitnessValue
# [1,]         0           1         1          1          1             0    -571501.2
# [2,]         1           1         1          1          1             0    -571444.1
# [3,]         1           0         1          1          1             1    -571626.7
# [4,]         1           1         1          1          1             0    -571424.0
# [5,]         1           1         1          1          1             1    -571471.6



# r2 <- runGA() # pop:70
# > r2$selectedFeatures
# Suburb Rooms Type Method SellerG Distance Postcode Bedroom2 Bathroom Car Landsize BuildingArea
# [1,]      1     1    1      1       1        1        1        1        1   1        0            1
# [2,]      1     1    1      1       0        1        1        0        1   1        1            1
# [3,]      1     1    1      1       0        1        1        0        1   1        1            1
# [4,]      1     1    1      1       0        1        1        0        1   1        1            1
# [5,]      1     1    1      0       0        1        1        0        1   1        1            1
# YearBuilt CouncilArea Lattitude Longtitude Regionname Propertycount fitnessValue
# [1,]         1           1         1          1          1             0    -571424.0
# [2,]         1           1         1          1          1             0    -571392.2
# [3,]         1           1         1          1          1             0    -571392.2
# [4,]         1           1         1          1          1             1    -571393.3
# [5,]         1           1         1          1          1             0    -571421.9




# r3 <- runGA() # pop:150
# > r3$selectedFeatures
# Suburb Rooms Type Method SellerG Distance Postcode Bedroom2 Bathroom Car Landsize BuildingArea
# [1,]      1     1    1      1       0        1        1        0        1   1        1            1
# [2,]      1     1    1      1       0        1        1        0        1   1        1            1
# [3,]      1     1    1      1       0        1        1        1        1   1        1            1
# [4,]      1     1    1      1       0        1        1        0        1   1        1            1
# [5,]      1     1    1      1       0        1        1        0        1   1        1            1
# YearBuilt CouncilArea Lattitude Longtitude Regionname Propertycount fitnessValue
# [1,]         1           1         1          1          1             0    -571392.2
# [2,]         1           1         1          1          1             0    -571392.2
# [3,]         1           1         1          1          1             0    -571394.1
# [4,]         1           1         1          1          1             0    -571392.2
# [5,]         1           1         1          1          1             1    -571393.3

# > getBestFitness()
# [1] -571392.2

# > getBestSolution()
# Suburb Rooms Type Method SellerG Distance Postcode Bedroom2 Bathroom Car Landsize BuildingArea
# [1,]      1     1    1      1       0        1        1        0        1   1        1            1
# YearBuilt CouncilArea Lattitude Longtitude Regionname Propertycount
# [1,]         1           1         1          1          1             0


# ========================================
# string <- c(1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,0)
# data <- getData()
# xx = data[,-ncol(data)]
# yy = data[,ncol(data)]
# featureFitness(string, xx, yy )
# [1] -571392.2
