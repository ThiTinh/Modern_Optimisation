#load library
library(tidyverse)
library(ggplot2)


head(numeric_data)
colnames(numeric_data[, -4])


# ===== Read best_selection (set number of features selected) data =====
setwd('D:\\House_price_prediction_Tinh')
best_selection <- read.csv(file ='data/best_selection.csv')
head(best_selection)

####### ============ GA MODEL SELECTION ============ #######
# Set number of features from 2 to 15 -> calculate AIC, BIC, R_squared for the model

names = c('number_features','AIC', 'BIC', 'R_squared')
results <- matrix(0, ncol = length(names), nrow = nrow(best_selection))
colnames(results) <- names
for (i in 1:nrow(best_selection)) {
  lst <- c(unname(best_selection[i,2:(length(best_selection)-1)]))
  col_list <- colnames(numeric_data[, -4])[which(lst==1)]
  GA_linearmodel <- lm(Price ~ ., data = numeric_data[, c('Price',col_list)])
  GA_AIC <- AIC(GA_linearmodel)
  GA_BIC <- BIC(GA_linearmodel)
  GA_R_squared <- summary(GA_linearmodel)$r.squared
  results[i,] <- c(best_selection[i,1], GA_AIC, GA_BIC, GA_R_squared)
  
}
print(results)
# > print(results)
#     number_features      AIC      BIC  R_squared
# [1,]              15 571392.2 571526.2 0.28943214
# [2,]              14 571420.3 571546.4 0.28833768
# [3,]              13 571449.2 571567.4 0.28721446
# [4,]              12 571500.0 571610.3 0.28528793
# [5,]              11 571560.1 571662.5 0.28301681
# [6,]              10 571623.2 571717.8 0.28062478
# [7,]               9 571715.6 571802.3 0.27714708
# [8,]               8 571811.4 571890.2 0.27352546
# [9,]               7 572021.1 572092.1 0.26562060
# [10,]               6 572260.0 572323.0 0.25652582
# [11,]               5 572786.0 572841.2 0.23618822
# [12,]               4 573048.3 573095.6 0.22580390
# [13,]               3 573480.1 573519.5 0.20844879
# [14,]               2 576557.0 576588.6 0.07355897

#### PLOT R_squared

r_squared <- results[,'R_squared']
num_features <- results[,'number_features']

# create a line plot of R-squared values
plot(num_features, r_squared, type = "l", xlab = "Number of Features", ylab = "R-squared")


#### PLOT AIC
aic <- results[,'AIC']
num_features <- results[,'number_features']

# create a line plot of R-squared values
plot(num_features, aic, type = "l", xlab = "Number of Features", ylab = "AIC")

