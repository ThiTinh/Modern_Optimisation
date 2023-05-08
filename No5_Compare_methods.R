#load library
library(tidyverse)
library(ggplot2)


head(numeric_data)
colnames(numeric_data[, -4])

# [1] "Suburb"        "Rooms"         "Type"          "Method"       
# [5] "SellerG"       "Distance"      "Postcode"      "Bedroom2"     
# [9] "Bathroom"      "Car"           "Landsize"      "BuildingArea" 
# [13] "YearBuilt"     "CouncilArea"   "Lattitude"     "Longtitude"   
# [17] "Regionname"    "Propertycount"


####### ============ GA MODEL SELECTION ============ #######
GA_string <- c(1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,0)
sum(GA_string == 1)
# [1] 15 # 15 variables

colnames(numeric_data[, -4][, which(GA_string == 1)])
# [1] "Suburb"       "Rooms"        "Type"         "Method"       "Distance"    
# [6] "Postcode"     "Bathroom"     "Car"          "Landsize"     "BuildingArea"
# [11] "YearBuilt"    "CouncilArea"  "Lattitude"    "Longtitude"   "Regionname"

GA_linearmodel = lm(Price~Suburb+Rooms+Type+
                   Method+Distance+Postcode+
                   Bathroom+Car+Landsize+
                   BuildingArea+YearBuilt+CouncilArea+Lattitude+
                   Longtitude+Regionname,
                 data = numeric_data)
# Calculate AIC
GA_AIC <- AIC(GA_linearmodel)
GA_AIC
# [1] 571392.2

# Calculate BIC
GA_BIC <- BIC(GA_linearmodel)
GA_BIC
# [1] 571526.2

# Calculate R-squared
GA_R_squared <- summary(GA_linearmodel)$r.squared
GA_R_squared
# [1] 0.2894321

summary(GA_linearmodel)

# Call:
#   lm(formula = Price ~ Suburb + Rooms + Type + Method + Distance + 
#        Postcode + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
#        CouncilArea + Lattitude + Longtitude + Regionname, data = numeric_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1852169  -244312   -33031   196447  9748007 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.155e+08  6.251e+06 -18.481  < 2e-16 ***
#   Suburb       -2.946e+02  4.010e+01  -7.348 2.10e-13 ***
#   Rooms         8.071e+04  6.158e+03  13.108  < 2e-16 ***
#   Type         -1.543e+05  7.599e+03 -20.308  < 2e-16 ***
#   Method       -1.114e+04  1.978e+03  -5.630 1.82e-08 ***
#   Distance     -3.496e+04  7.347e+02 -47.586  < 2e-16 ***
#   Postcode      6.105e+02  4.309e+01  14.170  < 2e-16 ***
#   Bathroom      1.152e+05  6.910e+03  16.665  < 2e-16 ***
#   Car           3.151e+04  4.079e+03   7.725 1.17e-14 ***
#   Landsize      5.692e+00  1.037e+00   5.488 4.13e-08 ***
#   BuildingArea  1.058e+03  7.321e+01  14.454  < 2e-16 ***
#   YearBuilt    -1.544e+03  1.474e+02 -10.478  < 2e-16 ***
#   CouncilArea  -3.060e+03  4.004e+02  -7.642 2.24e-14 ***
#   Lattitude    -7.435e+05  5.426e+04 -13.703  < 2e-16 ***
#   Longtitude    6.176e+05  4.928e+04  12.532  < 2e-16 ***
#   Regionname    1.972e+04  2.706e+03   7.288 3.27e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 530500 on 19551 degrees of freedom
# Multiple R-squared:  0.2894,	Adjusted R-squared:  0.2889 
# F-statistic: 530.9 on 15 and 19551 DF,  p-value: < 2.2e-16

####### ============ PSO MODEL SELECTION ============ #######
PSO_string <- c(1, 1, 1, 1, 0, 0 ,0, 1 ,1, 1, 0, 0, 1, 0, 0, 0, 0, 0)
sum(PSO_string == 1)
# [1] 8 # 8 variables

colnames(numeric_data[, -4][, which(PSO_string == 1)])
# [1] "Suburb"    "Rooms"     "Type"      "Method"    "Bedroom2"  "Bathroom" 
# [7] "Car"       "YearBuilt"

PSO_linearmodel = lm(Price~Suburb+Rooms+Type+
                      Method+Bedroom2+
                      Bathroom+Car+YearBuilt,
                    data = numeric_data)
# Calculate AIC
PSO_AIC <- AIC(PSO_linearmodel)
PSO_AIC
# [1] 574668.8

# Calculate BIC
PSO_BIC <- BIC(PSO_linearmodel)
PSO_BIC
# [1] 574747.6

# Calculate R-squared
PSO_R_squared <- summary(PSO_linearmodel)$r.squared
PSO_R_squared
# [1] 0.1592999

summary(PSO_linearmodel)
# Call:
#   lm(formula = Price ~ Suburb + Rooms + Type + Method + Bedroom2 + 
#        Bathroom + Car + YearBuilt, data = numeric_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -3026107  -347238   -98573   219997  9808268 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 9289752.63  287824.89  32.276  < 2e-16 ***
#   Suburb         -620.47      42.68 -14.539  < 2e-16 ***
#   Rooms        100654.48   12635.48   7.966 1.73e-15 ***
#   Type         -89782.56    8140.91 -11.029  < 2e-16 ***
#   Method        -8619.39    2149.14  -4.011 6.08e-05 ***
#   Bedroom2     -36237.66   12032.99  -3.012 0.002603 ** 
#   Bathroom     203705.89    7254.92  28.078  < 2e-16 ***
#   Car           16355.80    4385.38   3.730 0.000192 ***
#   YearBuilt     -4372.01     148.43 -29.455  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 576900 on 19558 degrees of freedom
# Multiple R-squared:  0.1593,	Adjusted R-squared:  0.159 
# F-statistic: 463.2 on 8 and 19558 DF,  p-value: < 2.2e-16

####### ============ SA MODEL SELECTION ============ #######
SA_string <- c(1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1) 
sum(SA_string == 1)
# [1] 15 # 15 variables

colnames(numeric_data[, -4][, which(SA_string == 1)])
# [1] "Suburb"        "Rooms"         "Type"          "Method"        "Distance"      "Postcode"     
# [7] "Bedroom2"      "Bathroom"      "Car"           "Landsize"      "BuildingArea"  "YearBuilt"    
# [13] "Lattitude"     "Longtitude"    "Propertycount"

SA_linearmodel = lm(Price~Suburb+Rooms+Type+Method+Distance+Postcode+
                      Bedroom2+Bathroom+Car+Landsize+BuildingArea+YearBuilt+
                      Lattitude+Longtitude+Propertycount,
                     data = numeric_data)
# Calculate AIC
SA_AIC <- AIC(SA_linearmodel)
SA_AIC
# [1] 571525

# Calculate BIC
SA_BIC <- BIC(SA_linearmodel)
SA_BIC
# [1] 571659

# Calculate R-squared
SA_R_squared <- summary(SA_linearmodel)$r.squared
SA_R_squared
# [1] 0.2845917

summary(SA_linearmodel)
# Call:
#   lm(formula = Price ~ Suburb + Rooms + Type + Method + Distance + 
#        Postcode + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
#        YearBuilt + Lattitude + Longtitude + Propertycount, data = numeric_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1973719  -245172   -33216   190722  9782624 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -9.305e+07  4.877e+06 -19.080  < 2e-16 ***
#   Suburb        -3.288e+02  4.033e+01  -8.154 3.74e-16 ***
#   Rooms          8.759e+04  1.177e+04   7.442 1.04e-13 ***
#   Type          -1.510e+05  7.626e+03 -19.807  < 2e-16 ***
#   Method        -1.111e+04  1.986e+03  -5.595 2.24e-08 ***
#   Distance      -3.570e+04  7.235e+02 -49.342  < 2e-16 ***
#   Postcode       6.445e+02  4.210e+01  15.308  < 2e-16 ***
#   Bedroom2      -4.936e+03  1.112e+04  -0.444   0.6572    
# Bathroom       1.168e+05  7.014e+03  16.656  < 2e-16 ***
#   Car            3.584e+04  4.079e+03   8.786  < 2e-16 ***
#   Landsize       5.492e+00  1.041e+00   5.276 1.34e-07 ***
#   BuildingArea   1.095e+03  7.341e+01  14.918  < 2e-16 ***
#   YearBuilt     -1.515e+03  1.479e+02 -10.249  < 2e-16 ***
#   Lattitude     -9.881e+05  4.511e+04 -21.904  < 2e-16 ***
#   Longtitude     3.980e+05  3.486e+04  11.417  < 2e-16 ***
#   Propertycount -1.639e+00  8.844e-01  -1.853   0.0639 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 532300 on 19551 degrees of freedom
# Multiple R-squared:  0.2846,	Adjusted R-squared:  0.284 
# F-statistic: 518.5 on 15 and 19551 DF,  p-value: < 2.2e-16


##################################################################
####### ============ RESULT ============ #######

# GA method:  variables= 15, AIC= 571392.2, BIC= 571526.2, R_squared= 0.2894321
# PSO method:  variables=  8, AIC= 574668.8, BIC= 574747.6, R_squared= 0.1592999
# SA method:  variables= 15, AIC= 571525, BIC= 571659, R_squared= 0.2845917

data <- data.frame(
  Method = c("GA", "PSO", "SA"),
  variables = c(15,8,15),
  AIC = c(571392.2, 574668.8, 571525),
  BIC = c(571526.2, 574747.6, 571659),
  R_squared = c(0.2894321, 0.1592999, 0.2845917)
)
data

# Method variables      AIC      BIC R_squared
# 1     GA        15 571392.2 571526.2 0.2894321
# 2    PSO         8 574668.8 574747.6 0.1592999
# 3     SA        15 571525.0 571659.0 0.2845917

