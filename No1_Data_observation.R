#load library
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(GA)



# ===== Read data =====
setwd('D:\\House_price_prediction_Tinh')
data <- read.csv(file ='data/Melbourne_housing_processed.csv')
head(data)

# Suburb         Address Rooms Type   Price Method SellerG       Date Distance
# 1 Abbotsford   68 Studley St     2    h  600000     SS  Jellis 2016-03-09      2.5
# 2 Abbotsford    85 Turner St     2    h 1480000      S  Biggin 2016-03-12      2.5
# 3 Abbotsford 25 Bloomburg St     2    h 1035000      S  Biggin 2016-04-02      2.5
# 4 Abbotsford    16 Maugie St     4    h  600000     SN  Nelson 2016-06-08      2.5
# 5 Abbotsford    53 Turner St     2    h  600000      S  Biggin 2016-06-08      2.5
# 6 Abbotsford    99 Turner St     2    h  600000      S Collins 2016-06-08      2.5
# Postcode Bedroom2 Bathroom Car Landsize BuildingArea YearBuilt
# 1     3067        2        1   1      126          120      1970
# 2     3067        2        1   1      202          120      1970
# 3     3067        2        1   0      156           79      1900
# 4     3067        3        2   2      400          220      2006
# 5     3067        4        1   2      201          120      1900
# 6     3067        3        2   1      202          120      1900
# CouncilArea Lattitude Longtitude            Regionname Propertycount
# 1 Yarra City Council  -37.8014   144.9958 Northern Metropolitan          4019
# 2 Yarra City Council  -37.7996   144.9984 Northern Metropolitan          4019
# 3 Yarra City Council  -37.8079   144.9934 Northern Metropolitan          4019
# 4 Yarra City Council  -37.7965   144.9965 Northern Metropolitan          4019
# 5 Yarra City Council  -37.7995   144.9974 Northern Metropolitan          4019
# 6 Yarra City Council  -37.7996   144.9989 Northern Metropolitan          4019

tail(data)

# ===== Data exploration =====
print(paste("Number of records: ", nrow(data)))
#[1] "Number of records:  19567"

print(paste("Number of features: ", ncol(data)))
#[1] "Number of features:  21"

summary(data)
# Suburb            Address              Rooms           Type          
# Length:19567       Length:19567       Min.   :1.000   Length:19567      
# Class :character   Class :character   1st Qu.:3.000   Class :character  
# Mode  :character   Mode  :character   Median :3.000   Mode  :character  
# Mean   :3.252                     
# 3rd Qu.:4.000                     
# Max.   :8.000                     
# Price             Method            SellerG              Date          
# Min.   :  131000   Length:19567       Length:19567       Length:19567      
# 1st Qu.:  600000   Class :character   Class :character   Class :character  
# Median :  821000   Mode  :character   Mode  :character   Mode  :character  
# Mean   : 1034300                                                           
# 3rd Qu.: 1270300                                                           
# Max.   :11200000                                                           
# Distance        Postcode       Bedroom2         Bathroom           Car        
# Min.   : 0.00   Min.   :3000   Min.   : 0.000   Min.   : 1.000   Min.   : 0.000  
# 1st Qu.: 7.30   1st Qu.:3046   1st Qu.: 3.000   1st Qu.: 1.000   1st Qu.: 1.000  
# Median :10.80   Median :3101   Median : 3.000   Median : 2.000   Median : 2.000  
# Mean   :11.78   Mean   :3115   Mean   : 3.232   Mean   : 1.663   Mean   : 1.811  
# 3rd Qu.:14.30   3rd Qu.:3150   3rd Qu.: 4.000   3rd Qu.: 2.000   3rd Qu.: 2.000  
# Max.   :48.10   Max.   :3977   Max.   :30.000   Max.   :12.000   Max.   :18.000  
# Landsize         BuildingArea    YearBuilt    CouncilArea       
# Min.   :    60.0   Min.   :  51   Min.   :1196   Length:19567      
# 1st Qu.:   328.0   1st Qu.: 120   1st Qu.:1960   Class :character  
# Median :   569.0   Median : 120   Median :1970   Mode  :character  
# Mean   :   681.7   Mean   : 143   Mean   :1965                     
# 3rd Qu.:   696.0   3rd Qu.: 146   3rd Qu.:1970                     
# Max.   :433014.0   Max.   :1044   Max.   :2106                     
# Lattitude        Longtitude     Regionname        Propertycount  
# Min.   :-38.19   Min.   :144.4   Length:19567       Min.   :   83  
# 1st Qu.:-37.86   1st Qu.:144.9   Class :character   1st Qu.: 4280  
# Median :-37.80   Median :145.0   Mode  :character   Median : 6543  
# Mean   :-37.81   Mean   :145.0                      Mean   : 7402  
# 3rd Qu.:-37.75   3rd Qu.:145.1                      3rd Qu.:10331  
# Max.   :-37.39   Max.   :145.5                      Max.   :21650  


colnames(data)
# [1] "Suburb"        "Address"       "Rooms"         "Type"         
# [5] "Price"         "Method"        "SellerG"       "Date"         
# [9] "Distance"      "Postcode"      "Bedroom2"      "Bathroom"     
# [13] "Car"           "Landsize"      "BuildingArea"  "YearBuilt"    
# [17] "CouncilArea"   "Lattitude"     "Longtitude"    "Regionname"   
# [21] "Propertycount"

# ===== Identify character features =====
character_cols <- sapply(data, is.character)
head(data[, character_cols])
# Suburb         Address Type Method SellerG       Date        CouncilArea
# 1 Abbotsford   68 Studley St    h     SS  Jellis 2016-03-09 Yarra City Council
# 2 Abbotsford    85 Turner St    h      S  Biggin 2016-03-12 Yarra City Council
# 3 Abbotsford 25 Bloomburg St    h      S  Biggin 2016-04-02 Yarra City Council
# 4 Abbotsford    16 Maugie St    h     SN  Nelson 2016-06-08 Yarra City Council
# 5 Abbotsford    53 Turner St    h      S  Biggin 2016-06-08 Yarra City Council
# 6 Abbotsford    99 Turner St    h      S Collins 2016-06-08 Yarra City Council
# Regionname
# 1 Northern Metropolitan
# 2 Northern Metropolitan
# 3 Northern Metropolitan
# 4 Northern Metropolitan
# 5 Northern Metropolitan
# 6 Northern Metropolitan

# ===== Convert character vector to numeric =====
# Suburb
factor_suburb <- factor(data$Suburb)
data$Suburb <- as.numeric(factor_suburb)
# Type
factor_type <- factor(data$Type)
data$Type <- as.numeric(factor_type)
# Method
factor_method <- factor(data$Method)
data$Method <- as.numeric(factor_method)
# SellerG
factor_seller <- factor(data$SellerG)
data$SellerG <- as.numeric(factor_seller)
# CouncilArea
factor_council <- factor(data$CouncilArea)
data$CouncilArea <- as.numeric(factor_council)
# Regionname
factor_region <- factor(data$Regionname)
data$Regionname <- as.numeric(factor_region)
head(data)

# ===== convert vector to date type =====
data$Date <- as.Date(data$Date)

summary(data)


# ===== Identify numeric features =====
numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]
head(numeric_data)
# Suburb Rooms Type   Price Method SellerG Distance Postcode Bedroom2 Bathroom Car
# 1      1     2    1  600000      7     131      2.5     3067        2        1   1
# 2      1     2    1 1480000      3      26      2.5     3067        2        1   1
# 3      1     2    1 1035000      3      26      2.5     3067        2        1   0
# 4      1     4    1  600000      5     198      2.5     3067        3        2   2
# 5      1     2    1  600000      3      26      2.5     3067        4        1   2
# 6      1     2    1  600000      3      58      2.5     3067        3        2   1
# Landsize BuildingArea YearBuilt CouncilArea Lattitude Longtitude Regionname
# 1      126          120      1970          32  -37.8014   144.9958          3
# 2      202          120      1970          32  -37.7996   144.9984          3
# 3      156           79      1900          32  -37.8079   144.9934          3
# 4      400          220      2006          32  -37.7965   144.9965          3
# 5      201          120      1900          32  -37.7995   144.9974          3
# 6      202          120      1900          32  -37.7996   144.9989          3
# Propertycount
# 1          4019
# 2          4019
# 3          4019
# 4          4019
# 5          4019
# 6          4019


# ===== Correlation matrix =====
corr_matrix <- cor(numeric_data)
corr_matrix
sorted_corr_matrix <- corr_matrix[order(corr_matrix[, 'Price']), ]
sorted_corr_matrix

# Suburb        Rooms         Type        Price       Method
# Distance       0.026777229  0.186053141 -0.104802031 -0.249043689 -0.025561371
# Type          -0.020885741 -0.364928882  1.000000000 -0.190968998  0.003570044
# Lattitude      0.144920103 -0.035494654 -0.050803388 -0.181560820 -0.012086665
# YearBuilt      0.013938772  0.039449881  0.216765366 -0.181074648 -0.019275226
# Suburb         1.000000000 -0.038938526 -0.020885741 -0.112936288 -0.001733031
# CouncilArea    0.097274375 -0.091144522  0.006499892 -0.094193551 -0.011290002
# Propertycount  0.113466999 -0.029737143  0.019498844 -0.035707344 -0.037394521
# Method        -0.001733031  0.006248436  0.003570044 -0.017714174  1.000000000
# SellerG        0.026151270  0.001866436 -0.027503526 -0.001402294  0.011938334
# Landsize       0.004114856  0.010909605  0.024025997  0.018367939  0.012416262
# Postcode      -0.026779650  0.096664623 -0.012819870  0.020516174 -0.005687787
# Regionname    -0.022001875 -0.016386365  0.034452438  0.092221757  0.022644926
# Car           -0.018368048  0.332699588 -0.169584469  0.110488022  0.001352609
# Longtitude    -0.150662418  0.109951184  0.014484981  0.139112724 -0.019016452
# Bedroom2      -0.037406358  0.924658782 -0.345198974  0.252641410  0.004432556
# BuildingArea  -0.041613044  0.466538574 -0.175145713  0.254255275  0.017499761
# Rooms         -0.038938526  1.000000000 -0.364928882  0.270779161  0.006248436
# Bathroom      -0.051525101  0.606638131 -0.126637804  0.285330927  0.017280936
# Price         -0.112936288  0.270779161 -0.190968998  1.000000000 -0.017714174
# SellerG    Distance     Postcode     Bedroom2     Bathroom
# Distance      -0.040775821  1.00000000  0.506160135  0.190516892  0.087433300
# Type          -0.027503526 -0.10480203 -0.012819870 -0.345198974 -0.126637804
# Lattitude      0.083541677 -0.11083737 -0.221857783 -0.034050752 -0.085561724
# YearBuilt     -0.028594189  0.31355944  0.078174925  0.048098968  0.141567335
# Suburb         0.026151270  0.02677723 -0.026779650 -0.037406358 -0.051525101
# CouncilArea    0.040922724 -0.05203386  0.011553467 -0.083731125 -0.070340813
# Propertycount  0.019163173  0.02393844  0.026578774 -0.030788570 -0.026268417
# Method         0.011938334 -0.02556137 -0.005687787  0.004432556  0.017280936
# SellerG        1.000000000 -0.04077582 -0.039449105  0.004464999 -0.002646537
# Landsize       0.001627179  0.04235941  0.039054225  0.011592871  0.026308686
# Postcode      -0.039449105  0.50616013  1.000000000  0.097333504  0.127466336
# Regionname    -0.011281661 -0.15210628 -0.060718403 -0.024350081  0.032301926
# Car            0.023382986  0.19464205  0.064647304  0.325292882  0.268007216
# Longtitude    -0.069524716  0.19587797  0.361034912  0.111638149  0.115074338
# Bedroom2       0.004464999  0.19051689  0.097333504  1.000000000  0.606880288
# BuildingArea  -0.006055754  0.05980799  0.073157015  0.438329267  0.469488411
# Rooms          0.001866436  0.18605314  0.096664623  0.924658782  0.606638131
# Bathroom      -0.002646537  0.08743330  0.127466336  0.606880288  1.000000000
# Price         -0.001402294 -0.24904369  0.020516174  0.252641410  0.285330927
# Car     Landsize BuildingArea    YearBuilt  CouncilArea
# Distance       0.194642045  0.042359411  0.059807994  0.313559440 -0.052033861
# Type          -0.169584469  0.024025997 -0.175145713  0.216765366  0.006499892
# Lattitude     -0.020718939  0.020028159 -0.048424543  0.076742294  0.101528635
# YearBuilt      0.123195716  0.021520372  0.089830900  1.000000000 -0.038745676
# Suburb        -0.018368048  0.004114856 -0.041613044  0.013938772  0.097274375
# CouncilArea   -0.106064384  0.012974218 -0.078613513 -0.038745676  1.000000000
# Propertycount  0.002647437 -0.015230278 -0.025679813 -0.001092251  0.007578108
# Method         0.001352609  0.012416262  0.017499761 -0.019275226 -0.011290002
# SellerG        0.023382986  0.001627179 -0.006055754 -0.028594189  0.040922724
# Landsize       0.022263559  1.000000000  0.003797444  0.021520372  0.012974218
# Postcode       0.064647304  0.039054225  0.073157015  0.078174925  0.011553467
# Regionname     0.022689431 -0.010392067  0.035908006 -0.045319659 -0.085132520
# Car            1.000000000  0.022263559  0.211022714  0.123195716 -0.106064384
# Longtitude     0.041769554 -0.004232194  0.068845442 -0.005629384 -0.125834547
# Bedroom2       0.325292882  0.011592871  0.438329267  0.048098968 -0.083731125
# BuildingArea   0.211022714  0.003797444  1.000000000  0.089830900 -0.078613513
# Rooms          0.332699588  0.010909605  0.466538574  0.039449881 -0.091144522
# Bathroom       0.268007216  0.026308686  0.469488411  0.141567335 -0.070340813
# Price          0.110488022  0.018367939  0.254255275 -0.181074648 -0.094193551
# Lattitude   Longtitude  Regionname Propertycount
# Distance      -0.11083737  0.195877975 -0.15210628   0.023938441
# Type          -0.05080339  0.014484981  0.03445244   0.019498844
# Lattitude      1.00000000 -0.344244165 -0.27339926   0.017463087
# YearBuilt      0.07674229 -0.005629384 -0.04531966  -0.001092251
# Suburb         0.14492010 -0.150662418 -0.02200187   0.113466999
# CouncilArea    0.10152863 -0.125834547 -0.08513252   0.007578108
# Propertycount  0.01746309  0.017454917 -0.11196551   1.000000000
# Method        -0.01208667 -0.019016452  0.02264493  -0.037394521
# SellerG        0.08354168 -0.069524716 -0.01128166   0.019163173
# Landsize       0.02002816 -0.004232194 -0.01039207  -0.015230278
# Postcode      -0.22185778  0.361034912 -0.06071840   0.026578774
# Regionname    -0.27339926 -0.524212415  1.00000000  -0.111965512
# Car           -0.02071894  0.041769554  0.02268943   0.002647437
# Longtitude    -0.34424417  1.000000000 -0.52421241   0.017454917
# Bedroom2      -0.03405075  0.111638149 -0.02435008  -0.030788570
# BuildingArea  -0.04842454  0.068845442  0.03590801  -0.025679813
# Rooms         -0.03549465  0.109951184 -0.01638636  -0.029737143
# Bathroom      -0.08556172  0.115074338  0.03230193  -0.026268417
# Price         -0.18156082  0.139112724  0.09222176  -0.035707344


# Plot
corr <- round(cor(numeric_data), 1)
ggcorrplot(corr,
           type = "lower",
           lab = TRUE, 
           lab_size = 3,  
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of Housing Dataset", 
           ggtheme=theme_bw)

# Plot boxplot for checking outliers
# par(mfrow=c(4, 3))  # divide graph area in 2 columns
# boxplot(numeric_data$Rooms, main="Room")
# boxplot(numeric_data$Price, main="Price")
# boxplot(numeric_data$Distance, main="Distance")
# boxplot(numeric_data$Bedroom2, main="Bedroom2")
# boxplot(numeric_data$Car, main="Car")
# boxplot(numeric_data$Landsize, main="Landsize")
# boxplot(numeric_data$BuildingArea, main="BuildingArea")
# boxplot(numeric_data$YearBuilt, main="YearBuilt")
# boxplot(numeric_data$Lattitude, main="Lattitude")
# boxplot(numeric_data$Longtitude, main="Longtitude")
# boxplot(numeric_data$Propertycount, main="Propertycount")

# Get information from column 'Price'
summary(numeric_data$Price)
# Plot
ggplot(numeric_data, aes(x=Price)) +
  geom_histogram(color="black", fill="white") +
  labs(title="Distribution of Price", x="Price", y="Frequency")

ggplot(numeric_data, aes(x=Price)) +
  geom_density(color="black", fill="lightblue") +
  labs(title="Distribution of Price", x="Price", y="Density")


# get dataset columns for prediction
colnames(numeric_data)

# [1] "Suburb"        "Rooms"         "Type"          "Price"
# [5] "Method"        "SellerG"       "Distance"      "Postcode"
# [9] "Bedroom2"      "Bathroom"      "Car"           "Landsize"
# [13] "BuildingArea"  "YearBuilt"     "CouncilArea"   "Lattitude"
# [17] "Longtitude"    "Regionname"    "Propertycount"





