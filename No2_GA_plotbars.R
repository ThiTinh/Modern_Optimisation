parseData <- function(data, firstcolumn, noRuns){
  col <- firstcolumn
  
  allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
  cols <- seq(col,noRuns*allstats, by=allstats)
  subdata <- data[,cols]
  noGens <- nrow(data)
  pdata <- matrix(nrow = noGens, ncol = 3)
  for (i in 1:noGens){
    pdata[i,1] = i
    pdata[i,2] = mean(subdata[i,])
    pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar. 
  }
  
  return (pdata)
}

findminmax <- function(data, minimise = TRUE){
  minmax <- NA
  if (minimise) minmax <- min(data[,2])
  else minmax <- max(data[,2])
  
  rownum <- which(data[,2] == minmax)
  if (length(rownum) > 1) rownum <- rownum[1]
  
  if (minimise)
    return (minmax - data [rownum,3])
  else return (minmax + data [rownum,3])
}

plotbars<- function(data1, data2, data3, 
                    cap1 = "GA1", cap2 = "GA2", cap3 = "GA3"){
  data = data1
  hues = c("black","blue","green")
  
  min1 = findminmax(data1)   #min(data1) - data1 [which(data1 == min(data1))+2*nrow(data1)]
  min2 = findminmax(data2)   #min(data2) - data2 [which(data2 == min(data2))+nrow(data2)]
  min3 = findminmax(data3)   #min(data3) - data3 [which(data3 == min(data3))+nrow(data3)]
  
  max1 = findminmax(data1, FALSE)   #max(data1) + data1 [which(data1 == max(data1))+nrow(data1)]
  max2 = findminmax(data2, FALSE)   #max(data2) + data2 [which(data2 == max(data2))+nrow(data2)]
  max3 = findminmax(data3, FALSE)   #max(data3) + data3 [which(data3 == max(data3))+nrow(data3)]
  
  minn = min(min1, min2, min3)
  maxx = max(max1, max2, max3)
  
  
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
  plot(df$x, df$y, type = "l", col = hues[1],  ylim=c(minn, maxx), #ylim = c(0.96, 0.985),   #choose ylim CAREFULLY as per your data ranges
       main = "Best Fitness Values", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[1]);    #plot the error bars mean-errorbar, mean+errorbar
  
  data = data2
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[2])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[2]); 
  
  data = data3
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[3])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[3]); 
  
  legend("topleft", legend = c(cap1, cap2, cap3), col = hues, lwd = 1,
         cex = 0.5)
}


# r1 <- runGA(noRuns = 5) # pop=30
# r2 <- runGA(noRuns = 5) # pop=70
# r3 <- runGA(noRuns = 5) # pop=150
# p1 <- parseData(r1$resultsMatrix,2,5)
# p2 <- parseData(r2$resultsMatrix,2,5)
# p3 <- parseData(r3$resultsMatrix,2,5)
# plotbars(p1,p2,p3, 'pop=30', 'pop=70', 'pop=150')

# Clearly pop=300 better than the other setups.
# Error bars do not overlap. 
# -> it indicates that there is a significant difference between the means of the groups being compared.
# This means that the difference between the means is statistically significant and not due to chance.


# Clearly we can see that population size of 300 is doing the best 
# because the average line is above the other average lines.
# the error bars for the 200 population size does not overlap with the error bars below it.
# This means that the top graph is not just better, but actually statistically significantly better.
# Statistical significance is important to show when we show performance differences.
