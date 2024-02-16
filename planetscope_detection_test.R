#Plotting and Analysis of Mosaic PS images
# 4 band
library(terra)
library(data.table)
library(ggplot2)
path <- "C:/Users/eahorner/Documents/GIS 712 Remote Sensing/Project/data2/"
file_list <- list.files(path, pattern= '*NDVI.tif')

for (i in 1:length(file_list)){
  temp <- terra::rast(paste0(path, file_list[i]))
  assign(paste0(gsub("-", "", substr(file_list[i], start = 1, stop = 13)), '_with_NDVI'), temp)
}

list_dates <- paste0(gsub("-", "", substr(file_list, start = 1, stop = 13)), '_with_NDVI')
date_list <- as.Date(substr(file_list, start = 4, stop = 13))

ndviStack <- c(PS20200604_with_NDVI, PS20200625_with_NDVI)
for (i in 3:length(date_list)){
  ndviStack <- c(ndviStack, get(date_list[i]))
}

writeRaster(ndviStack, paste0(path, 'ndviStack.tif'), overwrite = TRUE)

#Import points and extract values at points
#Look up
testPoints <- terra::vect("C:/Users/eahorner/Documents/GIS 712 Remote Sensing/Project/phenology_test.shp")

extrTest <- extract(ndviStack, testPoints, ID = FALSE)

#Spat Vector
dataSV <- cbind(testPoints, extrTest)

#Data.Frame
dataDF <- cbind(extrTest, testPoints)

infDF <- dataDF[dataDF$pixel == 'Infected', grep("NDVI_5", colnames(dataDF))]
infDT <- data.table(infDF)

infSummary <- cbind(infDT[, .(mean = colMeans(.SD), sd = lapply(.SD, sd)),], date_list)



notinfDF <- dataDF[dataDF$pixel == 'Not_Infected', grep("NDVI_5", colnames(dataDF))]
notinfDT <- data.table(notinfDF)

notinfSummary <- cbind(notinfDT[, .(mean = colMeans(.SD), sd = lapply(.SD, sd)),], date_list)

#Simple line plot
plot(date_list, infDF[1,], type = 'l', col = 'brown', ylim = c(-1, 1))
for(i in 2:length(infDF[,1])){
  lines(date_list, infDF[i,], col = 'brown')
}
for(i in 1:length(notinfDF[,1])){
  lines(date_list, notinfDF[i,], col = 'green')
}


#Mean and SD (not smoothed)
plot(date_list, infSummary$mean, col = 'firebrick1', type = 'l', lwd = 2.5, ylim = c(-0.2, 1.0))
lines(date_list, (infSummary$mean + (1.96*as.double(infSummary$sd))), col = 'firebrick1', lty = 'dashed')
lines(date_list, (infSummary$mean - (1.96*as.double(infSummary$sd))), col = 'firebrick1', lty = 'dashed')
polygon(c(date_list, rev(date_list)), c(infSummary$mean - (1.96*as.double(infSummary$sd)), rev(infSummary$mean + (1.96*as.double(infSummary$sd)))), col = adjustcolor('firebrick1', alpha.f = 0.4), border = FALSE)
lines(date_list, (infSummary$mean), col = 'firebrick1', lwd = 2.5)

polygon(c(date_list, rev(date_list)), c(notinfSummary$mean - (1.96*as.double(notinfSummary$sd)), rev(notinfSummary$mean + (1.96*as.double(notinfSummary$sd)))), col = adjustcolor('steelblue3', alpha.f = 0.4), border = FALSE)
lines(date_list, notinfSummary$mean, col = 'steelblue3', type = 'l', lwd = 2.5)
lines(date_list, (notinfSummary$mean + (1.96*as.double(notinfSummary$sd))), col = 'steelblue3', lty = 'dashed')
lines(date_list, (notinfSummary$mean - (1.96*as.double(notinfSummary$sd))), col = 'steelblue3', lty = 'dashed')
abline(v = as.Date('2021-05-08'), lwd = 1, lty = 'dashed', col = 'black')

#Smoothed Means
p <- ggplot() + 
    geom_point(data = infSummary, aes(x = date_list, y = mean), col = 'brown') +
    geom_smooth(data = infSummary, aes(x= date_list, y = mean), fill = 'brown', color = 'brown') +
  
    geom_point(data = notinfSummary, aes(x = date_list, y = mean), col = 'green') +
    geom_smooth(data = notinfSummary, aes(x = date_list, y = mean), fill = 'green', color = 'green')
  
p

infSummary$class = 'Infected'
notinfSummary$class = 'Not_Infected'

testSummary <- rbind(infSummary, notinfSummary)

p2 <- ggplot(testSummary, aes(x = date_list, y = mean, group = class, col = class, fill = class)) + geom_point() + geom_smooth()
p2


#test evi2
tevi2 <- 2.4 * ((PS20200604_with_NDVI$`PS-2020-06-04_mosaic_with_NDVI_4`- PS20200604_with_NDVI$`PS-2020-06-04_mosaic_with_NDVI_3`)/(PS20200604_with_NDVI$`PS-2020-06-04_mosaic_with_NDVI_4`+ PS20200604_with_NDVI$`PS-2020-06-04_mosaic_with_NDVI_3`+1))

# 8 band
# Repeat above but for 8 band images

#All files
spath8 <- "C:/Users/eahorner/Documents/GIS 712 Remote Sensing/Project/8 band imagery/PSScene/"
file_list8 <- list.files(spath8, pattern= '*composite.tif')

outpath8 <- "C:/Users/eahorner/Documents/GIS 712 Remote Sensing/Project/8 band imagery/"

#Get unique dates
dates_list <- substr(file_list8, start = 1, stop = 10)
unique_dates <- unique(dates_list)
plot_dates <- as.Date(unique_dates)

#Mosaic images (averaging where multiple overlap on same date) to create 1 image per unique date
for (i in unique_dates){
  if(length(grep(i, file_list8)) < 2){
    temp <- rast(paste0(spath8, grep(i, file_list8, value = TRUE)))
    names(temp) <- c('CoastalBlue1', 'Blue2', 'Green13', 'Green4', 'Yellow5', 'Red6', 'RedEdge7', 'NIR8')
    writeRaster(temp, paste0(outpath8, i, '.tif'), overwrite = TRUE)
  }
  if(length(grep(i, file_list8)) >= 2){
    temp <- sprc(paste0(spath8, grep(i, file_list8, value = TRUE)))
    m <- mosaic(temp)
    names(m) <- c('CoastalBlue1', 'Blue2', 'Green13', 'Green4', 'Yellow5', 'Red6', 'RedEdge7', 'NIR8')
    writeRaster(m, paste0(outpath8, i, '.tif'), overwrite = TRUE)
  }
}


#Calculate some indices (add them to each file)
rasts_list <- list.files(outpath8, pattern = '*.tif')
for (i in rasts_list){
  assign(paste0('R', gsub('-', '', substr(i, start = 1, stop = 10))), rast(paste0(outpath8, i)))
}

ar_list <- paste0('R', gsub('-', '', substr(rasts_list, start = 1, stop = 10)))

#Indices
for (i in 1:length(ar_list)){
  temp <- get(ar_list[i])
  temp$NDVI <- (temp$NIR8 - temp$Red6) / (temp$NIR8 + temp$Red6) #NDVI
  temp$EVI2 <- 2.4 * ( (temp$NIR8 - temp$Red6) / (temp$NIR8 + temp$Red6 + 1)  ) #EVI2
  temp$NRVI <- ((temp$Red6 / temp$NIR8) - 1) / ((temp$Red6 / temp$NIR8) + 1) #NRVI
  temp$CCI <- (temp$Green4 - temp$Red6) / (temp$Green4 + temp$Red6) #CCI
  temp$BI <- (temp$Blue2 / temp$NIR8) #B/I ratio
  assign(ar_list[i], temp)
}

names_list <- c('CoastalBlue1', 'Blue2', 'Green13', 'Green4', 'Yellow5', 'Red6', 'RedEdge7', 'NIR8', 'NDVI', 'EVI2', 'NRVI', 'CCI', 'BI')

#EVI2
# for (i in 1:length(ar_list)){
#   temp <- get(ar_list[i])
#   evi2Temp <- 2.4 * ( (temp$NIR8 - temp$Red6) / (temp$NIR8 + temp$Red6 + 1)  )
#   names(evi2Temp) <- c('EVI2')
#   assign(paste0(ar_list[i], ''), c(temp, evi2Temp))
# }

#NRVI
# for (i in 1:length(ar_list)){
#   temp <- get(ar_list[i])
#   nrviTemp <- ((temp$Red6 / temp$NIR8) - 1) / ((temp$Red6 / temp$NIR8) + 1)
#   names(nrviTemp) <- c('NRVI')
#   assign(paste0(ar_list[i], ''), c(temp, nrviTemp))
# }

#CCI
# for (i in 1:length(ar_list)){
#   temp <- get(ar_list[i])
#   cciTemp <- (temp$Green4 - temp$Red6) / (temp$Green4 + temp$Red6)
#   names(cciTemp) <- c('CCI')
#   assign(paste0(ar_list[i], ''), c(temp, cciTemp))
# }

ar_temp <- list(get(ar_list[1]))
for (i in 2:length(ar_list)){
  ar_temp <- append(ar_temp, get(ar_list[i]))
}
ar_rasts <- rast(ar_temp)


for ( i in 1:length(ar_list)){
  writeRaster(get(ar_list[i]), paste0(outpath8, 'Stacks/', ar_list[i], '.tif'), overwrite = TRUE)
}

writeRaster(ar_rasts, paste0(outpath8, 'Stacks/arStack.tif'), overwrite = TRUE)
testPoints <- terra::vect("C:/Users/eahorner/Documents/GIS 712 Remote Sensing/Project/phenology_test.shp")

eightBandExtract <- extract(ar_rasts, testPoints, ID = FALSE)

eightBandDF <- cbind(eightBandExtract, testPoints)

infEightBand <- eightBandDF[eightBandDF$pixel == 'Infected',]
notinfEightBand <- eightBandDF[eightBandDF$pixel == 'Not_Infected',]

#Data Table Stuff and Distributions
bigDT <- data.table(eightBandDF)

for (i in 1:length(names_list)){
  tempH <- as.vector(as.matrix(bigDT[pixel == 'Not_Infected', grep(names_list[i], names(bigDT)), with = FALSE]))
  tempH <- tempH[!is.na(tempH)]
  
  tempD <- fitdistrplus::fitdist(tempH, 'norm')
  assign(paste0(names_list[i], 'vector'), tempH)
  assign(paste0(names_list[i], 'dist'), tempD)
}

#Plots


#Smoothed
for (i in 1:length(names_list)){
  tempI <- cbind(data.table(infEightBand[grep(names_list[i], colnames(notinfEightBand))])[, .(mean = colMeans(.SD), sd = lapply(.SD, sd))], plot_dates)
  tempN <- cbind(data.table(notinfEightBand[grep(names_list[i], colnames(notinfEightBand))])[, .(mean = colMeans(.SD), sd = lapply(.SD, sd)),], plot_dates)
  
  tempI$class = 'Infected'
  tempN$class = 'Not_Infected'

  tempSummary <- rbind(tempI, tempN)
  tempSummary$band <- names_list[i]
  
  if (i == 1){
    bigSummary <- tempSummary
  }
  
  if (i > 1){
    bigSummary <- rbind(bigSummary, tempSummary)
  }
}

p <- ggplot(bigSummary, aes(x = plot_dates, y = mean, group = class, col = class, fill = class)) + geom_point() + geom_smooth() + facet_wrap(~band, scales = 'free') + geom_vline(xintercept = as.Date('2021-05-08'), linetype = 'dashed')
p

bigSummary$infTime <- ifelse(as.Date('2021-05-08') < bigSummary$plot_dates, 'after', 'before')

#Not smoothed
par(mfrow=c(3,5))
for (i in 1:length(names_list)){
  tempI <- cbind(data.table(infEightBand[grep(names_list[i], colnames(infEightBand))])[, .(mean = colMeans(.SD), sd = lapply(.SD, sd)),], unique_dates)
  tempN <- cbind(data.table(notinfEightBand[grep(names_list[i], colnames(notinfEightBand))])[, .(mean = colMeans(.SD), sd = lapply(.SD, sd)),], unique_dates)
  
  plot(plot_dates, tempI$mean, col = 'firebrick1', type = 'l', lwd = 2.5, main = paste0('Infected vs Uninfected; Band: ', names_list[i]) )
  #lines(plot_dates, (tempI$mean + (1.96*as.double(tempI$sd))), col = 'firebrick1', lty = 'dashed')
  #lines(plot_dates, (tempI$mean - (1.96*as.double(tempI$sd))), col = 'firebrick1', lty = 'dashed')
  #polygon(c(plot_dates, rev(plot_dates)), c(tempI$mean - (1.96*as.double(tempI$sd)), rev(tempI$mean + (1.96*as.double(tempI$sd)))), col = adjustcolor('firebrick1', alpha.f = 0.4), border = FALSE)
  lines(plot_dates, (tempI$mean), col = 'firebrick1', lwd = 2.5)
  
  #polygon(c(plot_dates, rev(plot_dates)), c(tempN$mean - (1.96*as.double(tempN$sd)), rev(tempN$mean + (1.96*as.double(tempN$sd)))), col = adjustcolor('steelblue3', alpha.f = 0.4), border = FALSE)
  lines(plot_dates, tempN$mean, col = 'steelblue3', type = 'l', lwd = 2.5)
  #lines(plot_dates, (tempN$mean + (1.96*as.double(tempN$sd))), col = 'steelblue3', lty = 'dashed')
  #lines(plot_dates, (tempN$mean - (1.96*as.double(tempN$sd))), col = 'steelblue3', lty = 'dashed')
  abline(v = as.Date('2021-05-08'), lwd = 2, lty = 'dashed', col = 'black')
  
}
par(mfrow=c(1,1))

#Difference Plots

#Smoothed
bigDiff <- subset(bigSummary[bigSummary$class == 'Infected'], select = -(class))
bigDiff$diff <- bigSummary$mean[bigSummary$class == 'Infected'] - bigSummary$mean[bigSummary$class == 'Not_Infected']
bigDiff$infTime <- ifelse(as.Date('2021-05-08') < bigDiff$plot_dates, 'after', 'before')

pd <- ggplot(bigDiff, aes(x = plot_dates, y = diff, group = infTime)) + geom_point() + facet_wrap(~band, scales = 'free') + geom_vline(xintercept = as.Date('2021-05-08'), linetype = 'dashed') + 
        labs(title = 'Infected Pixel Mean - Healthy Pixel Mean in each band/index', y = 'Difference', x = 'Date')
#, group = infTime
pd

#Not smoothed
par(mfrow=c(3,5))
for (i in 1:length(names_list)){
  tempI <- cbind(data.table(infEightBand[grep(names_list[i], colnames(infEightBand))])[, .(mean = colMeans(.SD), sd = lapply(.SD, sd)),], unique_dates)
  tempN <- cbind(data.table(notinfEightBand[grep(names_list[i], colnames(notinfEightBand))])[, .(mean = colMeans(.SD), sd = lapply(.SD, sd)),], unique_dates)
  
  plot(plot_dates, tempI$mean - tempN$mean, col = 'seagreen2', type = 'l', lwd = 2.5, main = paste0('Infected - Uninfected; Band: ', names_list[i]) )
  abline(h = mean(tempI$mean[1:33] - tempN$mean[1:33], na.rm = TRUE), lwd = 1.5, lty = 'dashed', col = 'steelblue3')
  abline(h = mean(tempI$mean[-(1:33)] - tempN$mean[-(1:33)], na.rm = TRUE), lwd = 1.5, lty = 'dashed', col = 'firebrick1')
  abline(v = as.Date('2021-05-08'), lwd = 2, lty = 'dashed', col = 'black')
}
par(mfrow=c(1,1))

#Plot with error (assuming normal)
for (i in 1:length(names_list)){
  bandPlot <- ggplot(bigSummary[band == paste0(names_list[i])], aes(x = plot_dates, y = mean, group = class, col = class, fill = class)) +
    geom_point() + geom_smooth(se = FALSE) +
    geom_linerange(aes(ymin = mean - (1.96 * sd), ymax = mean + (1.96 * sd), alpha = 0.5), linewidth = 1.4) +
    geom_vline(xintercept = as.Date('2021-05-08'), linetype = 'dashed') +
    labs(title = paste0('Infected vs Healthy Pixels, Band: ', names_list[i]), subtitle = 'Dots show mean pixel value, bars show 95% confidence interval calculated from standard deviation', y = 'Mean pixel value', x = 'Date')
  
  ggsave(paste0(outpath8, 'Plots/', names_list[i], '.png'), plot = bandPlot)
}

p <- ggplot(bigSummary, aes(x = plot_dates, y = mean, group = class, col = class, fill = class)) + geom_point() + geom_smooth(se = FALSE) + facet_wrap(~band, scales = 'free') + 
  geom_vline(xintercept = as.Date('2021-05-08'), linetype = 'dashed') + 
  geom_linerange(aes(ymin = mean - (1.96 * sd), ymax = mean + (1.96 * sd), alpha = 0.5), linewidth = 1.4) + 
  labs(title = paste0('Infected vs Healthy Pixels, Band: ', names_list[i]), subtitle = 'Dots show mean pixel value, bars show 95% confidence interval calculated from standard deviation', y = 'Mean pixel value', x = 'Date')
  
p


#Tests with individual rasters
library(ranger)

pfun <- \(...) {
  predict(...)$predictions
}

#34 (05/11/21)
i34e <- extract(get(ar_list[34]), testPoints, ID = FALSE)
i34m <- cbind(i34e, testPoints$pixel)
names(i34m) <- c(names(i34e), 'class')
i34m <- i34m[(i34m$class == 'Infected' | i34m$class == 'Not_Infected'),]
i34m$class <- as.factor(i34m$class)

i34rf <- ranger(class ~., data = i34m, importance = 'impurity')
#predict(get(ar_list[34]), i34rf, fun = pfun, na.rm = TRUE)

#30 (04/28/21)
i30e <- extract(get(ar_list[30]), testPoints, ID = FALSE)
i30m <- cbind(i30e, testPoints$pixel)
names(i30m) <- c(names(i30e), 'class')
i30m <- i30m[(i30m$class == 'Infected' | i30m$class == 'Not_Infected'),]
i30m$class <- as.factor(i30m$class)

i30rf <- ranger(class ~., data = i30m, importance = 'impurity')

#Combined
ic <- rbind(i30m, i34m)
icrf <- ranger(class ~., data = ic, importance = 'impurity')
