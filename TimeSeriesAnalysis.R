rm(list = ls())

library(forecast)
library(TTR)
library(zoo)
library(ggplot2)
library(dplyr)
library(Rmisc)
library(ggpubr)
#set.seed(5)

setwd("C:/Users/user/Documents/rapa_nui_evolution")
RainData = read.csv("RainData.csv")

RainData$YEAR = format(as.Date(RainData$DATE, format="%m/%d/%Y"),"%Y")
RainData$MONTH = format(as.Date(RainData$DATE, format="%m/%d/%Y"),"%m")
RainDataTraining = subset(RainData, YEAR == "2000" | YEAR == "2001" | YEAR == "2002")
dataTS = data.frame(date = RainDataTraining$DATE, precipitation = RainDataTraining$PRCP)
dataTS$precipitation[is.na(dataTS$precipitation)] <- 0
freq = floor(nrow(dataTS)/3)
RainTimeSeires <- ts(dataTS$precipitation, frequency=freq, start=c(2000,1))
#fill nan with 0, 10 random samples per month without nan, do this on monthly basis 
#look up best rain forecasting methods
plot.ts(RainTimeSeires)
RainTimeSeiresComponents <- decompose(RainTimeSeires)
plot(RainTimeSeiresComponents)

rainseriesforecasts <- HoltWinters(RainTimeSeires, beta=FALSE, gamma=FALSE)
plot(rainseriesforecasts)

RainDataTesting =  subset(RainData, YEAR == "2007" | YEAR == "2008" | YEAR == "2009")
dataTSTest = data.frame(date = RainDataTraining$DATE, precipitation = RainDataTraining$PRCP)
dataTSTest$precipitation[is.na(dataTSTest$precipitation)] <- 0
#dataTSTest = dataTSTest[,nrow(dataTS)]

cor(dataTSTest$precipitation[1:length(as.numeric(rainseriesforecasts$fitted[,2]))],as.numeric(rainseriesforecasts$fitted[,2]))^2

### Monthly analysis

for(i in 1:nrow(RainData)){
  if(RainData$PRCP_ATTRIBUTES[i] == ",O,I"){
    RainData$PRCP[i] = NA
  }
}

sampleSize = 10 
years = unique(RainData$YEAR)
months = unique(RainData$MONTH)
RainDataNoNan = RainData[!is.na(RainData$PRCP), ]
precipitation = c()
month = c()
year = c()
combinations = nrow(expand.grid(years,months))

for(i in 1:length(years)){
  for(j in 1:length(months)){
    tempData = subset(RainDataNoNan, MONTH == months[j] & YEAR == years[i])
    if(nrow(tempData) > 9){
      precipitation = c(sample(tempData$PRCP, sampleSize, FALSE), precipitation)
      month = c(rep(months[j], sampleSize), month)
      year = c(rep(years[i], sampleSize), year)
    }
  }
}

dataMonthly = data.frame(year = year, month = month, precipitation = precipitation)
dataMonthly <- dataMonthly[order(year, month),]

dataMonthlyMeans = aggregate(precipitation ~ month + year, data = dataMonthly, mean)
dataMonthlyMeans$Group = sample(0:1,nrow(dataMonthlyMeans),TRUE)
dataMonthlyMeans$MonthsSinceStart = 1:nrow(dataMonthlyMeans)
summary(lm(precipitation~MonthsSinceStart, dataMonthlyMeans))

dataMonthlyMeans$Date <- as.yearmon(paste(dataMonthlyMeans$year, dataMonthlyMeans$month), "%Y %m")
dataTSMontly = data.frame(date = dataMonthlyMeans$Date, precipitation = dataMonthlyMeans$precipitation)
freq = 12
RainTimeSeiresMonthly <- ts(dataTSMontly$precipitation, frequency=freq, start=c(as.numeric(min(dataMonthlyMeans$year)),1))
plot.ts(RainTimeSeiresMonthly)
RainTimeSeiresComponentsMonthly <- decompose(RainTimeSeiresMonthly)
plot(RainTimeSeiresComponentsMonthly)

signalToNoiseRatio = sqrt(mean(RainTimeSeiresComponentsMonthly$seasonal^2))/sqrt(mean(na.omit(RainTimeSeiresComponentsMonthly$random)^2))

## length of ts
ll <- length(RainTimeSeiresMonthly)
## frequency (ie, 12)
ff <- frequency(RainTimeSeiresMonthly)
## number of periods (years); %/% is integer division
periods <- ll%/%ff
## index of cumulative month
index <- seq(1, ll, by = ff) - 1
## get mean by month
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(RainTimeSeiresMonthly[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean = 0
mm <- mm - mean(mm)

## plot the monthly seasonal effects
plot.ts(mm, ylab = "Seasonal effect", xlab = "Month", cex = 1)

dataMonthlyMeansGraph = aggregate(PRCP ~ MONTH, data = RainData, mean)
plot(dataMonthlyMeansGraph$PRCP~dataMonthlyMeansGraph$MONTH, type = 'l' )

rainseriesforecastsmonthly <- HoltWinters(RainTimeSeiresMonthly, beta=FALSE, gamma=FALSE)
plot(rainseriesforecastsmonthly, xlab = "Year", ylab = "Average Rainfall/Month (Inches)", main = "")
legend(2009, 36.5, legend=c("Raw Data", "Fit"), lty = 1, col=c("black", "red"), cex=1, lwd = 3)

cor(dataMonthlyMeans$precipitation[1:length(as.numeric(rainseriesforecastsmonthly$fitted[,2]))],as.numeric(rainseriesforecastsmonthly$fitted[,2]))^2

#cor(rep(mean(dataMonthlyMeans$precipitation),length(dataMonthlyMeans$precipitation)), dataMonthlyMeans$precipitation)^2

#test and training set 

testSet = subset(dataMonthlyMeans, Group == 1)
trainSet = subset(dataMonthlyMeans, Group == 0)
RainTimeSeiresMonthlyTest <- ts(testSet$precipitation, frequency=freq, start=c(as.numeric(min(dataMonthlyMeans$year)),1))
rainseriesforecaststest <- HoltWinters(RainTimeSeiresMonthlyTest, beta=FALSE, gamma=FALSE)
plot(rainseriesforecaststest)

cor(trainSet$precipitation[1:length(as.numeric(rainseriesforecaststest$fitted[,2]))],as.numeric(rainseriesforecaststest$fitted[,2]))^2

# other analyses

rainseriesforecastsmonthly$SSE
rainseriesforecasts2 <- forecast:::forecast.HoltWinters(rainseriesforecastsmonthly, h=8)
forecast:::plot.forecast(rainseriesforecasts2)
acf(rainseriesforecasts2$residuals[2:length(rainseriesforecasts2$residuals)], lag.max=20)
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box") #test for non-zero autocorrelations, this indicates that 2, maybe 5 are significant 

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(rainseriesforecasts2$residuals[2:length(rainseriesforecasts2$residuals)])

### Heatmap of seasonal effect, simulate over sampling procedure

simulations = 500
sampleSize = 10 

for(i in 1:nrow(RainData)){
  if(RainData$PRCP_ATTRIBUTES[i] == ",O,I"){
    RainData$PRCP[i] = NA
  }
}

years = unique(RainData$YEAR)
months = unique(RainData$MONTH)
RainDataNoNan = RainData[!is.na(RainData$PRCP), ]

signalToNoiseRatioVec = c()
mmMatrix = matrix(, nrow = simulations, ncol = 12)
monthMatrix = matrix(, nrow = simulations, ncol = 12)

for(k in 1:simulations){
  
  precipitation = c()
  month = c()
  year = c()
  combinations = nrow(expand.grid(years,months))
  
  for(i in 1:length(years)){
    for(j in 1:length(months)){
      tempData = subset(RainDataNoNan, MONTH == months[j] & YEAR == years[i])
      if(nrow(tempData) > 9){
        precipitation = c(sample(tempData$PRCP, sampleSize, FALSE), precipitation)
        month = c(rep(months[j], sampleSize), month)
        year = c(rep(years[i], sampleSize), year)
      }
    }
  }
  
  dataMonthly = data.frame(year = year, month = month, precipitation = precipitation)
  dataMonthly <- dataMonthly[order(year, month),]
  dataMonthlyMeans = aggregate(precipitation ~ month + year, data = dataMonthly, mean)
  dataMonthlyMeans$Group = sample(0:1,nrow(dataMonthlyMeans),TRUE)
  dataMonthlyMeans$MonthsSinceStart = 1:nrow(dataMonthlyMeans)
  dataMonthlyMeans$Date <- as.yearmon(paste(dataMonthlyMeans$year, dataMonthlyMeans$month), "%Y %m")
  dataTSMontly = data.frame(date = dataMonthlyMeans$Date, precipitation = dataMonthlyMeans$precipitation)
  freq = 12
  RainTimeSeiresMonthly <- ts(dataTSMontly$precipitation, frequency=freq, start=c(as.numeric(min(dataMonthlyMeans$year)),1))
  RainTimeSeiresComponentsMonthly <- decompose(RainTimeSeiresMonthly)

  ll <- length(RainTimeSeiresMonthly)
  ff <- frequency(RainTimeSeiresMonthly)
  periods <- ll%/%ff
  index <- seq(1, ll, by = ff) - 1
  mm <- numeric(ff)
  for (i in 1:ff) {
    mm[i] <- mean(RainTimeSeiresMonthly[index + i], na.rm = TRUE)
  }
  mm <- mm - mean(mm)
  
  signalToNoiseRatioVec[k] = sqrt(mean(RainTimeSeiresComponentsMonthly$seasonal^2))/sqrt(mean(na.omit(RainTimeSeiresComponentsMonthly$random)^2))
  mmMatrix[k, ] = mm 
  monthMatrix[k,] = 1:12
  
}

mean(signalToNoiseRatioVec)
mmVector = c(mmMatrix)
monthVector = c(monthMatrix)

monthlyDataRaw = data.frame(Month = monthVector, SeasonalEffect = mmVector)
monthlyData = monthlyDataRaw
monthlyData <- 
  monthlyData %>%
  group_by(Month) %>%
  dplyr::summarise(avg_SeasonalEffect = mean(SeasonalEffect), 
                   uci_SeasonalEffect = CI(SeasonalEffect)[1], 
                   lci_SeasonalEffect = CI(SeasonalEffect)[3]) 

#What you get if you don't remove NAs, naive approach 
dataMonthlyAll = data.frame(year = RainData$YEAR, month = RainData$MONTH, precipitation = RainData$PRCP)
dataMonthlyAll <- dataMonthlyAll[order(year, month),]
dataMonthlyMeans = aggregate(precipitation ~ month + year, data = dataMonthlyAll, mean)
dataMonthlyMeans$Group = sample(0:1,nrow(dataMonthlyMeans),TRUE)
dataMonthlyMeans$MonthsSinceStart = 1:nrow(dataMonthlyMeans)
dataMonthlyMeans$Date <- as.yearmon(paste(dataMonthlyMeans$year, dataMonthlyMeans$month), "%Y %m")
RainTimeSeiresMonthly <- ts(dataTSMontly$precipitation, frequency=freq, start=c(as.numeric(min(dataMonthlyMeans$year)),1))
RainTimeSeiresComponentsMonthly <- decompose(RainTimeSeiresMonthly)

ll <- length(RainTimeSeiresMonthly)
ff <- frequency(RainTimeSeiresMonthly)
periods <- ll%/%ff
index <- seq(1, ll, by = ff) - 1
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(RainTimeSeiresMonthly[index + i], na.rm = TRUE)
}
mm <- mm - mean(mm)

monthlyDataAll = data.frame(Month = 1:12, SeasonalEffect = mm)

#graph it all together 

dataGraph = data.frame(DataSource = c(rep("Raw Data", 12), rep("Bootstrap Data", 12)), SeasonalEffect = c(monthlyDataAll$SeasonalEffect, monthlyData$avg_SeasonalEffect), lci_SeasonalEffect = c(monthlyDataAll$SeasonalEffect, monthlyData$lci_SeasonalEffect), uci_SeasonalEffect = c(monthlyDataAll$SeasonalEffect, monthlyData$uci_SeasonalEffect), Month = 1:12)

ggplot() + 
  geom_jitter(data = monthlyDataRaw, aes(x = Month, y = SeasonalEffect), width = 0.25) +
  geom_line(data = dataGraph, aes(x = Month, y = SeasonalEffect, color = DataSource), size = 1.5) +
  geom_ribbon(data = dataGraph, aes(x = Month, y = SeasonalEffect, ymin = lci_SeasonalEffect, ymax = uci_SeasonalEffect, color = DataSource), alpha = 0.2) +
  theme_bw() + ylab("Seasonal Effect") + xlab("Month") + labs(color = "Data Source") +
  theme(text = element_text(size=15)) + 
  theme(legend.position = c(0.2, 0.8), legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) +
  geom_line(data = monthlyDataAll, aes(x = as.factor(Month), y = SeasonalEffect)) +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr",  "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

dataMonthlyAll = data.frame(year = RainData$YEAR, month = RainData$MONTH, precipitation = RainData$PRCP)
dataMonthlyAll <- dataMonthlyAll[order(year, month),]
dataMonthlyMeans = aggregate(precipitation ~ month, data = dataMonthlyAll, mean)
plot(dataMonthlyMeans$precipitation~dataMonthlyMeans$month, type = 'l')

### Plot of biased sampling

monthVec = c()
for(i in 1:nrow(RainDataNoNan)){
  
  if(RainDataNoNan$MONTH[i]=="01"){
    monthVec[i] = "Jan"
  } else if(RainDataNoNan$MONTH[i]=="02"){
    monthVec[i] = "Feb"
  } else if(RainDataNoNan$MONTH[i]=="03"){
    monthVec[i] = "Mar"
  } else if(RainDataNoNan$MONTH[i]=="04"){
    monthVec[i] = "Apr"
  } else if(RainDataNoNan$MONTH[i]=="05"){
    monthVec[i] = "May"
  } else if(RainDataNoNan$MONTH[i]=="06"){
    monthVec[i] = "Jun"
  } else if(RainDataNoNan$MONTH[i]=="07"){
    monthVec[i] = "Jul"
  } else if(RainDataNoNan$MONTH[i]=="08"){
    monthVec[i] = "Aug"
  } else if(RainDataNoNan$MONTH[i]=="09"){
    monthVec[i] = "Sep"
  } else if(RainDataNoNan$MONTH[i]=="10"){
    monthVec[i] = "Oct"
  } else if(RainDataNoNan$MONTH[i]=="11"){
    monthVec[i] = "Nov"
  } else {
    monthVec[i] = "Dec"
  }
  
}

monthsRepresented = table(monthVec)
yearsRepresented = table(RainDataNoNan$YEAR)


monthsRepresented = data.frame(Month = names(monthsRepresented), Count = as.numeric(monthsRepresented))
monthsRepresented$Month <- factor(monthsRepresented$Month, levels = c("Jan", "Feb", "Mar", "Apr",  "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
monthsRepresented$Prob = monthsRepresented$Count/sum(monthsRepresented$Count)

number = c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3)
ProbInOrder = monthsRepresented$Prob[number]
CountInOrder = ProbInOrder*sum(monthsRepresented$Count)

yearsRepresented = data.frame(Year = names(yearsRepresented), Count = as.numeric(yearsRepresented))
yearsVec = 1966:2021

chisq.test(monthsRepresented$Count)
chisq.test(yearsRepresented$Count)

CountsFull = c()

for(i in 1:length(yearsVec)){
  
  if(any(yearsRepresented$Year == yearsVec[i])){
    CountsFull[i] = yearsRepresented$Count[yearsRepresented$Year == yearsVec[i]]
  } else {
    CountsFull[i] = 0
  }
  
}

yearsRepresented = data.frame(Year = yearsVec, Count = CountsFull)

p1 = ggplot(yearsRepresented, aes(x = Year, y = Count)) + geom_point(size = 2) + theme_bw() + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("A)") + theme(text = element_text(size=15)) + ylab("Number of Measurements")

p2 = ggplot(monthsRepresented, aes(x = Month, y = Count)) + geom_point(size = 2.5) + geom_hline(yintercept=(sum(monthsRepresented$Count) *1/12), linetype="dashed", color = "black", size=1.25) + theme_bw() + xlab("Month") + ylab("Number of Measurements") + theme(text = element_text(size=15)) + ggtitle("B)") + geom_segment(aes(x = 1:12, y = rep(sum(monthsRepresented$Count) *1/12, 12), xend = 1:12, yend = CountInOrder)) 

ggarrange(p1, p2, nrow = 1, ncol = 2)

residualsMonth = abs(ProbInOrder - (1/12))
residualsMonth = (ProbInOrder - (1/12))
rawData = subset(dataGraph, dataGraph$DataSource == "Raw Data")
bootstrapData = subset(dataGraph, dataGraph$DataSource == "Bootstrap Data")
biasDeviance = abs(bootstrapData$SeasonalEffect-rawData$SeasonalEffect)

cor.test(residualsMonth, biasDeviance, method = 'spearman')
plot(residualsMonth, biasDeviance)

cor.test(monthsRepresented$Count, biasDeviance, method = 'spearman')
plot(monthsRepresented$Count, biasDeviance)

cor.test(residualsMonth, biasDeviance, method = 'spearman')
plot(residualsMonth, biasDeviance)

cor.test(bootstrapData$SeasonalEffect, rawData$SeasonalEffect, method = 'spearman')
plot(bootstrapData$SeasonalEffect, rawData$SeasonalEffect)

max(abs(monthlyDataRaw$SeasonalEffect))/sqrt(mean(na.omit(RainTimeSeiresComponentsMonthly$random)^2))
