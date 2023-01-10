# Easter-Island-Rainfall-Time-Series
Time Series analysis of Rainfall on Easter Island

## Table of Contents

* [Rainfall Data](https://github.com/colinmichaellynch/Easter-Island-Rainfall-Time-Series/blob/main/RainData.csv)

* [Time Series Script](https://github.com/colinmichaellynch/Easter-Island-Rainfall-Time-Series/blob/main/TimeSeriesAnalysis.R)

* [Supporting Documentation](https://github.com/colinmichaellynch/Easter-Island-Rainfall-Time-Series/blob/main/Environmental%20stochasticity%20and%20resource%20heterogeneity%20may%20have%20driven%20the%20evolution%20of%20cooperation%20on%20Rapa%20Nui%20%20(1).docx) 

## Background

This project folder details a project where I showed that the randomness of rainfall on Easter Island could promote the evolution of cooperation. However, that project is predicated on the assumption that rainfall is actually random. Here, I test whether rainfall is random in the strict sense that I use in the modeling project as well as the more general sense. When testing my strict definition of randomness, I analyze the distributions of run times for consecutive rainy days or dry days to see if they resemble the distributions I used for the model. When testing the general notion of randomness, I perform a time series analysis to decompose the signal (the amount of rainfall per day) into its different components and then determine whether or not the seasonal effect of rain is greater than or less than the degree of noise in the signal. 

## Methods 

* In the model, rainfall was treated as a binary, discrete variable that could either be random or periodic. When rainfall was random, run times of rainfall and drought were drawn from an exponential distribution. Conversely, when rainfall was periodic, run times were drawn from a truncated exponential distribution. 

  - First, I convert data so that any day with rainfall is a rainy day whereas any day without rainfall is a drought. 

  - Use maximum likelihood estimation to fit exponential and truncated exponential distributions to interarrival times of rainfall and drought. 
  
  - Compared fits of each type of distribution with a chi-squared goodness of fit test as well as AIC/BIC. 
  
* This is an extremely specific definition of randomness, and it erases a lot of data by quantizing a continuous variable (amount of rainfall in inches) into a binary variable. Therefore, to see if rainfall was random generally, I decompose the continuous rainfall time series and compute the signal to noise ratio. The higher this ratio, the more predictable the rainfall. 
 
  - I use the Holt-Winters method to decompose the time series into trend, seasonal, and noise components. 

  - I use these to calculate the signal to noise ratio, which is the root mean squares of seasonality / root mean squares of noise 
  
  - I perform linear regression on raw data to see if there is a signifciant increase or decrease in rainfall since 1985.  

  - I calculate autocorrelation between rainfall between months to see if the rainfall of one month depends on the rainfall of the next. 
  
## Results 

* Rainfall and drougt distributions more closely follow an exponential distribution rather than a truncated one, as they have lower AIC/BIC values 

![](/Images/table.png)

* This means that rainfall is more aperiodic than periodic. 

![](/Images/distributions.png)

* The linear regression for the trendline is insignificant (p-value = 0.345, R^2 = 0.0862), meaning that rainfall hasn't changed over the past couple of decades and we may be able to extrapolate to the past 

![](/Images/timeSeries.png)

* The signal to noise ratio from Winter-Holts is 0.175, well below the typical threshold of 3

* This means that the rainfall of one month depends significantly on rainfall from the previous month. However, the autocorrelation of 0.186 is small according to Cohen’s thresholds for effect sizes (1992). 

![](/Images/autocorrelation.png)

* Together, these results illistrate that rainfall on Easter Island would have appeared random to the ancient inhabitants of the island.
