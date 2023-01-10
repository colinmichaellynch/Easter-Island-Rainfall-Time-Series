# Easter-Island-Rainfall-Time-Series
Time Series analysis of Rainfall on Easter Island

## Table of Contents

* Rainfall Data 

* Time Series Script

* Supporting Documentation 

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

![](/Images/distributions.png)

![](/Images/timeSeries.png)

![](/Images/autocorrelation.png)
