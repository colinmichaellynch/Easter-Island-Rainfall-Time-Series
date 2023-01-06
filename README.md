# Easter-Island-Rainfall-Time-Series
Time Series analysis of Rainfall on Easter Island

## Table of Contents

* Rainfall Data 

* Time Series Script

## Background

This project folder details a project where I showed that the randomness of rainfall on Easter Island could promote the evolution of cooperation. However, that project is predicated on the assumption that rainfall is actually random. Here, I test whether rainfall is random in the strict sense that I use in the modeling project as well as the more general sense. When testing my strict definition of randomness, I analyze the distributions of run times for consecutive rainy days or dry days to see if they resemble the distributions I used for the model. When testing the general notion of randomness, I perform a time series analysis to decompose the signal (the amount of rainfall per day) into its different components and then determine whether or not the seasonal effect of rain is greater than or less than the degree of noise in the signal. 

## Methods 

In the model, rainfall was treated as a binary, discrete variable that could either be random or periodic. When rainfall was random, run times of rainfall and drought were drawn from an exponential distribution. Conversely, when rainfall was periodic, run times were drawn from a truncated exponential distribution. 
