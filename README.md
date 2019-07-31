# RecessionForecast
This is a script developed in R-Studio to forecast the odds of a recession in USA.

# How does it function
You don't need to input any variable. It's all code it for you. Well, you just need your QUANDL API KEY.

The software will download the information related with the PMI, 10-year treasury bond yield, 3-month treasury bond yield, unemployment rate and known crisis periods in order to feed the logistic regression model to forecast the odds of a recession in United States. 

The training-set of the machine learning algorithm will be all the past history of the data downloaded from the previous tickets (around xx periods). The test-set would be a forecast for the next 36 month of those tickets using ARIMA Model.

# Final conclusion
The final result of the script will be a graph with an recession indicator.

Enjoy and feel free to share any improvements.
