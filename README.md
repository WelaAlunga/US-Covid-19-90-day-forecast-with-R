# US-Covid-19-90-day-forecast-with-R
Built a model to predict how Covid 19 cases will look like in the next 3 months in an effort to get a statistical read into the future of the virus. Explored data from John Hopkins University, who have been tracking cases since January 2020, used linear regression to build a model. Employed R intensively, utilizing libraries such as: covid19.analytics, ggplot2, dplyr, prophet, lubridate and a few others.

First cleaned up the data from the source and prepared to plot confirmed case in the US throughout 2020 and the start of 2021. Relied on the prophet function which is able to produce reliable and robust forecasts. Next, made use of the predict function, which by default produces the 95% confidence limits, to build a data set with dates and cases of the next 90 days. Plotted an interactive chart that spanned till April and explored yearly, weekly and daily trends of the cases. The graph of predicted and actual values showed a perfect fit. Finally R squared and adjusted R square were both equal 1 and p-value very low which was an indication of the success of the model.

We were a able to see that the confirmed cases can increase from the current 2.4 million to 4 million in April. However, there is a limitation to this project as it does not take to account the impact of the vaccines in action. With that being said the confirmed cases curve has not seen a decrease since the start of vaccination but we are hopeful in the next couple of months there will be a decrease in the curve. Also found that Friday and Saturday had the most reported confirmed cases weekly.

