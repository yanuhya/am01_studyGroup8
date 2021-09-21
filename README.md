# Final project - Ask a manager

As part of our final project we analysed the [ask a manager](https://docs.google.com/spreadsheets/d/1IPS5dBSGtwYVbjsfbaMCYIWnOuRmJcbequohNxCyGVw/edit?resourcekey#gid=1625408792) data, a survey in which 24000+ people participated and shared their salary and job information.

The majority of responses were received from white women in the US 

The key take aways from our analysis are:
1. There is a noticeable difference in pay for women and men and this difference is predominant in the US as compared to other countries.
2. Some industries value higher levels of education more than others. For instance, Law values higher education levels than Computing or Tech. 
3. In the US, there is a significant salary difference between respondents with a master's degree comapred to those with a bachelor degree.
4. Along the East and West coast of the US, we see that the salaries are higher than the other regions.

Steps we followed for creating our regression model:
* We dropped the rows which contained a ‘0’ entry for the annual salary column as this is the response variable we are trying to estimate
*	We created dummy variables for all the explanatory variables we intended to use for analysis
*	We performed a logarithmic transform on the response variables in an attempt to normalize the data
*	Created boxplot to check for outliers in spite of normalizing
*	Created separate data frame without outliers 
*	Split both data frames into training and testing data frames
*	Created two linear models – one full model and one reduced model using data from the data frame with outliers included
*	Trained and tested models on respective training and testing sets
*	Created two more linear models – one full model and one reduced model using data from the data frame with outliers excluded
*	Trained and tested models on respective training and testing sets
*	Used Akaike’s Information Criterion (AIC), Bayesian Information Criterion (BIC), and Root Mean Squared Error (RMSE) for model evaluation
*	Reduced model was preferred by both AIC and BIC; RMSE showed virtually no preference
