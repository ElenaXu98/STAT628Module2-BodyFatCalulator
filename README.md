# STAT628Module2-BodyFatCalulator
In this project, we provide a simple but efficient regression model to predict men's body fat percentage and more details are contained in the four folders.

# Code
There are two R code files contained in the code folder:
- datapreprocessing.R provides some visualization of the raw data, and removes or imputes some problematic data points. Besides, some new variables being used in the regression analysis are created in this file.
- main.R mainly contains the code for the model selection process by using stepwise regression analysis and cross-validation. And it also produces diagnostic plots for checking the model assumptions and the existence of influential points.

# Data
In the data folder:
- BodyFat.csv is a real data set contains 252 male with measurements of their body fat percentage and various anthropometric measurements. 
- New_data.Rdata is the Rdata file after data preprocessing.

# Image
In the image folder, we mainly provide some visualization of the raw data and the diagnostic plots for assumptions checking and influential points detecting:
- BodyfatVsHeight.png is the scatter plot of variables body fat and height.
- CheckAdiposity.png is the scatter plot that is used to compare the original value of adiposity and calculated adiposity.
- ResidulVsFittedvalue.png is the diagnostic plot for checking homoscedacity assumption of regression model.
- QQplot.png is the diagnostic plot for checking checking normality assumption of regression model.
- ResidualVsLeverage.png is the plot used to detect influential points.

# Summary
The two-page summary mainly introduces the whole process of this data analysis project and discusses about the strengths and weaknesses of our model.

# Toy Body Fat Calculator
- [Body Fat Calculator](https://elenaxu98.shinyapps.io/BodyFatShinyApp/)

# Authors
- Zihang Wang - (zwang2547@wisc.edu)
- Yinqiu Xu - (yxu475@wisc.edu)
- Sixu Li - (sli739@wisc.edu)



