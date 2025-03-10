# Microsoft-DS3-Projects-in-R

## Introduction
This project builds a predictive model for daily CitiBike trip counts in New York City using weather and calendar data. The goal is to understand how factors like temperature, precipitation, and holidays influence bike usage and to develop a robust model for forecasting future trips.
<html>
<img src = "https://github.com/user-attachments/assets/df24b475-d770-4f5f-8835-e54503efa230" width = "1000", Height = "350")>
</html>

## Objective
+ Develop a statistically rigorous predictive model for CitiBike trip counts.
+ Explore the relationship between weather conditions, seasonal trends, and ridership patterns.
+ Use machine learning and regression techniques to optimize model performance.

## Methodology

#### _Data Collection & Preprocessing_
+ Dataset: CitiBike trip data (2014) + weather data from Central Park.
+ Feature Engineering:
Added holidays, weekdays/weekends, and months as categorical variables.
Created interaction terms (e.g., prcp * tmin, snow * snwd).
Applied threshold-based transformations for precipitation.

#### _Exploratory Data Analysis (EDA)_
+ Visualized trip count distributions across time, weather, and categorical factors.
+ Identified nonlinear relationships between temperature, precipitation, and ridership.

#### _Model Development & Selection_

+ Implemented multiple linear regression models with feature interactions.
+ Used 5-fold cross-validation for model evaluation.
+ Applied polynomial regression to capture seasonal trends in tmax, tmin, and month.

#### _Performance Evaluation_
+ Root Mean Squared Error (RMSE): 2835 (optimized model).
+ R² Score: 92% (adjusted R² = 91.8%), indicating strong predictive power.
+ Final Model Features: Weather variables, holiday/weekend indicators, polynomial seasonality terms.

## Results & Insights
+ Temperature is the most significant predictor—ridership increases with moderate temperatures.
+ Precipitation has a threshold effect—light rain has little impact, but heavy rain reduces trips significantly.
+ Holidays & weekends lower trip counts, as commuters form the primary user base.
+ Polynomial regression improves accuracy, capturing seasonal ridership patterns.

## Skills Demonstrated
#### Machine Learning & Statistical Modeling
+ Linear regression, interaction modeling, polynomial regression.
+ Feature engineering & transformation for better model accuracy.
+ Cross-validation & RMSE evaluation for model selection.

#### Data Science & Analysis
+ Data wrangling with tidyverse (dplyr, tidyr, ggplot2).
+ Time-series & trend analysis using CitiBike data.
+ Geospatial & weather-based analysis.

#### Technical Stack
+ Programming: R (tidyverse, modelr, broom)
+ Data Visualization: ggplot2, scales, geom_smooth
+ Statistical Techniques: Cross-validation, RMSE, polynomial regression
+ Reproducibility: RMarkdown, model saving (.Rdata file)
