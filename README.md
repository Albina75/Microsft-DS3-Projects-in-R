# Microsft-DS3-Projects-in-R

## Introduction
This project builds a predictive model for daily CitiBike trip counts in New York City using weather and calendar data. The goal is to understand how factors like temperature, precipitation, and holidays influence bike usage and to develop a robust model for forecasting future trips.

## Objective
+ Develop a statistically rigorous predictive model for CitiBike trip counts.
+ Explore the relationship between weather conditions, seasonal trends, and ridership patterns.
+ Use machine learning and regression techniques to optimize model performance.

## Methodology

### Data Collection & Preprocessing
+ Dataset: CitiBike trip data (2014) + weather data from Central Park.
+ Feature Engineering:
1. Added holidays, weekdays/weekends, and months as categorical variables.
2. Created interaction terms (e.g., prcp * tmin, snow * snwd).
3. Applied threshold-based transformations for precipitation.

### Exploratory Data Analysis (EDA)
+ Visualized trip count distributions across time, weather, and categorical factors.
+ Identified nonlinear relationships between temperature, precipitation, and ridership.

### Model Development & Selection

+ Implemented multiple linear regression models with feature interactions.
+ Used 5-fold cross-validation for model evaluation.
+ Applied polynomial regression to capture seasonal trends in tmax, tmin, and month.

### Performance Evaluation
+ Root Mean Squared Error (RMSE): 2835 (optimized model).
+ R² Score: 92% (adjusted R² = 91.8%), indicating strong predictive power.
+ Final Model Features: Weather variables, holiday/weekend indicators, polynomial seasonality terms.

### Results & Insights
+ Temperature is the most significant predictor—ridership increases with moderate temperatures.
+ Precipitation has a threshold effect—light rain has little impact, but heavy rain reduces trips significantly.
+ Holidays & weekends lower trip counts, as commuters form the primary user base.
+ Polynomial regression improves accuracy, capturing seasonal ridership patterns.
