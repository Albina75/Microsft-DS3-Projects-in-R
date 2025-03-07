library(scales)
library(tidyverse)
library(modelr)
library(broom)

trips_per_day <- read_tsv('trips_per_day.tsv')
head(trips_per_day)

# Load in the holidays data
holidays <- read_delim('US_holidays_2014.txt', delim = ,)

# Join the trips and holiday data
trips_holidays <- left_join(trips_per_day, holidays, by = 'ymd') %>% 
  mutate(month = month(ymd), is_holiday = (ymd %in% holidays$ymd))

# Plot the data
trips_holidays %>% 
  group_by(holiday) %>% 
  ggplot(aes(x = ymd, y = num_trips, color = holiday, alpha = is_holiday)) +
  geom_point() +
  scale_y_continuous(label = comma)

# Find the difference between the holiday trip count deviation from the monthly average and the standard deviation of the monthly average
trips_holidays_sd <- trips_holidays %>%
  group_by(month) %>%
  mutate(mo_avg = mean(num_trips), mo_sd = sd(num_trips)) %>% 
  filter(!is.na(holiday)) %>% 
  mutate(diff_avg = num_trips-mo_avg) %>%
  mutate(diff_sd = abs(diff_avg)-(mo_sd*1.5))

summary(trips_holidays_sd$mo_sd)

# Plot the deviation of the difference of each holiday trip count from the standard deviation of the month.
trips_holidays_sd %>% 
  ggplot(aes(x = diff_sd, y = holiday)) +
  geom_point() +
  geom_vline(xintercept = 0)

# Keep all the holidays to the right of the line, i.e significant differences.
holidays_best <- trips_holidays_sd %>% 
  filter(diff_sd > 0)

# Add a logical column to the data to mark whether the day is a holiday (TRUE) or not (FALSE).
trips_per_day <- trips_per_day %>% 
  mutate(holiday = (ymd %in% holidays_best$ymd)) %>% 
  replace_na(list(holiday=F))

# Now replot the data
trips_per_day %>%
  ggplot(aes(x = ymd, y = num_trips, alpha = holiday)) +
  geom_point() +
  scale_y_continuous(label = comma)

# Add a column to mark day of week
trips_weekdays <- trips_per_day %>% 
  mutate(day = weekdays(ymd))

# Plot the average number of trips based on day of the week
trips_weekdays %>% 
  group_by(day) %>% 
  summarise(avg = mean(num_trips)) %>% 
  ggplot(aes(x = day, y = avg)) +
  geom_point() +
  scale_y_continuous(label = comma)

trips_weektype <- trips_weekdays %>% 
  mutate(weektype = ifelse(day=="Saturday"|day=="Sunday", "Weekend", "Weekday"))

# Now replot the data, and notice if weekend has on average fewer trips than weekdays
trips_weektype %>%
  ggplot(aes(x = ymd, y = num_trips, alpha = weektype)) +
  geom_point() +
  scale_y_continuous(label = comma)

# Update the data
trips_per_day <- trips_weektype %>% select(-day)

# Add in a column to mark day of week
trips_months <- trips_per_day %>% 
  mutate(month = month(ymd, label = T))

# Plot the average number of trips based on day of the week
trips_months %>% 
  group_by(month) %>% 
  summarise(avg = mean(num_trips)) %>% 
  ggplot(aes(x = month, y = avg)) +
  geom_point() +
  scale_y_continuous(label = comma)

# Update the data
trips_per_day <- trips_months

set.seed(18)

num_days <- nrow(trips_per_day)
frac_model <- 0.9
num_model <- floor(num_days * frac_model)

# randomly sample rows for the model set 
ndx <- sample(1:num_days, num_model, replace=F)

# used for the model
trips_per_day_model <- trips_per_day[ndx, ] %>% arrange(ymd)

# used for the final test set
trips_per_day_final_test <- trips_per_day[-ndx, ]

head(trips_per_day_model)
head(trips_per_day_final_test)

# features: prcp, snwd, snow, tmax, tmin, holiday, weektype, month

ggplot(trips_per_day_model, aes(x = prcp, y = num_trips)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = snwd, y = num_trips)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = snow, y = num_trips)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = tmax, y = num_trips)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = tmin, y = num_trips)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = holiday, y = num_trips)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = weektype, y = num_trips)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = month, y = num_trips)) +
  geom_point() +
  scale_y_continuous(label = comma)

set.seed(42)
num_folds <- 5
num_days <- nrow(trips_per_day_model)
num_train <- floor(num_days * (1-(1/num_folds)))

ndx <- sample(1:num_days, num_train, replace=F)

trips_per_day_model <- trips_per_day_model[ndx, ] %>%
  mutate(fold = (row_number() %% num_folds) + 1)

plot_function <- function(K, avg_validate_err, se_validate_err, title) {
  # plot the validate error, highlighting the value of k with the lowest average error
  plot_data <- data.frame(K, avg_validate_err, se_validate_err)
  ggplot(plot_data, aes(x=K, y=avg_validate_err)) +
    geom_pointrange(aes(ymin=avg_validate_err - se_validate_err,
                        ymax=avg_validate_err + se_validate_err,
                        color=avg_validate_err == min(avg_validate_err))) +
    geom_line(color = "red") +
    scale_x_continuous(breaks=1:12) +
    theme(legend.position="none") +
    xlab(title) +
    ylab('RMSE on validation data')
}

kfold_model_test <- function(model_list) {
  K <- 1:length(model_list)
  avg_validate_err <- c()
  se_validate_err <- c()
  for (k in K) {
    
    # do 5-fold cross-validation within each value of k
    validate_err <- c()
    for (f in 1:num_folds) {
      # fit on the training data
      trips_per_day_train <- filter(trips_per_day_model, fold != f)
      model <- lm(model_list[k], data=trips_per_day_train)
      
      # evaluate on the validation data
      trips_per_day_validate <- filter(trips_per_day_model, fold == f)
      validate_err[f] <- sqrt(mean((predict(model, trips_per_day_validate) - trips_per_day_validate$num_trips)^2))
    }
    
    # compute the average validation error across folds
    # and the standard error on this estimate
    avg_validate_err[k] <- mean(validate_err)
    se_validate_err[k] <- sd(validate_err) / sqrt(num_folds)
  }
  plot_function(K, avg_validate_err, se_validate_err, 'Model')
}

model_list <- c("num_trips ~ prcp + snwd + snow + tmax + tmin + holiday + weektype +
                  month",
                "num_trips ~ prcp*tmin + prcp*tmax + snwd*tmin + snow*tmin + 
                  snow*snwd + holiday + weektype + month", 
                "num_trips ~ prcp*tmin + prcp*tmax + snow*snwd*tmin + holiday +
                  weektype + month",
                "num_trips ~ prcp*tmin + prcp*tmax + snwd*tmin + snow*tmin + 
                  snow*snwd + holiday + weektype + month + tmin*tmax",
                "num_trips ~ prcp*tmin + prcp*tmax + snwd*tmin + snow*tmin + 
                  snow*snwd + holiday + weektype + month + snwd*tmax + snow*tmax",
                "num_trips ~ prcp*tmin + prcp*tmax + snwd*tmin + snow*tmin + 
                  snow*snwd + holiday + weektype + month + snwd*tmax + snow*tmax +
                  tmin*tmax",
                "num_trips ~ prcp*tmin + prcp*tmax + snow*snwd*tmin + holiday +
                  weektype + month + snwd*tmax + snow*tmax + tmin*tmax")
kfold_model_test(model_list)


# Compute the benchmarks
avg_tmin <- mean(trips_per_day_model$tmin)
hlfsdup_tmax <- mean(trips_per_day_model$tmax)+(sd(trips_per_day_model$tmax)/2)
avg_prcp <- mean(trips_per_day_model$prcp)
hlfsdup_prcp <- mean(trips_per_day_model$prcp)+(sd(trips_per_day_model$prcp)/2)

# Plot prcp against trip count, seperating with color by high and low tmin, and plot lines to signify the benchmarks
ggplot(trips_per_day_model, aes(x = prcp, y = num_trips, color = tmin < avg_tmin)) +
  geom_point() +
  geom_vline(xintercept = avg_prcp) +
  geom_vline(xintercept = hlfsdup_prcp, linetype = 'dashed') +
  scale_y_continuous(label = comma)

# Plot prcp against trip count, seperating with color by high and tmax, and plot lines to signify the benchmarks
ggplot(trips_per_day_model, aes(x = prcp, y = num_trips, color = tmax > hlfsdup_tmax)) +
  geom_point() +
  geom_vline(xintercept = avg_prcp) +
  geom_vline(xintercept = hlfsdup_prcp, linetype = 'dashed') +
  scale_y_continuous(label = comma)

# Plot tmax against trip count, separating with color by high and low prcp based on the two different benchmarks
ggplot(trips_per_day_model, aes(x = tmax, y = num_trips, color = prcp > avg_prcp)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = tmax, y = num_trips, color = prcp > hlfsdup_prcp)) +
  geom_point() +
  scale_y_continuous(label = comma)

# Plot tmin against trip count, separating with color by high and low prcp based on the two different benchmarks
ggplot(trips_per_day_model, aes(x = tmin, y = num_trips, color = prcp > avg_prcp)) +
  geom_point() +
  scale_y_continuous(label = comma)

ggplot(trips_per_day_model, aes(x = tmin, y = num_trips, color = prcp > hlfsdup_prcp)) +
  geom_point() +
  scale_y_continuous(label = comma)

model_list <- c("num_trips ~ prcp*tmin + prcp*tmax + snwd*tmin + snow*tmin + 
                  snow*snwd + holiday + weektype + month",
                "num_trips ~ I(prcp > hlfsdup_prcp)*tmin + 
                  I(prcp > hlfsdup_prcp)*tmax + snwd*tmin + snow*tmin + snow*snwd +
                  holiday + weektype + month",
                "num_trips ~ I(prcp > avg_prcp)*tmin + I(prcp > hlfsdup_prcp)*tmax +
                  snwd*tmin + snow*tmin + snow*snwd + holiday + weektype + month",
                "num_trips ~ I(prcp > 0)*tmin + I(prcp > hlfsdup_prcp)*tmax + 
                  snwd*tmin + snow*tmin + snow*snwd + holiday + weektype + month",
                "num_trips ~ prcp*I(tmin < avg_tmin) + prcp*I(tmax > hlfsdup_tmax) +
                  snwd*tmin + snow*tmin + snow*snwd + holiday + weektype + month",
                "num_trips ~ I(prcp > 0)*I(tmin < avg_tmin) + 
                  I(prcp > hlfsdup_prcp)*I(tmax > hlfsdup_tmax) + snwd*tmin + 
                  snow*tmin + snow*snwd + holiday + weektype + month")
kfold_model_test(model_list)

# fit a model for each polynomial degree
K <- 1:7
avg_validate_err <- c()
se_validate_err <- c()
for (k in K) {
  
  # do 5-fold cross-validation within each value of k
  validate_err <- c()
  for (f in 1:num_folds) {
    # fit on the training data
    trips_per_day_train <- filter(trips_per_day_model, fold != f)
    model <- lm(num_trips ~ poly(tmin, k, raw=T), data=trips_per_day_train)
    
    # evaluate on the validation data
    trips_per_day_validate <- filter(trips_per_day_model, fold == f)
    validate_err[f] <- sqrt(mean((predict(model, trips_per_day_validate) - trips_per_day_validate$num_trips)^2))
  }
  
  # compute the average validation error across folds
  # and the standard error on this estimate
  avg_validate_err[k] <- mean(validate_err)
  se_validate_err[k] <- sd(validate_err) / sqrt(num_folds)
}

# plot the validate error, highlighting the value of k with the lowest average error
plot_function(K, avg_validate_err, se_validate_err, 'Polynomial Degree')

# fit a model for each polynomial degree
K <- 1:7
avg_validate_err <- c()
se_validate_err <- c()
for (k in K) {
  
  # do 5-fold cross-validation within each value of k
  validate_err <- c()
  for (f in 1:num_folds) {
    # fit on the training data
    trips_per_day_train <- filter(trips_per_day_model, fold != f)
    model <- lm(num_trips ~ poly(tmax, k, raw=T), data=trips_per_day_train)
    
    # evaluate on the validation data
    trips_per_day_validate <- filter(trips_per_day_model, fold == f)
    validate_err[f] <- sqrt(mean((predict(model, trips_per_day_validate) - trips_per_day_validate$num_trips)^2))
  }
  
  # compute the average validation error across folds
  # and the standard error on this estimate
  avg_validate_err[k] <- mean(validate_err)
  se_validate_err[k] <- sd(validate_err) / sqrt(num_folds)
}

# plot the validate error, highlighting the value of k with the lowest average error
plot_function(K, avg_validate_err, se_validate_err, 'Polynomial Degree')


# fit a model for each polynomial degree
K <- 1:7
avg_validate_err <- c()
se_validate_err <- c()
for (k in K) {
  
  # do 5-fold cross-validation within each value of k
  validate_err <- c()
  for (f in 1:num_folds) {
    # fit on the training data
    trips_per_day_train <- filter(trips_per_day_model, fold != f)
    model <- lm(num_trips ~ poly(month, k, raw=T), data=trips_per_day_train)
    
    # evaluate on the validation data
    trips_per_day_validate <- filter(trips_per_day_model, fold == f)
    validate_err[f] <- sqrt(mean((predict(model, trips_per_day_validate) - trips_per_day_validate$num_trips)^2))
  }
  
  # compute the average validation error across folds
  # and the standard error on this estimate
  avg_validate_err[k] <- mean(validate_err)
  se_validate_err[k] <- sd(validate_err) / sqrt(num_folds)
}

# plot the validate error, highlighting the value of k with the lowest average error
plot_function(K, avg_validate_err, se_validate_err, 'Polynomial Degree')

model_list <- c("num_trips ~ I(prcp > avg_prcp)*tmin + I(prcp > hlfsdup_prcp)*tmax +
                  snwd*tmin + snow*tmin + snow*snwd + holiday + weektype + month",
                "num_trips ~ I(prcp > avg_prcp)*tmin + I(prcp > hlfsdup_prcp)*tmax +
                  snwd*tmin + snow*tmin + snow*snwd + holiday + weektype + 
                  poly(month, 4, raw=T) + poly(tmax, 4, raw=T) + poly(tmin, 4, raw=T)")
kfold_model_test(model_list)

model <- lm(num_trips ~ I(prcp > 0.15)*tmin + I(prcp > 0.36)*tmax + 
              snwd*tmin + snow*tmin + snow*snwd + holiday + weektype + 
              poly(month, 4, raw=T) + poly(tmax, 4, raw=T) + poly(tmin, 4, raw=T),
            data=trips_per_day)

rmse(model, trips_per_day) 

summary(model)

plot_2014 <- trips_per_day %>%
  add_predictions(model) %>% 
  ggplot(aes(x = ymd, y = num_trips)) +
  geom_point() +
  geom_smooth(aes(y = pred), se=F) +
  labs(x = 'Date', y = 'Daily trips') +
  scale_y_continuous()

plot_2014

save(model, plot_2014, file = 'predict_citibike.Rdata')

trips_model <- trips_per_day_model %>%
  add_predictions(model)

trips_model %>% 
  ggplot(aes(x = num_trips, y = pred)) +
  geom_point() +
  geom_abline(color = 'blue') + 
  labs(x = 'Actual', y = 'Predicted') +
  scale_y_continuous()