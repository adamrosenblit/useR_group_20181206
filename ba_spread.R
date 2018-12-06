library(tidyverse)
library(reticulate)
library(caret)
library(gridExtra)

# Import list of tickers from Excel
ticker_dat <- read_csv('tickers.csv')

# Create function to get stick prices from Yahoo API
get_prices <- function(ticker){
    tidyquant::tq_get(ticker, get = "stock.prices", from = '1990-01-02', to = '2018-11-27') %>% 
        mutate(ticker = ticker)
}

# Download stock prices, calculate spread, and lag the spread by one day
features <- map_df(ticker_dat$ticker, get_prices) %>%
    mutate(ba_spread = high - low,
           ba_spread_pct = ba_spread/((high+low)/2)) %>%
    group_by(ticker) %>%
    mutate(prior_ba_spread_pct = lag(ba_spread_pct, n = 1, order_by = date)) %>%
    select(date, ticker, prior_ba_spread_pct) %>%
    slice(-1) %>%
    spread(ticker, prior_ba_spread_pct) %>%
    ungroup()

#write_csv(spread_dat, 'spread_dat.csv', na = '')
#spread_dat <- read_csv('spread_dat.csv')

# Read VIX data downloaded from St. Louis Federal Reserve
target <- bind_rows(read_csv('vixarchive.csv'), read_csv('vixcurrent.csv')) %>%
    mutate(vix_delta = vix_close - lag(vix_close, n = 1, order_by = date),
           vix_change = case_when(
               vix_delta > 0 ~ 'higher volatility',
               vix_delta <= 0 ~ 'lower volatility'
           )) %>%
    slice(-1) %>%
    select(date, vix_change)

#write_csv(vix_dat, 'vix_dat.csv', na = '')
#vix_dat <- read_csv('vix_dat.csv')

# Collect data for some basic line plots to identify trends
plot_dat <- inner_join(features, target, by = 'date') %>%
    gather(key = 'ticker', value = 'ba_spread_pct', AAPL:T) %>%
    filter(lubridate::year(date) == 2018)

# Write function to create many plots at once
plot_func <- function(abbrev){
    ggplot(data = plot_dat %>% filter(ticker == abbrev)) +
        geom_point(mapping = aes(x = date, y = ba_spread_pct, color = vix_change)) +
        geom_line(mapping = aes(x = date, y = ba_spread_pct), color = "light grey") +
        ggtitle(label = abbrev)
}

# Create plots
ggs <- map(unique(plot_dat$ticker), plot_func)


# create the dataset for python ML 
model_df <- target %>% inner_join(features, by = c('date')) %>%
    mutate(vix_change = as_factor(vix_change))


# Drop the date column as it is unecessary for modeling.
# David encompased the temporal nature of the data in the calculation of vix_change.
model_df <- model_df %>% select(-date)

# Create logistic regression model
# set.seed(323)
# intrain <- createDataPartition(y = model_df$vix_change, p = 0.80, list = FALSE)
# training <- model_df[intrain,]
# testing <- model_df[-intrain,]
# 
# insubtrain <- createDataPartition(y = training$vix_change, p = 0.80, list = FALSE)
# training_test <- training[-insubtrain,]
# training_train <- training[insubtrain,]

# Create control for train (10-fold cross validation)
# control <- trainControl(method = "repeatedcv", number = 10, repeats = 2, verboseIter = FALSE)

# GLM - Logistic Regression
# fit_glm <- train(vix_change ~ ., method = "glm", family = "binomial", data = training_train, trControl = control)

#GLM - Logistic Regression
# predict_glm <- predict(fit_glm, newdata = training_test)
# test_glm <- confusionMatrix(predict_glm, training_test$vix_change)

# ggplot of confusion matrix and table of accuracy & kappa stats
# glm_plot <- ggplot(training_test, aes(x = predict_glm, y = vix_change)) + 
#     geom_jitter(width = 0.3, height = 0.3, alpha = 0.5, color = "dark blue") +
#     labs(title = "GLM", x = "prediction", y = "actual")
# 
# data.frame(model = "GLM",
#            accuracy = test_glm$overall[[1]],
#            kappa = test_glm$overall[[2]])

# Fit final model
# fit_glm <- train(vix_change ~ ., method = "glm", family = "binomial", data = training, trControl = control)
# predict_glm <- predict(fit_glm, newdata = testing)
# cm_glm <- confusionMatrix(predict_glm, testing$vix_change)
# 
# g_glm <- ggplot(testing, aes(x = predict_glm, y = vix_change)) + 
#     geom_jitter(width = 0.3, height = 0.3, alpha = 0.5, color = "dark blue") +
#     labs(title = "GLM", x = "prediction", y = "actual")
# 
# data.frame(model = "GLM",
#            accuracy = cm_glm$overall[[1]],
#            kappa = cm_glm$overall[[2]])


# Create XGBoost model in Python


# had to create project level .RProfile via terminal and and set the value of RETICULATE_PYTHON in order to get
# reticulate to use the correct installation of python (anaconda dist).  use_python() did not work
# for some reason, possibly order of operation?  See termincal commands
py_config()
# use_python('/Users/adamrosenblit/anaconda3/bin/python', required=TRUE)
# use_condaenv(condaenv='reticulate_demo_env', conda='/Users/adamrosenblit/anaconda3/bin/conda')
# py_config()
# py_discover_config()
# conda install -y -c conda-forge xgboost
# pip install xgboost
# py_install('xgboost')
# https://rstudio.github.io/reticulate/articles/python_packages.html
 
pd <- import('pandas')
np <- import('numpy')
xgb <- import('xgboost')
ms <- import('sklearn.model_selection')
pp <- import('sklearn.preprocessing')
mtx <- import('sklearn.metrics')

features <- model_df %>% select(-vix_change)
features[1:10,]

target <- model_df %>% select(vix_change)
target[1:10,]

encoder <- pp$LabelEncoder()
encoder <- encoder$fit(target) 
decoder <- data.frame(key=encoder$transform(encoder$classes_), value=encoder$classes_)
y <- encoder$transform(target)
X <- features

# need to make sure random_state value is an integer ... running without the suffix "L" doesn't work
test_train_split <- ms$train_test_split(X, y, stratify=y, test_size=0.2, random_state=42L)
X_train <- test_train_split[[1]]
X_test <- test_train_split[[2]]
y_train <- test_train_split[[3]]
y_test <- test_train_split[[4]]

# fit model on training data, using default params
# https://xgboost.readthedocs.io/en/latest/parameter.html
model_0 <- xgb$XGBClassifier(objective='binary:logistic')
model_0$fit(X_train, y_train)

# make predictions for test data and compare with actuals
model_0_y_pred <- model_0$predict(X_test)

# evaluate predictions
model_0_accuracy <- mtx$accuracy_score(y_test, model_0_y_pred)
print(paste('Accuracy: ', round(model_0_accuracy * 100.0,2), '%', sep=''))

# plot feature importance (ie, num splits per feature)
# https://stackoverflow.com/questions/34218245/how-is-the-feature-score-importance-in-the-xgboost-package-calculated
# plt <- import('matplotlib.pyplot')
# xgb$plot_importance(model.0)
# plt$show()
# https://towardsdatascience.com/interpreting-random-forest-and-other-black-box-models-like-xgboost-80f9cc4a3c38

model_0_feature_importances <- arrange(data.frame(feature_name=colnames(X), importance=model_0$feature_importances_),desc(importance))
model_0_feature_importances

# https://machinelearningmastery.com/feature-importance-and-feature-selection-with-xgboost-in-python/
# https://xgboost.readthedocs.io/en/latest/parameter.html
# https://towardsdatascience.com/fine-tuning-xgboost-in-python-like-a-boss-b4543ed8b1e
# https://rstudio.github.io/reticulate/reference/dict.html

keys <- list(
    'n_estimators', 
    'learning_rate', 
    'subsample', 
    'max_depth', 
    'min_child_weight', 
    'gamma',
    'colsample_bytree'
)
st <- import('scipy.stats')
values <- list(
    st$randint(100, 1000),
    st$uniform(0.1, 0.9),
    st$uniform(0.1, 0.9),
    st$randint(1,10),
    st$randint(1,10),
    st$uniform(0,10),
    st$uniform(0.1, 0.9)
)
params <- py_dict(keys, values, convert = TRUE)

# nthreads=-1 allows xgboost to parallelize training across processors
# https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.RandomizedSearchCV.html
# http://danielhnyk.cz/how-to-use-xgboost-in-python/

model_1 <- xgb$XGBClassifier(objective='binary:logistic', nthreads=-1)
tuned_model_1 <- ms$RandomizedSearchCV(model_1, param_distributions=params, n_jobs=1, n_iter=50L)
tuned_model_1$fit(X_train, y_train)  
best_estimator_m1 <- tuned_model_1$best_estimator_
best_estimator_m1

best_estimator_m1_y_pred <- best_estimator_m1$predict(X_test)
best_estimator_m1_accuracy <- mtx$accuracy_score(y_test, best_estimator_m1_y_pred)
print(paste('Accuracy: ', round(best_estimator_m1_accuracy * 100.0,2), '%', sep=''))


# Create ggplot2 confusion matrix plot
xg_plot <- bind_cols(prediction = best_estimator_m1_y_pred, actual = y_test) %>%
    mutate(accuracy = ifelse(prediction == actual, 'TRUE', 'FALSE')) %>%
ggplot(aes(x = best_estimator_m1_y_pred, y = y_test, color = accuracy)) + 
    geom_jitter(width = 0.3, height = 0.3, alpha = 0.5, size = 2) +
    labs(title = "XGBoost", x = "prediction", y = "actual") +
    scale_color_manual(values=c("#00008B", "#FF4500")) +
    theme(axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14)) +
    theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 14)) +
    theme(plot.title = element_text(size = 18, hjust = 0.5))
xg_plot


# Create data frame that contains feature importances
best_estimator_m1_feature_importances <- arrange(data.frame(feature_name=colnames(X), importance=best_estimator_m1$feature_importances_),desc(importance))
best_estimator_m1_feature_importances


# Plot feature importances
feature_plot <- best_estimator_m1_feature_importances %>%
    ggplot(aes(x = reorder(feature_name, -importance), y = importance)) + 
    geom_bar(stat = 'identity', color = 'dark red', fill = 'dark red') +
    labs(title = "Feature importance", x = "ticker", y = "importance") +
    theme(axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14)) +
    theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 14)) +
    theme(plot.title = element_text(size = 18, hjust = 0.5))
feature_plot

# leverage the pickle class from python by way of two reticulate functions, py_save_object and py_load_object
# https://github.com/rstudio/reticulate/blob/master/R/pickle.R
py_save_object(best_estimator_m1, 'xgboost_vix_clf.model')
py_save_object(X_test, 'X_test.df')
py_save_object(y_test, 'y_test.df')
py_save_object(decoder, 'decoder.df')

# saving this off as an R object
dump(c('best_estimator_m1_feature_importances'), 'feature_importance.df')



# or, run a python session from R console
# repl_python()
# 
# import pandas as pd
# import numpy as np
# from sklearn.model_selection import train_test_split
# from sklearn.preprocessing import LabelEncoder
# import sklearn.metrics as mtx
# 
# # get model_df from R session
# model_df = r.model_df
# 
# # Remember, like most programming languages, python uses zero based indexing.
# # The intervals for indexing are defined mathematically [a,b), a half-opne interval, which means "include a, and everything < b".
# # This is different from R, which uses [a,b], a closed interval, which translates to "include a, and everything <= b".
# # If you feel like understanding why zero based indexing is more common, https://en.wikipedia.org/wiki/Zero-based_numbering
# model_df.iloc[0:10,]
# # Another way to get the first 10 rows for any bash/linux guys out there
# model_df.head(10)
# 
# target_name = 'vix_change'
# # This is called a List Comprehension, and is a fundamental operation in base python.
# # Essentially, it's  a distributed/optimized for-loop
# feature_names = [c for c in model_df.columns if c != target_name]
# features, target = model_df[feature_names], model_df[target_name]
# 
# encoder = LabelEncoder()
# encoder = encoder.fit(target)
# decoder = dict(zip(encoder.transform(encoder.classes_), encoder.classes_))
# y = encoder.transform(target)
# X = features.copy()
# 
# X_train, X_test, y_train, y_test = train_test_split(X, y, stratify=y, test_size=0.2, random_state=42)
# 
# py_model_0 = xgb.XGBClassifier(objective='binary:logistic')
# py_model_0.fit(X_train, y_train)
# 
# # make predictions for test data and compare with actuals
# X_test = r.X_test
# py_model_0_y_pred = py_model_0.predict(X_test)
# 
# # evaluate predictions
# py_model_0_accuracy = mtx.accuracy_score(y_test, py_model_0_y_pred)
# print('Accuracy: ' + '%' + str(round(py_model_0_accuracy*100.0,2)))
# 
# exit

# or, read in a python script
# source_python("create_xgboost_model.py")

# or, embed the code used for the repl_python() session in RMarkdown
# ```{python}
#   <insert_python_code_here>
# ````
