###############################################################################################
#   TOOLS FOR DAVID TO IMPORT MODEL ARTIFACTS 
###############################################################################################

# R libraries
library(reticulate)
library(tidyverse)

# python libraries
mtx <- import('sklearn.metrics')

# load relevant python model artifacts
imported_model <- py_load_object('xgboost_vix_clf.model')
imported_X_test <- py_load_object('X_test.df')
imported_y_test <- py_load_object('y_test.df')
imported_decoder <- py_load_object('decoder.df')
imported_model_y_pred <- imported_model$predict(imported_X_test)
imported_model_accuracy <- mtx$accuracy_score(imported_y_test, imported_model_y_pred)

print(paste('Accuracy: %', round(imported_model_accuracy * 100.0,2), sep=''))

# feature importance df for plotting in ggplot2 ... this was an R dataframe object that I saved using dump()
imported_feature_importances <- source('feature_importance.df')
imported_feature_importances$value

# reconstruct original test data, decode the target and add prediction to front of df
results_df <- cbind(imported_model_y_pred, imported_y_test, imported_X_test)
results_df[1:10,]
results_df <- results_df %>%
    rename(vix_change=imported_y_test, pred_vix_change=imported_model_y_pred)
results_df$vix_change <- recode(results_df$vix_change, '0'='higher volatility', '1'='lower volatility')
results_df$pred_vix_change <- recode(results_df$pred_vix_change, '0'='higher volatility', '1'='lower volatility')
results_df[1:10,]
