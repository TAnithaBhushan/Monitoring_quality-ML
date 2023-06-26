library(tidymodels)
tidymodels_prefer()
library(recipes)
library(parsnip)
library(xgboost)
#library(dials)
library(tidyverse)
library(themis)
set.seed(1234)

xgbs_rec <- recipe(wQI ~ ., data = data_train) %>%
  step_bsmote(wQI,over_ratio = 0.25) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()
xgbs_rec
#xgbd_rec %>%
# bake(new_data = NULL) %>%
#count(wQI)


test_proc<- bake(xgbs_rec, new_data = data_test)
library(parsnip)
library(xgboost)
xgbs_spec <-boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

xgbs_spec

xgbs_fit <- xgbs_spec %>%
  fit(wQI ~ ., data = juice(xgbs_rec))

xgbs_fit


set.seed(1234)
validation_splits <- mc_cv(juice(xgbs_rec), prop = 0.7, strata = wQI)
validation_splits
xgbs_model <- 
  boost_tree(
    mtry = 3, trees = 1000, min_n = 10, tree_depth =5,
    learn_rate = 1, loss_reduction = 1, sample_size = 0.5,
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  translate()


xgbs_wflow <- 
  workflow() %>% 
  add_formula(
    wQI ~ Temp + DO +PH+CONDUCTIVITY+BOD+NITRATE_NITRITE+FECAL_COLIFORM+TOTAL_COLIFORM) %>% 
  add_model(xgbs_model) 
xgbs_wflow

tic("Training time of XGBoost")
xgbs_res <- fit_resamples(
  
  xgbs_model,
  
  xgbs_rec,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)
toc()
library(xgboost)
library(dplyr)

wa_test_xgbs<-bind_cols(xgbs_res %>%
                           unnest(.predictions) %>%
                           mutate(model = "xgboost")) %>%
  group_by(model) 

xgbs_res %>%
  collect_metrics()
xgbs_conf <- xgbs_res %>%
  unnest(.predictions) %>%
  conf_mat(wQI, .pred_class)

xgbs_conf
library(yardstick)
library(caret)
xgbs_metrics <- metric_set( accuracy,mcc, f_meas,precision,recall)
xgbs_metrics(wa_test_xgbs, truth = wQI, estimate = .pred_class)

