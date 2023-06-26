library(tidymodels)
tidymodels_prefer()
library(recipes)
library(parsnip)
library(xgboost)
#library(dials)
library(tidyverse)
library(themis)
set.seed(1234)

xgr_rec <- recipe(wQI ~ ., data = data_train) %>%
  step_rose(wQI,over_ratio = 1) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()
xgr_rec
#xgbd_rec %>%
# bake(new_data = NULL) %>%
#count(wQI)


test_proc<- bake(xgr_rec, new_data = data_test)
library(parsnip)
library(xgboost)
xgr_spec <-boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

xgr_spec

xgr_fit <- xgada_spec %>%
  fit(wQI ~ ., data = juice(xgr_rec))

xgr_fit


set.seed(1234)
validation_splits <- mc_cv(juice(xgr_rec), prop = 0.7, strata = wQI)
validation_splits
xgr_model <- 
  boost_tree(
    mtry = 3, trees = 1000, min_n = 10, tree_depth =5,
    learn_rate = 1, loss_reduction = 1, sample_size = 0.5,
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  translate()


xgr_wflow <- 
  workflow() %>% 
  add_formula(
    wQI ~ Temp + DO +PH+CONDUCTIVITY+BOD+NITRATE_NITRITE+FECAL_COLIFORM+TOTAL_COLIFORM) %>% 
  add_model(xgr_model) 
xgr_wflow

tic("Training time of XGBoost")
xgr_res <- fit_resamples(
  
  xgr_model,
  
  xgr_rec,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)
toc()
library(xgboost)
library(dplyr)

wa_test_xgr<-bind_cols(xgr_res %>%
                           unnest(.predictions) %>%
                           mutate(model = "xgboost")) %>%
  group_by(model) 

xgr_res %>%
  collect_metrics()
xgr_conf <- xgr_res %>%
  unnest(.predictions) %>%
  conf_mat(wQI, .pred_class)

xgr_conf
library(yardstick)
library(caret)
xgr_metrics <- metric_set( accuracy,mcc, f_meas,precision,recall)
xgr_metrics(wa_test_xgr, truth = wQI, estimate = .pred_class)

