library(tidymodels)
tidymodels_prefer()
library(recipes)
library(parsnip)
library(xgboost)
#library(dials)
library(tidyverse)
library(themis)
set.seed(1234)

xgada_rec <- recipe(wQI ~ ., data = data_train) %>%
  step_adasyn(wQI,over_ratio = 1) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()
xgada_rec
#xgbd_rec %>%
# bake(new_data = NULL) %>%
#count(wQI)


test_proc<- bake(xgada_rec, new_data = data_test)
library(parsnip)
library(xgboost)
xgada_spec <-boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

xgada_spec

xgada_fit <- xgada_spec %>%
  fit(wQI ~ ., data = juice(xgada_rec))

xgada_fit


set.seed(1234)
validation_splits <- mc_cv(juice(xgada_rec), prop = 0.7, strata = wQI)
validation_splits
xgada_model <- 
  boost_tree(
    mtry = 3, trees = 1000, min_n = 10, tree_depth =5,
    learn_rate = 1, loss_reduction = 1, sample_size = 0.5,
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  translate()


xgada_wflow <- 
  workflow() %>% 
  add_formula(
    wQI ~ Temp + DO +PH+CONDUCTIVITY+BOD+NITRATE_NITRITE+FECAL_COLIFORM+TOTAL_COLIFORM) %>% 
  add_model(xgada_model) 
xgada_wflow

tic("Training time of XGBoost")
xgada_res <- fit_resamples(
  
  xgada_model,
  
  xgada_rec,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)
toc()
library(xgboost)
library(dplyr)

wa_test_xgada<-bind_cols(xgada_res %>%
                          unnest(.predictions) %>%
                          mutate(model = "xgboost")) %>%
  group_by(model) 

xgada_res %>%
  collect_metrics()
xgada_conf <- xgada_res %>%
  unnest(.predictions) %>%
  conf_mat(wQI, .pred_class)

xgada_conf
library(yardstick)
library(caret)
xgada_metrics <- metric_set( accuracy,mcc, f_meas,precision,recall)
xgada_metrics(wa_test_xgada, truth = wQI, estimate = .pred_class)

