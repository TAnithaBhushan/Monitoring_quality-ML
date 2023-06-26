library(tidymodels)
tidymodels_prefer()
library(recipes)
library(parsnip)
library(xgboost)
#library(dials)
library(tidyverse)
library(themis)
set.seed(1234)

xgbd_rec <- recipe(wQI ~ ., data = data_train) %>%
  step_downsample(wQI) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()
xgbd_rec
#xgbd_rec %>%
# bake(new_data = NULL) %>%
#count(wQI)


test_proc<- bake(xgbd_rec, new_data = data_test)
library(parsnip)
library(xgboost)
xgbd_spec <-boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

xgbd_spec

xgbd_fit <- xgbd_spec %>%
  fit(wQI ~ ., data = juice(xgbd_rec))

xgbd_fit


set.seed(1234)
validation_splits <- mc_cv(juice(xgbd_rec), prop = 0.7, strata = wQI)
validation_splits
xg_model <- 
  boost_tree(
    mtry = 3, trees = 1000, min_n = 10, tree_depth =5,
    learn_rate = 1, loss_reduction = 1, sample_size = 0.5,
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  translate()


xg_wflow <- 
  workflow() %>% 
  add_formula(
    wQI ~ Temp + DO +PH+CONDUCTIVITY+BOD+NITRATE_NITRITE+FECAL_COLIFORM+TOTAL_COLIFORM) %>% 
  add_model(xg_model) 
xg_wflow

tic("Training time of XGBoost")
xgbd_res <- fit_resamples(
  
  xg_model,
  
  xgbd_rec,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)
toc()
library(xgboost)
library(dplyr)

wa_test_xgbd<-bind_cols(xgbd_res %>%
                          unnest(.predictions) %>%
                          mutate(model = "xgboost")) %>%
  group_by(model) 

xgbd_res %>%
  collect_metrics()
xgbd_conf <- xgbd_res %>%
  unnest(.predictions) %>%
  conf_mat(wQI, .pred_class)

xgbd_conf
library(yardstick)
library(caret)
xgbd_metrics <- metric_set( accuracy,mcc, f_meas,precision,recall)
xgbd_metrics(wa_test_xgbd, truth = wQI, estimate = .pred_class)

