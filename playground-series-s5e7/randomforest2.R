#CV
set.seed(42)

split <- initial_split(train,
                       prop = 0.75,
                       strata = Personality)

train_data <- training(split)
val_data  <- testing(split)


rf_rec <- recipe(x = train_data, formula = "Personality ~ .") %>% 
  update_role(id, new_role = "id") %>% 
  step_impute_bag(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors())

rf_model <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

wf_rf <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_rec)

rf_param <- 
  rf_model %>% 
  parameters()

rf_param <-
  rf_param %>% 
  finalize(train %>% select(-id))

grid_rf <- grid_regular(
  rf_param
)

folds <- vfold_cv(train_data,v = 5)

tunned_rf <- tune_grid(
  wf_rf,
  resamples = folds,
  grid = grid_rf,
  metrics = metric_set(accuracy,roc_auc,precision,f_meas),
  control = control_grid(verbose = T,save_pred = T)
)

tunned_rf %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc")

autoplot(tunned_rf)

show_best(tunned_rf)
best_rf <- select_best(tunned_rf, metric = "f_meas")

final_rf <- finalize_model(rf_model, best_rf)

final_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(split)

final_res %>%
  collect_metrics()

rf_fit <- fit(final_wf, train)

test <- read_csv(file = "playground-series-s5e7/test.csv")
preds_tbl <- predict(rf_fit, test)

ss <- read_csv(file = "playground-series-s5e7/sample_submission.csv")

ss$Personality <- preds_tbl$.pred_class

write_csv(x = ss,file = "playground-series-s5e7/submission_rf_02.csv")
