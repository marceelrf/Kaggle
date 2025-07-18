# CV
set.seed(42)

split <- initial_split(train,
                       prop = 0.75,
                       strata = Personality)

train_data <- training(split)
val_data  <- testing(split)

# Receita
svm_rec <- recipe(x = train_data, formula = "Personality ~ .") %>% 
  update_role(id, new_role = "id") %>% 
  step_impute_bag(all_numeric_predictors()) %>% 
  step_impute_bag(all_nominal_predictors()) %>% 
  step_mutate(var1 = factor(ifelse(Stage_fear == Drained_after_socializing, "Yes", "No"))) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

# Modelo SVM radial
svm_model <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Workflow
wf_svm <- workflow() %>%
  add_model(svm_model) %>%
  add_recipe(svm_rec)

# Grade de hiperparâmetros
grid_svm <- grid_regular(
  cost(range = c(-5, 2)),         # log2 scale (interpretação comum para custo: 2^-5 a 2^2)
  rbf_sigma(range = c(-5, 1)),    # idem para sigma
  levels = 5
)

folds <- vfold_cv(train_data, v = 5)

# Tunagem
tunned_svm <- tune_grid(
  wf_svm,
  resamples = folds,
  grid = grid_svm,
  metrics = metric_set(accuracy, roc_auc, precision, f_meas),
  control = control_grid(verbose = TRUE, save_pred = TRUE)
)

# Avaliação dos resultados
tunned_svm %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc")

autoplot(tunned_svm)

show_best(tunned_svm)
best_svm <- select_best(tunned_svm, metric = "f_meas")

# Modelo final
final_svm <- finalize_model(svm_model, best_svm)

final_wf_svm <- workflow() %>%
  add_recipe(svm_rec) %>%
  add_model(final_svm)

# Avaliação na validação
final_res_svm <- final_wf_svm %>%
  last_fit(split)

final_res_svm %>%
  collect_metrics()

# Ajuste final no conjunto de treino completo
svm_fit <- fit(final_wf_svm, train)

# Previsões para o conjunto de teste
test <- read_csv(file = "playground-series-s5e7/test.csv")
preds_tbl_svm <- predict(svm_fit, test)

ss <- read_csv(file = "playground-series-s5e7/sample_submission.csv")
ss$Personality <- preds_tbl_svm$.pred_class

write_csv(x = ss, file = "playground-series-s5e7/submission_svm_01.csv")
