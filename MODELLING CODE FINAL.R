
#### Configuration & Set Up ####

# Required packages
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(h2o)
library(ggplot2)
library(xgboost)
library(data.table)
library(purrr)
library(smotefamily)
library(pROC)


# Initialising h2o for modelling
h2o.init()

options(scipen = 999)
set.seed(129486)


#### Data Loading ####

# Loading provided data
modelling_data <- read_excel("Modeling Task_v3b.xlsx")

# Adjusting strings for consistency
modelling_data_use <- modelling_data %>%
  clean_names() %>% 
  mutate(across(
    c(acquisition_source, main_bet_sport, registration_device, first_bet_device),
    ~ .x %>%
      str_replace_all("([a-z])([A-Z])", "\\1_\\2") %>% 
      str_to_lower() %>%                 
      str_replace_all("[^a-z0-9]+", "_") %>%          
      str_replace_all("_+", "_") %>%                
      str_replace_all("^_|_$", "")                  
  )) %>% 
  as.data.table()


#### Functions ####

# Comparing column proportions and distributions across sets 
compare_cols <- function(df1, df2, col_name) {
  
  label1 <- deparse(substitute(df1))
  label2 <- deparse(substitute(df2))
  
  col1 <- df1[[col_name]]
  col2 <- df2[[col_name]]
  
  if (is.character(col1) || is.factor(col1)) {
    cat(paste0("Proportions for ", label1, ":\n"))
    print(round(prop.table(table(setNames(col1, col_name), useNA = "ifany")), 3))
    
    cat(paste0("\nProportions for ", label2, ":\n"))
    print(round(prop.table(table(setNames(col2, col_name), useNA = "ifany")), 3))
    
  } else if (is.numeric(col1)) {
    cat(paste0("Summary for ", label1, ":\n"))
    print(summary(col1))
    
    cat(paste0("\nSummary for ", label2, ":\n"))
    print(summary(col2))
    
  } else {
    cat("Other columns type")
  }
  
}

# Comparing effects between variables (including across factors & response)
compare_vars <- function(data, var1, var2) {
  
  x <- rlang::sym(var1)
  y <- rlang::sym(var2)
  
  # Boxplot if numeric vs categorical
  if (is.numeric(data[[var1]])) {
    ggplot(data, aes(x = !!y, y = !!x)) +
      geom_boxplot() +
      labs(title = paste(var1, "by", var2),
           x = var2,
           y = var1)
    
  # Stacked plot if categorical vs categorical
  } else {
    ggplot(data, aes(x = !!x, fill = !!y)) +
      geom_bar(position = "fill") +
      labs(title = paste(var2, "by", var1),
           x = var1,
           y = "Proportion")
  }
}

# Data processing for extracting quarters
extract_year_qt <- function(date) {
  
  month <- month(date)
  year <- year(date)
  
  quarter <- case_when(
    month %in% c(1, 2, 3)  ~ "Q1",
    month %in% c(4, 5, 6)   ~ "Q2",
    month %in% c(7, 8, 9)   ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  )
  
  paste0(year, "_", quarter)
  
}

# Specific function to train model given input parameters and factors 
train_gbm_model <- function(training_data_h2o, model_params, model_vars) {
  
  
  gbm_model <- h2o.gbm(
    x                  = model_vars,
    y                  = "survey_answer",
    training_frame     = training_data_h2o,
    distribution       = "multinomial",
    weights_column     = "balance_weight",
    nfolds             = model_params$nfolds,
    fold_assignment    = "Modulo",
    ntrees             = model_params$ntrees,
    max_depth          = model_params$max_depth,
    min_rows           = model_params$min_child_weight,
    learn_rate         = model_params$eta,
    sample_rate        = model_params$subsample,
    col_sample_rate    = model_params$colsample_by_tree,
    stopping_metric    = model_params$eval_metric,
    stopping_rounds    = model_params$stopping_rounds,
    stopping_tolerance = model_params$stopping_tolerance,
    seed               = 129486
  )
  
  return(gbm_model)
}

# Evlualtion table & metrics based on trained GBM
evaluate_gbm_model <- function(training_data_h2o, test_data_h2o, model_params, model_vars) {
  
  gbm_model <- train_gbm_model(training_data_h2o, model_params, model_vars)
  
  # Training model
  train_perf <- h2o.performance(gbm_model, train = TRUE)
  train_cm   <- h2o.confusionMatrix(gbm_model, newdata = training_data_h2o) %>% as.data.frame()
  
  train_logloss            <- h2o.logloss(train_perf)
  train_mean_class_error   <- h2o.mean_per_class_error(train_perf)
  train_error_rate_choc    <- train_cm["Chocolate", "Error"]
  train_error_rate_straw   <- train_cm["Strawberry", "Error"]
  train_error_rate_vanil   <- train_cm["Vanilla", "Error"]
  train_overall_error_rate <- train_cm["Totals", "Error"]
  
  train_preds  <- h2o.predict(gbm_model, training_data_h2o)
  train_labels <- as.vector(training_data_h2o$survey_answer)
  train_probs  <- as.data.frame(train_preds[, -1])
  train_auc    <- multiclass.roc(response = train_labels, predictor = as.matrix(train_probs))$auc
  
  # Test model
  test_perf <- h2o.performance(gbm_model, newdata = test_data_h2o)
  test_cm   <- h2o.confusionMatrix(gbm_model, newdata = test_data_h2o) %>% as.data.frame()
  
  test_logloss            <- h2o.logloss(test_perf)
  test_mean_class_error   <- h2o.mean_per_class_error(test_perf)
  test_error_rate_choc    <- test_cm["Chocolate", "Error"]
  test_error_rate_straw   <- test_cm["Strawberry", "Error"]
  test_error_rate_vanil   <- test_cm["Vanilla", "Error"]
  test_overall_error_rate <- test_cm["Totals", "Error"]
  
  test_preds  <- h2o.predict(gbm_model, test_data_h2o)
  test_labels <- as.vector(test_data_h2o$survey_answer)
  test_probs  <- as.data.frame(test_preds[, -1])
  test_auc    <- multiclass.roc(response = test_labels, predictor = as.matrix(test_probs))$auc
  
  # Comparison Table 
  data.table(
    
    # Params
    ntrees                  = model_params$ntrees,
    max_depth               = model_params$max_depth,
    min_rows                = model_params$min_child_weight,
    learn_rate              = model_params$eta,
    sample_rate             = model_params$subsample,
    col_sample_rate         = model_params$colsample_by_tree,
    stopping_metric         = model_params$eval_metric,
    stopping_rounds         = model_params$stopping_rounds,
    stopping_tolerance      = model_params$stopping_tolerance,
    nfolds                  = model_params$nfolds,
    
    # Training
    train_logloss           = train_logloss,
    train_mean_class_error  = train_mean_class_error,
    train_error_choc        = train_error_rate_choc,
    train_error_straw       = train_error_rate_straw,
    train_error_vanil       = train_error_rate_vanil,
    train_error_overall     = train_overall_error_rate,
    train_auc               = train_auc,
    
    # Test
    test_logloss            = test_logloss,
    test_mean_class_error   = test_mean_class_error,
    test_error_choc         = test_error_rate_choc,
    test_error_straw        = test_error_rate_straw,
    test_error_vanil        = test_error_rate_vanil,
    test_error_overall      = test_overall_error_rate,
    test_auc                = test_auc
  )
}


#### Model Data Set Up ####

# Specifying labelled data and conducting training/test split
model_data <- modelling_data_use %>% 
  filter(
    survey_answer != "No Answer"
  ) %>% 
  mutate(
    set = ifelse(row_number() %in% sample(seq_len(n()), size = floor(0.8 * n())), "tr", "te"),
    survey_answer = as.factor(survey_answer)
  ) 

#### Feature Engineering ####

# Main feature processing function
data_processing <- list(
  
  # Variables adjusted
  adjust_var = function(d){
    
    d[,`:=`(
      first_bet_device = recode(first_bet_device, "adjustment" = "internet"),
      registration_date = pmin(first_bet_date, registration_date)
    )
    ]
    
  }, 
  
  # Variables created
  create_var = function(d){
    
    d[,`:=`(
      first_bet_lag = as.numeric(difftime(first_bet_date, registration_date, units = "days")),
      device_change_flag = ifelse(registration_device != first_bet_device, "Y", "N"),
      registration_date_season = extract_year_qt(registration_date),
      first_bet_date_season = extract_year_qt(first_bet_date)
    )
    ]
    
  }
  
)

model_data <- data_processing$adjust_var(model_data)
model_data <- data_processing$create_var(model_data)


#### Exploratory Data Analysis ####

training_data <- model_data %>% filter(set == "tr")
test_data <- model_data %>% filter(set == "te")

# Comparing distributions/samples across sets
compare_cols(training_data, prediction_data, "state")

# Assessing variable relationships 
compare_vars(training_data, "first_bet_date_season", "survey_answer")


#### Variable Selection ####

# Specifying variables in the model 
model_vars <- c(
  "age",
  "state",
  "registration_device",
  "first_bet_device",
  "acquisition_source",
  "main_bet_sport",
  "first_week_turnover",
  "first_bet_lag",
  "device_change_flag",
  "registration_date_season",
  "first_bet_date_season"
)

#### Model Data Preparation ####

# Preparing model data with initial weights (inverse)
model_data_h2o <- model_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  group_by(survey_answer) %>%
  mutate(balance_weight = 1 / n()) %>%
  ungroup() %>%
  select(c("survey_answer", all_of(model_vars)), set, balance_weight) %>% 
  as.data.table()

# Preparing training data with hybrid sampling and normalising the weights by class 
sampling_targets <- c(Chocolate = 4000, Strawberry = 1500, Vanilla = 2000)

training_data_h2o <- model_data_h2o %>%
  filter(set == "tr") %>%
  select(-set) %>%
  group_by(survey_answer) %>%
  group_split() %>%
  map_df(~ {
    class_name <- unique(.x$survey_answer)
    target_n <- sampling_targets[[as.character(class_name)]]
    actual_n <- nrow(.x)
    slice_sample(.x, n = target_n, replace = actual_n < target_n)
  }) %>%
  mutate(balance_weight = balance_weight / mean(balance_weight)) %>% 
  ungroup() %>%
  as.h2o()

# Preparing test data 
test_data_h2o <- model_data_h2o %>%
  filter(set == "te") %>%
  select(-set) %>%
  as.h2o()


#### Model Training & Tuning ####

single_mod = TRUE

# Training a single model 
if(single_mod){

  # Hyperparameters
  model_params <- data.table(
    eval_metric          = "logloss",
    ntrees               = 261,
    min_child_weight     = 33,
    max_depth            = 9,
    eta                  = 0.1,
    subsample            = 0.9,
    colsample_by_tree    = 1,
    nfolds               = 5,
    stopping_rounds      = 100,
    stopping_tolerance   = 0.001
  )
  
  # Model training 
  gbm_model <- train_gbm_model(training_data_h2o, model_params, model_vars)
  
  # Model evaluation
  print(h2o.performance(gbm_model, train = TRUE))
  print(h2o.performance(gbm_model, newdata = test_data_h2o))
  print(h2o.varimp(gbm_model))

  train_preds <- h2o.predict(gbm_model, training_data_h2o)
  train_labels <- as.vector(training_data_h2o$survey_answer)
  train_probs <- as.data.frame(train_preds[, -1])  
  train_roc <- multiclass.roc(response = train_labels, predictor = as.matrix(train_probs))
  print(train_roc$auc)
  
  test_preds <- h2o.predict(gbm_model, test_data_h2o)
  test_labels <- as.vector(test_data_h2o$survey_answer)
  test_probs <- as.data.frame(test_preds[, -1])
  test_roc <- multiclass.roc(response = test_labels, predictor = as.matrix(test_probs))
  print(test_roc$auc)

} else {
  
  # Number of models to test in grid
  model_num = 100
  
  # Hyperparameter grid to test 
  model_params <- data.table(
    eval_metric         = sample(c("logloss", "misclassification"), model_num, replace = TRUE, prob = c(0.7, 0.3)),
    ntrees              = sample(20:500, model_num, replace = TRUE),
    max_depth           = sample(4:10, model_num, replace = TRUE),
    min_child_weight    = sample(20:200, model_num, replace = TRUE),
    eta                 = sample(c(0.1, 0.2), model_num, replace = TRUE),
    subsample           = sample(c(0.7, 0.8, 0.9, 1.0), model_num, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.4)),
    colsample_by_tree   = sample(c(0.7, 0.8, 0.9, 1.0), model_num, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.4)),
    stopping_rounds     = 10,
    stopping_tolerance  = 0.001,
    nfolds              = sample(2:5, model_num, replace = TRUE)
  )
  
  # Parameter tuning comparison/results 
  model_tuning <- rbindlist(lapply(1:nrow(model_params), function(i) {
    params_i <- as.list(model_params[i])
    evaluate_gbm_model(training_data_h2o, test_data_h2o, params_i, model_vars)
  }))

}

# Saving the selected model 
h2o.saveModel(gbm_model, "/home/analytics-user")


#### Model Predictions #### 

saved_model <- h2o.loadModel("GBM_model_R_1762149770095_538")

# Manipulating overall data
prediction_data <- modelling_data_use

# Manipulating prediction data 
prediction_data <- data_processing$adjust_var(prediction_data)
prediction_data <- data_processing$create_var(prediction_data)

prediction_data_h2o <- prediction_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(all_of(model_vars)) %>% 
  as.data.table() %>% 
  as.h2o()

# Generating predictions
generated_predictions <- h2o.predict(saved_model, prediction_data_h2o)

# Export table
all_predictions <- bind_cols(
  prediction_data,
  data.table(survey_answer_pred = as.vector(generated_predictions$predict))
) %>% 
  select(
    account_id,
    survey_answer,
    survey_answer_pred,
    starts_with("registration"),
    starts_with("first_bet"),
    device_change_flag,
    everything()
  )

write.csv(all_predictions, "all_predictions.csv")