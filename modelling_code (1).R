
#### Configuration & Set Up ####

library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(h2o)

h2o.init()


#### Data Loading ####

modelling_data <- read_excel("modelling_task_v3b.xlsx")

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
  ))


#### Model Data Set Up ####

set.seed(129486)

model_data <- modelling_data_use %>% 
  filter(
    survey_answer != "No Answer"
  ) %>% 
  mutate(
    set = ifelse(row_number() %in% sample(seq_len(n()), size = floor(0.8 * n())), "tr", "te"),
    survey_answer = as.factor(survey_answer)
  ) 

data_h2o <- as.h2o(model_data)



prediction_data <- modelling_data_use %>% 
  filter(
    survey_answer == "No Answer"
  )


#### Exploratory Analysis ####





#### Data Processing #### 



#### Model Training #### 

predictors <- setdiff(names(data_h2o), c("account_id", "survey_answer", "set"))

gbm_model <- h2o.gbm(
  x = predictors,
  y = "survey_answer",
  training_frame = data_h2o[data_h2o$set == "tr", ],
  distribution = "multinomial",  # multiclass
  ntrees = 200,                   # number of trees
  max_depth = 6,                  # tree depth
  learn_rate = 0.1,               # learning rate (shrinkage)
  sample_rate = 0.8,              # row subsampling
  col_sample_rate = 0.8,          # column subsampling
)


#### Model Evaluation ####

h2o.performance(gbm_model, train = TRUE) 
h2o.varimp_plot(gbm_model)   



#### Predictions ####










  
  








