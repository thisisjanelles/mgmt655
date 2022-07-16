# Install Packages ----

devtools::install_github("https://github.com/Mikata-Project/ggthemr.git")
install.packages("parsnip")
library(parsnip)
pacman::p_load(tidyverse, lubridate,
               tidymodels,
               skimr, GGally, ggstatsplot, Hmisc, jtools, huxtable, interactions,
               usemodels, ranger, doParallel, vip,
               DT, plotly,
               ggthemes, scales, ggthemr, ggfortify, ggstance, ggalt,
               broom, modelr,
               shiny, shinydashboard,
               finetune, xgboost
               )

# Read Dataset ----
input_data <- read_csv("input_dataset.csv")

input_data %>% group_by(Country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

skim(input_data)
view(input_data)

input_data_with_row_id <- 
  input_data %>% 
  as_tibble() %>%
  rowid_to_column()

skim(input_data_with_row_id)

# Convert 2021 to numeric and create a new column called delta
city_rank_delta <- input_data_with_row_id %>% 
  select(City, `2021`, `2022`) %>% 
  filter(`2021` != "-") %>% 
  mutate(`2021` = as.numeric(`2021`)) %>%
  mutate(delta = `2021` - `2022`) %>%
  arrange(desc(delta))

city_rank_delta

# Actual work

# Create convert to % function
# Diverging bar plot of city rank delta
city_rank_delta_plot <- 
  ggplot(data = city_rank_delta,
       aes(x = reorder(`City`, delta), y = delta)) +
  geom_bar(stat = "identity") + 
  geom_bar(data = subset(city_rank_delta, delta > 0),
           aes(`City`, delta),
           fill = "chartreuse3",
           stat = "identity") +
  geom_bar(data = subset(city_rank_delta, delta < 0),
           aes(`City`, delta),
           fill = "tomato",
           stat = "identity") +
  scale_y_continuous(breaks = seq(-58, 10, by = 2)) +
  labs(title = "Changes in Work/Life Balance Rank from 2021 to 2022",
       subtitle = "This shows the delta change of a city's ranking") +
  geom_text(aes(label = City),
            vjust = 0.5,
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_blank())

city_rank_delta_plot

# Create function to convert % numbers into decimal
convert_to_percentage <- function(column_name) {
  as.numeric(sub("%", "", column_name)) / 100
}

# Notes from 7/12:
# City Country Rank - can be dropped - interdependent
# Missing variables - can you impute? e.g. if city is missing
# create 2 recipes : 1 with variable, 1 without
# if you can't impute - might want to use 97 for both
# dashboard sliders: let decision makers what to look at - which features are important
# set dashboard defaults for non-important features - select or slider input
# decision aids
# importance - deployed based on feature importance - high correlation with magnitude abscorrelation

# Replace '-' cells in Vacation days with NA
input_data_with_row_id$`Vacations Taken (Days)` <- 
  input_data_with_row_id$`Vacations Taken (Days)` %>% na_if("-")

skim(input_data_with_row_id)

# Convert Vacation days from chr to dbl
after_vacations <- input_data_with_row_id %>% 
  mutate(`Vacations Taken (Days)`= as.double(`Vacations Taken (Days)`))

skim(after_vacations)

# Convert all other percentage characters to numeric values
cleaned_data <- after_vacations %>% 
  mutate(across(c(`Inflation`, 
                  `Overworked Population`,
                  `Remote Jobs`,
                  `Multiple Jobholders`
                  ), 
                  convert_to_percentage)
         )  # %>% try without getting rid of the NAs
 # na.omit()

#THE DATA IS NOW CLEEEEEAAAANNNN
skim(cleaned_data)

# cleaned_data <- cleaned_data %>% select(-City, -`2021`, -Country)

# THE REAL INTELLIGENCE STARTS NOW ----

# RECIPE ----

## Recipe for TOTAL SCORE----
recipe_score <- 
  recipe(formula = `TOTAL SCORE`~ .,
         data = cleaned_data) %>% 
  step_rm(rowid, City, `2021`, Country, `2022`) %>% 
  step_impute_knn(`Vacations Taken (Days)`) %>%
  step_normalize(all_numeric_predictors()) # setting Ms at 0; SDs at 1

## Recipe for 2022 rank----
recipe_rank <- 
  recipe(formula = `2022`~ .,
         data = cleaned_data) %>% 
  step_rm(rowid, City, `2021`, Country, `TOTAL SCORE`) %>% 
  step_impute_knn(`Vacations Taken (Days)`) %>%
  step_normalize(all_numeric_predictors()) # setting Ms at 0; SDs at 1

# recipe_score %>% tidy()

# BAKING ----

## Baking for total score ----
baked_score <- 
  recipe_score %>% # plan 
  prep() %>% # for calculation
  bake(cleaned_data) 

## Baking for rank ----
baked_rank <-
  recipe_rank %>%
  prep() %>%
  bake(cleaned_data)

baked_rank

# CORRELATION ----

## Correlation for score ----

baked_score %>% 
  as.matrix(.) %>%
  rcorr(.) %>%
  tidy(.) %>%
  rename(var1 = column1,
         var2 = column2,
         CORR = estimate) %>%
  mutate(absCORR = abs(CORR)) %>%
  filter(var1 == "TOTAL SCORE" | var2 == "TOTAL SCORE") %>%
  DT::datatable()  

## Correlation for rank ----
baked_rank %>% 
  as.matrix(.) %>%
  rcorr(.) %>%
  tidy(.) %>%
  rename(var1 = column1,
         var2 = column2,
         CORR = estimate) %>%
  mutate(absCORR = abs(CORR)) %>%
  filter(var1 == "2022" | var2 == "2022") %>%
  DT::datatable()  


# test code: check how a variable varies with the output
# then decide on a regression model output

cleaned_data %>% 
  ggplot(aes(x = `Inclusivity & Tolerance`, y = `TOTAL SCORE`)) +
  geom_point(color = "dodgerblue",
             alpha = 0.3) +
  geom_smooth(method = "loess",
              formula = y ~ x,
              se = F,
              color = "purple") +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = F,
              color = "green") +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, degree = 2),
              color = "tomato3") +
  theme_bw()

# SPLITTING ----

set.seed(22062801)

data_split <- 
  cleaned_data %>% 
  initial_split(prop = 0.80)

data_split

## CREATE TRAIN AND TEST SETS

data_train <- # training(rent_split)
  data_split %>% 
  training() # 80%

data_test <- 
  data_split %>% 
  testing() # 20%


# CREATE MODELS ----
## Random Forest ----

rf_model <- 
  rand_forest() %>%
  set_args(trees = 1000, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger",
             importance = "permutation") %>% 
  set_mode("regression")


## XG Boost ----

XG_BOOST <- # extreme gradient boosting
  boost_tree(trees = 500L,
             mtry = tune(),
             min_n = tune(),
             tree_depth = tune(),
             sample_size = tune(),
             learn_rate = tune()
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

## Linear Regression
ols_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# CREATE WORKFLOWS ----

## Random Forest with score----
workflow_rf_score <-
  workflow() %>% 
  add_recipe(recipe_score) %>% 
  add_model(rf_model)

## Random Forest with rank ----
workflow_rf_rank <-
  workflow() %>% 
  add_recipe(recipe_rank) %>% 
  add_model(rf_model)

## XG Boost with score ----
workflow_xg_score <- 
  workflow() %>% 
  add_recipe(recipe_score) %>% 
  add_model(XG_BOOST)

## XG Boost with rank ----
workflow_xg_rank <- 
  workflow() %>% 
  add_recipe(recipe_rank) %>% 
  add_model(XG_BOOST)

## Linear regression with score ----
workflow_ols_score <- 
  workflow() %>% 
  add_recipe(recipe_score) %>% 
  add_model(ols_model)

## Linear regression with rank ----
workflow_ols_rank <- 
  workflow() %>% 
  add_recipe(recipe_rank) %>% 
  add_model(ols_model)

# CROSS VALIDATION ----

## cross validation for Random Forest ----
set.seed(22062802)

cv_rf <- 
  data_train %>% 
  vfold_cv(v = 10)

cv_rf

## cross validation for XG BOOST ----

### score ----
set.seed(22201701)

cv_xg_score <- 
  data_train %>% 
  vfold_cv(v = 10,
           strata = `TOTAL SCORE`) # put output variable name here

### rank ----
set.seed(22201702)

cv_xg_rank <- 
  data_train %>% 
  vfold_cv(v = 10,
           strata = `2022`)

doParallel::registerDoParallel()

## cross validation for Random Forest ----
set.seed(22062802)

cv_ols <- 
  data_train %>% 
  vfold_cv(v = 10)

cv_ols

# TUNING ----
## Random Forest score tuning ----
### Score ----
tuned_rf_score <-
  workflow_rf_score %>% 
  tune_grid(resamples = cv_rf,
            grid = 3:10)

### Rank ----
tuned_rf_rank <-
  workflow_xg_rank %>%
  tune_grid(resamples = cv_rf,
            grid = 3:10)

## XG Boost tuning ----
# Moved package install to top of script
# install.packages("finetune")
# library(finetune)

# set.seed(22201703)
# Moved package install to top of script
# install.packages("xgboost") # Extreme Gradient Boosting
# library(xgboost)

set.seed(22201702)

grid_xg <-
  grid_max_entropy(
    mtry(c(5L, 10L),
    ),
    min_n(c(10L, 40L)
    ),
    tree_depth(c(5L, 10L)
    ),
    sample_prop(c(0.5, 1.0)
    ),
    learn_rate(c(-2, -1)
    ),
    size = 20
  )

### Score ----
tuned_xg_score <- 
  workflow_xg_score %>% 
  tune_grid(resamples = cv_xg_score,
            grid = grid_xg,
            control = control_grid(save_pred = T)
  )

### Rank ----
tuned_xg_rank <- 
  workflow_xg_rank %>% 
  tune_grid(resamples = cv_xg_rank,
            grid = grid_xg,
            control = control_grid(save_pred = T)
  )

## OLS Tuning ----
### Score ----
tuned_ols_score <-
  workflow_ols_score %>%
  tune_grid(resamples = cv_ols,
            grid = 3:10)

### Rank ----
tuned_ols_rank <-
  workflow_ols_rank %>%
  tune_grid(resamples = cv_ols,
            grid = 3:10)

# tuned_rf_rank <-
#   workflow_xg_rank %>%
#   tune_grid(resamples = cv_rf,
#             grid = 3:10)

# PARAMETERS AFTER TUNING ----

## Random Forest parameters ----

### Score ----
parameters_tuned_rf_score <- 
  tuned_rf_score %>% 
  select_best(metric = "rmse")

### Rank ----
parameters_tuned_rf_rank <- 
  tuned_rf_rank %>% 
  select_best(metric = "rmse")

## XG Boost parameters ----

### Score ----
parameters_tuned_xg_score <- 
  tuned_xg_score %>% 
  select_best(metric = "rmse")

### Rank ----
parameters_tuned_xg_rank <- 
  tuned_xg_rank %>% 
  select_best(metric = "rmse")

## Linear regression parameters ----

### Score ----
parameters_tuned_ols_score <- 
  tuned_ols_score %>% 
  select_best(metric = "rmse")

### Rank ----
parameters_tuned_ols_rank <- 
  tuned_ols_rank %>% 
  select_best(metric = "rmse")

# FINALIZE WORKFLOW ----

## Random Forest ----

### Score ----
finalized_workflow_rf_score <-
  workflow_rf_score %>% 
  finalize_workflow(parameters_tuned_rf_score)

### Rank ----
finalized_workflow_rf_rank <-
  workflow_rf_rank %>% 
  finalize_workflow(parameters_tuned_rf_rank)

## XG Boost ----

### Score ----
finalized_workflow_xg_score <- 
  workflow_xg_score %>% 
  finalize_workflow(parameters_tuned_xg_score)

### Rank ----
finalized_workflow_xg_rank <- 
  workflow_xg_rank %>% 
  finalize_workflow(parameters_tuned_xg_rank)

## Linear regression ----

### Score ----
finalized_workflow_ols_score <-
  workflow_ols_score %>% 
  finalize_workflow(parameters_tuned_ols_score)

### Rank ----
finalized_workflow_ols_rank <-
  workflow_ols_rank %>% 
  finalize_workflow(parameters_tuned_ols_rank)

# LAST FIT ----

## Random Forest ----

### Score ----
fit_rf_score <-
  finalized_workflow_rf_score %>% 
  last_fit(data_split)

### Rank ----
fit_rf_rank <-
  finalized_workflow_rf_rank %>% 
  last_fit(data_split)

## XG Boost ----

### Score ----
fit_xg_score <-
  finalized_workflow_xg_score %>% 
  last_fit(data_split)

### Rank ----
fit_xg_rank <-
  finalized_workflow_xg_rank %>% 
  last_fit(data_split)


## Linear regression ----

### Score ----
fit_ols_score <-
  finalized_workflow_ols_score %>% 
  last_fit(data_split)

### Score ----
fit_ols_rank <-
  finalized_workflow_ols_rank %>% 
  last_fit(data_split)

# PERFORMANCE ----

## Random Forest ----

### Score ----
performance_rf_score <- 
  fit_rf_score %>% 
  collect_metrics() %>% 
  mutate(algorithm = "Random Forest for score")

### Rank ----
performance_rf_rank <- 
  fit_rf_rank %>% 
  collect_metrics() %>% 
  mutate(algorithm = "Random Forest for rank")

## XG Boost ----

### Score ----
performance_xg_score <- 
  fit_xg_score %>% # for_performance(fit_xg) %>% 
  collect_metrics() %>%
  mutate(algorithm = "XG Boost for score")

### Rank ----
performance_xg_rank <- 
  fit_xg_rank %>% # for_performance(fit_xg) %>% 
  collect_metrics() %>%
  mutate(algorithm = "XG Boost for rank")

## Linear regression ----

### Score ----
performance_ols_score <- 
  fit_ols_score %>% 
  collect_metrics() %>% 
  mutate(algorithm = "Linear regression for score")

### Rank ----
performance_ols_rank <- 
  fit_ols_rank %>% 
  collect_metrics() %>% 
  mutate(algorithm = "Linear regression for rank")

# COMPARE PERFORMANCE OF DIFFERENT ALGORITHMS AND RECIPES ----

bind_rows(performance_rf_score,
          performance_rf_rank,
          performance_xg_score,
          performance_xg_rank,
          performance_ols_score,
          performance_ols_rank) %>% 
  select(-.estimator,
         -.config) %>% 
  pivot_wider(names_from = .metric,
              values_from = .estimate) %>% 
  datatable() %>% 
  formatRound(columns = c("rmse",
                          "rsq"),
              digits = 2)

# CHECK PREDICTIONS ----
## Random Forest ----
### Score ----
prediction_rf_score <- 
  fit_rf_score %>% 
  collect_predictions() %>%
  mutate(algorithm = "Random Forest for score")
### Rank ----
prediction_rf_rank <- 
  fit_rf_rank %>% 
  collect_predictions() %>%
  mutate(algorithm = "Random Forest for rank")

## XG Boost ----
### Score ----
prediction_xg_score <-
  fit_xg_score %>%
  collect_predictions() %>%
  mutate(algorithm = "XG Boost for score") 

### Rank ----
prediction_xg_rank <-
  fit_xg_rank %>%
  collect_predictions() %>%
  mutate(algorithm = "XG Boost for rank")

## Linear regression ----
### Score ----
prediction_ols_score <- 
  fit_ols_score %>% 
  collect_predictions() %>%
  mutate(algorithm = "Linear regression for score")

### Rank ----
prediction_ols_rank <- 
  fit_ols_rank %>% 
  collect_predictions() %>%
  mutate(algorithm = "Linear regression for rank")

# CONSOLIDATE PREDICTIONS ----

cleaned_data <- 
  cleaned_data %>% 
  tibble::rowid_to_column(".row")

a <- data_test %>%
  rename(.row = rowid)

data_with_predictions <-
  a %>% 
  inner_join(prediction_rf_score,
             prediction_rf_rank,
             prediction_xg_score,
             prediction_xg_rank,
             prediction_ols_score,
             prediction_ols_rank,
             by = ".row")

# TODO: Create a data frame comparing the outputs of all models with the original output.

# CREATE PLOTS ----
rf_score_plot <- 
  prediction_rf_score %>% 
  select(.row, `TOTAL SCORE`, .pred)%>%
  ggplot(aes(x = `TOTAL SCORE`,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted score of city",
       x = "Actual score of city",
       title = "Predicting city work-life balance score using RF") + 
  theme_bw()

rf_rank_plot <- 
  prediction_rf_rank %>% 
  select(.row, `2022`, .pred) %>%
  ggplot(aes(x = `2022`,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted rank of city",
       x = "Actual rank of city",
       title = "Predicting city work-life balance rank using RF") + 
  theme_bw()

xg_score_plot <- 
  prediction_xg_score %>% 
  select(.row, `TOTAL SCORE`, .pred) %>%
  ggplot(aes(x = `TOTAL SCORE`,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted score of city",
       x = "Actual score of city",
       title = "Predicting city work-life balance score using XG") + 
  theme_bw()

xg_rank_plot <- 
  prediction_rf_rank %>% 
  select(.row, `2022`, .pred) %>%
  ggplot(aes(x = `2022`,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted rank of city",
       x = "Actual rank of city",
       title = "Predicting city work-life balance rank using XG") + 
  theme_bw()

grid.arrange(rf_score_plot, rf_rank_plot, 
             xg_score_plot, xg_rank_plot,
             ncol = 2)

ols_score_plot <- 
  prediction_ols_score %>% 
  select(.row, `TOTAL SCORE`, .pred) %>%
  ggplot(aes(x = `TOTAL SCORE`,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted score of city",
       x = "Actual score of city",
       title = "Predicting city work-life balance score using OLS") + 
  theme_bw()


ols_rank_plot <- 
  prediction_ols_rank %>% 
  select(.row, `2022`, .pred) %>%
  ggplot(aes(x = `2022`,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted rank of city",
       x = "Actual rank of city",
       title = "Predicting city work-life balance rank using OLS") + 
  theme_bw()

grid.arrange(rf_score_plot, rf_rank_plot, 
             xg_score_plot, xg_rank_plot,
             ols_score_plot, ols_rank_plot,
             ncol = 2)

# Results??

