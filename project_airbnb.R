# SETUP ----
## Packages ----
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

## Read Dataset & Clean Data ----
input_data <- read_csv("AB_NYC_2019.csv")

skim(input_data)

cleaned_data <- input_data %>% 
  filter(price != 0) %>%
  select(-name, -host_name, -host_id, -last_review) %>%
  mutate(across(c(neighbourhood_group,
                  room_type,
                  neighbourhood), as.factor)) %>%
  mutate(log10_price = log10(price)) %>%
  select(-price) %>%
  drop_na()

skim(cleaned_data)

# VARIABLE ANALYSIS ----

## Neighbourhood Group ----
ggthemr("fresh")

cleaned_data %>%
  ggplot(aes(neighbourhood_group)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  geom_label(aes(label = percent((..count..)/sum(..count..)),
                 y = (..count..)/sum(..count..)), 
             stat = "count",
             size = 5,
             fill = "white") +
  labs(title = "Percentage of Airbnb Listings by Neighborhood Group",
       x = "Neighborhood Group",
       y = "Percentage")

## Average Price VS Neighbourhood Group ----
cleaned_data %>%
  group_by(neighbourhood_group) %>%
  summarise(avg_price = mean(log10_price)) %>%
  mutate(avg_price = 10^avg_price) %>%
  ggplot(aes(x = neighbourhood_group, y = avg_price)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_y_continuous(labels = dollar) +
  geom_label(aes(label = round(avg_price, digits = 2),
                 y = avg_price, 
                 stat = "count",
                 size = 5,
                 fill = "white")) +
  labs(title = "Average price of Airbnb Listings by Neighborhood Group",
       x = "Neighborhood Group",
       y = "Average Price")
theme_bw

## Average Price VS Room Type ----
cleaned_data %>%
  group_by(room_type) %>%
  summarise(avg_price = mean(log10_price)) %>%
  mutate(avg_price = 10^avg_price) %>%
  ggplot(aes(x = room_type, y = avg_price)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_y_continuous(labels = dollar) +
  geom_label(aes(label = round(avg_price, digits = 2),
                 y = avg_price, 
                 stat = "count")) +
  labs(title = "Average price of Airbnb Listings by Room type",
       x = "Room Type",
       y = "Average Price")
theme_bw()

## Longitude VS log10Price ----
cleaned_data %>%
  ggplot(aes(x = longitude, y = log10_price)) + 
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

## Latitude vs log10Price ----
cleaned_data %>%
  ggplot(aes(x = latitude, y = log10_price)) + 
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

## Number of Reviews VS log10 Price ----
cleaned_data %>%
  ggplot(aes(x = number_of_reviews, y = log10_price)) + 
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

## Host Listings Count VS log10Price ----
cleaned_data %>%
  ggplot(aes(x = calculated_host_listings_count, y = log10_price)) + 
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

## Availability_365 VS log10Price
cleaned_data %>%
  ggplot(aes(x = availability_365, y = log10_price)) + 
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

# RECIPE FOR EDA ----

## Recipe for log10_price ----
recipe_eda <- 
  recipe(formula = log10_price ~ .,
         data = cleaned_data) %>% 
  step_rm(id) %>%
  step_other(neighbourhood, threshold = 0.05) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

## Baking for log10_price ----
baked_eda <- 
  recipe_eda %>% # plan 
  prep() %>% # for calculation
  bake(cleaned_data)

glimpse(baked_eda)

# CORRELATION ----

## Correlation score ----

corr_table <- baked_eda %>% 
  as.matrix(.) %>%
  rcorr(.) %>%
  tidy(.) %>%
  rename(var1 = column1,
         var2 = column2,
         CORR = estimate) %>%
  mutate(absCORR = abs(CORR)) %>%
  filter(var1 == "log10_price" | var2 == "log10_price") %>%
  DT::datatable()

corr_table

# SPLITTING ----

set.seed(22062801)

data_split <- 
  cleaned_data %>% 
  initial_split(prop = 0.80)

data_split

## Create Training and Testing Sets ----

data_train <- # training(rent_split)
  data_split %>% 
  training() # 80%

# TESTING ----

data_train <- data_train %>%
  sample_n(5000)

data_test <- 
  data_split %>% 
  testing() # 20%

# RECIPE FOR PREDICTION ----
## Recipe 1: Price with BoxCox inputs ----
recipe_price_box <- 
  recipe(formula = `log10_price`~ .,
         data = data_train) %>%
  step_rm(id) %>%
  step_other(neighbourhood, threshold = 0.05) %>%
  step_BoxCox(calculated_host_listings_count) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

## Recipe 2: Price without BoxCox inputs ----
recipe_price <- 
  recipe(formula = `log10_price`~ 
           neighbourhood_group + neighbourhood + latitude + longitude +
           room_type + minimum_nights + number_of_reviews + 
           reviews_per_month + availability_365,
         data = data_train) %>% 
  step_other(neighbourhood, threshold = 0.05) %>%
  step_novel(neighbourhood_group, neighbourhood, room_type) %>% 
  step_unknown(neighbourhood_group, neighbourhood, room_type) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

# BAKING ----
## Baking for price with BoxCox ----
baked_price_box <- 
  recipe_price_box %>% # plan 
  prep() %>% # for calculation
  bake(data_train)

glimpse(baked_price_box)

## Baking for price without BoxCox ----
baked_price <- 
  recipe_price %>% # plan 
  prep() %>% # for calculation
  bake(data_train) 

glimpse(baked_price)

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

## Linear Regression ----
ols_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# CREATE WORKFLOWS ----

## Random Forest for price with BoxCox ----
workflow_rf_price_box <-
  workflow() %>% 
  add_recipe(recipe_price_box) %>% 
  add_model(rf_model)

## Random Forest with price ----
workflow_rf_price <-
  workflow() %>% 
  add_recipe(recipe_price) %>% 
  add_model(rf_model)

## XG Boost for price with BoxCox ----
workflow_xg_price_box <- 
  workflow() %>% 
  add_recipe(recipe_price_box) %>% 
  add_model(XG_BOOST)

## XG Boost with price ----
workflow_xg_price <- 
  workflow() %>% 
  add_recipe(recipe_price) %>% 
  add_model(XG_BOOST)

## Linear regression for price with BoxCox ----
workflow_ols_price_box <- 
  workflow() %>% 
  add_recipe(recipe_price_box) %>% 
  add_model(ols_model)

## Linear regression with price ----
workflow_ols_price <- 
  workflow() %>% 
  add_recipe(recipe_price) %>% 
  add_model(ols_model)

# CROSS VALIDATION ----

## Cross validation for Random Forest ----
set.seed(22062802)

cv_rf <- 
  data_train %>% 
  vfold_cv(v = 10)

cv_rf

## Cross validation for XG BOOST ----

### Score ----
set.seed(22201701)

cv_xg <- 
  data_train %>% 
  vfold_cv(v = 10,
           strata = log10_price) # output variable

cv_xg

## Cross validation for Linear regression ----
set.seed(22062802)

cv_ols <- 
  data_train %>% 
  vfold_cv(v = 10)

cv_ols

doParallel::registerDoParallel()

# TUNING ----
## Random Forest score tuning ----
### Price with BoxCox ----
tuned_rf_price_box <-
  workflow_rf_price_box %>% 
  tune_grid(resamples = cv_rf,
            grid = 3:10)

### Price ----
tuned_rf_price <-
  workflow_rf_price %>% 
  tune_grid(resamples = cv_rf,
            grid = 3:10)

## XG Boost tuning ----

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

### Price with BoxCox ----
tuned_xg_price_box <- 
  workflow_xg_price_box %>% 
  tune_grid(resamples = cv_xg,
            grid = grid_xg,
            control = control_grid(save_pred = T)
  )

### Price ----
tuned_xg_price <- 
  workflow_xg_price %>% 
  tune_grid(resamples = cv_xg,
            grid = grid_xg,
            control = control_grid(save_pred = T)
  )

## OLS Tuning ----
### Price with BoxCox ----
tuned_ols_price_box <-
  workflow_ols_price_box %>%
  tune_grid(resamples = cv_ols,
            grid = 3:10)

### Price ----
tuned_ols_price <-
  workflow_ols_price %>%
  tune_grid(resamples = cv_ols,
            grid = 3:10)


# PARAMETERS AFTER TUNING ----

## Random Forest parameters ----

### Price with box ----
parameters_tuned_rf_price_box <- 
  tuned_rf_price_box %>% 
  select_best(metric = "rmse")

### Price ----
parameters_tuned_rf_price <- 
  tuned_rf_price %>% 
  select_best(metric = "rmse")

## XG Boost parameters ----

### Price with BoxCox ----
parameters_tuned_xg_price_box <-
  tuned_xg_price_box %>%
  select_best(metric = "rmse")

### Price ----
parameters_tuned_xg_price <-
  tuned_xg_price %>%
  select_best(metric = "rmse")

## Linear regression parameters ----

### Price with box ----
parameters_tuned_ols_price_box <- 
  tuned_ols_price_box %>% 
  select_best(metric = "rmse")

### Price ----
parameters_tuned_ols_price <- 
  tuned_ols_price %>% 
  select_best(metric = "rmse")

# FINALIZE WORKFLOW ----

## Random Forest ----

### Price with box ----
finalized_workflow_rf_price_box <-
  workflow_rf_price_box %>% 
  finalize_workflow(parameters_tuned_rf_price_box)

### Price ----
finalized_workflow_rf_price <-
  workflow_rf_price %>% 
  finalize_workflow(parameters_tuned_rf_price)

## XG Boost ----

### Price with box ----
finalized_workflow_xg_price_box <- 
  workflow_xg_price_box %>% 
  finalize_workflow(parameters_tuned_xg_price_box)

### Price ----
finalized_workflow_xg_price <- 
  workflow_xg_price %>% 
  finalize_workflow(parameters_tuned_xg_price)

## Linear regression ----

### Price with box ----
finalized_workflow_ols_price_box <-
  workflow_ols_price_box %>% 
  finalize_workflow(parameters_tuned_ols_price_box)

### Price ----
finalized_workflow_ols_price <-
  workflow_ols_price %>% 
  finalize_workflow(parameters_tuned_ols_price)

# LAST FIT ----

## Random Forest ----

### Price with box ----
fit_rf_price_box <-
  finalized_workflow_rf_price_box %>% 
  last_fit(data_split)

### Price ----
fit_rf_price <-
  finalized_workflow_rf_price %>% 
  last_fit(data_split)

## XG Boost ----

### Price with box ----
fit_xg_price_box <-
  finalized_workflow_xg_price_box %>% 
  last_fit(data_split)

### Price ----
fit_xg_price <-
  finalized_workflow_xg_price %>% 
  last_fit(data_split)

## Linear regression ----

### Price with box ----
fit_ols_price_box <-
  finalized_workflow_ols_price_box %>% 
  last_fit(data_split)

### Price ----
fit_ols_price <-
  finalized_workflow_ols_price %>% 
  last_fit(data_split)

# PERFORMANCE ----

## Random Forest ----

### Price with BoxCox ----
performance_rf_price_box <- 
  fit_rf_price_box %>% 
  collect_metrics() %>% 
  mutate(algorithm = "Random Forest for price with BoxCox")

### Price ----
performance_rf_price <- 
  fit_rf_price %>% 
  collect_metrics() %>% 
  mutate(algorithm = "Random Forest for price")

## XG Boost ----

### Price with BoxCox ----
performance_xg_price_box <- 
  fit_xg_price_box %>% # for_performance(fit_xg) %>% 
  collect_metrics() %>%
  mutate(algorithm = "XG Boost for price with BoxCox")

### Price ----
performance_xg_price <- 
  fit_xg_price %>% # for_performance(fit_xg) %>% 
  collect_metrics() %>%
  mutate(algorithm = "XG Boost for price")

## Linear regression ----

### Price with BoxCox ----
performance_ols_price_box <- 
  fit_ols_price_box %>% 
  collect_metrics() %>% 
  mutate(algorithm = "Linear regression for price with BoxCox")

### Price ----
performance_ols_price <- 
  fit_ols_price %>% 
  collect_metrics() %>% 
  mutate(algorithm = "Linear regression for price")

# COMPARE PERFORMANCE OF DIFFERENT ALGORITHMS AND RECIPES ----

bind_rows(performance_rf_price_box,
          performance_rf_price,
          performance_xg_price_box,
          performance_xg_price,
          performance_ols_price_box, 
          performance_ols_price) %>%
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
### Price with BoxCox ----
prediction_rf_price_box <- 
  fit_rf_price_box %>% 
  collect_predictions() %>%
  mutate(algorithm = "Random Forest for price with BoxCox")

### Price ----
prediction_rf_price <- 
  fit_rf_price %>% 
  collect_predictions() %>%
  mutate(algorithm = "Random Forest for price")

## XG Boost ----
### Price with BoxCox ----
prediction_xg_price_box <-
  fit_xg_price_box %>%
  collect_predictions() %>%
  mutate(algorithm = "XG Boost for price with BoxCox") 

### Price ----
prediction_xg_price <-
  fit_xg_price %>%
  collect_predictions() %>%
  mutate(algorithm = "XG Boost for price")

## Linear regression ----
### Price with BoxCox ----
prediction_ols_price_box <- 
  fit_ols_price_box %>% 
  collect_predictions() %>%
  mutate(algorithm = "Linear regression for price with BoxCox")

### Price ----
prediction_ols_price <- 
  fit_ols_price %>% 
  collect_predictions() %>%
  mutate(algorithm = "Linear regression for price")

# CONSOLIDATE PREDICTIONS ----

cleaned_data <- 
  cleaned_data %>% 
  tibble::rowid_to_column(".row")

data_test_with_row <- 
  data_test %>%
  rename(.row = id)

# CREATE PLOTS ----
## Random Forest ----

### Price with BoxCox ----
rf_price_box_plot <- 
  prediction_rf_price_box %>% 
  select(.row, log10_price, .pred)%>%
  ggplot(aes(x = log10_price,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted Price of Airbnb",
       x = "Actual price of Airbnb",
       title = "Predicting Airbnb price in NYC using RF with BoxCox") + 
  theme_bw()

### Price ----
rf_price_plot <- 
  prediction_rf_price %>% 
  select(.row, log10_price, .pred) %>%
  ggplot(aes(x = log10_price,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted Price of Airbnb",
       x = "Actual price of Airbnb",
       title = "Predicting Airbnb price in NYC using RF") + 
  theme_bw()

## XG Boost ----

###Price with BoxCox ----

xg_price_box_plot <- 
  prediction_xg_price_box %>% 
  select(.row, log10_price, .pred) %>%
  ggplot(aes(x = log10_price,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted Price of Airbnb",
       x = "Actual price of Airbnb",
       title = "Predicting Airbnb price in NYC using XG with BoxCox") + 
  theme_bw()

### Price ----
xg_price_plot <- 
  prediction_rf_price %>% 
  select(.row, log10_price, .pred) %>%
  ggplot(aes(x = log10_price,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted Price of Airbnb",
       x = "Actual price of Airbnb",
       title = "Predicting Airbnb price in NYC using XG") + 
  theme_bw()

## OLS ----

### Price with BoxCox ----

ols_price_box_plot <- 
  prediction_ols_price_box %>% 
  select(.row, log10_price, .pred) %>%
  ggplot(aes(x = log10_price,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted Price of Airbnb",
       x = "Actual price of Airbnb",
       title = "Predicting Airbnb price in NYC using OLS with BoxCox") + 
  theme_bw()

ols_price_plot <- 
  prediction_ols_price %>% 
  select(.row, log10_price, .pred) %>%
  ggplot(aes(x = log10_price,
             y = .pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted Price of Airbnb",
       x = "Actual price of Airbnb",
       title = "Predicting Airbnb price in NYC using OLS") + 
  theme_bw()

grid.arrange(rf_price_box_plot,
             rf_price_plot,
             xg_price_box_plot,
             xg_price_plot,
             ols_price_box_plot,
             ols_price_plot,
             ncol = 2)

# FINALIZED MODEL ----
## Feature Importance ----

library(vip)

# VERY IMPORTANT ----
finalized_model <- 
  finalized_workflow_xg_price %>% 
  fit(cleaned_data)

feature_importance_PREP <-
  XG_BOOST %>% # model
  finalize_model(select_best(tuned_xg_price)
  ) %>%
  set_engine("xgboost",
             importance = "permutation")

feature_importance <- 
  workflow() %>% 
  add_recipe(recipe_price) %>% 
  add_model(feature_importance_PREP) %>% 
  fit(data_train) %>% 
  extract_fit_parsnip() %>% # pull_workflow_fit()
  vip(aesthetic = list(fill = "deepskyblue2",
                       alpha = 0.50)
  ) 

feature_importance + 
  theme_bw()

# SAVE FINAL MODEL ----
finalized_model %>% saveRDS("finalized_model.rds")

# SAVE RDATA ----
save.image("project_airbnb.RData")
