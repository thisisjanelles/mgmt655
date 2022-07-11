# Packages ----
pacman::p_load(tidyverse, lubridate,
               tidymodels,
               skimr, GGally, ggstatsplot, Hmisc, jtools, huxtable, interactions,
               usemodels, ranger, doParallel, vip,
               DT, plotly,
               ggthemes, scales, ggthemr, ggfortify, ggstance, ggalt,
               broom, modelr,
               shiny, shinydashboard
               )
# Download dataset
# https://www.kaggle.com/datasets/prasertk/cities-with-the-best-worklife-balance-2022

# Read dataset ----
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

view(input_data_with_row_id)

# Convert 2021 to numeric and create a new column called delta
city_rank_delta <- input_data_with_row_id %>% 
  select(City, `2021`, `2022`) %>% 
  filter(`2021` != "-") %>% 
  mutate(`2021` = as.numeric(`2021`)) %>%
  mutate(delta = `2021` - `2022`) %>%
  arrange(desc(delta))

# Actual work

# Create convert to % function
# Diverging bar plot of city rank delta
city_rank_delta_plot <- 
  ggplot(data = city_rank_delta,
       aes(x = reorder(City, delta), y = delta)) +
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

# Create function to convert % numbers into decimal
convert_to_percentage <- function(column_name) {
  as.numeric(sub("%", "", column_name)) / 100
}

#clean up the data

# Replace '-' in Vacation days with NA
input_data_with_row_id$`Vacations Taken (Days)` <- 
  input_data_with_row_id$`Vacations Taken (Days)` %>% na_if("-")
  
# convert vacation days from character to double
after_vacations <- input_data_with_row_id %>% 
  mutate(`Vacations Taken (Days)`= as.double(`Vacations Taken (Days)`))

# convert all other percentage characters to numeric values
cleaned_data <- after_vacations %>% 
  mutate(across(c(`Inflation`, 
                  `Overworked Population`,
                  `Remote Jobs`,
                  `Multiple Jobholders`
                  ), 
                  convert_to_percentage)) %>%
  na.omit()

#THE DATA IS NOW CLEEEEEAAAANNNN
skim(cleaned_data)

cleaned_data <- cleaned_data %>% select(-City, -`2021`, -Country)

# THE REAL SHIZZ STARTS NOW ----
## Recipe ----
recipe_score <- 
  recipe(formula = `TOTAL SCORE`~ .,
         data = cleaned_data) %>% 
  step_rm(rowid) %>% 
  step_normalize(all_numeric_predictors()) # setting Ms at 0; SDs at 1

## Baking ----
baked_score <- 
  recipe_score %>% # plan 
  prep() %>% # for calculation
  bake(cleaned_data) 

#correlation
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

# Random forest

set.seed(22062801)

data_split <- 
  cleaned_data %>% 
  initial_split(prop = 0.80)

data_split

## Executing

data_train <- # training(rent_split)
  data_split %>% 
  training() # 80%

data_test <- 
  data_split %>% 
  testing() # 20%

## Random Forest ----

rf_model <- 
  rand_forest() %>%
  set_args(trees = 1000, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger",
             importance = "permutation") %>% 
  set_mode("regression")

workflow_rf <-
  workflow() %>% 
  add_recipe(recipe_score) %>% 
  add_model(rf_model)


set.seed(22062802)

CV <- 
  data_train %>% 
  vfold_cv(v = 10)

CV

doParallel::registerDoParallel()

tuned_RF <-
  workflow_rf %>% 
  tune_grid(resamples = CV,
            grid = 3:10)

parameters_tuned_RF <- 
  tuned_RF %>% 
  select_best(metric = "rmse")

## finalize_workflow()

finalized_workflow_RF <-
  workflow_rf %>% 
  finalize_workflow(parameters_tuned_RF)

fit_rf <-
  finalized_workflow_RF %>% 
  last_fit(data_split)

performance_rf <- 
  fit_rf %>% 
  collect_metrics() %>% 
  mutate(algo = "Random Forest")

performance_rf

prediction_rf <- 
  fit_rf %>% 
  collect_predictions()

prediction_rf

cleaned_data <- 
  cleaned_data %>% 
  tibble::rowid_to_column("ID")

data_with_predictions <-
  cleaned_data %>% 
  inner_join(prediction_rf,
             by = ID)

for_your_plotly <- 
  prediction_rf %>% 
  select(.row, `TOTAL SCORE`, .pred)

plotly_object <- 
  for_your_plotly %>% 
  ggplot(aes(x = `TOTAL SCORE`,
             y =.pred)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.50) + 
  geom_abline(color = "red",
              lty = 2) +
  labs(y = "Predicted score of city",
       x = "Actual score of city",
       title = "Predicting city work-life balance score") + 
  theme_bw()

ggplotly(plotly_object)

# Results??