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

test_var <- 
  input_data %>% 
  as_tibble() %>%
  rowid_to_column()

row.names(test_var) <- test_var$rowid

view(test_var)

# Convert 2021 to numeric
test_var %>% 
  select(., City, `2021`, `2022`) %>% 
  filter(., `2021` != "-") %>% 
  mutate(`2021` = as.numeric(`2021`))

# Set dir path

# Load dataset

# Actual work

# Results??