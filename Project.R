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

ggplot(data = city_rank_delta,
       aes(y = City, x = delta)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = "Test", 
                    labels = c("Above Average", "Below Average")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "City Delta") +
  theme_fivethirtyeight()
  #geom_bar(orientation = "y", stat = "identity") +

# ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
#   geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
#   scale_fill_manual(name="Mileage", 
#                     labels = c("Above Average", "Below Average"), 
#                     values = c("above"="#00ba38", "below"="#f8766d")) + 
#   labs(subtitle="Normalised mileage from 'mtcars'", 
#        title= "Diverging Bars") + 
#   coord_flip()
# Make this red and green

# Set dir path

# Load dataset

# Actual work

# Results??