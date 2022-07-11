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

input_data_with_row_id <- 
  input_data %>% 
  as_tibble() %>%
  rowid_to_column()

# New object showing change in city rank
city_rank_delta <- input_data_with_row_id %>% 
  select(City, `2021`, `2022`) %>% 
  # Filter missing cells
  filter(`2021` != "-") %>% 
  # Convert 2021 column to numeric
  mutate(`2021` = as.numeric(`2021`)) %>%
  # Calculate change in rank from 2021 to 2022
  mutate(delta = `2021` - `2022`) %>%
  # Arrange in descending order
  arrange(desc(delta))

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

#try something
input_data_with_row_id$`Vacations Taken (Days)` <- 
  input_data_with_row_id$`Vacations Taken (Days)` %>% na_if("-")
  
after_vacations <- input_data_with_row_id %>% 
  mutate(`Vacations Taken (Days)`= as.double(`Vacations Taken (Days)`))

after_percentages <- after_vacations %>% 
  mutate(across(c(`Inflation`, 
                  `Overworked Population`,
                  `Remote Jobs`,
                  `Multiple Jobholders`
                  ), 
                  convert_to_percentage))

skim(after_percentages)


# Results??