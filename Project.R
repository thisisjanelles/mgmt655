# Install Packages ----
pacman::p_load(tidyverse, lubridate,
               tidymodels,
               skimr, GGally, ggstatsplot, Hmisc, jtools, huxtable, interactions,
               usemodels, ranger, doParallel, vip,
               DT, plotly,
               ggthemes, scales, ggthemr, ggfortify, ggstance, ggalt,
               broom, modelr,
               shiny, shinydashboard
               )
# Source data - https://www.kaggle.com/datasets/prasertk/cities-with-the-best-worklife-balance-2022

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

a <- cleaned_data %>% select(-City, -`2021`, -Country)

# THE REAL SHIZZ STARTS NOW ----
## Recipe ----
recipe_score <- 
  recipe(formula = `TOTAL SCORE`~ .,
         data = a) %>% 
  step_rm(rowid) %>% 
  step_normalize(all_numeric_predictors()) # setting Ms at 0; SDs at 1

## Baking ----
baked_score <- 
  recipe_score %>% # plan 
  prep() %>% # for calculation
  bake(new_data = a) 

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

a %>% 
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

# Results??