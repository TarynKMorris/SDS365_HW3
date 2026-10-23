library(tidyverse)
library(skimr)
library(slider)
library(MatchIt)
library(corrplot)

## Process Data

pm25 <- read_csv("data/pm25_data.csv",
                 col_types = cols(date = col_date())
  )

skim(pm25)
# pm25 (and thus above) 90% complete
# death, temp 100% complete
# Death and temp appear to have fairly normal distributions
# PM25 distribution heavily skewed right

pm_long <- pm25 |> 
  mutate(group = ifelse(is.na(above), "Above is NA", "Above is Complete")) |> 
  mutate(date = as.numeric(date)) |> 
  pivot_longer(
    cols = c(date, death, temp), 
    names_to = "Var_Name", 
    values_to = "Value"
  )

pm_long|> 
  ggplot(aes(x = Value, fill = group, color = group)) +
  geom_histogram(
    position = "identity", 
    alpha = 0.4, 
    bins = 30
  ) +
  facet_wrap(~ Var_Name, scales = "free", ncol = 1) + 
  labs(
    title = "Differences between present and missing data",
    x = "Value",
    y = "Frequency",
    fill = "Group",
    color = "Group"
  ) +
  theme_minimal()

# Missing data appears to simply be older, and the data have similar 
# distributions for death + temp compared to the complete data

# mortaility is raw, but population grows.
# To account for this, alter propensity by year/5year
pm25_add <- pm25 %>%
  filter(!is.na(pm25)) |> # NAs of no use for measuring effect
  arrange(date) %>% 
  mutate(
    is_above = ifelse(!is.na(above) & above == "above", 1, 0),
    count_above_in_prev_30d = slide_index_sum(
      x = is_above,
      i = date,
      before = days(30),
      after = days(-1)
    ),
    year = as.factor(year(date)) 
  )

## Propensity Score Matching

#No matching
null_match <- matchit(is_above ~ death + pm25 + temp + 
                    year + count_above_in_prev_30d,
                  data = pm25_add,
                  method = NULL,
                  distance = "glm")
# imbalance in mean clear for many covariates, ps matching needed

# uneven classes - cannot 1:1 nearest neighbor match

ps_match <- matchit(is_above ~ death + pm25 + temp + 
                      year + count_above_in_prev_30d,
                    data = pm25_add,
                    method = 'nearest', # nearest neighbor matching
                    distance = "glm") # logistic reg