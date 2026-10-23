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

pm25_add <- pm25 %>%
  filter(!is.na(pm25)) |> # NAs of no use for measuring effect
  arrange(date) %>% 
  mutate(
    is_above = ifelse(!is.na(above) & above == "above", 1, 0),
    count_above_in_prev_14d = slide_index_sum(
      x = is_above,
      i = date,
      before = days(14),
      after = days(-1)
    ),
    count_above_in_prev_30d = slide_index_sum(
      x = is_above,
      i = date,
      before = days(30),
      after = days(-1)
    ),
    count_above_in_prev_60d = slide_index_sum(
      x = is_above,
      i = date,
      before = days(60),
      after = days(-1)
    ),
    # mortaility is raw, but population grows.
    # To account for this, alter propensity by year/5year
    year = as.factor(year(date)),
    month = as.factor(month(date)),
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"),
    is_weekend = if_else(wday(date, week_start = 1) %in% c(6, 7),
                       T, F)
    )

M <- pm25_add |> 
  select(death, temp, pm25, is_above,count_above_in_prev_14d,
                        count_above_in_prev_30d, count_above_in_prev_60d) |>
  cor()
corrplot::corrplot(M)

## Propensity Score Matching

#No matching
null_match <- matchit(is_above ~ death + temp  + month + year +
                        count_above_in_prev_30d + season + is_weekend,,
                  data = pm25_add,
                  method = NULL,
                  distance = "glm")

library(cobalt)
love.plot(null_match, drop.distance = TRUE)
# imbalance in mean for classes (esp. temp, prev, death)
# clear for many covariates, ps matching needed 
# year maybe not needed


# uneven classes - cannot 1:1 nearest neighbor match

ps_match <- matchit(is_above ~ death + temp + year + season +
                     count_above_in_prev_30d  + is_weekend,
                    data = pm25_add,
                    method = 'nearest', # nearest neighbor matching
                    distance = "glm") # logistic reg
# All treated found their match



love.plot(ps_match, drop.distance = TRUE)
# much better! Look at temp - we know it was hightly correlated with death,
# significantly with pm25, minorly with is_above

matched <- match.data(ps_match)

t.test(death ~ is_above, data = matched)