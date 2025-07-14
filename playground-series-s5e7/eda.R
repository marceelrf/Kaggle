library(tidyverse)
library(tidymodels)

train <- readr::read_csv("playground-series-s5e7/train.csv")

dim(train)

# NA per column
train %>% 
  summarise(across(everything(), \(x) sum(is.na(x)))) %>% 
  View()

# NA per id
na_per_id <- 
  train %>% 
  mutate(na_columns = rowSums(is.na(.))) %>% 
  select(id,na_columns)

## Worst scenarios
na_per_id %>% 
  slice_max(order_by = na_columns)

## Histogram
na_per_id %>% 
  ggplot(aes(x = na_columns)) +
  geom_histogram()

# Distributions
train %>% 
  select(where(is.numeric),-id) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_histogram() +
  facet_wrap(~name)

train %>% 
  select(where(is.character)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x=value, fill = name)) +
  geom_bar()+
  facet_wrap(~name, scale = "free_x")

# Imputations - analyse by imputations
train_imput <-
  train %>% 
  recipe(formula = "Personality ~ .") %>% 
  update_role(id, new_role = "id") %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  prep() %>% 
  bake(NULL)

train_imput %>% 
  select(where(is.numeric),-id) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_histogram() +
  facet_wrap(~name)

train_imput %>% 
  select(where(is.factor)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x=value, fill = name)) +
  geom_bar()+
  facet_wrap(~name, scale = "free_x")

GGally::ggpairs(select(train_imput,-id))

GGally::ggcorr(select(train_imput,-id),
               label = TRUE,
               label_size = 3,
               label_color = "white")

## Numerical variables

train_imput %>% 
  select(-id) %>% 
  summarise(across(where(is.numeric), 
                   list(mean=mean, median=median, min=min, max=max)))

summary(train_imput)

## Relations with Personality

train_imput %>% 
  ggplot(aes(x=Post_frequency, fill = Personality)) +
  geom_histogram(position = "dodge")

train_imput %>% 
  ggplot(aes(x=Social_event_attendance, fill = Personality)) +
  geom_histogram(position = "dodge") +
  facet_wrap(~Stage_fear)

train_imput %>% 
  ggplot(aes(x=Time_spent_Alone , fill = Personality)) +
  geom_histogram(position = "dodge")

train_imput %>% 
  ggplot(aes(x=Post_frequency, y = Time_spent_Alone,
             col = Personality)) +
  geom_point()

train_imput %>% 
  ggplot(aes(x = Drained_after_socializing, fill = Personality)) +
  geom_bar(position = "dodge")

train_imput %>% 
  ggplot(aes(x = Stage_fear, fill = Personality)) +
  geom_bar(position = "dodge")
