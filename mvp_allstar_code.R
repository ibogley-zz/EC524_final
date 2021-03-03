library(pacman)
p_load(tidyverse, skimr, janitor, tidymodels, magrittr, tune,glmnet, haven)

#final_df changed outcome variables to numeric

final_df_num <- final_df %>%
  mutate(mvp = as_factor(as.numeric(mvp)), allstar = as_factor(as.numeric(allstar)))

final_split <- final_df_num %>% initial_split(prop = .8)

final_train <- final_split %>% training()
final_test <- final_split %>% testing()
final_recipe <- final_train %>% recipe(mvp ~ .)

final_clean <- final_recipe %>% prep() %>% juice()

final_cv <- final_train %>% vfold_cv(v = 3)

model_en <- logistic_reg() %>%
  set_engine("glm")
workflow_en = workflow() %>%
  add_model(model_en) %>%
  add_recipe(final_recipe)

cv_en = workflow_en %>%
  tune_grid(
    final_cv,
    grid = grid_regular(mixture(), penalty(), levels = 5:5),
    metrics = metric_set(accuracy)
  )

#Show the best model

cv_en %>% show_best()