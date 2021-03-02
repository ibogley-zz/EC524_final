#merging the data

#rename names to players in playerstats_df

playerstats_df<- playerstats_df%>%rename(player = name)



df_halffull <- left_join(seasonstats_df, players_df,playerstats_df, by = "player")


full_df <- left_join(df_halffull, mvp, by ="season")

#rename player. y mvp 
full_df <- full_df%>%rename(mvp = player.y)



# begin modelling 
library(pacman)
p_load(
  tidyverse, modeldata, skimr, janitor,
  kknn, tidymodels, recipes, parsnip, 
  magrittr, datasets, rpart.plot, baguette,ranger,mlbench,parallel,
  data.table, xgboost, mclapply
)


set.seed(101)
train_test_split = full_df%>%initial_split(.8)

df_train = train_test_split %>% training()
df_test = train_test_split %>% testing()



df_recipe = full_df%>%recipe(mvp ~.)%>%
  step_knnimpute(all_predictors(), neighbors=5)%>%
  step_dummy(all_predictors() & all_nominal())%>%
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors()) %>% 
  step_lincomb(all_predictors())


df_clean= df_recipe%>% prep()%>%juice()

df_clean


set.seed(101)

df_cv = train_df %>%vfold_cv(v=5)


#define boost model

mvp_boost = boost_tree(
  mtry= NULL,
  trees=100,
  min_n =NULL,
  tree_depth = tune(),
  learn_rate = tune()
  
) %>% set_engine(
  engine = "xgboost") %>% 
  set_mode("classification")



#define workflow 

mvp_boost_wf =
  workflow()%>% add_model(mvp_boost)%>%add_recipe(df_recipe)

#run model

cv_boost = mvp_boost_wf %>%
  tune_grid(
    df_cv,
    grid = grid_regular(tree_depth(), learn_rate(), levels = 5:5),
    metrics = metric_set(accuracy)
  )

#show the best model

cv_boost %>% show_best()

#finalize workflow and use it to predict onto test data


final_boost = 
  cancer_boost_wf %>%
  finalize_workflow(select_best(cv_boost, "accuracy"))


#fit final model

final_fit_boost = final_boost %>% fit(data =df_train)


#predict onto test data


y_hat2 = final_fit_boost %>% predict(new_data = df_test, type="class")



cm_booost = conf_mat(
  data =tibble(
    y_hat2 = y_hat2 %>% unlist(),
    y = df_test$class
  ),
  truth = y, estimate = y_hat2
)
#view matrix 
cm_booost









