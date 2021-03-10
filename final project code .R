

# begin modelling 
library(pacman)
p_load(
  tidyverse, modeldata, skimr, janitor,
  kknn, tidymodels, recipes, parsnip, 
  magrittr, datasets, rpart.plot, baguette,ranger,mlbench,parallel,
  data.table, xgboost, AER, tune, glmnet, write_csv()
)

p_load(tidyverse,ggplot2)

set.seed(101)

final_recipe <- final_train %>% recipe(allstar ~ .) %>%step_rm(season)%>%
  step_rm(player) %>% step_rm(collage) %>% step_rm(pos)%>%step_rm(mvp)%>%
  step_rm(birth_city) %>% step_rm(birth_state) %>%
  step_normalize(all_predictors() & all_numeric()) %>%
  step_dummy(all_predictors() & all_nominal()) %>%
  step_modeimpute(all_predictors()&all_nominal()) %>%
  step_meanimpute(all_predictors()&all_numeric())
final_clean <- final_recipe %>% prep() %>% juice()









final_df$allstar <- as.factor(final_df$allstar)

train_test_split = final_df%>%initial_split(.8)


final_train <- final_df[final_df$year %in% train_years,]
final_test <- final_df[final_df$year %in% test_years,]

final_cv <- final_train %>% vfold_cv(v = 3,strata = "year")

final_df$mvp <- as.factor(final_df$mvp)



set.seed(101)




#define boost model

allstar_boost = boost_tree(
  mtry= NULL,
  trees=10,
  min_n =NULL,
  tree_depth = tune(),
  learn_rate = tune()
  
) %>% set_engine(
  engine = "xgboost") %>% 
  set_mode("classification")



#define workflow 

allstar_boost_wf =
  workflow()%>% add_model(allstar_boost)%>%add_recipe(final_recipe)

#run model

cv_boost = allstar_boost_wf %>%
  tune_grid(
    final_cv,
    grid = grid_regular(tree_depth(), learn_rate(), levels = 5:5),
    metrics = metric_set(accuracy)
  )

#show the best model

cv_boost %>% show_best()

#finalize workflow and use it to predict onto test data


final_boost = 
  allstar_boost_wf %>%
  finalize_workflow(select_best(cv_boost, "accuracy"))


#fit final model

final_fit_boost = final_boost %>% fit(data =final_train)


#predict onto test data


y_hat = final_fit_boost %>% predict(new_data = final_test, type="class")


cm_boost = conf_mat(
  data =tibble(
    y_hat = y_hat %>% unlist(),
    y = final_test$allstar
  ),
  truth = y, estimate = y_hat
)
#view matrix 
cm_boost


head(p_hat)

p_hat %>% length()
final_test %>% nrow()


boost_df = data.frame(
  player = final_test$player,
  year = final_test$year,
  allstar = p_hat
)


boost_df


boost_df%>%filter(allstar>0.2)%>%ggplot(boost_df, aes(x=year, y=allstar..pred1))



write.csv(x = submit_df, file = "Boost-Predictions1.csv")



#######################3


final_df$mvp<- as.factor(final_df$mvp)

final_df$allstar<- as.numeric(final_df$allstar)
set.seed(101)
train_test_split = final_df%>%initial_split(.8)
#train test split
df_train = train_test_split %>% training()
df_test = train_test_split %>% testing()



#turn allstar into a numeric variable
final_df$allstar<- as.numeric(as.character(final_df$allstar))











#Elasticnet regression

model_enlinreg = linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")

#Define the workflow

workflow_enlinreg = workflow() %>% 
  add_model(model_en) %>% 
  add_recipe(final_recipe)

##run model
cv_enlinreg = workflow_enlinreg %>%
  tune_grid(
    final_cv,
    grid = grid_regular(mixture(), penalty(), levels = 5:5),
    metrics = metric_set(rmse)
  )
#find best model
cv_enlinreg %>% collect_metrics() %>% arrange(mean)



#finalize 

enlinreg_final <- workflow_enlinreg%>%
  finalize_workflow(select_best(cv_enlinreg, metric="rmse"))

#fit final model

enlinreg_final_fit = enlinreg_final%>%fit(data=final_train)


#Predict onto the test data

test_hat = enlinreg_final_fit %>% predict(new_data = final_test)

head(test_hat)


enlinreg_df = data.frame(
  player = final_test$player,
  year = final_test$year,
  allstar = test_hat
)


enlinreg_df

enlinreg_df%>%filter(.pred>0.5, year==2004)%>%ggplot(aes(x=player, y=.pred))+geom_col()

enlinreg_df%>%filter(.pred>0.5, year==2008)%>%ggplot(aes(x=player, y=.pred, fill=player))+geom_col()
##predicted tmac but he isnt there, did not predict carmelo, no yao ming, no jason kidd, no wade, 
# predicts manu ginobli 9











