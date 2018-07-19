
## Modeling

# Create feature set:
df_features <- clean_data_df("New_York_City_Restaurants.xlsx") %>% make_feature_set()

# Split into training and test set:
set.seed(1)
train_ids <- sample(unique(df_features$id), 0.6*length(unique(df_features$id)))
test_ids <- setdiff(unique(df_features$id), train_ids)

df_train <- df_features %>% filter(id %in% train_ids)
df_test <- df_features %>% filter(id %in% test_ids)
write.csv(df_test, "test_restaurants.csv")
rm(df_test)


library(caret) 
library(e1071)
library(rpart.plot)

# Set hyperparameters for cross-validated classification and regression tree
set.seed(42)
myControl <- trainControl(
  
  method = "cv",
  number = 10
  
)
cp.grid <- expand.grid(.cp = (1:10)*0.01)


# Model 1: Classification tree
set.seed(3333)
(class_tree_model <- df_train %>% select(-days_until_next, -id, -inspection_date) %>% 
    train(days_until_next_categ ~ ., 
          data = ., 
          method = "rpart", 
          trControl = myControl, 
          tuneGrid = cp.grid)
)
rpart.plot ( class_tree_model$finalModel, type = 3, digits = 3, fallen.leaves = TRUE )
saveRDS ( class_tree_model, "class_tree_model.rds" )

# Model 2: Regression tree
set.seed(3333)
(reg_tree_model <- df_train %>% select(-days_until_next_categ, -id, -inspection_date) %>% 
    train(as.numeric(days_until_next) ~ .,
          data = .,
          method = "rpart",
          trControl = myControl,
          tuneGrid = cp.grid))
rpart.plot ( reg_tree_model$finalModel, type = 3, digits = 3, fallen.leaves = TRUE )
saveRDS ( reg_tree_model, "reg_tree_model.rds" )

# Model 3: Linear Regression 
(lin_reg_model <- df_train %>% 
    select(-days_until_next_categ, -id, -inspection_date) %>%
    train(as.numeric(days_until_next) ~ .,
          data = .,
          method = "glm"))
saveRDS ( lin_reg_model, "lin_reg_model.rds" )