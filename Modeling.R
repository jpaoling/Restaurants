## Modeling

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

# Set hyperparameters for cross-validated classification tree
set.seed(42)
myControl <- trainControl(
  
  method = "cv",
  number = 10
  
)

cp.grid <- expand.grid(.cp = (1:10)*0.01)

set.seed(3333)
(model_restaurants <- df_train %>% select(-days_until_next, -id, -inspection_date) %>% 
    train(days_until_next_categ ~ ., 
          data = ., 
          method = "rpart", 
          trControl = myControl, 
          tuneGrid = cp.grid)
)

