
## Modeling
# Data set at
# https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j
# Saved as "New_York_City_Restaurants.xlsx" in working directory

# hier zur API

# Load packages to be used:
library(caret) 
library(e1071)
library(rpart.plot)

# Read in data set
df_raw <- read_excel( "New_York_City_Restaurants.xlsx", 
                      col_types = c(rep("text", 8), 
                                    "date", 
                                    rep("text", 4), 
                                    "numeric",
                                    "text",
                                    rep("date", 2),
                                    "text")) 

# Create feature set:
df_features <- df_raw %>% 
  tame_df() %>% 
  make_features_raw() %>% 
  impute_features() 

# Split into training and test set:
set.seed(1)
train_ids <- sample(unique(df_features$id), 0.6*length(unique(df_features$id)))
test_ids <- setdiff(unique(df_features$id), train_ids)


df_train <- df_features %>% filter(id %in% train_ids) %>% add_target_feature()
df_test <- df_features %>% filter(id %in% test_ids)
write.csv(df_test, "test_restaurants.csv")



# Set hyperparameters for cross-validated classification and regression tree
set.seed(42)
myControl <- trainControl(
  
  method = "cv",
  number = 10
  
)
cp.grid <- expand.grid(.cp = (1:10)*0.01)


# Model 1: Linear Regression 
(lin_reg_model <- df_train %>% 
    train(as.numeric(days_until_next) ~ grade + dummy_InitialInspection +
          score + cuisine_descr + viol_vermin + viol_not_scored + viol_facility + 
          viol_food_temperature + viol_hygiene + viol_food_protection + 
          viol_food_source + viol_other_scored + critical_flag,
          data = .,
          method = "lm"))
saveRDS ( lin_reg_model, "lin_reg_model.rds" )

# Model 2: Regression tree
set.seed(3333)
(reg_tree_model <- df_train %>% select(-days_until_next_categ, -id, -inspection_date) %>% 
    train(as.numeric(days_until_next) ~ .,
          data = .,
          method = "rpart",
          na.action = na.pass,
          trControl = myControl,
          tuneGrid = cp.grid))
rpart.plot ( reg_tree_model$finalModel, type = 3, digits = 3, fallen.leaves = TRUE )
saveRDS ( reg_tree_model, "reg_tree_model.rds" )


# Model 3: Classification tree
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








# Evaluate final model on training data: 
confusionMatrix(data = predict(class_tree_model, df_train),
                reference = df_train$days_until_next_categ)

# ... and on the test data:
confusionMatrix(data = predict(class_tree_model, df_test),
                reference = df_test$days_until_next_categ)
