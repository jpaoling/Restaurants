
## Modeling
# Data set at
# https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j
# Saved as "New_York_City_Restaurants.xlsx" in working directory

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

# Tame data set:
df_tame <- df_raw %>% 
  tame_df()
saveRDS(df_tame, "df_tame.rds")

# Create feature set:
df_features <- df_tame %>% 
  make_features_raw() %>% 
  impute_features() %>% 
  add_target_feature()
saveRDS(df_features, "df_features.rds")

# Split into training and test set:
set.seed(1)
train_ids <- sample(unique(df_features$id), 0.6*length(unique(df_features$id)))
test_ids <- setdiff(unique(df_features$id), train_ids)


df_train <- df_features %>% filter(id %in% train_ids)
write_rds(df_train, "df_train.rds")
df_test <- df_features %>% filter(id %in% test_ids)
write_rds(df_test, "df_test.rds")
rm(df_test)


# Set hyperparameters for cross-validated classification and regression tree
set.seed(42)
myControl <- trainControl(
  
  method = "cv",
  number = 10
  
)
cp.grid <- expand.grid(.cp = (1:10)*0.01)


# Model 1: Linear Regression 
(lin_reg_model <- df_train %>% 
    train(as.numeric(days_until_next) ~ grade + inspection_type2 +
          score + cuisine_descr + viol_vermin + viol_not_scored + viol_facility + 
          viol_food + viol_hygiene + viol_not_scored + critical_flag,
          data = .,
          method = "lm"))
write_rds ( lin_reg_model, "lin_reg_model.rds" )

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
write_rds ( reg_tree_model, "reg_tree_model.rds" )


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
write_rds ( class_tree_model, "class_tree_model.rds" )




# Evaluate final model on training data: 
conf_train <- confusionMatrix(data = predict(class_tree_model, df_train),
                              reference = df_train$days_until_next_categ) 

write_rds("confusion_train.rds")

# ... and on the test data:
df_test <- readRDS("df_test.rds")
conf_test <- confusionMatrix(data = predict(class_tree_model, df_test),
                             reference = df_test$days_until_next_categ)

write_rds("confusion_test.rds")
