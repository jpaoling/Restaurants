## Prediction of Category

Predict_categ <- function(id, df_feat) {
    test <- df_feat %>% filter(id %in% id)
    predict(model_restaurants, test, type = "class")
}