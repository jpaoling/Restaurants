
## Prediction 

# Construct feature set for prediction

make_df_pred <- function ( id_nr ) {
  
  clean_data_df ( "New_York_City_Restaurants.xlsx" ) %>% make_feature_set ( is_train = FALSE ) %>%
    
    impute_features() %>% 
    
    filter ( id == id_nr ) %>% 
    
    filter ( inspection_date == max ( inspection_date ) )
  
}
