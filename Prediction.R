
## Prediction 

# Construct feature set for prediction


make_df_pred <- function ( id, filename ) {
  
  make_feature_set ( file_name = filename, is_train = FALSE ) %>% 
    
    filter ( id == id & inspection_date == max ( inspection_date ) )
  
}



pred_function <- function ( model_type = "Classification tree", id, filename ) {
  
  df <- make_df_pred ( id, filename )
  
  if ( model_type == "Classification tree" ) {
    
    model <- readRDS ( "class_tree_model.rds" )
    
  } else if ( model_type == "Regression tree" ) {
    
    model <- readRDS ( "reg_tree_model.rds" )
    
  } else {
    
    model <- readRDS ( "lin_reg_model.rds" )
    
  }
    
  predict ( model, df )
  
}
  
  
  

  
  
 
  

