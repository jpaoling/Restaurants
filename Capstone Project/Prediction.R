
## Prediction 

# Construct feature set for prediction

make_pred <- function ( id_nr ) {
  
  df_features %>% 
    group_by(id) %>% 
    filter(id == id_nr,
           inspection_date == max(inspection_date)) %>% 
    predict(class_tree_model, .) 
  
}

write_rds(make_pred, "make_pred.rds")