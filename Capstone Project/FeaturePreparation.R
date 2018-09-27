## Feature preparation

library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(rpart)
library(rpart.plot)

## Clean data set: einlesen herausnehmen!

clean_data_df <- function ( df_raw ) {
  
  df_raw %>% 
    clean_names() %>% 
    # Rename variables
    rename(id = camis, rest_name = dba, cuisine_descr = cuisine_description,
           violation_descr = violation_description) %>% 
    # Inspection date: Ignore values "1900-01_01"
    mutate(inspection_date = ymd(inspection_date)) %>% 
    filter(!(inspection_date == "1900-01-01")) %>%
    # convert 'cuisine description' into factor
    mutate(cuisine_descr = as_factor(cuisine_descr)) %>% 
    # Cuisine_description: Relabel level "CafÃƒÂ©/Coffee/Tea" and "Latin (...
    mutate(cuisine_descr = fct_recode(cuisine_descr,
                                      Coffee_Tea = "CafÃƒÂ©/Coffee/Tea",
                                      Latin = "Latin (Cuban, Dominican, Puerto Rican, South & Central American)")) %>% 
    # only keep the 20 most frequent cuisines    
    mutate(cuisine_descr = fct_lump(f = cuisine_descr, n = 20)) %>% 
    # Boro: recode missing values and convert into factor
    mutate(boro = na_if(boro, "Missing")) %>% 
    mutate(boro = as_factor(boro)) %>% 
    # Actions taken: convert into a factor and (re)name its levels
    mutate(action = as_factor(action)) %>% 
    mutate(action = fct_recode(action, 
                               YesViol = "Violations were cited in the following area(s).",
                               ReOpened = "Establishment re-opened by DOHMH",
                               Closed = "Establishment Closed by DOHMH.  Violations were cited in the following area(s) and those requiring immediate action were addressed.",
                               ReClosed = "Establishment re-closed by DOHMH",
                               NoViol = "No violations were recorded at the time of this inspection.")) %>% 
    # Convert 'violation_code' into factor
    mutate(violation_code = as_factor(violation_code)) %>% 
    # Violation types: new variable violation_group
    mutate(violation_group = case_when(
      violation_code %in% str_c("02", LETTERS[1:10]) ~ "food_temperature",
      violation_code %in% c(str_c("03", LETTERS[1:7]), str_c("09", LETTERS[1:3])) ~ "food_source",
      violation_code %in% str_c("04", LETTERS[1:10]) ~ "food_protection",
      violation_code %in% c(str_c("05", LETTERS[1:9]), str_c("10", LETTERS[1:10])) ~ "facility",
      violation_code %in% str_c("06", LETTERS[1:9]) ~ "hygiene",
      violation_code %in% str_c("04", LETTERS[11:15]) ~ "vermin",
      violation_code %in% c("07A", "99B") ~ "other_scored",
      !is.na(violation_code) ~ "not_scored"
    )) %>% 
    # Create indicator variables for levels of 'violation_group'
    mutate(viol_vermin = case_when(violation_group == "vermin" ~ 1, TRUE ~ 0),
           viol_not_scored = case_when(violation_group == "not_scored" ~ 1, TRUE ~ 0),
           viol_facility = case_when(violation_group == "facility" ~ 1, TRUE ~ 0),
           viol_food_temperature = case_when(violation_group == "food_temperature" ~ 1, TRUE ~ 0),
           viol_hygiene = case_when(violation_group == "hygiene" ~ 1, TRUE ~ 0),
           viol_food_protection = case_when(violation_group == "food_protection" ~ 1, TRUE ~ 0),
           viol_food_source = case_when(violation_group == "food_source" ~ 1, TRUE ~ 0),
           viol_other_scored = case_when(violation_group == "other_scored"  ~ 1, TRUE ~ 0)
    ) %>% 
    # Inspection type: collapse into fewer categories
    separate(inspection_type, into = c("before_slash", "after_slash"), sep = " / ") %>% 
    mutate(after_slash = str_replace_all(after_slash, fixed(" "), fixed(""))) %>% 
    mutate(inspection_type2 = str_replace_all(after_slash, fixed("-i"), fixed("I"))) %>% 
    # Indicator variable for level 'Initial Inspection'
    mutate(dummy_InitialInspection = case_when(
      inspection_type2 == "InitialInspection" ~ "Initial",
      TRUE ~ "Not_Initial"
    )) %>% 
    # Grade: Recode NA's with inspection type = 'InitialInspection' and 'score' > 14 to "Not Yet Graded"
    mutate(grade = case_when(
      is.na(.$grade) &
        .$inspection_type2 == "InitialInspection" &
        .$score > 14 ~ "Not Yet Graded",
      TRUE ~ grade
    )) %>% 
    mutate(grade = as_factor(grade))
  
  
}



## Generate feature set: 
 
make_feature_set <- function ( df_raw, is_train ) {
  
  make_df_GradeInspecScoreCuisine <- function ( df ) {
    
    df %>% 
      select ( id, inspection_date, dummy_InitialInspection, score, 
               grade, cuisine_descr ) %>% 
      group_by ( id, inspection_date ) %>% 
      arrange ( id, inspection_date ) %>% 
      summarise_at ( vars(c("grade","dummy_InitialInspection", "score", "cuisine_descr")), 
                     .funs = list(first) ) 

  }
  
  make_df_sumViolFlags <- function(df) {
    
    df %>% 
      select ( id, inspection_date, starts_with("viol_"), starts_with("critical") ) %>% 
      mutate( critical_flag = case_when( critical_flag == "Critical" ~ 1, 
                                         TRUE ~ 0 ) ) %>% 
      group_by ( id, inspection_date ) %>% 
      arrange ( id, inspection_date ) %>% 
      summarise_at ( vars(starts_with( "viol" ), starts_with( "critical" ) ), ~ sum( ., na.rm = TRUE ) ) 

  }
  
  make_df <- function(df) {
    
    left_join ( make_df_GradeInspecScoreCuisine ( df ) , make_df_sumViolFlags ( df ) ,
               by = c ( "id", "inspection_date" ) )

  }
  
  # Data set with features and response
  if ( is_train ) {
    
    df_features <- make_df ( df_raw ) %>% 
      mutate ( days_until_next = lead ( inspection_date ) - inspection_date ) %>% 
      filter ( !is.na ( days_until_next ) ) %>% 
      ungroup() %>% 
      mutate ( days_until_next_categ = case_when (
        between ( as.numeric ( days_until_next ), 0, 100 ) ~ "within 2 to 3 months",
        between ( as.numeric ( days_until_next ), 101, 300 ) ~ "within 10 months",
        TRUE ~ "in more than 10 months" ),
        days_until_next_categ = as.factor ( days_until_next_categ ) ) 
    
    
  } else {
    
    df_features <- make_df ( df_raw ) %>% ungroup()
    
  }
  
  df_features
  
}
  
# Impute missing values for 'score' and 'grade':

impute_features <- function ( df_features_raw ) {
  
  scoreFit <- df_features_raw %>%
    filter ( !is.na ( score ) ) %>%
    select ( score, grade, dummy_InitialInspection, cuisine_descr,
             starts_with ( "viol_" ), critical_flag ) %>%
    rpart ( score ~ ., data = ., method = "anova" )
  
  gradeFit <- df_features_raw %>%
    filter ( !is.na ( grade ) ) %>%
    select ( score, grade, dummy_InitialInspection, cuisine_descr,
             starts_with ( "viol_" ), critical_flag ) %>%
    rpart ( grade ~ ., data = ., method = "class" )
  
  
  df_features_raw %>%
    filter(is.na(score)) %>%
    mutate(score = predict(scoreFit, .)) %>%
    bind_rows(df_features_raw %>%
                filter(!is.na(score))) %>%
    filter(is.na(grade)) %>% mutate(grade = predict(gradeFit, ., type = "class")) %>%
    bind_rows(df_features_raw %>%
                filter(!is.na(grade)))
  
}

  
  
  

