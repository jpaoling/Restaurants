## Feature preparation

library(tidyverse)
library(lubridate)
library(readxl)
library(rpart)
library(rpart.plot)

## Clean data set:

clean_data_df <- function ( file_name ) {
  
  # Read in data set
  suppressWarnings ( df_raw <- read_excel( file_name ) )

  # Rename variables
  new_names <- c("id", "rest_name", "boro", "building", "street", "zipcode", 
               "phone", "cuisine_descr", "inspection_date", "action", 
               "violation_code", "violation_descr", "critical_flag", 
               "score", "grade", "grade_date", "record_date", 
               "inspection_type")
  names(df_raw) <- new_names
  rm(new_names)
  
  # Inspection date: Ignore values "1900-01_01"
  df_raw$inspection_date <- ymd(df_raw$inspection_date) 
  df_raw <- df_raw %>% 
    filter(!inspection_date == "1900-01-01")

  # Cuisine_description: Relabel level "CafÃƒÂ©/Coffee/Tea" which is at position 14 in levels vector
  cuis_ind <- levels(df_raw$cuisine_descr) == levels(df_raw$cuisine_descr)[14]
  levels(df_raw$cuisine_descr)[cuis_ind] <- "Coffee_Tea"

  # Cuisine_description: Relabel level "Latin (..." into "Latin"
  levels(df_raw$cuisine_descr)[levels(df_raw$cuisine_descr) == 
                                 "Latin (Cuban, Dominican, Puerto Rican, South & Central American)"] <- "Latin"

  # Cuisine_description: bin the new 
  df_raw <- df_raw %>% 
    mutate(cuisine_descr = fct_lump(f = cuisine_descr, n = 20))

  # Boro: Recoding of missing values:
  df_raw$boro <- as.factor(df_raw$boro)
  levels(df_raw$boro)[levels(df_raw$boro) == "Missing"] <- NA
  summary(df_raw$boro)


  # Acions taken: convert into a factor and (re)name its levels
  df_raw$action <- as.factor(df_raw$action)
  levels(df_raw$action) <- c("Closed", "ReClosed", "ReOpened", "NoViol", "YesViol")
  summary(df_raw$action)


  # Violation types: new variable violation_group
  df_raw$violation_code <- as.factor(df_raw$violation_code)
  df_raw <- 
    df_raw %>% 
    mutate(violation_group = case_when(
      violation_code %in% str_c("02", LETTERS[1:10]) ~ "food_temperature",
      violation_code %in% c(str_c("03", LETTERS[1:7]), str_c("09", LETTERS[1:3])) ~ "food_source",
      violation_code %in% str_c("04", LETTERS[1:10]) ~ "food_protection",
      violation_code %in% c(str_c("05", LETTERS[1:9]), str_c("10", LETTERS[1:10])) ~ "facility",
      violation_code %in% str_c("06", LETTERS[1:9]) ~ "hygiene",
      violation_code %in% str_c("04", LETTERS[11:15]) ~ "vermin",
      violation_code %in% c("07A", "99B") ~ "other_scored",
      !is.na(violation_code) ~ "not_scored"
    )
    )
  
  # Indicator variable for violation group
  for(level in unique(df_raw$violation_group)){
    if (is.na(level)) {
      next
    } else {
      df_raw[paste("viol", level, sep = "_")] <- 
        ifelse(df_raw$violation_group == level, 1, 0)
    }
  }

  # Inspection type: collapse into fewer categories
  df_raw$inspection_type2 <-  df_raw %>% 
    select(inspection_type) %>% 
    separate(inspection_type, into = c("before_slash", "after_slash"), sep = " / ") %>% 
    transmute(after_slash = str_replace_all(after_slash, fixed(" "), fixed(""))) %>% 
    transmute(inspection_type2 = str_replace_all(after_slash, fixed("-i"), fixed("I"))) %>% 
    .$inspection_type2
  
  # Indicator variable for level 'Initial Inspection'
  df_raw$dummy_InitialInspection <- 
    as.factor(ifelse(df_raw$inspection_type2 == "InitialInspection", "Initial", "Not_Initial"))


  # Grade: Recode NA's with inspection type = 'InitialInspection' and 'score' > 14 to "Not Yet Graded"
  df_raw <- df_raw %>%
    mutate(grade = case_when(
      is.na(.$grade) &
        .$inspection_type2 == "InitialInspection" &
        .$score > 14 ~ "Not Yet Graded",
      TRUE ~ grade
    ))
  
  # convert into factor:
  df_raw <- df_raw %>% 
    mutate(grade = as.factor(grade))
  
  df_raw

}



## Generate feature set: 
 
make_feature_set <- function ( df_raw, is_train ) {
  
  make_df_GradeInspecScoreCuisine <- function ( df ) {
    
    df %>% 
      select ( id, inspection_date, dummy_InitialInspection, score, 
               grade, cuisine_descr ) %>% 
      group_by( id, inspection_date ) %>% 
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
  
  # Impute missing values for score and grade:

impute_features <- function ( df_features ) {
  
  scoreFit <- df_features %>%
    filter ( !is.na ( score ) ) %>%
    select ( score, grade, dummy_InitialInspection, cuisine_descr,
           starts_with ( "viol_" ), critical_flag ) %>%
    rpart ( score ~ ., data = ., method = "anova" )
  df_features$score[is.na( df_features$score )] <-
    predict(scoreFit, df_features[is.na(df_features$score), ])
  
  gradeFit <- df_features %>%
    filter(!is.na(grade)) %>%
    select(score, grade, dummy_InitialInspection, cuisine_descr,
           starts_with("viol_"), critical_flag) %>%
    rpart(grade ~ ., data = ., method = "class")
  df_features$grade[is.na(df_features$grade)] <-
    predict(gradeFit, df_features[is.na(df_features$grade), ], type = "class")
  
  
  df_features
  
}
  
  
  

