## Feature preparation

library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(rpart)
library(rpart.plot)

## Tame data:

tame_df <- function ( df_raw ) {
  
  df_raw %>% 
    clean_names() %>% 
    # Rename variables
    rename(id = camis, rest_name = dba, 
           cuisine_descr = cuisine_description,
           violation_descr = violation_description) %>% 
    # Inspection date: Ignore values "1900-01_01"
    mutate(inspection_date = ymd(inspection_date)) %>% 
    filter(!(inspection_date == "1900-01-01")) %>%
    # convert 'cuisine description' into factor
    mutate(cuisine_descr = as_factor(cuisine_descr)) %>% 
    # Cuisine_description: Relabel levels 
    mutate(cuisine_descr = 
             fct_recode(cuisine_descr, 
                        "Coffee_Tea" = 
                          levels(cuisine_descr) %>% 
                          str_subset( ., "Coffee/Tea"),
                        "Latin" = levels(cuisine_descr) %>% 
                          str_subset( ., "Latin"))) %>% 
    # only keep the 20 most frequent cuisines    
    mutate(cuisine_descr = fct_lump(f = cuisine_descr, n = 20)) %>% 
    # Boro: recode missing values and convert into factor
    mutate(boro = na_if(boro, "Missing")) %>% 
    mutate(boro = as_factor(boro)) %>% 
    # Actions taken: convert into a factor and (re)name its levels
    mutate(action = as_factor(action)) %>% 
    mutate(action = fct_recode(action, 
                               "YesViol" = levels(action)[1],
                               "ReOpened" = levels(action)[2],
                               "Closed" = levels(action)[3],
                               "ReClosed" = levels(action)[4],
                               "NoViol" = levels(action)[5])) %>% 
    # Convert 'violation_code' into factor
    # mutate(violation_code = as_factor(violation_code)) %>% 
    # Violation types: new variable violation_group
    mutate(
      violation_group = 
        case_when(
          violation_code %in% 
            str_c("02", LETTERS[1:10]) ~ "food_temperature",
          violation_code %in% 
            c(str_c("03", LETTERS[1:7]), 
              str_c("09", LETTERS[1:3])) ~ "food_source",
          violation_code %in% 
            str_c("04", LETTERS[1:10]) ~ "food_protection",
          violation_code %in% 
            c(str_c("05", LETTERS[1:9]), 
              str_c("10", LETTERS[1:10])) ~ "facility",
          violation_code %in% 
              str_c("06", LETTERS[1:9]) ~ "hygiene",
          violation_code %in% 
              str_c("04", LETTERS[11:15]) ~ "vermin",
          violation_code %in% 
              c("07A", "99B") ~ "other_scored",
          !is.na(violation_code) ~ "not_scored"
          )
      ) %>% 
    mutate(
      violation_group = as_factor(violation_group),
      violation_group = fct_collapse(violation_group,
                                     food = c("food_temperature",
                                              "food_protection",
                                              "food_source",
                                              "other_scored")
                                     )
      ) %>% 
    # Create indicator variables for levels of 'violation_group'
    mutate(
      viol_vermin = 
        case_when(violation_group == "vermin" ~ 1, 
                  TRUE ~ 0),
      viol_facility = 
        case_when(violation_group == "facility" ~ 1, 
                  TRUE ~ 0),
      viol_food = 
        case_when(violation_group == "food" ~ 1, 
                  TRUE ~ 0),
      viol_hygiene = 
        case_when(violation_group == "hygiene" ~ 1, 
                  TRUE ~ 0),
      viol_not_scored = 
        case_when(violation_group == "not_scored"  ~ 1, 
                  TRUE ~ 0)
    ) %>% 
    # Inspection type: collapse into fewer categories
    separate(inspection_type, 
             into = c("before_slash", "after_slash"), 
             sep = " / ") %>% 
    mutate(after_slash = 
             str_replace_all(after_slash, fixed(" "), 
                             fixed(""))) %>% 
    mutate(inspection_type2 = 
             str_replace_all(after_slash, fixed("-i"), 
                             fixed("I")),
           inspection_type2 = as_factor(inspection_type2)) %>% 
    # Indicator variable for level 'Initial Inspection'
    mutate(inspection_type2 = fct_other(inspection_type2,
                                        keep = "InitialInspection")) %>%
    # Grade: Recode NA's with 
    # inspection type = 'InitialInspection' 
    # and 'score' > 14 to "Not Yet Graded"
    mutate(grade = case_when(
      is.na(.$grade) &
        .$inspection_type2 == "InitialInspection" &
        .$score > 14 ~ "Not Yet Graded",
      TRUE ~ grade
    )) 
  
}


## Make feature set:

make_features_raw <- function ( df ) {
  
  df %>% 
    select ( id, inspection_date, inspection_type2, score, 
             grade, cuisine_descr ) %>% 
    group_by ( id, inspection_date ) %>% 
    arrange ( id, inspection_date ) %>% 
    summarise_at ( 
      vars(c("grade","inspection_type2", 
             "score", "cuisine_descr")), 
                   .funs = list(first) ) %>%
    left_join ( 
      df %>% 
        select ( id, inspection_date, starts_with("viol_"), 
                 starts_with("critical") ) %>%
        mutate( critical_flag = 
                  case_when( critical_flag == "Critical" ~ 1,
                                           TRUE ~ 0 ) ) %>%
        group_by ( id, inspection_date ) %>%
        arrange ( id, inspection_date ) %>%
        summarise_at ( vars(starts_with( "viol" ), 
                            starts_with( "critical" ) ),
                       ~ sum( ., na.rm = TRUE ) ),
      by = c ( "id", "inspection_date" ) ) %>% 
    ungroup()
  
}


# Impute features

impute_features <- function ( df ) {
  
  df %>%
    filter(is.na(score)) %>%
    mutate(score = 
             predict(df %>%
                       filter ( !is.na ( score ) ) %>%
                       select ( 
                         score, grade, inspection_type2, cuisine_descr,
                         starts_with ( "viol_" ), critical_flag ) %>%
                       rpart ( score ~ ., data = ., method = "anova" ), .)) %>%
    bind_rows(df %>% filter(!is.na(score))) %>%
    filter(is.na(grade)) %>% 
    mutate(grade = 
             predict(df %>%
                       filter ( !is.na ( grade ) ) %>%
                       select ( 
                         score, grade, inspection_type2, cuisine_descr,
                         starts_with ( "viol_" ), critical_flag ) %>%
                       rpart ( grade ~ ., data = ., method = "class" ), ., 
                     type = "class"),
           grade = 
             as.character(grade)) %>%
    bind_rows(df %>%
                filter(!is.na(grade)))

}

# Add target feature

add_target_feature <- function(df) {
  
  df %>%  
    group_by(id) %>% 
    arrange(id, inspection_date) %>% 
    mutate ( 
      days_until_next = lead ( inspection_date ) - inspection_date 
      ) %>% 
    filter(!is.na(days_until_next)) %>% 
    ungroup() %>% 
    mutate ( 
      days_until_next_categ = 
        case_when (
          between ( 
            as.numeric ( days_until_next ), 0, 100 
            ) ~ "within 2 to 3 months",
          between ( 
            as.numeric ( days_until_next ), 101, 300 
            ) ~ "within 10 months",
          TRUE ~ "in more than 10 months" ),
      days_until_next_categ = as.factor(days_until_next_categ)) %>% 
    ungroup() 
  
}

  
  
  

