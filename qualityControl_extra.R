## ---------------------------
##
## Script name: qualityControl_extra.R
##
## Purpose of script: 
##   Supplemental quality control script to format additional
##   tree attributes for the database.
##
## Author: Russell Kwong
##
## Date Updated: 2025-12-29
##
## Email: rk584@cornell.edu
##
## ---------------------------
##
## Notes:
## 
##
## ---------------------------

# load packages


library('devtools')
library('here')
library('tidyverse')

# set directory location
i_am('scripts/qualityControl_extra.R')

date_format <- function(date_value){
  # returns year from character date field
  #date_year <- year(ymd(df[[date_field]]))
  
  return(year(ymd(date_value)))
}

date_format_lap <- function(df, date_fieldList){
  df[date_fieldList] <- lapply(df[date_fieldList], function(x){
    df[[x]] <- year(ymd(df[[x]]))
  })
}

datetime_format <- function(date_value){
  return(year(ymd_hms(date_value, truncated = 5)))
}

canopyArea_calc <- function(filename, areaFormat){
  # areaFormat 1 -- two cols of canopy width in feet
  # areaFormat 2 -- one col of canopy width in feet
  # areaFormat 3 -- one col of canopy with in feet (class)
  
  city_meta <- filter(file_metadata, FILENAME == filename)
  localID_colname <- city_meta$TREE_ID
  canopy_w1 <- city_meta$CROWN1
  canopy_w2 <- city_meta$CROWN2 
  
  colList <- c(localID_colname, canopy_w1, canopy_w2) %>% 
    .[!. %in% c('')]
  
  city <- read_csv(here('data/preprocess', filename), 
                   col_types = cols(.default = col_character()))
  canCols <- select(city, all_of(colList)) %>% 
    add_column(canopyArea = NA) %>% 
    rename(localID = localID_colname)
  
  if (areaFormat == 1){
    canCols <- canCols %>% 
      rename(CROWN1 = canopy_w1, 
             CROWN2 = canopy_w2) %>% 
      mutate(CROWN1 = as.numeric(gsub('[^0-9.]', '', CROWN1)), 
             CROWN2 = as.numeric(gsub('[^0-9.]', '', CROWN2)), 
             canopyArea = round(pi * (CROWN1 / 2) * (CROWN2 / 2) * 0.09290304, 2)) 
  } else if (areaFormat == 3){
    canCols <- canCols %>% 
      rename(crwClass = canopy_w1) %>% 
      mutate(crwClass = str_replace_all(crwClass, c('to' = '-')), 
             crwClass = gsub("'", '', crwClass), 
             crown_bound = ifelse(str_sub(crwClass, 2, 2) == 'T', 
                                  1, 0), 
             crwClass = gsub('[[:alpha:]]| ', '', crwClass), 
             crwClass = ifelse(crown_bound == 1, paste0(crwClass, '-', crwClass), crwClass)) %>% 
      separate(crwClass, 
               into = c("crw_lb", "crw_ub"), 
               sep = c('-'), 
               remove = FALSE) %>% 
      mutate(across(all_of(c("crw_lb", "crw_ub")), trimws), 
             crw_ub = ifelse(crown_bound == 1, crw_lb, crw_ub), 
             across(all_of(c("crw_lb", "crw_ub")), 
                    as.numeric)) %>% 
      mutate(crwClassCalc = rowMeans(.[,c("crw_lb", "crw_ub")], na.rm = FALSE), 
             canopyArea = round(pi * (crwClassCalc / 2)^2 * 0.09290304, 2))
  }
  
  return(canCols)
}

canopy_area_range <- function(filename){
  city_
}

conditionCrosswalk <- data.frame(
  CONDITION = c('Dead', 'Poor', 'Fair', 'Good', 'Excellent', 'Removal'), 
  CONDITION_int = c(1, 2, 3, 4, 5, 9)
)

conditionFormat <- function(treeList){
  treeList <- merge(treeList, conditionCrosswalk, 
                    by = c("CONDITION"), 
                    all.x = TRUE) 
  # %>% 
  #   select(-c("CONDITION")) %>% 
  #   rename(CONDITION = 'CONDITION_int')
  
  return(treeList)
}
                           