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

# Calculate crown radius (m) for individual inventory 
crownRad_calc <- function(filename){
  
  # Determine structure: (1) crown radius, (2) crown widths, (3) crown area
  city_meta <- filter(file_metadata, FILENAME == filename)
  crwColList <- list(crwRad = city_meta$CROWNRAD, 
       crwWid1 = city_meta$CROWN1, 
       crwWid2 = city_meta$CROWN2, 
       crwArea = city_meta$CROWNAREA)
  
  # Read in inventory
  city_inv <- read_csv(here('data/preprocess', filename), 
                       col_types = cols(.default = col_character()))
  
  crwCols <- select(city_inv, any_of(unlist(crwColList)))

  # Condition (1) -- crown radius
  if (str_length(crwColList$crwRad) > 0) {
    crwList <- unique(select(crwCols, all_of(c("crwRad"))))
    crwList <- crwList[rowSums(is.na(crwList)) != ncol(crwList), ]
    if (nrow(crwList) > 0) {
      crwCross <- crownRad_1(crwList)
      xCols <- c("CROWNRAD")
    }
  # Condition (2) -- crown width(s)  
  } else if (str_length(crwColList$crwWid1) > 0) {
    crwList <- unique(select(crwCols, any_of(c("crwWid1", "crwWid2"))))
    crwList <- crwList[rowSums(is.na(crwList)) != ncol(crwList), ]
    if (nrow(crwList) > 0) {
      crwCross <- crownRad_2(crwList)
      xCols <- c("CROWN1", "CROWN2")
    }
  # Condition (3) -- crown area
  } else if (str_length(crwColList$crwArea) > 0) { 
    crwList <- unique(select(crwCols, all_of(c("crwArea"))))
    crwList <- crwList[rowSums(is.na(crwList)) != ncol(crwList), ]
    if (nrow(crwList) > 0) {
      crwCross <- crownRad_3(crwList)
      xCols <- c("CROWNAREA")
    }
  }
  
  yCols <- colnames(crwCross)[str_detect(colnames(crwCross), '_ORIG')]
  
  return(list(crwCross = crwCross, 
              xCols = xCols, 
              yCols = yCols))
}

# Helper function for crown radius calculation, condition 1
crownRad_1 <- function(crwCross){
  # retain original columns for joining
  crwCross <- mutate(crwCross, crwRad_ORIG = crwRad)
  
  # detect classes if any in measurement
  if (any(str_detect(crwCross$crwRad, '[<>T-]'))) {
    crwClassCross <- crownClasses(select(crwCross, crw = "crwRad"))
    crwCross <- merge(crwCross, crwClassCross, 
                      by.x = "crwRad_ORIG", by.y = "crw_ORIG", 
                      all.x = TRUE)
    
    crwCross <- mutate(crwCross, crwRad = crwCalc)
  }
  
  crwCross <- crwCross %>% 
    mutate_at(c("crwRad"), as.numeric) %>% 
    drop_na(any_of(c("crwRad"))) 
  
  # Convert ft measurement to m, 1 ft = 0.3048 m
  crwCross <- mutate(crwCross, crwClean = 0.3048 * crwRad)
  
  return(crwCross)  
}

# Helper function for crown radius calculation, condition 2
crownRad_2 <- function(crwCross){
  # retain original columns for joining
  crwCount <- length(colnames(crwCross))
  crwCross <- mutate(crwCross, crwWid1_ORIG = crwWid1)
  
  # one crown width, 1 ft = 0.3048 m
  if (crwCount == 1) {
    # if there are classes in the crown measurement
    if (any(str_detect(crwCross$crwWid1, '[<>T-]'))) {
      crwClassCross <- crownClasses(select(crwCross, crw = "crwWid1"))
      crwCross <- merge(crwCross, crwClassCross, 
                        by.x = "crwWid1_ORIG", by.y = "crw_ORIG", 
                        all.x = TRUE)
      
      crwCross <- mutate(crwCross, crwWid1 = crwCalc)
    }
    
    crwCross <- crwCross %>% 
      mutate_at(c("crwWid1"), as.numeric) %>% 
      drop_na(any_of(c("crwWid1"))) 
    
    crwCross <- mutate(crwCross, crwClean = 0.3048 * (crwWid1 / 2))
    
  # two crown widths, 1 ft = 0.3048 m
  } else if (crwCount == 2) {
    # retain original columns for joining
    crwCross <- mutate(crwCross, crwWid2_ORIG = crwWid2)
    
    crwCross <- crwCross %>% 
      mutate(crwWid2 = ifelse(!is.na(crwWid1) & is.na(crwWid2), 
                              crwWid1, crwWid2)) %>% 
      unique() %>% 
      mutate_at(c("crwWid1", "crwWid2"), as.numeric) %>% 
      drop_na(any_of(c("crwWid1", "crwWid2")))
    
    crwCross <- mutate(crwCross, crwClean = 0.3048 * ((crwWid1 + crwWid2) / 4))
  }
  return(crwCross)
}

# Helper function for crown radius calculation, condition 3
crownRad_3 <- function(crwCross){
  # retain original columns for joining
  crwCross <- mutate(crwCross, crwArea_ORIG = crwArea)
  
  # if there are classes in the crown measurement
  if (any(str_detect(crwCross$crwArea, '[<>T-]'))) {
    crwClassCross <- crownClasses(select(crwCross, crw = "crwArea"))
    crwCross <- merge(crwCross, crwClassCross, 
                      by.x = "crwArea_ORIG", by.y = "crwArea", 
                      all.x = TRUE)
    
    crwCross <- mutate(crwCross, crwArea = crwCalc)
  }
    
  crwCross <- crwCross %>% 
    mutate_at(c("crwArea"), as.numeric) %>% 
    drop_na(any_of(c("crwArea"))) 
  
  # Convert area (sq ft) to radius (m), radius = sqrt(pi * r^2), 1 ft = 0.3048 m
  crwCross <- mutate(crwCross, crwClean = 0.3048 * sqrt(crwArea / pi))
    
  return(crwCross)  
}

# Helper function to calculate midpoint of class range
crownClasses <- function(crwClassList){
  
  crwClass <- mutate(crwClassList, crw_ORIG = crw, 
                     crw = gsub('[^0-9.-]', '', crw)) %>% 
    separate(col = crw, 
             into = c("crw_lb", "crw_ub"), 
             sep = '-', 
             remove = FALSE,
             fill = 'right') %>% 
    mutate(crw_ub = ifelse(is.na(crw_ub), crw_lb, crw_ub)) %>% 
    mutate(across(c("crw_lb", "crw_ub"), as.numeric))
  
  crwClass <- mutate(crwClass, crwCalc = (crw_lb + crw_ub) / 2)
  
  return(select(crwClass, all_of(c("crw_ORIG", "crwCalc"))))
}

# Crosswalk for tree condition strings
COND_cross <- read_csv(here('data/files0306/COND_cross.csv'), 
                       col_types = cols(.default = col_character()))

# Function to translate condition strings to standardized numbers 1-5, -1, 9. 
conditionFormat <- function(city_inv){
  # REMV (-1) - removed trees, vacant sites, stumps
  CONDstr_remv <- na.omit(unlist(COND_cross$REMV))
  # CRIT (1) - dead, critical, very poor
  CONDstr_crit <- na.omit(unlist(COND_cross$CRIT))
  # POOR (2) - poor, hazard, compromised
  CONDstr_poor <- na.omit(unlist(COND_cross$POOR))
  # FAIR (3) - fair
  CONDstr_fair <- na.omit(unlist(COND_cross$FAIR))
  # GOOD (4) - good, healthy
  CONDstr_good <- na.omit(unlist(COND_cross$GOOD))
  # EXCL (5) - excellent, very good, vigor
  CONDstr_excl <- na.omit(unlist(COND_cross$EXCL))
  # UNCL (9) - unknown, non-conditions, other
  CONDstr_uncl <- na.omit(unlist(COND_cross$UNCL))
  
  city_inv <- city_inv %>% 
    mutate(COND_clean = 
             case_when(str_to_lower(CONDITION) %in% CONDstr_remv ~ -1, 
                       str_to_lower(CONDITION) %in% CONDstr_crit ~ 1, 
                       str_to_lower(CONDITION) %in% CONDstr_poor ~ 2, 
                       str_to_lower(CONDITION) %in% CONDstr_fair ~ 3, 
                       str_to_lower(CONDITION) %in% CONDstr_good ~ 4, 
                       str_to_lower(CONDITION) %in% CONDstr_excl ~ 5, 
                       str_to_lower(CONDITION) %in% CONDstr_uncl ~ 9))
  
  return(city_inv)
}

# MULTI STEMS COL =================
formatMultiStems <- function(tree_inv){
  tree_inv <- tree_inv %>% 
    mutate(MULTI_STEMS_ORIG = MULTI_STEMS, 
           MULTI_STEMS = case_when(
             str_detect(MULTI_STEMS_ORIG, regex('No', ignore_case = TRUE)) ~ FALSE, 
             str_detect(MULTI_STEMS_ORIG, regex('Yes', ignore_case = TRUE)) ~ TRUE, 
             .default = NA
           )
    ) %>% 
    select(-c("MULTI_STEMS_ORIG"))
  
  return(tree_inv)
}
                           