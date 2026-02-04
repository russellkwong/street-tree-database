## ---------------------------
##
## Script name: qualityControl_dbh.R
##
## Purpose of script: 
##   Quality control script to flag trees for invalid DBH measurements. 
##
## Author: Russell Kwong
##
## Date Updated: 2025-11-08
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
i_am('scripts/qualityControl_dbh.R')

# read in inventory and column information
pull_dbh <- function(filename){
  # find which col has bot species name
  city_meta <- filter(file_metadata, FILENAME == filename)
  # localID_colname <- colname_format(city_meta$TREE_ID)
  # dbhExact_colname <- colname_format(city_meta$DBH_EXACT)
  # dbhClass_colname <- colname_format(city_meta$DBH_CLASS)
  localID_colname <- city_meta$TREE_ID
  dbhExact_colname <- city_meta$DBH_EXACT
  dbhClass_colname <- city_meta$DBH_CLASS
  
  colList <- c(localID_colname, dbhExact_colname, dbhClass_colname) %>% 
    .[!. %in% c('', 'ID_str')] 
  
  city <- read_csv(here('data/preprocess', filename), 
                   col_types = cols(.default = col_character()))
  dbhCols <- select(city, all_of(colList)) %>% 
    add_column(dbhClean = NA)
  
  # city <- read.csv(here('data/preprocess', filename), 
  #                 colClasses = c(dbhClass = 'character')) 
  # dbhCols <- select(city, all_of(colList)) %>% 
  #  add_column(dbhClean = NA)
  
  
  # if missing a localid, then add in R
  if(localID_colname == 'ID_str'){
    dbhCols <- rowid_to_column(dbhCols, "ID_str")
    localID_colname <- "ID_str"
  }
  
  # rename localid col
  tryCatch({
    dbhCols <- dbhCols %>%
      rename(localID = localID_colname)
  }, error = function(e) {}
  )
  
  # rename dbh class col if exists
  tryCatch({
    dbhCols <- dbhCols %>%
      rename(dbhClass = dbhClass_colname)
  }, error = function(e) {}
  )
  
  # rename dbh exact col if exists
  tryCatch({
    dbhCols <- dbhCols %>%
      rename(dbhExact = dbhExact_colname)
  }, error = function(e) {}
  )
  
  if ('dbhClass' %in% names(dbhCols)){ 
    dbhCols <- dbh_classes(dbhCols, 'dbhClass') 
    if ('dbhExact' %in% names(dbhCols)){ 
      dbhCols <- dbhCols %>% 
        mutate(dbhClean = ifelse(is.na(dbhExact), dbhClassCalc, dbhExact))
    } else {
      dbhCols <- dbhCols %>% 
        mutate(dbhClean = dbhClassCalc)
    }
  } else if ('dbhExact' %in% names(dbhCols)){
    dbhCols <- dbhCols %>% 
      mutate(dbhClean = as.numeric(gsub('\\"', '', dbhExact)))
  }
  
  dbhCols <- dbhCols %>% 
    mutate(dbhClean = as.numeric(gsub('\\(.\\)', '', dbhClean)))
  
  # add flags for invalid DBH
  dbhFlags <- dbhCols %>% 
    mutate(dbh_flag = ifelse(dbhClean <= 0, 1, 0), 
           dbh_flag = ifelse(dbhClean > 100, 1, dbh_flag), 
           dbh_flag = ifelse(is.na(dbhClean), 1, dbh_flag))
  
  # convert DBH inches to cm (1 in = 2.54 cm)
  dbhFlags <- dbhFlags %>% 
    mutate(dbhClean = dbhClean * 2.54)
  
  return(dbhFlags)
}

# convert dbh class to exact DBH  
dbh_classes <- function(df, dbh_field){
  df_diag <- df %>% 
    mutate(dbhClassOrig = dbhClass, 
           dbhClass = str_replace_all(dbhClass, c('to' = '-')), 
           dbhBound = ifelse(grepl('[[:alpha:]]', dbhClass), 1, 0), 
           dbhClass = gsub('[^0-9.-]', '', dbhClass), 
           dbhClass = ifelse(dbhBound == 1, paste0(dbhClass, '-', dbhClass), dbhClass)) %>% 
    separate(!!dbh_field, 
             into = c("dbh_lb", "dbh_ub"), 
             sep = c('-'), 
             remove = FALSE) %>% 
    mutate(across(c(dbh_lb, dbh_ub), 
                  trimws), 
           dbh_lb = str_replace_all(dbh_lb, c('"' = '')),
           dbh_ub = str_replace_all(dbh_ub, c('"' = '')), 
           across(c(dbh_lb, dbh_ub), 
                  as.numeric)) %>% 
    mutate(dbhClassCalc = rowMeans(.[,c("dbh_lb", "dbh_ub")], na.rm = FALSE))
  
  return(df_diag)
}

pull_dbh2 <- function(filename){
  city_meta <- filter(file_metadata, FILENAME == filename)
  dbhExact_colname <- city_meta$DBH_EXACT
  dbhClass_colname <- city_meta$DBH_CLASS
  
  colList <- c(dbhExact_colname, dbhClass_colname) %>% 
    .[!. %in% c('')]
  
  city <- read_csv(here('data/preprocess', filename),  
                   col_types = cols(.default = col_character()))
  dbhCols <- select(city, all_of(colList)) %>% 
    add_column(dbhClean = NA)
  
  joinxCols <- c()
  joinyCols <- c()
  # rename dbh class col if exists
  tryCatch({
    dbhCols$dbhClass = dbhCols[[dbhClass_colname]]
  }, error = function(e) {}
  )
  
  # rename dbh exact col if exists
  tryCatch({
    dbhCols$dbhExact = dbhCols[[dbhExact_colname]]
  }, error = function(e) {}
  )
  
  dbhCols <- dbhCols[!duplicated(dbhCols[colList]),]
  
  if ('dbhClass' %in% names(dbhCols)){ 
    dbhCols <- dbh_classes(dbhCols, 'dbhClass') 
    joinxCols <- append(joinxCols, 'DBH_CLASS')
    joinyCols <- append(joinyCols, 'dbhClassOrig')
    if ('dbhExact' %in% names(dbhCols)){ 
      dbhCols <- dbhCols %>% 
        mutate(dbhExactOrig = dbhExact, 
               dbhClean = ifelse(is.na(dbhExact), dbhClassCalc, dbhExact), 
               dbhClean = as.numeric(gsub('[^0-9.-]', '', dbhClean)))
      joinxCols <- append(joinxCols, 'DBH_EXACT')
      joinyCols <- append(joinyCols, 'dbhExactOrig')
    } else {
      dbhCols <- dbhCols %>% 
        mutate(dbhClean = dbhClassCalc)
    }
  } else if ('dbhExact' %in% names(dbhCols)){
    dbhCols <- dbhCols %>% 
      mutate(dbhExactOrig = dbhExact, 
             # dbhClean = as.numeric(gsub('\\"', '', dbhExact)), 
             dbhClean = as.numeric(gsub('[^0-9.-]', '', dbhExact)))
    joinxCols <- append(joinxCols, 'DBH_EXACT')
    joinyCols <- append(joinyCols, 'dbhExactOrig')
  }
  
  dbhCols <- dbhCols %>% 
    mutate(dbhClean = as.numeric(gsub('\\(.\\)', '', dbhClean)))
  
  # add flags for invalid DBH
  dbhFlags <- dbhCols %>% 
    mutate(dbh_flag = ifelse(dbhClean <= 0, 1, 0), 
           dbh_flag = ifelse(dbhClean > 100, 1, dbh_flag), 
           dbh_flag = ifelse(is.na(dbhClean), 1, dbh_flag))
  
  # convert DBH inches to cm (1 in = 2.54 cm)
  dbhFlags <- dbhFlags %>% 
    mutate(dbhClean = dbhClean * 2.54) %>% 
    select(any_of(c("dbhClassOrig", "dbhExactOrig", "dbhClean", "dbh_flag")))
  
  return(list('xCols' = joinxCols, 
              'yCols' = joinyCols, 
              'dbhEdited' = dbhFlags))
}

pull_height <- function(filename){
  # find which cols have height/height class
  city_meta <- filter(file_metadata, FILENAME == filename)
  if (str_length(city_meta$HEIGHT_EXACT) > 0 | 
      str_length(city_meta$HEIGHT_CLASS) > 0){
    localID_colname <- city_meta$TREE_ID
    hgtExact_colname <- city_meta$HEIGHT_EXACT
    hgtClass_colname <- city_meta$HEIGHT_CLASS
    
    colList <- c(localID_colname, hgtExact_colname, hgtClass_colname) %>% 
      .[!. %in% c('', 'ID_str')]
    
    city <- read_csv(here('data/preprocess', filename), 
                     col_types = cols(.default = col_character()))
    hgtCols <- select(city, all_of(colList)) %>% 
      add_column(hgtClean = NA)
    
    # rename localid col
    tryCatch({
      hgtCols <- hgtCols %>%
        rename(localID = localID_colname)
    }, error = function(e) {}
    )
    
    # rename height class col if exists
    tryCatch({
      hgtCols <- hgtCols %>%
        rename(hgtClass = hgtClass_colname)
    }, error = function(e) {}
    )
    
    # rename height exact col if exists
    tryCatch({
      hgtCols <- hgtCols %>%
        rename(hgtExact = hgtExact_colname)
    }, error = function(e) {}
    )
    
    if ('hgtClass' %in% names(hgtCols)){ 
      hgtCols <- hgt_classes(hgtCols) 
      if ('hgtExact' %in% names(hgtCols)){ 
        hgtCols <- hgtCols %>% 
          mutate(hgtClean = ifelse(is.na(hgtExact), hgtClassCalc, hgtExact))
      } else {
        hgtCols <- hgtCols %>% 
          mutate(hgtClean = hgtClassCalc)
      }
    } else if ('hgtExact' %in% names(hgtCols)){
      hgtCols <- hgtCols %>% 
        mutate(hgtClean = as.numeric(gsub("\\'", '', hgtExact)))
    } 
    # convert feet to meters
    hgtCols <- hgtCols %>% 
      mutate(hgtClean = round(hgtClean * 0.3048, 2))
  } else {
    hgtCols <- data.frame(localID = NA, 
                          hgtClean = NA)
  }
  
  return(hgtCols)
}

pull_height2 <- function(filename){
  # find which cols have height/height class
  city_meta <- filter(file_metadata, FILENAME == filename)
  if (str_length(city_meta$HEIGHT_EXACT) > 0 | 
      str_length(city_meta$HEIGHT_CLASS) > 0){
    hgtExact_colname <- city_meta$HEIGHT_EXACT
    hgtClass_colname <- city_meta$HEIGHT_CLASS
    
    colList <- c(hgtExact_colname, hgtClass_colname) %>% 
      .[!. %in% c('')]
    
    city <- read_csv(here('data/preprocess', filename), 
                     col_types = cols(.default = col_character()))
    hgtCols <- select(city, all_of(colList)) %>% 
      add_column(hgtClean = NA)
    
    joinxCols <- c()
    joinyCols <- c()
    
    # rename height class col if exists
    tryCatch({
      hgtCols$hgtClass = hgtCols[[hgtClass_colname]]
    }, error = function(e) {}
    )
    
    # rename height exact col if exists
    tryCatch({
      hgtCols$hgtExact = hgtCols[[hgtExact_colname]]
    }, error = function(e) {}
    )
    
    hgtCols <- hgtCols[!duplicated(hgtCols[colList]),]
    
    if ('hgtClass' %in% names(hgtCols)){ 
      hgtCols <- hgt_classes(hgtCols) 
      joinxCols <- append(joinxCols, 'HEIGHT_CLASS')
      joinyCols <- append(joinyCols, 'hgtClassOrig')
      if ('hgtExact' %in% names(hgtCols)){ 
        hgtCols <- hgtCols %>% 
          mutate(hgtExactOrig = hgtExact, 
                 hgtClean = ifelse(is.na(hgtExact), hgtClassCalc, hgtExact), 
                 hgtClean = as.numeric(gsub('[^0-9.-]', '', hgtClean)))
        joinxCols <- append(joinxCols, 'HEIGHT_EXACT')
        joinyCols <- append(joinyCols, 'hgtExactOrig')
      } else {
        hgtCols <- hgtCols %>% 
          mutate(hgtClean = hgtClassCalc)
      }
    } else if ('hgtExact' %in% names(hgtCols)){
      hgtCols <- hgtCols %>% 
        mutate(hgtExactOrig = hgtExact, 
               # hgtClean = as.numeric(gsub("\\'", '', hgtExact)))
               hgtClean = as.numeric(gsub('[^0-9.-]', '', hgtExact)))
      joinxCols <- append(joinxCols, 'HEIGHT_EXACT')
      joinyCols <- append(joinyCols, 'hgtExactOrig')
    } 
    
    # convert feet to meters
    hgtCols <- hgtCols %>% 
      mutate(hgtClean = round(hgtClean * 0.3048, 2)) %>% 
      select(any_of(c("hgtClassOrig", "hgtExactOrig", "hgtClean")))
  } else {
    # hgtCols <- data.frame(hgtClean = NA)
    joinxCols <- c()
    joinyCols <- c()
  }
  
  return(list('xCols' = joinxCols, 
              'yCols' = joinyCols, 
              'hgtEdited' = hgtCols))
}

# convert dbh class to exact DBH  
hgt_classes <- function(df){
  df_diag <- df %>% 
    mutate(hgtClassOrig = hgtClass, 
           hgtClass = str_replace_all(hgtClass, c('to' = '-')), 
           hgtBound = ifelse(grepl('[T+<>]', hgtClass), 1, 0), 
           hgtClass = gsub('[^0-9.-]', '', hgtClass),
           hgtClass = ifelse(hgtBound == 1, paste0(hgtClass, '-', hgtClass), hgtClass)) %>% 
    separate(hgtClass, 
             into = c("hgt_lb", "hgt_ub"), 
             sep = c('-'), 
             remove = FALSE) %>% 
    mutate(across(c(hgt_lb, hgt_ub), 
                  trimws), 
           hgt_lb = str_replace_all(hgt_lb, c("'" = '')),
           hgt_ub = str_replace_all(hgt_ub, c("'" = '')), 
           hgt_ub = ifelse(hgt_ub == '', hgt_lb, hgt_ub), 
           across(c(hgt_lb, hgt_ub), 
                  as.numeric)) %>% 
    mutate(hgtClassCalc = rowMeans(.[,c("hgt_lb", "hgt_ub")], na.rm = FALSE))
  
  return(df_diag)
}


# RUNNING EXAMPLES =============================
# Pulls Ithaca inventory and flags trees with invalid DBH
# baltimore_dbh <- pull_dbh('baltimore_md.csv')

# DBH Exact only - ithaca_ny.csv
# DBH Class only - toledo_oh.csv
# DBH Exact/Class - norfolk_va.csv