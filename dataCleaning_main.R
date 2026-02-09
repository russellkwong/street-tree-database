## ---------------------------
##
## Script name: dataCleaning_main.R
##
## Purpose of script: 
##   Main quality control script to remove invalid trees for the database.
##
## Author: Russell Kwong
##
## Date Updated: 2025-11-03
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
i_am('scripts/dataCleaning_main.R')

source(here('scripts/qualityControl_spp.R'))
source(here('scripts/qualityControl_dbh.R'))
source(here('scripts/qualityControl_extra.R'))

# create list of inventories
filelist <- list.files(path = here('data/preprocess'), 
                       pattern = ".csv$", 
                       full.names = FALSE) %>% 
  .[. != 'file_metadata.csv'] %>% 
  .[. != 'providence_ri.csv'] %>% 
  .[. != 'durham_nc.csv'] %>% 
  .[. != 'hamilton_oh.csv'] %>% 
  .[. != 'muskegon_mi.csv'] %>% 
  .[. != 'madison_in.csv'] %>% 
  .[. != 'terrehaute_in.csv'] %>% 
  .[. != 'toledo_oh.csv']

# Read metadata to find which column has the botanical species name
file_metadata <- read.csv(here('data', 'file_metadata.csv'))

# Create collector list for inventories
R2LIST <- data.frame()

# Iterate through each inventory and add to R2LIST
for (x in filelist){
  citydbh <- pull_dbh(x) %>% 
    select(localID, dbhClean, dbh_flag) %>% 
    mutate(CITY = x)
  R2LIST <- rbind(R2LIST, citydbh)
}

# pull all relevant columns
colname_format <- function(field_name){
  colname_f <- gsub('^\\_', 'X_', gsub('\\s', '\\.', field_name))
  return(colname_f)
}

coltype_char <- function(city_field){
  if (length(city_field) > 0){
    return(as.character(city_field))
  }
}

# combine all relevant columns and cleaned DBH
# read_csv allows spaces
pull_inv <- function(filename){
  city_meta <- filter(file_metadata, FILENAME == filename)
  
  print(paste('Reading', filename))
  
  city_colList <- c(city_meta)[!c(city_meta) %in% c('')] %>% 
    within(., rm(notes))
#     lapply(., colname_format) read_csv allows spaces, read.csv does not
  city_inv <- read_csv(here('data/preprocess', city_colList$FILENAME), 
                       col_types = cols(.default = col_character())) %>% 
    select(all_of(unlist(city_colList[-(1:4)]))) %>% 
    mutate(CITY = city_colList$FILENAME)
  
  # format DBH
#   city_dbh <- pull_dbh(filename) %>% 
#     select(localID, dbhClean, dbh_flag) %>% 
#     mutate(localID = as.character(localID))
  
#   city_inv <- merge(city_inv, city_dbh, 
#                     by.x = c("TREE_ID"), 
#                     by.y = c("localID"), 
#                     all.x = TRUE)
  
  city_dbh <- pull_dbh2(filename)
  
  city_inv <- merge(city_inv, city_dbh$dbhEdited, 
                    by.x = city_dbh$xCols, 
                    by.y = city_dbh$yCols, 
                    all.x = TRUE)
  
  # format height
#   city_hgt <- pull_height(filename) %>% 
#     select(localID, hgtClean) %>% 
#     mutate(localID = as.character(localID))
  
#   city_inv <- merge(city_inv, city_hgt, 
#                     by.x = c("TREE_ID"), 
#                     by.y = c("localID"), 
#                     all.x = TRUE)
  
  if (!is.null(city_colList$HEIGHT_CLASS) | 
      !is.null(city_colList$HEIGHT_EXACT)){ 
    city_hgt <- pull_height2(filename)
  
    city_inv <- merge(city_inv, city_hgt$hgtEdited, 
                      by.x = city_hgt$xCols, 
                      by.y = city_hgt$yCols, 
                      all.x = TRUE)
  }

  # format dates
  city_inv <- city_inv %>% 
    mutate(across(any_of(c("DATE_INV", "DATE_UPDATE", "DATE_PLANT")), 
                  ~ datetime_format(.x)))
  
#  city_inv <- city_inv %>% 
#    mutate(DATE_INV = datetime_format(DATE_INV))
  
  # calculate canopy area
  # if (filename == 'arlington_va.csv'){
  #   city_canArea <- canopyArea_calc(filename, 1) %>% 
  #     select(localID, canopyArea) 
  #   city_inv <- merge(city_inv, city_canArea, 
  #                     by.x = c("TREE_ID"), 
  #                     by.y = c("localID"))
  # } else if (filename == 'capecoral_fl.csv'){
  #   city_canArea <- canopyArea_calc(filename, 3) %>% 
  #     select(localID, canopyArea)
  #   city_inv <- merge(city_inv, city_canArea, 
  #                     by.x = c("TREE_ID"), 
  #                     by.y = c("localID"))
  # }
  
#   botSPP_list <- rbind(botSPP_list, unique(select(city_inv, any_of(c("SPP_BOT")))))
  
#   comSPP_list <- rbind(comSPP_list, select(city_inv, SPP_COM))

  return(city_inv)
}

# Extracted columns
R1LIST <- lapply(X = filelist, FUN = pull_inv) %>% 
  do.call(bind_rows, .) 

SPPBOT_list <- data.frame(SPP_BOT = unique(R1LIST$SPP_BOT))
SPPCOM_list <- data.frame(SPP_COM = unique(R1LIST$SPP_COM))

SPPCOM_list2 <- SPPCOM_list %>% 
  rename(SPP_COM_ORIG = "SPP_COM") %>% 
  mutate(SPP_COM = SPP_COM_ORIG, 
         SPP_COM = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", SPP_COM)) %>% 
  mutate(COM_CULTIVAR = str_extract(SPP_COM, "(?<=\\').*(?=\\')")) %>% 
  mutate(SPP_COM = gsub("\\s*\\'[^']+\\'", '', SPP_COM), 
         SPP_COM = gsub("[^[:alpha:], ']", '', SPP_COM), 
         SPP_COM = str_to_lower(trimws(str_squish(SPP_COM))), 
         SPP_COM = ifelse(str_sub(SPP_COM, start = -1, end = -1) == ',', 
                          str_sub(SPP_COM, start = 1, end = -2), 
                          SPP_COM)) 

SPPOTHER_list <- data.frame(SPP_OTHER = unique(R1LIST$SPP_OTHER))
SPPCODE_list <- data.frame(SPP_CODE = unique(R1LIST$SPP_CODE))

# filtering matching from SPP_BOT treemendous
SPPBOT_tmd <- spp_diag2(SPPBOT_list)

SPPBOT_exact <- filter(SPPBOT_tmd, matched == TRUE) %>% 
  mutate(Matched.Species = gsub('^spec$', 'spp.', Matched.Species), 
         Matched.Species = gsub('^speciosa$', 'spp.', Matched.Species), 
         SPP_edit = paste(Matched.Genus, Matched.Species)) %>% 
  select(c("BOT_ORIG", "SPP_edit")) 

SPPBOT_fuzzy <- filter(SPPBOT_tmd, matched == FALSE | is.na(matched))

# filtering matching from SPP_COM 
SPPCOM_matching <- spp_com(unique(select(SPPCOM_list2, SPP_COM)))

SPPCOM_exact <- filter(SPPCOM_matching, distance == 0) %>% 
  rename(SPPCOM_edit = "SPP_BOT") %>% 
  select("SPP_COM", "SPPCOM_edit")

SPPCOM_fuzzy <- filter(SPPCOM_matching, distance > 0)
# export and manually match names with SPPCOM_fuzzy

# matching from SPP_CODE
sppcode_cross <- spp_code(drop_na(data.frame("SPP_CODE" = unique(R1LIST$SPP_CODE))))

# JOIN BACK WITH R1LIST

R2LIST <- merge(R1LIST, SPPBOT_exact, 
                by.x = "SPP_BOT", by.y = "BOT_ORIG", 
                all.x = TRUE) 
R2LIST <- merge(R2LIST, SPPCOM_exact, 
                by.x = "SPP_COM", by.y = "SPP_COM", 
                all.x = TRUE) 
R2LIST <- merge(R2LIST, sppcode_cross, 
                by = "SPP_CODE", 
                all.x = TRUE)
rm(SPPBOT_exact, SPPBOT_fuzzy, SPPCOM_exact, SPPCOM_fuzzy)


R2LIST_nomatch <- R2LIST %>% 
  filter(is.na(SPP_edit) & is.na(SPPCOM_edit) & is.na(SPPCODE_edit)) %>% 
  select(c("SPP_COM", "SPP_BOT", "SPP_OTHER", "SPP_CODE", "GENUS", "SPECIES", "CULTIVAR", 
           "SPP_edit", "SPPCOM_edit", "SPPCODE_edit")) %>% 
  unique()

write.csv(R2LIST_nomatch, here('unmatch_sp260202.csv'))

R2LIST <- conditionFormat(R2LIST)

R3LIST <- merge(R2LIST, R2spp_crosswalk, 
                by = c("SPP_COM", "SPP_BOT", "SPP_OTHER", "SPP_CODE", "GENUS", "SPECIES", "CULTIVAR"), 
                all.x = TRUE)

city_dbh_check <- group_by(R3LIST, CITY) %>% 
  summarise(invalid_dbh = sum(dbh_flag))

R3LIST <- mutate(R3LIST, SPP_FINAL = coalesce(SPP_edit, SPPCOM_edit, SPPCODE_edit, SPPMATCH_EDIT))
  
R3LIST_isnafinal <- filter(R3LIST, is.na(SPP_FINAL)) %>% 
  select(all_of(c("SPP_BOT", "SPP_COM", "SPP_CODE", "SPP_OTHER", 
                  "GENUS", "SPECIES", "CULTIVAR"))) %>% 
  unique()

R3LIST_isnafinal <- unique(R3LIST_isnafinal)

R3LIST <- filter(R3LIST, dbh_flag == 0)

R3LIST <- filter(R3LIST, SPP_FINAL != 'REMOVE') %>% 
  filter(!is.na(SPP_FINAL)) 


R3LIST <- R3LIST %>% 
  mutate(CULMATCH_EDIT = na_if(CULMATCH_EDIT, '')) %>% 
  mutate(CUL_FINAL = coalesce(CULTIVAR, CULMATCH_EDIT))

R3LIST <- R3LIST[, !names(R3LIST) %in% 
                   c("SPP_BOT", "SPP_COM", "SPP_OTHER", "SPP_CODE", 
                     "GENUS", "SPECIES", "CULTIVAR", 
                     "DBH_EXACT", "DBH_CLASS", "dbh_flag", 
                     "HEIGHT_EXACT", "HEIGHT_CLASS", 
                     "SPP_edit", "SPPCOM_edit", "SPPCODE_edit", 
                     "SPPMATCH_EDIT", "CULMATCH_EDIT")] 

R3LIST <- R3LIST %>% 
  select(all_of(c("SPP_FINAL", "CUL_FINAL", "dbhClean", 
                "LONG_col", "LAT_col", "CITY", "CONDITION", 
                "hgtClean", "STATUS", "STEMS", 
                "DATE_INV", "DATE_UPDATE", "DATE_PLANT", "AGE", 
                "CROWN1", "CROWN2", "CIRC_EXACT", "RISK", "FAILURE", "WIDTH",
                "TREE_ID")))

colnames(R3LIST) <- 
  c("SPECIES", "CULTIVAR", "DBH", "LON", "LAT", "CITY", "CONDITION", 
    "HEIGHT", "STATUS", "STEMS", "DATE_INV", "DATE_UPDATE", "DATE_PLANT", "AGE", 
    "CROWN1", "CROWN2", "CIRC_EXACT", "RISK", "FAILURE", "WIDTH", "LOCAL_ID")

R4LIST <- R3LIST %>% 
  arrange(CITY, LOCAL_ID, DBH, LAT)
R4LIST$TREEID <- rownames(R4LIST)
R4LIST <- relocate(R4LIST, last_col())

# FIND ISSUES
R3NA <- filter(R3LIST, is.na(SPP_FINAL))

write.csv(R3LIST, here('R3LIST_260202.csv'))

length(unique(R3LIST$SPECIES))

## EXTRA COMMANDS

unique(select(R1LIST, "CONDITION"))

filelist = filelist[1:10]

file_metadata[153, "FILENAME"] <- 'shrewsbury_ma.csv'

nrow(filter(R1LIST, dbh_flag == 0))

R1LIST %>% select(c("CITY", "TREE_ID")) %>% 
  group_by(CITY) %>% 
  summarise(total_row = n(), 
            na_count = sum(is.na(TREE_ID)), 
            validTree = total_row - na_count, 
            uniqueID = length(unique(TREE_ID)))

city_inv <- read_csv(here('data/preprocess/columbus_oh.csv'), 
                     col_types = cols(.default = col_character()))
city_inv <- pull_inv('annarbor_mi.csv') 
city_inv <- pull_dbh2('buffalo_ny.csv')

print(city_inv$xCols)

na_count <- sapply(city_inv, function(y) sum(length(unique(y)))) %>% 
  data.frame()


file_metadata[24, "LONG_col"] <- 'X'

unique(select(city_inv, CONDITION))

# DIAGNOSTICS ================
print(sum(R2LIST$dbh_flag == 1))

file_metadata[55, "CONDITION"] <- 'COND'

filelist <- filelist[1:10]

R2DIAG <- R2LIST %>% 
  group_by(CITY) %>% 
  summarise(invalid = sum(dbh_flag == 1), 
            total = n()) %>% 
  mutate(perc_inv = round(invalid / total, 2))

View(R2DIAG)

285821/297015

rochester <- pull_dbh('lowell_ma.csv')

sum(R1LIST$dbh_flag == 0)

write.csv(R1LIST, here('R1LIST.csv'))

# before: 87.4%
# after: 89.4%

# dbhflag 0 before: 95.9%
# dbhflag 0 after: 96.2%