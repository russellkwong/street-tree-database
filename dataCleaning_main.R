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
  .[. != 'toledo_oh.csv'] %>% 
  .[. != 'pinellas_fl.csv']

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
    select(all_of(unlist(city_colList[-(1:5)]))) %>% 
    mutate(CITY = city_colList$FILENAME)
  
  city_dbh <- pull_dbh2(filename)
  
  city_inv <- merge(city_inv, city_dbh$dbhEdited, 
                    by.x = city_dbh$xCols, 
                    by.y = city_dbh$yCols, 
                    all.x = TRUE)
  
  if (!is.null(city_colList$HEIGHT_CLASS) | 
      !is.null(city_colList$HEIGHT_EXACT)){ 
    city_hgt <- pull_height2(filename)
  
    city_inv <- merge(city_inv, city_hgt$hgtEdited, 
                      by.x = city_hgt$xCols, 
                      by.y = city_hgt$yCols, 
                      all.x = TRUE)
  }
  
  # Calculate crown radius in meters
  crwColList <- list(crwRad = city_meta$CROWNRAD, 
                     crwWid1 = city_meta$CROWN1, 
                     crwWid2 = city_meta$CROWN2, 
                     crwArea = city_meta$CROWNAREA)
  
  # Remove column names if no info in metadata
  crwColList <- crwColList[crwColList != '']
  
  # Crown radius conversion stored in $crwCross
  # cols to join stored in $xCols and $yCols
  if (length(crwColList) > 0){
    city_crown <- crownRad_calc(filename)
    
    city_inv <- merge(city_inv, 
                      select(city_crown$crwCross, any_of(c(city_crown$yCols, "crwClean"))), 
                      by.x = city_crown$xCols[1:length(city_crown$yCols)], 
                      by.y = city_crown$yCols, 
                      all.x = TRUE)
  }

  # format dates
  city_inv <- city_inv %>% 
    mutate(across(any_of(c("DATE_INV", "DATE_UPDATE", "DATE_PLANT")), 
                  ~ datetime_format(.x)))
  
  # format condition
  if (!is.null(city_colList$CONDITION)) {
    city_inv <- conditionFormat(city_inv)
  }
  

#   botSPP_list <- rbind(botSPP_list, unique(select(city_inv, any_of(c("SPP_BOT")))))
  
#   comSPP_list <- rbind(comSPP_list, select(city_inv, SPP_COM))

  # format tree age_yr column (round to one decimal)
  city_inv <- city_inv %>% 
    mutate(across(any_of(c("AGE_YR")), ~ round(as.numeric(.x), 1)))
  
  return(city_inv)
}

# Extracted columns
R1LIST <- lapply(X = filelist, FUN = pull_inv) %>% 
  do.call(bind_rows, .) 

SPPBOT_list <- data.frame(SPP_BOT = unique(R1LIST$SPP_BOT))

SPPCODE_list <- data.frame(SPP_CODE = unique(R1LIST$SPP_CODE))

# filtering matching from SPP_BOT treemendous
SPPBOT_tmd <- spp_diag2(SPPBOT_list)
# write_csv(SPPBOT_tmd, here('data/files0306/SPPBOT_tmd.csv'))

SPPBOT_exact <- filter(SPPBOT_tmd, matched == TRUE) %>% 
  mutate(Matched.Species = gsub('^spec$', 'spp.', Matched.Species), 
         SPPBOT_tmd = paste(Matched.Genus, Matched.Species)) %>% 
  select(c("BOT_ORIG", "SPPBOT_tmd")) 

SPPBOT_fuzzy <- filter(SPPBOT_tmd, matched == FALSE | is.na(matched))

# filtering matching from SPP_COM 
# DEPR SPPCOM_matching <- spp_com(unique(select(SPPCOM_list2, SPP_COM)))

# upgrade sppcom with zoom
# DEPR SPPCOM_matching <- spp_com_zoom(unique(select(SPPCOM_list2, SPPCOM_clean)))
SPPCOM_matching <- sppcom_zoom(unique(R1LIST$SPP_COM))
  
SPPCOM_cross <- SPPCOM_matching$SPPCOM_cross

# SPPCOM_cross <- merge(SPPCOM_list2, SPPCOM_cross, 
#                       by.x = "SPPCOM_clean", by.y = "SPPCOM_clean", 
#                       all.x = TRUE) %>% 
#   select(-c("SPPCOM_clean"))

SPPCOM_fuzzy <- SPPCOM_matching$zoom_fuzzy
# export and manually match names with SPPCOM_fuzzy

# matching from SPP_CODE
sppcode_cross <- spp_code(drop_na(data.frame("SPP_CODE" = unique(R1LIST$SPP_CODE))))

# JOIN BACK WITH R1LIST

R2LIST <- merge(R1LIST, SPPBOT_exact, 
                by.x = "SPP_BOT", by.y = "BOT_ORIG", 
                all.x = TRUE) 
R2LIST <- merge(R2LIST, SPPCOM_cross, 
                by.x = "SPP_COM", by.y = "SPP_COM_ORIG", 
                all.x = TRUE) 
R2LIST <- merge(R2LIST, sppcode_cross, 
                by = "SPP_CODE", 
                all.x = TRUE)
rm(SPPBOT_exact, SPPBOT_fuzzy, SPPCOM_cross, SPPCOM_fuzzy)


R2LIST_nomatch <- R2LIST %>% 
  filter(is.na(SPPBOT_tmd) & is.na(SPPCOM_cross) & is.na(SPPCODE_cross)) %>% 
  select(c("SPP_COM", "SPP_BOT", "SPP_OTHER", "SPP_CODE", "GENUS", "SPECIES", "CULTIVAR", 
           "CITY", "SPPBOT_tmd", "SPPCOM_cross", "SPPCODE_cross")) %>% 
  unique()

# write_csv(R2LIST_nomatch, here('data/files0306/unmatch_sp26310.csv'))

R2tmdcross <- read.csv(here('data/files0306/SPPBOT_tmdcross_0319.csv'))

# R3LIST <- merge(R2LIST, R2spp_crosswalk, 
#                 by = c("SPP_COM", "SPP_BOT", "SPP_OTHER", "SPP_CODE", "GENUS", "SPECIES", "CULTIVAR"), 
#                 all.x = TRUE)

R2LIST <- merge(R2LIST, select(R2tmdcross, c("BOT_ORIG", "SPPBOT_cross", "SPPBOTVAR_cross")), 
                by.x = "SPP_BOT", by.y = "BOT_ORIG", 
                all.x = TRUE) 

R2LIST <- merge(R2LIST, R2spp_crosswalk, 
                by = c("SPP_COM", "SPP_BOT", "SPP_OTHER", "SPP_CODE", "GENUS", "SPECIES", "CULTIVAR"), 
                all.x = TRUE) %>% 
  rename(UNMATCH_cross = "SPPMATCH_EDIT", 
         UNMATCHvar_cross = "CULMATCH_EDIT") 

R2LIST <- mutate(R2LIST, INV_GESP = 
                   case_when(!is.na(GENUS) & !is.na(SPECIES) ~ paste(GENUS, SPECIES), 
                             !is.na(GENUS) & is.na(SPECIES) ~ GENUS, 
                             .default = NA))

city_dbh_check <- group_by(R3LIST, CITY) %>% 
  summarise(invalid_dbh = sum(dbh_flag))

R2LIST <- mutate(R2LIST, SPP_FINAL = coalesce(SPPBOT_tmd, SPPCOM_cross, 
                                              SPPCODE_cross, SPPBOT_cross, 
                                              UNMATCH_cross, INV_GESP), 
                 VAR_FINAL = coalesce(CULTIVAR, COM_CULTIVAR, 
                                      SPPBOTVAR_cross, UNMATCHvar_cross))

# R2LIST_0319 SAVED BEFORE CONDITION FORMAT
# write_csv(R2LIST, here('data/files0306/R2LIST_0324.csv'))
  
R3LIST_isnafinal <- filter(R3LIST, is.na(SPP_FINAL)) %>% 
  select(all_of(c("SPP_BOT", "SPP_COM", "SPP_CODE", "SPP_OTHER", 
                  "GENUS", "SPECIES", "CULTIVAR"))) %>% 
  unique()

# Delete trees with invalid DBH or species name
R3LIST <- filter(R2LIST, dbh_flag == 0) %>% 
  filter(!is.na(SPP_FINAL) & SPP_FINAL != 'REMOVE')

# Delete intermediate columns
R3LIST <- select(R3LIST, 
                 -c("dbh_flag", "DBH_EXACT", "DBH_CLASS", "CONDITION", 
                    "HEIGHT_EXACT", "HEIGHT_CLASS", "CROWN1", "CROWN2", 
                    "CROWNRAD", "CROWNAREA", 
                    "SPPBOT_tmd", "SPPCOM_cross", "SPPCODE_cross", 
                    "SPPBOT_cross", "UNMATCH_cross", 
                    "SPP_BOT", "SPP_COM", "SPP_OTHER", "SPP_CODE", 
                    "GENUS", "SPECIES", "INV_GESP", 
                    "CULTIVAR", "COM_CULTIVAR", "SPPBOTVAR_cross", "UNMATCHvar_cross"))


R3LIST <- merge(R3LIST, select(ref_species, c("SCIENTIFIC_NAME", "SPCD")), 
                by.x = "SPP_FINAL", by.y = "SCIENTIFIC_NAME", all.x = TRUE)

SPP_align_xwalk <- SPP_alignment(unique(select(R3LIST, SPP_FINAL)))
# SPP_align_xwalk <- gen_spcdxwalk(SPP_gbiflookup)

R3LIST <- R3LIST %>% 
  mutate(species_clean = normalize_species(SPP_FINAL)) %>% 
  merge(select(SPP_align_xwalk, c("species_clean", "gbif_canonicalName")), 
        by.x = "species_clean", by.y = "species_clean", 
        all.x = TRUE) %>% 
  select(-c("species_clean"))

R3LIST <- R3LIST %>% 
  filter(gbif_canonicalName != 'Plantae') %>% 
  separate(col = "gbif_canonicalName", 
           into = c("GENUS", "SPECIES"), 
           sep = ' ', 
           fill = 'right') %>% 
  mutate(SPECIES = ifelse(is.na(SPECIES), 'spp.', SPECIES))

R3LIST <- R3LIST %>% 
  rename(FILENAME = "CITY") %>% 
  merge(select(file_metadata, c("CITY", "STATE_ABBR", "FILENAME")), 
        by.x = "FILENAME", by.y = "FILENAME", all.x = TRUE)

R3LIST %>% filter(!is.na(MULTI_STEMS)) %>% select(MULTI_STEMS) %>% unique()

R3LIST <- formatMultiStems(R3LIST)
R3LIST <- filter(R3LIST, as.numeric(LONG_col) > -93 & 
         as.numeric(LONG_col) < -66 & 
         as.numeric(LAT_col) > 23 & 
         as.numeric(LAT_col) < 48)
R3LIST <- mutate(R3LIST, across(all_of(c("LONG_col", "LAT_col")), 
                                ~as.numeric(.x)))


R4LIST <- R3LIST %>% 
  select(all_of(c("GENUS", "SPECIES", "VAR_FINAL", 
                  "dbhClean", "LONG_col", "LAT_col", 
                  "CITY", "STATE_ABBR", "hgtClean", "crwClean", "COND_clean", 
                  "STEMS", "MULTI_STEMS", 
                  "DATE_INV", "DATE_UPDATE", "DATE_PLANT", "AGE_YR", "AGE_CLASS", 
                  "TREE_ID")))

colnames(R4LIST) <- 
  c("GENUS", "SPECIES", "VARIETY", 
    "DBH_CM", "LON", "LAT", 
    "CITY", "STATE", "HEIGHT_M", "CROWNRAD_M", "CONDITION", 
    "STEMS_NO", "MULTI_STEM", 
    "INV_YR", "UPDATE_YR", "PLANT_YR", "AGE_YR", "AGE_CLASS", 
    "LOCAL_ID") 

R4LIST <- R4LIST %>% 
  arrange(STATE, CITY, LAT, DBH_CM)
R4LIST$TREEID <- rownames(R4LIST)
R4LIST <- relocate(R4LIST, last_col())

R4_sample <- R4LIST %>% 
  group_by(CITY, STATE) %>% 
  slice_sample(n = 25, replace = FALSE) %>% 
  arrange(as.numeric(TREEID))

R4LIST %>% group_by(CITY, STATE) %>% summarise(n = n()) %>% View()

write_csv(R4LIST, here('data/files0306/R4LIST_0324.csv'))
write_csv(R4_sample, here('data/files0306/R4sample_0324.csv'))


# FIND ISSUES
R3NA <- filter(R3LIST, is.na(SPP_FINAL))

write.csv(R3LIST, here('R3LIST_260202.csv'))

length(unique(R3LIST$SPECIES))

# check treecount
R3_v0319 <- group_by(R3LIST, CITY) %>% 
  summarise(R3_v0319 = n())

treesCountEdit <- merge(treesCountEdit, R3_v0319, 
                        by = "CITY", all = TRUE)

treesCountEdit %>% drop_na() %>% 
  summarise(pre = sum(trees), 
            prePerc = sum(trees) / sum(treesBefore), 
            zoom = sum(postzoom),  
            zoomPerc = sum(postzoom) / sum(treesBefore), 
            post = sum(zoomtidy), 
            postPerc = sum(zoomtidy) / sum(treesBefore)) %>%
  View()



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


R4LIST_sample <- R3LIST %>% 
  mutate(species_clean = normalize_species(SPP_FINAL)) %>% 
  merge(select(SPP_align_xwalk, c("species_clean", "COMMON_NAME", "gbif_usageKey", "SPCD")), 
        by.x = "species_clean", by.y = "species_clean", 
        all.x = TRUE)

R4LIST_sample <- R4LIST_sample %>% 
  select(all_of(c("SPP_FINAL", "COMMON_NAME", "gbif_usageKey", 
                  "SPCD", "CUL_FINAL", "dbhClean", "LONG_col", 
                  "LAT_col", "CITY")))

colnames(R4LIST_sample) <- 
  c("SPECIES", "SPECIES_COM", "gbif_usageKey", "usda_fiadb_SPCD", "CULTIVAR", 
    "DBH_cm", "LON", "LAT", "city_filename", "CITY", "STATE", "FIPS") 

R4LIST_sample <- R4LIST_sample %>% 
  mutate(CITY = case_when(city_filename == 'groton_ct.csv' ~ 'Groton', 
                          city_filename == 'bloomington_il.csv' ~ 'Bloomington'), 
         STATE = case_when(city_filename == 'groton_ct.csv' ~ 'Connecticut', 
                           city_filename == 'bloomington_il.csv' ~ 'Illinois'), 
         FIPS = case_when(city_filename == 'groton_ct.csv' ~ '0934250', 
                          city_filename == 'bloomington_il.csv' ~ '1706613'))

R4LIST_sample %>% 
  arrange(STATE, CITY, LON) %>% View()

write_csv(R4LIST_sample, here('data/files0306/R4_sample.csv'))

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