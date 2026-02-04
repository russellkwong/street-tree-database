## ---------------------------
##
## Script name: qualityControl_spp.R
##
## Purpose of script: 
##   Quality control script to extract species names and run treemendous 
##
## Author: Russell Kwong
##
## Date Updated: 2025-12-04
##
## Email: rk584@cornell.edu
##
## ---------------------------
##
## Notes:
## SCRIPT R2 will send a separately compiled lists of (1) SPPBOT, 
## (2) SPPCOM, (3) SPPOTHER
##
## ---------------------------

# install packages
# install.packages('fuzzyjoin')

# load packages
library('devtools')
library('here')
library('fuzzyjoin')
library('treemendous')
library('data.table')
library('tidyverse')

# set directory location
i_am('scripts/qualityControl_spp.R')

# load USDA PLANTS database
usda_plants <- read_delim(here('scripts/plantlst.txt'))
spp_crosswalk <- usda_plants %>% 
  rename(ScientificName = "Scientific Name with Author", 
         CommonName = "Common Name") %>% 
  mutate(ScientificName = str_replace_all(ScientificName, '×', 'x')) %>% 
  mutate(SPP_BOT = str_match(ScientificName, '\\b\\w+\\b\\s\\b\\w+\\b')) %>% 
  mutate(SPP_BOT = ifelse(str_count(SPP_BOT[, 1], '[:upper:]') == 2, 
                          str_match(ScientificName, '^.?[^\\s]+'), 
                          SPP_BOT))

colnames(spp_crosswalk) <- lapply(colnames(spp_crosswalk), 
                                  FUN = colname_format)
# =============
# (1) SPPBOT MATCHING
#
# Notes:
# Input for spp_bot() is a list of unique botanical species names.
# tmd_exact is a crosswalk table for Botanical Name -> Botanical Name (clean)
# tmd_fuzzy must be manually edited for accurate crosswalk of 
#   Botanical Name -> Botanical Name (clean)
# =============

# processing compiled list of SPPBOT
spp_diag2 <- function(spp_list){
  city_matching <- spp_list %>% 
    mutate(BOT_ORIG = SPP_BOT, 
           # remove common name note (ithaca)
           SPP_BOT = ifelse(str_detect(SPP_BOT, " - "), 
                            str_extract(SPP_BOT, ".+(?= -)"), 
                            SPP_BOT), 
           # replace 'X' with 'x' for hybrid in name (ithaca)
           SPP_BOT = str_replace_all(SPP_BOT, 
                                     pattern = ' X ', 
                                     replacement = ' x '),
           # remove cultivar in single quotes
           Cultivar = str_extract(SPP_BOT, "((?<=\').+).+(?=\')"), 
           SPP_BOT = ifelse(str_detect(SPP_BOT, "'"), 
                            str_extract(SPP_BOT, ".+(?= \')"), 
                            SPP_BOT),
           # remove hybrid after 'x' 
           Hybrid = str_extract(SPP_BOT, "(?<= x ).+"), 
           SPP_BOT = ifelse(str_detect(SPP_BOT, " x "), 
                            str_extract(SPP_BOT, ".+(?= x )"), 
                            SPP_BOT)) %>% 
    separate_wider_delim(SPP_BOT, 
                         delim = ' ', 
                         names = c("Genus", "Species"), 
                         too_few = 'align_start', 
                         too_many = 'drop') %>% 
    mutate(Species = ifelse(is.na(Species), Hybrid, Species)) %>% 
    # if species name is 'x', take first word of hybrid
    mutate(Species = ifelse(Species == 'x', 
                            ifelse(str_sub(Hybrid, 1, 1) == "'", 
                                   Species, 
                                   str_extract(str_c(Hybrid, ' '), "^[^ ]*")), 
                            Species)) %>% 
    mutate(Genus = str_to_sentence(Genus), 
           Species = str_to_lower(Species), 
           Species = str_trim(Species))
  
  # remove parentheses from species name
  city_matching <- city_matching %>% 
    mutate(Genus = str_replace_all(Genus, '\\(', ''), 
           Species = str_replace_all(Species, '\\)', ''))
  
  city_clean <- city_matching %>%
    filter(Genus != '') %>% 
    filter(!is.na(Species)) %>% 
    select(c(Genus, Species)) %>% 
    mutate(Genus = str_to_sentence(Genus), 
           Species = str_to_lower(Species), 
           Species = str_trim(Species)) %>% 
    mutate(Genus = str_replace_all(Genus, "[^[:alpha:]]", ""), 
           Species = str_replace_all(Species, "[^[:alpha:]]", "")) %>% 
    distinct(Genus, Species) %>% 
    filter(Genus != '') %>% 
    mutate(Genus = str_to_sentence(Genus), 
           Species = str_to_lower(Species))
  
  result <- city_clean %>% matching(backbone = 'GBIF')
  
  summarize_output(result)
  
  matched_res <- city_matching %>% 
    select(c(BOT_ORIG, Genus, Species, Cultivar, Hybrid)) %>%
    merge(select(result,
                 c(Orig.Genus, Orig.Species, 
                   Matched.Genus, Matched.Species, matched, direct_match)), 
          by.x = c("Genus", "Species"), 
          by.y = c("Orig.Genus", "Orig.Species"), 
          all.x = TRUE)
  
  return(matched_res)
}

# =============
# (2) SPPCOMMON MATCHING
# 
# Notes: 
# Input for spp_com() is a list of unique common species names.
# spp_com_exact is a crosswalk table for Common Name -> Botanical Name
# spp_com_fuzzy must be manually edited for accurate crosswalk of 
#   Common Name -> Botanical Name
# =============


nameFLFormatting <- function(spp_namedf, spp_form){
  # one comma
  spp_namedf <- spp_namedf %>% 
    mutate(spp_form = str_count(SPP_COM, ','), 
           form1 = str_squish(sub('^(.*), (.*)$', '\\2 \\1', SPP_COM)), 
           form2 = str_squish(sub('^(.*), (.*), (.*)$', '\\2 \\1, \\3', SPP_COM))) %>% 
    rowwise() %>% 
    mutate(CommonName = ifelse(spp_form == 1, 
                                  form1, form2)) %>% 
    separate_wider_delim(CommonName, 
                         delim = ', ', 
                         names = c("CommonName", "Variety"), 
                         too_few = 'align_start', 
                         too_many = 'drop')
  
  return(select(spp_namedf, c(SPP_COM, CommonName, Variety)))
}

spp_com <- function(spp_list){
  spp_list_clean <- nameFLFormatting(spp_list)
  spp_com_matched <- stringdist_join(x = spp_list_clean, 
                                     y = select(spp_crosswalk, c(CommonName, SPP_BOT)),  
                  by = c('CommonName' = 'CommonName'), 
                  max_dist = 0.1, 
                  method = 'jw', 
                  mode = 'left', 
                  ignore_case = TRUE, 
                  distance_col = "distance")
  spp_com_matched %>% 
    group_by(SPP_COM) %>% 
    slice(which.min(distance)) %>% 
    return()
}

# =============
# (3) SPPCODE MATCHING
# 
# Notes: 
# Input for spp_code() is a list of unique common species codes.
# EDIT spp_com_exact is a crosswalk table for Common Name -> Botanical Name
# EDIT spp_com_fuzzy must be manually edited for accurate crosswalk of 
#   Common Name -> Botanical Name
# =============

spp_code <- function(spp_list){
  sppcode_step1 <- merge(spp_list, 
                         spp_crosswalk, 
                         by.x = "SPP_CODE", 
                         by.y = "Synonym.Symbol", 
                         all.x = TRUE) 
  
  sppcode_step2 <- merge(select(filter(sppcode_step1, is.na(SPP_BOT)), SPP_CODE), 
                         filter(spp_crosswalk, is.na(Synonym.Symbol)), 
                         by.x = "SPP_CODE", 
                         by.y = "Symbol", 
                         all.x = TRUE)
  
  sppcode_step1 <- filter(sppcode_step1, !is.na(SPP_BOT))
  sppcode_cross <- rbind(select(sppcode_step1, all_of(c("SPP_CODE", "SPP_BOT"))), 
                         select(sppcode_step2, all_of(c("SPP_CODE", "SPP_BOT")))) %>%
    rename(SPPCODE_edit = 'SPP_BOT')
  
  return(sppcode_cross)
}

# =============
# (4) UNMATCHED CROSSWALK
# 
# Notes: 
# Input for spp_code() is a list of unique common species codes.
# EDIT spp_com_exact is a crosswalk table for Common Name -> Botanical Name
# EDIT spp_com_fuzzy must be manually edited for accurate crosswalk of 
#   Common Name -> Botanical Name
# =============

R2spp_crosswalk <- read.csv(here('data/sppcom_cross0121.csv')) %>% 
  rename(SPPMATCH_EDIT = "SPP_EDIT", 
         CULMATCH_EDIT = "CUL_EDIT") %>% 
  .[, !names(.) %in% c("ID")]

