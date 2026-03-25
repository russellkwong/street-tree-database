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
# install.packages('zoomerjoin')

# load packages
library('devtools')
library('here')
library('zoomerjoin')
library('treemendous')
library('data.table')
library('tidyverse')

# set directory location
i_am('scripts/qualityControl_spp.R')

# read Alex sp work up.R file
source(here('scripts/sp work up.R'))

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

spp_crosswalk <- spp_crosswalk %>% 
  mutate(str_bot_len = str_length(ScientificName)) %>% 
  .[with(., order(SPP_BOT, str_bot_len)), ] 

spp_crosswalk <- spp_crosswalk[!duplicated(spp_crosswalk$SPP_BOT), ]
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
           # remove double spaces and trailing spaces
           SPP_BOT = str_squish(SPP_BOT), 
           # remove common name note (ithaca)
           SPP_BOT = ifelse(str_detect(SPP_BOT, " - "), 
                            str_extract(SPP_BOT, ".+(?= -)"), 
                            SPP_BOT), 
           # replace X. and ×. with 'x'
           SPP_BOT = str_replace_all(SPP_BOT, 
                                     pattern = regex('X\\.{0,1}+\\s|×\\.{0,1}', 
                                                     ignore_case = TRUE), 
                                     replacement = 'x '), 
           # replace double quotes and angled with single quotes
           SPP_BOT = gsub('[\u2018\u2019\u201A\u201B\u2032\u2035\u22\u201C\u201D]',
                          "'", SPP_BOT), 
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
                         names = c("Genus", "Species", "Extra"), 
                         too_few = 'align_start', 
                         too_many = 'merge') %>% 
    mutate(Species = ifelse(is.na(Species), Hybrid, Species)) %>% 
    # if species name is 'x', take first word of hybrid
    mutate(Species = ifelse(Species == 'x', 
                            ifelse(str_sub(Hybrid, 1, 1) == "'", 
                                   Species, 
                                   str_extract(str_c(Hybrid, ' '), "(\\w+)\\1")), 
    #                                str_extract(str_c(Hybrid, ' '), "^\\w+")), 
                            Species)) %>% 
    mutate(Genus = str_to_sentence(Genus), 
           Species = str_to_lower(Species), 
           Species = str_trim(Species))
  
  # remove non-alpha characters from species name
  city_matching <- city_matching %>% 
    mutate(Genus = str_replace_all(Genus, '[()"\'.,]', ''), 
           Species = str_replace_all(Species, '[()"\'.,]', '')) %>% 
    # consolidate extra and cultivar cols
    mutate(Cultivar = 
             ifelse(is.na(Cultivar), 
                    Extra, 
                    ifelse(!is.na(Extra), 
                           paste(Extra, Cultivar), 
                           Cultivar)))
  
  View(city_matching)
  
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

sppcom_prep <- function(sppcom_list){
  SPPCOM_list <- data.frame(SPP_COM_ORIG = sppcom_list) %>% 
    mutate(SPPCOM_clean = SPP_COM_ORIG, 
           SPPCOM_clean = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", SPPCOM_clean)) %>% 
    mutate(COM_CULTIVAR = str_extract(SPPCOM_clean, "(?<=\\').*(?=\\')")) %>% 
    mutate(SPPCOM_clean = gsub("\\s*\\'[^']+\\'", '', SPPCOM_clean), 
           SPPCOM_clean = gsub("[^[:alpha:], ']", '', SPPCOM_clean), 
           SPPCOM_clean = str_to_lower(trimws(str_squish(SPPCOM_clean))), 
           SPPCOM_clean = ifelse(str_sub(SPPCOM_clean, start = -1, end = -1) == ',', 
                                 str_sub(SPPCOM_clean, start = 1, end = -2), 
                                 SPPCOM_clean)) 
  
  return(SPPCOM_list)
}

nameFLFormatting <- function(spp_namedf, spp_form){
  # one comma
  spp_namedf <- spp_namedf %>% 
    mutate(spp_form = str_count(SPPCOM_clean, ','), 
           form1 = str_squish(sub('^(.*), (.*)$', '\\2 \\1', SPPCOM_clean)), 
           form2 = str_squish(sub('^(.*), (.*), (.*)$', '\\2 \\1, \\3', SPPCOM_clean))) %>% 
    rowwise() %>% 
    mutate(CommonName = ifelse(spp_form == 1, 
                                  form1, form2)) %>% 
    separate_wider_delim(CommonName, 
                         delim = ', ', 
                         names = c("CommonName", "Variety"), 
                         too_few = 'align_start', 
                         too_many = 'drop')
  
  return(select(spp_namedf, c(SPPCOM_clean, CommonName, Variety)))
}

spp_com <- function(spp_list){
  spp_list_clean <- nameFLFormatting(spp_list) 
  spp_com_matched <- stringdist_join(x = spp_list_clean, 
                                     y = select(spp_crosswalk, c(CommonName, SPP_BOT)),  
                  by = c('CommonName' = 'CommonName'), 
                  max_dist = 0.15, 
                  method = 'jw', 
                  mode = 'left', 
                  ignore_case = TRUE, 
                  distance_col = "distance")
  spp_com_matched %>% 
    group_by(SPP_COM) %>% 
    slice(which.min(distance)) %>% 
    return()
}

sppcom_zoom <- function(sppcom_list){
  spp_orig <- sppcom_prep(sppcom_list)
  spp_clean <- unique(select(spp_orig, SPPCOM_clean))
  
  spp_clean <- nameFLFormatting(spp_clean) %>% 
    drop_na(CommonName) 
  print(nrow(spp_clean))
  spp_com_matched <- jaccard_left_join(a = spp_clean, 
                                       b = drop_na(select(spp_crosswalk, c(CommonName, SPP_BOT)), CommonName), 
                                       by = c("CommonName" = "CommonName"), 
                                       similarity_column = "J") 
  
  # filter common names matched (zoom) to multiple entries in USDA PLANTS
  multmatch <- spp_com_matched[duplicated(spp_com_matched$CommonName.x), ]$CommonName.x %>% 
    unique()
  
  # example: mark the following as duplicate of ComX, ComY, SPPBOT, J
  # SPPCOM_clean    CommonName.x    CommonName.y  SPP_BOT                 J
  # corktree, amur  amur corktree   Amur corktree Phellodendron amurense  0.8461538
  # amur corktree   amur corktree   Amur corktree Phellodendron amurense  0.8461538
  # WANT TO MOVE TO MATCH
  
  # example: mark the following as onetoMany matches of SPP_clean and SPP_BOT
  # SPPCOM_clean  CommonName.x  CommonName.y  SPP_BOT         J
  # maple, amur   amur maple    Amur maple    Acer japonicum  0.8000000
  # maple, amur   amur maple    Amur maple    Acer ginnala    0.8000000
  # WANT TO REMOVE (keep in fuzzy)
  
  zoom_fuzzy <- filter(spp_com_matched, CommonName.x %in% multmatch) %>% 
    group_by(CommonName.x, CommonName.y, SPP_BOT, J) %>% 
    mutate(duplicate = n()) %>% 
    group_by(SPPCOM_clean) %>% 
    mutate(oneToMany = n()) 
  
  zoom_validDup <- zoom_fuzzy %>% 
    filter(duplicate > 1 & oneToMany == 1) 
  
  # raw matching is spp_com_matched
  # zoom_fuzzy is duplicate entries and oneToMany matching of SPPclean -> SPP_BOT
  # zoom_match is oneToOne matching of SPPclean -> SPP_BOT
  # zoom_nonmatch is matching of SPPclean -> SPP_BOT == NA
  
  zoom_fuzzy <- filter(zoom_fuzzy, duplicate == 1 | oneToMany > 1)
  
  # filter one-to-one matches between common name and USDA PLANTS
  zoom_match <- filter(spp_com_matched, !CommonName.x %in% multmatch) 
  
  # filter matches of SPPclean to SPP_BOT == NA
  zoom_nonmatch <- zoom_match %>% 
    filter(is.na(SPP_BOT)) %>% 
    rename(SPPCOM_cross = "SPP_BOT")
  
  # add duplicated one-to-one matches and add to zoom_match list
  zoom_match <- rbind(zoom_match, 
                      select(zoom_validDup, -c("duplicate", "oneToMany"))) %>% 
    rename(SPPCOM_cross = "SPP_BOT") 

  # SPPCOM_cross is three col crosswalk table between OrigComName and CrossBotName
  # created by merging original com name list with zoom_match by cleaned name
  print(dim(spp_orig))
  
  SPPCOM_cross <- merge(spp_orig, 
                        select(zoom_match, c("SPPCOM_clean", "SPPCOM_cross")), 
                        by.x = "SPPCOM_clean", by.y = "SPPCOM_clean", 
                        all.x = TRUE)
  
  return(list(SPPCOM_cross = select(SPPCOM_cross, -c("SPPCOM_clean")), 
              zoom_match = zoom_match, 
              zoom_nonmatch = zoom_nonmatch, 
              zoom_fuzzy = zoom_fuzzy))
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
    rename(SPPCODE_cross = 'SPP_BOT')
  
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

# =============
# (5) R3 UNMATCHED

R3spp_crosswalk <- read.csv(here('data/files0209/R3LIST_isnafinal.csv'))
R3spp_crosswalk <- R3spp_crosswalk %>% 
  mutate(SPPBOT_edit = ifelse(grepl('Unidentifi', SPP_COM), 'REMOVE', NA), 
         SPPBOT_edit = ifelse(grepl('^\\(', SPP_BOT), 
                              str_extract(SPP_BOT, '(?<=\\().*(?=\\))'), 
                              SPPBOT_edit))

# ============= 
# (6) SPECIES MATCHING WITH GBIF AND SPCD
# Adapted from Alex

SPP_alignment <- function(sppbot_list){
  # Normalize species strings
  sppbot_list_clean <- sppbot_list %>% 
    mutate(spp_clean = normalize_species(SPP_FINAL), 
           name_rank = case_when(
             str_count(spp_clean, '\\S+') == 1 ~ "genus", 
             str_count(spp_clean, '\\S+') == 2 ~ 'species', 
             TRUE ~ 'unknown'
           )
    )
  
  View(sppbot_list_clean)
  
  # Get unique names to avoid hammering the API
  unique_names <- sppbot_list_clean %>% distinct(spp_clean) %>% 
    filter(!is.na(spp_clean), spp_clean != '')
  
  # Run - this may take a few minutes depending on n unique species
  gbif_lookup <- map_dfr(unique_names$spp_clean, query_gbif) %>% 
    as.data.frame()
  
  # gbif_lookup[gbif_lookup$gbif_confidence < 90,]
  
  # REF_SPECIES has GENUS, SPECIES columns — build a canonical name
  ref_species_clean <- ref_species |> 
    mutate(
      fia_binomial = str_trim(paste(GENUS, SPECIES)),
      fia_binomial = if_else(str_detect(fia_binomial, "spp"), GENUS, fia_binomial)
    )
  
  ref_species_dedup <- ref_species_clean |> 
    filter(!str_detect(SCIENTIFIC_NAME, "\\bvar\\.\\b|\\bsubsp\\.\\b|\\bssp\\.\\b")) |> 
    arrange(fia_binomial, SPCD) |> 
    distinct(fia_binomial, .keep_all = TRUE)
  
  crosswalk <- gbif_lookup |>
    left_join(
      ref_species_dedup |> select(SPCD, fia_binomial, COMMON_NAME, E_SPGRPCD),
      by = c("gbif_canonicalName" = "fia_binomial"),
      relationship = "many-to-one"
    )
  
  return(crosswalk)
}

gen_spcdxwalk <- function(gbif_lookup){
  ref_species_clean <- ref_species |> 
    mutate(
      fia_binomial = str_trim(paste(GENUS, SPECIES)),
      fia_binomial = if_else(str_detect(fia_binomial, "spp"), GENUS, fia_binomial)
    )
  
  ref_species_dedup <- ref_species_clean |> 
    filter(!str_detect(SCIENTIFIC_NAME, "\\bvar\\.\\b|\\bsubsp\\.\\b|\\bssp\\.\\b")) |> 
    arrange(fia_binomial, SPCD) |> 
    distinct(fia_binomial, .keep_all = TRUE)
  
  crosswalk <- gbif_lookup %>% 
    left_join(
      ref_species_dedup |> select(SPCD, fia_binomial, COMMON_NAME, E_SPGRPCD),
      by = c("gbif_canonicalName" = "fia_binomial"),
      relationship = "many-to-one"
    )
  
  return(crosswalk)
}



