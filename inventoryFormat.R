library('here')
library('readxl')
library('tidyverse')

i_am('TreeDatabase/inventoryFormat.R')

city_inv <- read.csv(here('TreeDatabase/sarasota_fl.csv'))
city_inv <- read.csv(here('qualitycontrol/data/preprocess/auburn_in.csv'))
invalid_spp <- 'Vacant|Stump|Inactive|Planting|Eliminated|Proposed|NoTree|DNR'

colname_invformat <- function(field_name){
  colname_f <- gsub('\\s', '\\_', field_name)
  colname_f <- gsub('\\.', '\\_', colname_f)
  return(colname_f)
}

treekeeper_spp_format <- function(city_inv){
  city_inv <- city_inv %>% 
    mutate(Species_rep = Species, 
           Species_rep = gsub('\\(', '_SPLIT_', Species_rep), 
           Species_rep = gsub('\\)', '', Species_rep), 
           Species_rep = gsub('\\:', ',', Species_rep)) %>% 
    separate(col = Species_rep, 
             into = c("SPP_COM_rep", "SPP_BOT_rep"), 
             sep = '_SPLIT_') %>% 
    mutate(SPP_COM_rep = trimws(SPP_COM_rep), 
           SPP_BOT_rep = trimws(SPP_BOT_rep))
  
  return(city_inv)
}

write_preprocess <- function(city_inv, city_filename){
  filename = paste0('qualitycontrol/data/preprocess/', city_filename)
  write.csv(city_inv, here(filename), row.names = FALSE)
}

tkpr_simple <- function(city_filename){
  city_inv <- read.csv(here(paste0('TreeDatabase/', city_filename)))
  
  colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
  city_inv <- filter(city_inv, !grepl(invalid_spp, Species, ignore.case = TRUE)) %>%
    filter(DBH > 0)
  city_inv <- treekeeper_spp_format(city_inv)
  
  write_preprocess(city_inv, city_filename)
  
  return(city_inv)
}

tkpr_simple2 <- function(city_filename, spp_col = 'Species', dbh_col = 'DBH', 
                         export = TRUE){
  city_inv <- read.csv(here(paste0('TreeDatabase/', city_filename)))
  
  colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
  city_inv <- filter(city_inv, !grepl(invalid_spp, city_inv[[spp_col]], 
                                      ignore.case = TRUE)) %>%
    filter(.[[dbh_col]] > 0)
  city_inv <- treekeeper_spp_format(city_inv)
  
  if (export == TRUE){
    write_preprocess(city_inv, city_filename)
  }
  
  return(city_inv)
}

tkpr_multi <- function(city_filelist, city_filenameFull, 
                       spp_col = "Species", dbh_col = "DBH", export = TRUE){
  city_invFull <- lapply(city_filelist, function(x){
    city_inv <- read.csv(here(paste0('TreeDatabase/', x)))

    colnames(city_inv) <- lapply(colnames(city_inv), FUN = colname_invformat)
    city_inv <- filter(city_inv, !grepl(invalid_spp, city_inv[[spp_col]], 
                                        ignore.case = TRUE)) %>%
      filter(.[[dbh_col]] > 0)
    city_inv <- treekeeper_spp_format(city_inv)
  })

  city_invFull <- bind_rows(city_invFull)
  if (export == TRUE){
    write_preprocess(city_invFull, city_filenameFull)
  }
  return(city_invFull)
}

# === KNOXVILLE, TN ===============
# city_inv <- read.csv(here('TreeDatabase/knoxville_tn.csv'))
# 
# colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
# city_inv <- city_inv %>%
#   filter(Site_Type %in% c('Tree', 'Community IRA Tree (Future)',
#                           'Community IRA Tree (Planted)', 'Trees Knoxville Tree')) %>%
#   filter(!grepl('^_', Species), 
#          !grepl(invalid_spp, Species, ignore.case = TRUE)) %>% 
#   filter(Diameter > 0)
# city_inv <- treekeeper_spp_format(city_inv)
# 
# write_preprocess(city_inv, 'knoxville_tn.csv')

# === SPRINGFIELD, MA =============
# citylist <- c('springfield_ma.csv',
#               'springfield_ma_P.csv',
#               'springfield_ma_S.csv')
# 
# city_invFull <- sapply(citylist, function(x){
#   city_inv <- read.csv(here(paste0('TreeDatabase/', x)))
# 
#   colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
#   city_inv <- filter(city_inv, !grepl(invalid_spp, Species, ignore.case = TRUE)) %>%
#     filter(DBH > 0) %>% 
#     treekeeper_spp_format()
# })
# 
# city_invFull <- bind_rows(city_invFull)
# write_preprocess(city_invFull, 'springfield_ma.csv')

# === STERLING HEIGHTS, MI ========
# city_inv <- read.csv(here('TreeDatabase/sterlingheights_mi.csv'))
# 
# colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
# city_inv <- filter(city_inv, !grepl(invalid_spp, Species, ignore.case = TRUE)) %>% 
#   filter(DBH > 0)
# city_inv <- treekeeper_spp_format(city_inv)
# 
# write_preprocess(city_inv, 'sterlingheights_mi.csv')

# === AMHERST, NY =================
# city_inv <- read.csv(here('TreeDatabase/amherst_ny.csv'))
# 
# colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
# city_inv <- filter(city_inv, !grepl(invalid_spp, Species, ignore.case = TRUE)) %>% 
#   filter(DBH > 0)
# city_inv <- treekeeper_spp_format(city_inv)
# 
# write_preprocess(city_inv, 'amherst_ny.csv')

# === HARTFORD, CT ================
# citylist <- c('hartford_ct.csv',
#               'hartford_ct_H.csv')
# 
# city_invFull <- sapply(citylist, function(x){
#   city_inv <- read.csv(here(paste0('TreeDatabase/', x)))
# 
#   colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
#   city_inv <- filter(city_inv, !grepl(invalid_spp, Species, ignore.case = TRUE)) %>%
#     filter(DBH > 0)
#   city_inv <- treekeeper_spp_format(city_inv)
# })
# 
# city_invFull <- bind_rows(city_invFull)
# write_preprocess(city_invFull, 'hartford_ct.csv')

# === ALBANY, NY =================
# city_inv <- read.csv(here('TreeDatabase/albany_ny.csv'))
# 
# colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
# city_inv <- filter(city_inv, !grepl(invalid_spp, Species, ignore.case = TRUE)) %>% 
#   filter(Tree_Size__DBH_ > 0)
# city_inv <- treekeeper_spp_format(city_inv)
# 
# write_preprocess(city_inv, 'albany_ny.csv')

# === CONCORD, MA =================
# city_inv <- read.csv(here('TreeDatabase/concord_ma.csv'))
# 
# colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
# city_inv <- filter(city_inv, !grepl(invalid_spp, Species, ignore.case = TRUE)) %>%
#   filter(DBH > 0)
# city_inv <- treekeeper_spp_format(city_inv)
# 
# write_preprocess(city_inv, 'concord_ma.csv')

# === NEWTON, MA =================
# city_inv <- read.csv(here('TreeDatabase/newton_ma.csv'))
# 
# colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
# city_inv <- filter(city_inv, !grepl(invalid_spp, Species, ignore.case = TRUE)) %>%
#   filter(DBH > 0)
# city_inv <- treekeeper_spp_format(city_inv)
# 
# write_preprocess(city_inv, 'newton_ma.csv')

# === MARATHON CO, WI =============
# city_inv <- tkpr_simple('marathon_wi_P.csv')

# === LOWER MERION, PA ============
# city_invFull <- tkpr_multi(city_filelist = c('lowermerion_pa.csv', 
#                                              'lowermerion_pa_PR.csv'), 
#                            city_filenameFull = 'lowermerion_pa.csv')

# === HIGHLAND PARK, IL ===========
# city_inv <- tkpr_simple2('highlandpark_il.csv', dbh_col = "Diameter")

# === PARK RIDGE, IL ==============
city_inv <- tkpr_simple2('parkridge_il.csv')

# === BLOOMINGTON, IL =============
# city_inv <- tkpr_multi(city_filelist = c('bloomington_il.csv', 'bloomington_il_P.csv'), 
#                        city_filenameFull = 'bloomington_il.csv')

# === DES PLAINES, IL =============
# city_inv <- tkpr_simple2(city_filename = 'desplaines_il.csv')

# === ST. CHARLES, IL =============
# city_inv <- tkpr_simple2(city_filename = 'stcharles_il.csv', export = FALSE)
# city_inv <- subset(city_inv, Site_ID != 2822)
# write_preprocess(city_inv, 'stcharles_il.csv')

# === PRINCETON, NJ ===============
# city_inv <- tkpr_simple2(city_filename = 'princeton_nj.csv')

# === GOSHEN, IN ==================
# city_inv <- tkpr_simple2(city_filename = 'goshen_in.csv', export = FALSE)
# city_inv <- subset(city_inv, SPP_BOT_rep != 'UNASSIGNED')
# write_preprocess(city_inv, city_filename = 'goshen_in.csv')

# === WILMETTE, IL ================
# city_inv <- tkpr_simple2(city_filename = 'wilmette_il.csv', export = FALSE)
# rownames(city_inv) <- NULL
# city_inv$ID_rep <- rownames(city_inv)
# write_preprocess(city_inv, city_filename = 'wilmette_il.csv')

# === MIAMI LAKES, FL =============
# city_inv <- tkpr_simple2(city_filename = 'miamilakes_fl.csv', export = FALSE)
# city_inv <- subset(city_inv, !is.na(SPP_BOT_rep))
# write_preprocess(city_inv, city_filename = 'miamilakes_fl.csv')

# === URBANA, IL ==================
# city_inv <- tkpr_multi(city_filelist = c('urbana_il.csv', 
#                                          'urbana_il_P.csv'), 
#                        city_filenameFull = 'urbana_il.csv')

# === GASTONIA, NC ================
# city_inv <- tkpr_simple2(city_filename = 'gastonia_nc.csv')

# === MUSKEGON, MI ================
# city_inv <- tkpr_simple2(city_filename = 'muskegon_mi.csv')

# === FREDERICK, MD ===============
# city_inv <- tkpr_simple2(city_filename = 'frederick_md.csv', export = FALSE)
# city_inv <- mutate(city_inv, SPP_BOT_rep = ifelse(SPP_BOT_rep == 'Basswood', 
#                                                   'Tilia americana', 
#                                                   SPP_BOT_rep))
# write_preprocess(city_inv, city_filename = 'frederick_md.csv')

# === NORMAL, IL ==================
# city_inv <- tkpr_simple2(city_filename = 'normal_il.csv')

# === FALLS CHURCH, VA ============
# city_inv <- tkpr_simple2(city_filename = 'fallschurch_va.csv')

# === NEWPORT, RI =================
# city_inv <- tkpr_simple2(city_filename = 'newport_ri.csv')

# === VALPARAISO, IN ==============
# city_inv <- tkpr_simple2(city_filename = 'valparaiso_in.csv', export = FALSE)
# city_inv <- filter(city_inv, !grepl('Mixed Wild|Unknown', SPP_BOT_rep, 
#                                     ignore.case = TRUE))
# write_preprocess(city_inv, city_filename = 'valparaiso_in.csv')

# === IRONDEQUOIT, NY =============
# city_inv <- tkpr_simple2(city_filename = 'irondequoit_ny.csv', export = FALSE)
# rownames(city_inv) <- NULL
# city_inv$ID_rep <- rownames(city_inv)
# write_preprocess(city_inv, city_filename = 'irondequoit_ny.csv')

# === BINGHAMTON, NY ==============
# city_inv <- tkpr_simple2(city_filename = 'binghamton_ny.csv', 
#                          dbh_col = "Diameter")

# === GRAND ISLAND, NY ============
# city_inv <- tkpr_simple2(city_filename = 'grandisland_ny.csv')

# === TOLEDO, OH ==================
# city_inv <- city_inv %>% 
#   separate(Species, into = c('SPP_BOT_rep', 'SPP_CODE_rep'), sep = ' - ', 
#            remove = FALSE) 
# 
# city_inv <- filter(city_inv, ActiveInactive == 'Active') %>% 
#   mutate(SPP_BOT_rep = trimws(SPP_BOT_rep), 
#          DBH = trimws(DBH)) %>% 
#   subset(., !(SPP_BOT_rep  %in% c('', 'STUMP'))) %>% 
#   filter(DBH != '')
# 
# write_preprocess(city_inv, city_filename = 'toledo_oh.csv')

# === INDIANA STATE ===============
## ====== COLUMBUS
# city_inv <- read_excel(here('TreeDatabase', 'state_in.xlsx'), 
#                        sheet = 'Columbus') 
# city_inv <- filter(city_inv, !grepl('Stump|Vacant', SPP))
# 
# write_preprocess(city_inv, city_filename = 'columbus_in.csv')

## ====== CLARKSVILLE
# city_inv <- read_excel(here('TreeDatabase', 'state_in.xlsx'), 
#                        sheet = 'Clarksville') 
# city_inv <- filter(city_inv, !grepl(invalid_spp, Scientific, ignore.case = TRUE)) %>% 
#   filter(DBH > 0)
# 
# write_preprocess(city_inv, city_filename = 'clarksville_in.csv')

## ====== AUBURN
# city_inv <- read_excel(here('TreeDatabase', 'state_in.xlsx'), 
#                        sheet = 'Auburn')
# city_inv <- city_inv %>% 
#   rename(SPP_COM_rep = "...1", 
#          SPP_BOT_rep = "...2")
# city_inv <- filter(city_inv, !grepl(invalid_spp, SPP_BOT_rep, ignore.case = TRUE)) %>% 
#   filter(DBH > 0)
# 
# rownames(city_inv) <- NULL
# city_inv$ID_rep <- rownames(city_inv)
# write_preprocess(city_inv, city_filename = 'auburn_in.csv')

## ====== MUNCIE
# city_inv <- read_excel(here('TreeDatabase', 'state_in.xlsx'), 
#                        sheet = 'Muncie')
# city_inv <- city_inv %>% 
#   rename(SPP_OTHER_rep = "...6", 
#          SPP_BOT_rep = "...20", 
#          SPP_COM_rep = "...21") 
# colnames(city_inv) <- sapply(colnames(city_inv), FUN = colname_invformat)
# city_inv <- city_inv %>% 
#   filter(!grepl(invalid_spp, SPP_BOT_rep, ignore.case = TRUE)) %>% 
#   mutate(SPP_BOT_rep = trimws(SPP_BOT_rep), 
#          SPP_COM_rep = gsub('\\)', '', SPP_COM_rep)) %>% 
#   filter(DBH > 0)
# 
# rownames(city_inv) <- NULL
# city_inv$ID_rep <- rownames(city_inv)
# write_preprocess(city_inv, city_filename = 'muncie_in.csv')

## ====== RICHMOND
# city_inv <- read_excel(here('TreeDatabase', 'state_in.xlsx'), 
#                        sheet = 'Richmond')
# city_inv <- city_inv %>% 
#   rename(SPP_BOT_rep = "...2") 
# city_inv <- filter(city_inv, !grepl(invalid_spp, SPP_BOT_rep, ignore.case = TRUE)) %>% 
#   filter(DBH > 0)
# 
# rownames(city_inv) <- NULL
# city_inv$ID_rep <- rownames(city_inv)
# write_preprocess(city_inv, city_filename = 'richmond_in.csv')

## ====== TERRE HAUTE
# city_inv <- read_excel(here('TreeDatabase', 'state_in.xlsx'),
#                        sheet = 'Terre Haute', 
#                        col_types = 'text')
# 
# city_inv <- filter(city_inv, TREE_STATU == 'ACTIVE TREE') %>% 
#   filter(!grepl(invalid_spp, COMMON_NAM, ignore.case = TRUE)) %>% 
#   filter(DBH > 0) %>% 
#   mutate(SPP_COM_rep = gsub('-', ', ', COMMON_NAM))
# 
# rownames(city_inv) <- NULL
# city_inv$ID_rep <- rownames(city_inv)
# write_preprocess(city_inv, city_filename = 'terrehaute_in.csv')

## ====== CRAWFORDSVILLE
# city_inv <- read_excel(here('TreeDatabase', 'state_in.xlsx'),
#                        sheet = 'Crawfordsville') 
# 
# city_inv <- filter(city_inv, !grepl(invalid_spp, SPP, ignore.case = TRUE)) %>%
#   filter(DBH > 0)
# 
# rownames(city_inv) <- NULL
# city_inv$ID_rep <- rownames(city_inv)
# write_preprocess(city_inv, city_filename = 'crawfordsville_in.csv')

# === SARASOTA CO, FL =============
# city_inv <- read.csv(here('TreeDatabase/sarasota_fl.csv'))
# 
# city_inv <- filter(city_inv, !grepl(invalid_spp, genusname, ignore.case = TRUE)) %>% 
#   filter(genusname != '') %>% 
#   filter(diameterbreastheight > 0)
# 
# write_preprocess(city_inv, city_filename = 'sarasota_fl.csv')

# === WESTON, FL ==================
# city_inv <- read.csv(here('TreeDatabase/weston_fl.csv'))
# 
# city_inv <- filter(city_inv, !grepl(invalid_spp, BOTNAME, ignore.case = TRUE)) %>% 
#   filter(!(BOTNAME == '' & NAME == '' & GENUS == '' & SPECIES == '')) %>% 
#   filter(DIAMETER > 0)
# 
# write_preprocess(city_inv, city_filename = 'weston_fl.csv')

# === GREENWICH, CT ===============
# city_inv <- tkpr_simple2(city_filename = 'greenwich_ct.csv')

# === VERMONT STATE ===============
# city_inv <- read.csv(here('TreeDatabase/state_vt.csv'))
# 
# city_inv <- filter(city_inv, !grepl('PLANT|VACANT', ConditionID)) %>%
#   filter(SPECIES != '') %>%
#   filter(Diameter != '')
# 
# write_preprocess(city_inv, city_filename = 'state_vt.csv')

# === PENNSYLVANIA STATE ==========
# city_inv <- tkpr_simple2(city_filename = 'state_pa.csv', dbh_col = 'Diameter', 
#                          export = FALSE) %>%
#   filter(SPP_BOT_rep != 'Unknown Tree') %>% 
#   filter(SPP_COM_rep != '')
# 
# rownames(city_inv) <- NULL
# city_inv$ID_rep <- rownames(city_inv)
# write_preprocess(city_inv, city_filename = 'state_pa.csv')

# === LEXINGTON, MA ===============
# city_inv <- read.csv(here('TreeDatabase/lexington_ma.csv'))
# 
# city_inv <- filter(city_inv, !grepl(invalid_spp, Botanical)) %>% 
#   filter(Exact_DBH > 0) %>% 
#   mutate(DBH_Range = trimws(DBH_Range), 
#          Height_Ran = trimws(Height_Ran)) %>% 
#   filter(Latitude > 42)
# 
# write_preprocess(city_inv, city_filename = 'lexington_ma.csv')

# === NORTH MIAMI BEACH, FL =======
# city_inv <- read.csv(here('TreeDatabase/northmiamibeach_fl.csv'))
# 
# city_inv <- filter(city_inv, !grepl(invalid_spp, BOTANICAL)) %>% 
#   filter(EXACT_DBH > 0)
# 
# write_preprocess(city_inv, city_filename = 'northmiamibeach_fl.csv')

# === LOMBARD, IL =================
city_inv <- read.csv(here('TreeDatabase/lombard_il.csv')) 

city_inv <- filter(city_inv, !grepl(invalid_spp, Scientific_Name)) %>% 
  filter(Diameter > 0)

write_preprocess(city_inv, city_filename = 'lombard_il.csv')

# ===

# === DIAGNOSTICS =================
length(unique(city_inv$Tree_ID))

unique(city_inv$ConditionID)

city_inv <- filter(city_inv, SPP_BOT_rep == 'Tilia americana')
city_inv[2051,] %>% View()

na_count <- sapply(city_inv, function(y) sum(length(which(is.na(y))))) %>% 
  data.frame()

city_inv <- read.csv(here('qualitycontrol/R3LIST_260202.csv')) 

city_invsort <- sort(city_inv, )
