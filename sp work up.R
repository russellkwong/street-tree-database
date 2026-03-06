# Install packages
install.packages(c("rgbif", "tidyverse", "fuzzyjoin", "stringr"))

# treemendous from GitHub
library(data.table)
library(rgbif)
library(tidyverse)
library(fuzzyjoin)

# normalize species strings
normalize_species <- function(x) {
  x <- str_trim(x)
  x <- str_squish(x)
  x <- str_to_sentence(x)
  x <- str_remove_all(x, "'[^']*'")                    # remove cultivar 'names'
  x <- str_remove_all(x, "var\\.\\s*\\S+")             # remove var. epithet
  x <- str_remove_all(x, "\\s*spp?\\.?")               # remove spp. / sp. / sp
  x <- str_remove_all(x, "\\.\\s*$")                   # remove any trailing period
  x <- str_squish(x)
  x <- str_trim(x)
  return(x)
}

# # Tests
# normalize_species("Acer spp.")    # -> "Acer"
# normalize_species("Prunus spp.")   # -> "Prunus"
# normalize_species("Prunus sp")    # -> "Prunus"
# normalize_species("Acer ")        # -> "Acer"
# normalize_species("QUERCUS ALBA var. elongata 'Green Giant'")  # -> "Quercus alba"

r4 <- fread(here::here("tabular_data","kwong","R4List.csv"))

r4 <- r4 |>
  mutate(
    species_clean = normalize_species(SPECIES),
    # Flag what type of name we have
    name_rank = case_when(
      str_count(species_clean, "\\S+") == 1 ~ "genus",
      str_count(species_clean, "\\S+") >= 2 ~ "species",
      TRUE ~ "unknown"
    )
  )

head(r4)

### Step 3 match against GBIF backbone

# Get unique names to avoid hammering the API
unique_names <- r4 |>
  distinct(species_clean) |>
  filter(!is.na(species_clean), species_clean != "")

# Function to query GBIF with rate limiting
query_gbif <- function(name) {
  Sys.sleep(0.1)  # be polite to the API
  result <- tryCatch(
    name_backbone(name = name, kingdom = "Plantae", rank = "species"),
    error = function(e) NULL
  )
  if (is.null(result)) return(tibble(species_clean = name))
  
  tibble(
    species_clean      = name,
    gbif_usageKey      = result$usageKey,
    gbif_scientificName = result$scientificName,
    gbif_canonicalName = result$canonicalName,
    gbif_genus         = result$genus,
    gbif_species       = result$species,
    gbif_rank          = result$rank,
    gbif_status        = result$status,       # ACCEPTED, SYNONYM, DOUBTFUL
    gbif_matchType     = result$matchType,    # EXACT, FUZZY, HIGHERRANK, NONE
    gbif_confidence    = result$confidence
  )
}

# Run — this may take a few minutes depending on n unique species
gbif_lookup <- map_dfr(unique_names$species_clean, query_gbif)
gbif_lookup 

gbif_lookup <- as.data.frame(gbif_lookup)
hist(gbif_lookup$gbif_confidence)

gbif_lookup[gbif_lookup$gbif_confidence < 90,]


#### Step 4 Join to USDA Plants REF_SPECIES

ref_species <- read.csv(file.path("tabular_data", "FIADB_URBAN_ENTIRE_CSV/REF_SPECIES.csv"))

# REF_SPECIES has GENUS, SPECIES columns — build a canonical name
ref_species <- ref_species |>
  mutate(
    fia_binomial = str_trim(paste(GENUS, SPECIES)),
    fia_binomial = if_else(str_detect(fia_binomial, "spp"), GENUS, fia_binomial)
  )


ref_species_dedup <- ref_species |>
  filter(!str_detect(SCIENTIFIC_NAME, "\\bvar\\.\\b|\\bsubsp\\.\\b|\\bssp\\.\\b")) |>
  arrange(fia_binomial, SPCD) |>
  distinct(fia_binomial, .keep_all = TRUE)

# Verify it worked
ref_species_dedup |> count(fia_binomial) |> filter(n > 1)
# Should return 0 rows

# Then clean join
crosswalk <- gbif_lookup |>
  left_join(
    ref_species_dedup |> select(SPCD, fia_binomial, COMMON_NAME, E_SPGRPCD),
    by = c("gbif_canonicalName" = "fia_binomial"),
    relationship = "many-to-one"
  )





dim(crosswalk)
table(is.na(crosswalk$gbif_species))
table(crosswalk$gbif_rank)


r4_names <- as.data.frame(table(r4$SPECIES, r4$species_clean))
colnames(r4_names)[1] ="r4_SPECIES"
colnames(r4_names)[2] ="species_clean"
r4_names <- r4_names[r4_names$Freq>0, ]
length(unique(r4_names$r4_SPECIES))
length(unique(r4_names$species_clean))


r4_names$gbif_usageKey <-   crosswalk$gbif_usageKey[match(r4_names$species_clean, crosswalk$species_clean)]
r4_names$genus <- crosswalk$gbif_genus[match(r4_names$species_clean, crosswalk$species_clean)]
r4_names$species <- crosswalk$gbif_species[match(r4_names$species_clean, crosswalk$species_clean)]
r4_names$confidence <- crosswalk$gbif_confidence[match(r4_names$species_clean, crosswalk$species_clean)]

r4_names$match_type <- crosswalk$gbif_matchType[match(r4_names$species_clean, crosswalk$species_clean)]
r4_names$fia_COMMON_NAME <- crosswalk$COMMON_NAME[match(r4_names$species_clean, crosswalk$species_clean)]
r4_names$fia_SPCD <- crosswalk$SPCD[match(r4_names$species_clean, crosswalk$species_clean)]

# order
r4_names <- r4_names[order(-r4_names$Freq),]

write.csv(r4_names,file="street_tree_taxonomic_list.csv")

head(r4_names)
tail(r4_names[order(r4_names$Freq),])

table(r4_names$match_type)

table(is.na(r4_names$genus))



################################################################################

# Join crosswalk to r4
r4 <- r4 |>
  left_join(crosswalk, by = "species_clean")

# --- Match summary ---
cat("Total r4 rows:            ", nrow(r4), "\n")
cat("Matched to GBIF:          ", sum(!is.na(r4$gbif_usageKey)), "\n")
cat("Matched to REF_SPECIES:   ", sum(!is.na(r4$SPCD)), "\n")
cat("Unmatched:                ", sum(is.na(r4$gbif_usageKey)), "\n")

# --- Unique species summary ---
r4 |>
  distinct(species_clean, .keep_all = TRUE) |>
  count(match_quality) |>
  mutate(pct = round(n / sum(n) * 100, 1))

# --- Full unique species table ---
r4 |>
  distinct(species_clean, .keep_all = TRUE) |>
  select(species_clean, gbif_canonicalName, gbif_rank, gbif_status, 
         match_quality, gbif_confidence, SPCD, COMMON_NAME) |>
  arrange(match_quality, desc(gbif_confidence))


sp <- as.data.frame(table(r4$species_clean, r4$CITY))
sp <- sp[sp$Freq>0,]
sp[order(sp$Var1),]

# --- What didn't match? ---
r4 |>
  filter(is.na(gbif_usageKey)) |>
  count(species_clean, sort = TRUE)
