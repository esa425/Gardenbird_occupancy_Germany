library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(data.table)
library(readr)
library(gert)

# Setze deinen Namen
git_config_global_set("user.name", "esa425")

# Setze deine E-Mail-Adresse
git_config_global_set("user.email", "209086843+esa425@users.noreply.github.com")
# 1. Prüfen, ob die alte Datei noch im Cache festsitzt
git_status() 
# Fügt den exakten Pfad mit Schrägstrichen in die .gitignore ein
cat("\nBird_data/Raw_data/Final_Table_corrected.csv\n", file = ".gitignore", append = TRUE)
# Wirft die Datei im Unterordner aus dem Git-Cache (Löscht sie NICHT von der Festplatte!)
#system('git rm --cached "Bird_data/Processed_data/260730_bird_prop_landuse_ZIP3_final.csv"')
system('git rm -f --cached "Bird_data/Raw_data/Final_Table_corrected.csv"')
git_status()
# Sperrt nur die konkreten Riesen-Dateien und das .gpkg-Format
cat("\nBird_data/Raw_data/Final_Table_corrected.csv\n*.gpkg\n", file = ".gitignore", append = TRUE)


ybase_path <- file.path("Bird_data", "Raw_data" )

#____________
# Data tables___________________________________________________________________
#____________
# bird SdG-data
# path to files
file1 <- file.path(base_path, "Final_Table_corrected.csv")
# read data
bird <- fread(file1, colClasses = list(character = "ZIP_code"))
str(bird)

# count number of ZIP_codes
length(unique(bird$ZIP_code))

# delete entries with NA in column SdG_participation----------------------------
bird2 <- bird |>
  filter(!is.na(SdG_participation))

# count number of ZIP_codes
length(unique(bird2$ZIP_code)) # 8119

#-------------------------------------------------------------------------------
#Land use data
#-------------------------------------------------------------------------------
# select only unique ZIP codes (5 digits) for analysis
base_path1 <- file.path("Landuse_data", "Raw_data" )

# path to files
file2 <- file.path(base_path1, "250521_CORINE_Landuse_5u3_rename.csv")

# read data
landuse <- fread(file2, colClasses = list(character = "ZIP_3", 
                                          character = "plz_code"))
landuse <- landuse |> rename(ZIP_code = plz_code)

# create list of unique zipcode data (3 and 5 digits)
landuse_zip3_5 <-landuse |> distinct(ZIP_code, ZIP3_ID)
landuse_zip3 <- landuse |> distinct(ZIP_3, ZIP3_ID)
length(unique(landuse_zip3_5$ZIP_code))
length(unique(landuse_zip3$ZIP3_ID))

write.csv(landuse_zip3, file.path("Landuse_data", "Processed_data", "260730_landuse_zip3_ID.csv"))

# check unique ZIP codes
duplicated_entries1 <- as.data.frame(duplicated(landuse$ZIP_code)) # -> true
duplicated_entries2 <- as.data.frame(duplicated(landuse_zip3_5$ZIP_code)) # -> false

# variable adjustments
str(landuse)
landuse$mylevel <- as.factor(landuse$mylevel)
landuse$ClassArea <- as.numeric(landuse$ClassArea)
landuse$CatchArea <- as.numeric(landuse$CatchArea)
landuse$CatchArea3 <- as.numeric(landuse$CatchArea3)
landuse$Percentage <- as.numeric(landuse$Percentage)

# selection ZIP codes
ZIP_select <- landuse |> distinct(ZIP_code)
length(unique(ZIP_select$ZIP_code))# 7654
duplicated_entries3 <- as.data.frame(duplicated(ZIP_select$ZIP_code)) # -> false

# missing ZIP codes
# miss<- anti_join(ZIP_select, bird2, by ="ZIP_code")# 49 missing ZIP codes in bird data set

# join bird data with ZIP code selection
bird_garden_zip <- semi_join(bird2, ZIP_select, by = "ZIP_code")
length(unique(bird_garden_zip$ZIP_code))# 7605 -> 7654 - 49
sum(is.na(bird_garden_zip$ZIP_3))# 0 NA values

# join ZIP code with three digits
bird_garden3_5 <- left_join(bird_garden_zip, landuse_zip3_5, by = "ZIP_code")
length(unique(bird_garden3_5$ZIP3_ID)) # 1172
length(unique(bird_garden3_5$Participant_id)) # 557111
# count number of sites/observations per year and make an average
count_sites <- bird_garden3_5 |> group_by(Year) |> 
  summarize(tot_sites = length(unique(Participant_id))) |> 
  mutate(tot_all= sum(tot_sites),
         mean_all = mean(tot_sites))

#-------------------------------------------------------------------------------
# 1. count SdG participation per year and ZIP code (3 digits)

count_part <- bird_garden3_5 |>
  distinct(Year, ZIP3_ID, Participant_id, .keep_all = TRUE) |>  # count every participant just once
  group_by(Year, ZIP3_ID) |>
  summarize(
    null_prop = sum(SdG_participation == "0", na.rm = TRUE),
    eins_prop = sum(SdG_participation == "1", na.rm = TRUE),
    zwei_prop = sum(SdG_participation == "2", na.rm = TRUE),
    total_participants = n())
count_part <- as.data.frame(count_part)
count_part$ZIP3_ID <- as.character(count_part$ZIP3_ID)
str(count_part)

#-------------------------------------------------------------------------------
# species selection breeding birds
#-------------------------------------------------------------------------------
file3 <- file.path(base_path, "BrutvoegelGER_list_240604_2007-21.csv")
species_breed <- read.csv(file3)
species <- species_breed |> dplyr::select(1:3)
str(species)

# further selection common garden birds only
bird_garden <- c(
  "Amsel","Bachstelze","Blaumeise","Buchfink","Buntspecht","Dohle","Eichelhaeher",
  "Elster","Feldsperling","Gartenbaumlaeufer","Gimpel","Girlitz","Goldammer",
  "Gruenfink","Gruenspecht","Haubenmeise","Hausrotschwanz","Haussperling",
  "Heckenbraunelle","Kleiber","Kohlmeise","Moenchsgrasmuecke","Nebelkraehe",
  "Rabenkraehe","Ringeltaube","Rotkehlchen","Saatkraehe","Schwanzmeise",
  "Singdrossel","Star", "Stieglitz","Tannenmeise","Tuerkentaube", "Zaunkoenig",
  "Zilpzalp")

# select only common garden birds from bird data set
vogel_filter <- species |> 
  filter(Species %in% bird_garden)
length(unique(vogel_filter$Species)) 

# bird selection, choose just Breeding and common garden birds for NABU data----
bird_garden_com <- semi_join(bird_garden3_5, vogel_filter, by = join_by(Species))

# check number of species and individuals
length(unique(bird_garden_com$Species))
bird_garden_com$ZIP3_ID <- as.character(bird_garden_com$ZIP3_ID)

# individuals
bird_count <- bird_garden_com |> group_by(ZIP3_ID, Species) |> 
  summarise(Number_indi = sum(Number_of_individuals),
            year = length(unique(Year))) 
bird_count2 <- bird_count |> group_by(ZIP3_ID) |> 
  summarize(tot_indi = sum(Number_indi, na.rm=TRUE)) |> 
  mutate(tot_indi2 = sum(tot_indi))

#-------------------------------------------------------------------------------
# fill zero for  missing values: ZIP code (3 digits), Year and Species
#-------------------------------------------------------------------------------

# 1. complete with zero
df_part_0 <- complete(count_part, ZIP3_ID, Year, fill = list(null_prop=0,
                                                             total_participants=0,
                                                             eins_prop=0,
                                                             zwei_prop=0))
length(unique(df_part_0$ZIP3_ID)) # 1172

# count species per garden and calculate proportion
bird_garden_com |>
  group_by(ZIP3_ID, Year, Species) |>
  summarise(garden_spec = n_distinct(Participant_id, na.rm = TRUE)) |>
  ungroup() |>
  complete(ZIP3_ID, Year, Species, fill = list(garden_spec = 0)) |>
  inner_join(df_part_0, by = c("ZIP3_ID", "Year")) |>
  mutate(prop_garden = ifelse(total_participants > 0,
                              garden_spec / total_participants, NA),
         null_prop3 = null_prop / total_participants) -> bird_garden_0

#-------------------------------------------------------------------------------
# LANDUSE DATA
# ------------------------------------------------------------------------------
str(landuse)
landuse$ZIP3_ID <- as.character(landuse$ZIP3_ID)

# filling zero for missing landuse area per ZIP3_ID
landuse |>
  dplyr::select(ZIP3_ID, mylevel, ClassArea) |>
  group_by(ZIP3_ID, mylevel) |>
  summarise(Classarea3 = sum(ClassArea)) |>
  mutate(CatchArea3 = sum(Classarea3),
         PropClass3 = Classarea3 / CatchArea3) |>
  dplyr::select(ZIP3_ID, mylevel, PropClass3) |>
  pivot_wider(names_from = mylevel, names_prefix = "lu",
              values_from = PropClass3, values_fill = 0) |>
  mutate(lu_tot = rowSums(across(where(is.numeric)))) -> landuse3

# check
landuse3 <- as.data.frame(landuse3)
str(landuse3)
landuse3$ZIP3_ID <- as.character(landuse3$ZIP3_ID)

# join with bird data
bird_landuse_join <- inner_join(bird_garden_0, landuse3, by = "ZIP3_ID")
length(unique(bird_landuse_join$ZIP3_ID)) # 1172 

# join zip3
landuse_zip3$ZIP3_ID <- as.character(landuse_zip3$ZIP3_ID)
bird_landuse_join2 <- left_join(bird_landuse_join, landuse_zip3, by = "ZIP3_ID")

# save this
write.csv(bird_landuse_join2, file.path("Bird_data", "Processed_data", "260730_bird_prop_landuse_ZIP3_final.csv"))
write_csv(bird_landuse_join2,  file.path("Bird_data", "Processed_data","260730_bird_prop_landuse_ZIP3_final.csv.gz"))

