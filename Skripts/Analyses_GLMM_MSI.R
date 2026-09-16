library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(glmmTMB)
library(devtools)
# download here: https://github.com/BiologicalRecordsCentre/BRCindicators
library(BRCindicators) 
library(data.table)


base_path <- file.path("Bird_data", "Processed_data" )
#____________
# Data tables___________________________________________________________________
#____________
# all data
file1 <- file.path(base_path,"260730_bird_prop_landuse_ZIP3_final.csv")
bird_land <- fread(file1, sep = ",", dec = ".",
                   colClasses = list(character="ZIP_3", character="ZIP3_ID"))

bird_land <- bird_land |> rename(garden_tot = total_participants)
length(unique(bird_land$ZIP3_ID))# 1172
length(unique(bird_land$ZIP_3))# 671

any(is.na(bird_land))# true
str(bird_land)
check <- bird_land |> distinct(ZIP3_ID)

# rename dataset and get non-scaled years
bird_sample <- bird_land
year_non_scal <- bird_sample |> distinct(Year)

#-------------------------------------------------------------------------------
# Traits
#-------------------------------------------------------------------------------
base_path1 <- file.path("Bird_data", "Raw_data" )
file2 <- file.path(base_path1,"250529_Brutvoegel_list_trait_2.csv")
traits <- read.csv(file2, sep = ",", dec = ".")

# scale variables---------------------------------------------------------------
str(bird_sample)
bird_sample[, Year := as.numeric(Year)]
bird_sample[, null_prop3 := as.numeric(null_prop3)]

bird_sample[, Year := round(scale(Year), 3)]
bird_sample[, null_prop3 := scale(null_prop3)]

# as factor and integer---------------------------------------------------------
bird_sample[, ZIP_3 := as.factor(ZIP_3)]
bird_sample[, ZIP3_ID := as.factor(ZIP3_ID)]
bird_sample[, Species := as.factor(Species)]
str(bird_sample)

# filter NAs, otherwise model makes issues--------------------------------------
bird_sample <- bird_sample[!is.na(null_prop3)]
bird_sample <- bird_sample[!is.na(garden_tot)]
nAS<- is.na(bird_sample) 

# prepare dataset for modeling--------------------------------------------------
bird_sample2 <- bird_sample |> distinct(ZIP3_ID, ZIP_3, Year, null_prop3, Species,
                                         garden_spec, garden_tot, prop_garden)

# setDT(traits)
bird_sample2 <- merge(bird_sample2, traits, by = "Species", all.x = TRUE)

bird_sample2$Species<- as.character(bird_sample2$Species)
length(unique(bird_sample2$ZIP3_ID))

#Test run filter ZIP codes
#  set.seed(123)  # for reproduction
#  sample_zips <- sample(unique(bird_sample2$ZIP3_ID), 5)
# 
# # choose all lines with these ZIP-Codes
#   bird_sample2 <- bird_sample2 |>
#     filter(ZIP3_ID %in% sample_zips)
#  test_zip<- subset(bird_sample2, ZIP3_ID=="1012")
# join years
year_scal <-bird_sample2 |> distinct(Year)
year_join<- as.data.frame(cbind(year_scal$Year,year_non_scal$Year))
year_join<- year_join|> rename(year = V1) 

#-------------------------------------------------------------------------------
# How many species per bird group
#-------------------------------------------------------------------------------
# trait count, how many species per trait_group
# remove unnecessary variables for diet types
remove_cols <- c("Species_lat", "Species_eng", "X", "ClassProp3", 
                 "Percentage","V1", "Migration","mylevel", "Nest_site")
cols_to_remove <- intersect(names(bird_sample2), remove_cols)
bird_sample2[, (cols_to_remove) := NULL]

trait_count <- bird_sample2 |> group_by(Species, Habitat, Diet) |> summarize(number=n_distinct(Species))
trait_habitat <-trait_count |>  group_by(Habitat) |> summarize(number_spec= n_distinct(Species))
trait_diet <- trait_count |>  group_by(Diet) |> summarize(number_spec= n_distinct(Species))
trait_all <- trait_count |>  group_by(Diet, Habitat) |> summarize(number_spec= n_distinct(Species))

trait_all_select3<- trait_all|>
  group_by(Habitat, Diet) |>
  summarise(number_spec2 = sum(number_spec), .groups = 'drop')

# plot count
# Create the bar plot
my_colors2 <- c("granivorous" = "darkgoldenrod4",
                "insectivorous" = "darkorange",
                "omnivorous" = "darkgrey",
                "invertebrates"="grey")

# Create the bar plot
ns <- ggplot(trait_all_select3, aes(x = Habitat, y = number_spec2, fill = Diet)) +
  geom_bar(stat = "identity", color= "black") +
  theme_bw() +
  labs(x = "Habitat preference", y = "Number of species",
       title = "Number of species by habitat preference and diet guild", fill="Diet guild")+
  scale_fill_manual(values = my_colors2) +  # Apply the custom color palette
  geom_text(aes(label = number_spec2), 
            position = position_stack(vjust = 0.5),  # Center the text in the stack
            color = "white") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    text = element_text(family = "sans"))
ns
ggsave(filename ="Results/Figures/260730_count_diet_habitat_species_selection.png", 
       plot = ns, width = 16, height = 12, units = "cm")


# Diet-------------------------------------------------------------------------- 
# remove unnecessary variables for diet types-----------------------------------
# remove_cols <- c("Species_lat", "Species_eng", "Habitat", "X", "ClassProp3", 
#                  "Percentage","V1", "Migration","mylevel", "Nest_site")
# cols_to_remove <- intersect(names(bird_sample2), remove_cols)
# bird_sample2[, (cols_to_remove) := NULL]

# bird diet groups--------------------------------------------------------------
bird_insect <- bird_sample2[Diet == "insectivorous"]
bird_grani <- bird_sample2[Diet == "granivorous"]
bird_omni  <- bird_sample2[Diet == "omnivorous" | Diet=="invertebrates"]

# Habitat-----------------------------------------------------------------------
remove_cols2 <- c("Species_lat", "Species_eng", "Diet", "X", "ClassProp3",
                  "Percentage","V1", "Migration","mylevel", "Nest_site")
cols_to_remove2 <- intersect(names(bird_sample2), remove_cols2)
bird_sample2[, (cols_to_remove2) := NULL]

#filter habitat groups----------------------------------------------------------
bird_urban<- bird_sample2[Habitat=="settlements" ]
bird_sev <- bird_sample2[Habitat=="several"]
bird_forest  <- bird_sample2[Habitat=="forests"]
bird_farm  <- bird_sample2[Habitat=="farmland"] # only 4 species
# 
# Nest-type---------------------------------------------------------------------
# remove_cols3 <- c("Species_lat", "Species_eng", "Diet","Habitat", "X", "ClassProp3",
#                  "Percentage","V1", "Migration","mylevel")
# cols_to_remove3 <- intersect(names(bird_sample2), remove_cols3)
# bird_sample2[, (cols_to_remove3) := NULL]
# 
# filter nest groups
# bird_OA<- bird_sample2[Nest_site=="trees" ]
# bird_GC <- bird_sample2[Nest_site=="shrubs"]
# bird_G  <- bird_sample2[Nest_site=="ground-level"]
# bird_H  <- bird_sample2[Nest_site=="buildings"]

#__________________________
# MODEL GLMM
#__________________________
# Function with GLMM model------------------------------------------------------

bird_func3_zip <- function(data, specs) {
  # empty list to store predictions
  filtered_data <- list()
  
  # Iteration for every species
  for (spec in specs) {
    # filter for current species
    spec.sub <- subset(data, Species == spec)
    
    # check if spec.sub is empty
    if (nrow(spec.sub) == 0) {
      next  #leave this species if no data is present
    }
    
    # Model
    mod_1 <- glmmTMB(prop_garden ~ Year + null_prop3 + (1 + Year | ZIP3_ID),
                     family = binomial(link = "logit"), weights = garden_tot, data = spec.sub)
    
    # new dataframe
    newdat <- expand.grid(
      ZIP3_ID = unique(spec.sub$ZIP3_ID),
      Year = unique(spec.sub$Year),
      Species = spec,
      garden_tot = mean(spec.sub$garden_tot, na.rm = TRUE),
      null_prop3 = mean(spec.sub$null_prop3, na.rm = TRUE)
      
    )
    
    # variable types
    newdat$Year <- as.numeric(newdat$Year)  # ensure that Year is numeric
    newdat$null_prop3 <- as.numeric(newdat$null_prop3)  # null_prop3 is numeric
    
    # Prediction with standard error
    pred_results <- predict(mod_1, newdata = newdat, type = "response", se.fit = TRUE)
    
    # insert predictions to new dataframe
    newdat$pred_prob <- pred_results$fit
    newdat$se_fit <- pred_results$se
    
    # calculation for every county
    for (k in unique(newdat$ZIP3_ID)) {
      # Subset for current ZIP code area
      sub_newdat <- subset(newdat, ZIP3_ID == k)
      
      # calculation for every county
      sub_newdat$index <- (sub_newdat$pred_prob / sub_newdat$pred_prob[1]) * 100
      sub_newdat$se_index <- (sqrt((sub_newdat$se_fit)^2 + (sub_newdat$se_fit[1])^2))*100
      
      sub_newdat$se_index[1] <- 0
      
      # results in newdat
      newdat[newdat$ZIP3_ID == k, "index"] <- sub_newdat$index
      newdat[newdat$ZIP3_ID == k, "se_index"] <- sub_newdat$se_index
    }
    
    # add to list
    filtered_data[[spec]] <- newdat[, c("ZIP3_ID", "Species", "Year", "index", "se_index")]  }
  
  # add predictions to list
  final_result <- bind_rows(filtered_data)
  
  if (nrow(final_result) == 0) {
    return(data.frame(ZIP3_ID = character(), Year = integer(), 
                      Species = character(), index = numeric(), se_index = numeric()))
  }
  
  result2 <- subset(final_result, select = c(Species, Year, index, se_index, ZIP3_ID))
  colnames(result2) <- c("species", "year", "index", "se", "ZIP3_ID")
  
  # give back data
  return(result2)
}

# run function------------------------------------------------------------------
# ALL SPECIES-------------------------------------------------------------------
all_spec <- unique(bird_sample2$Species)
all_birds <-  bird_func3_zip(bird_sample2, all_spec)
write.csv(all_birds, file.path("Bird_data", "Processed_data", "260730_glmm_all_species.csv"))
check_zip <- all_birds |> distinct(ZIP3_ID)

# insectivore species 17
all_insect <- unique(bird_insect$Species)
all_insect <-  bird_func3_zip(bird_sample2, all_insect)
write.csv(all_birds, file.path("Bird_data", "Processed_data", "260730_glmm_insect_species.csv"))

#granivore 8
all_grani <- unique(bird_grani$Species)
all_grani_2 <-  bird_func3_zip(bird_sample2, all_grani)
write.csv(all_birds, file.path("Bird_data", "Processed_data", "260730_glmm_grani_species.csv"))

#Omnivore 10
all_omni <- unique(bird_omni$Species)
all_omni_2 <-  bird_func3_zip(bird_sample2, all_omni)
write.csv(all_birds, file.path("Bird_data", "Processed_data", "260730_glmm_omni_species.csv"))

# add right years---------------------------------------------------------------
year_join$year <- as.numeric(year_join$year)

#all species
all_birds_2 <- left_join(all_birds,year_join, by ="year") |> dplyr::select(-year) 
all_birds_2<- all_birds_2|> rename(year = V2) |> dplyr::select(species, year, index, se, ZIP3_ID)

#granivore
all_grani_3 <- left_join(all_grani_2,year_join, by ="year") |> dplyr::select(-year) 
all_grani_3<- all_grani_3|> rename(year = V2) |> dplyr::select(species, year, index, se, ZIP3_ID)

#insectivore
all_insect_3 <- left_join(all_insect,year_join, by ="year") |> dplyr::select(-year) 
all_insect_3<- all_insect_3|> rename(year = V2)|> dplyr::select(species, year, index, se, ZIP3_ID)

#omnivore
all_omni_3 <- left_join(all_omni_2,year_join, by ="year") |> dplyr::select(-year) 
all_omni_3<- all_omni_3|> rename(year = V2)|> dplyr::select(species, year, index, se, ZIP3_ID)

#-------------------------------------------------------------------------------
# Habitat
#-------------------------------------------------------------------------------
# urban species 8
all_urban <- unique(bird_urban$Species) # 8
all_urban_2 <-  bird_func3_zip(bird_sample2, all_urban)

# several 8
all_sev <- unique(bird_sev$Species) # 8
all_sev_2 <-bird_func3_zip(bird_sample2, all_sev)

# forest 15
all_forest <- unique(bird_forest$Species) # 15
all_forest_2 <-  bird_func3_zip(bird_sample2, all_forest)

#add right years----------------------------------------------------------------
year_join$year <- as.numeric(year_join$year)

# settlements
all_urban_3 <- left_join(all_urban_2, year_join, by ="year") |> dplyr::select(-year) 
all_urban_3 <- all_urban_3|> rename(year = V2) |> dplyr::select(species, year, index, se, ZIP3_ID)
write.csv(all_urban_3, file.path("Bird_data", "processed_data", "260805_glmm_urban.csv"))
# several
all_sev_3 <- left_join(all_sev_2,year_join, by ="year") |> dplyr::select(-year) 
all_sev_3 <- all_sev_3|> rename(year = V2) |> dplyr::select(species, year, index, se, ZIP3_ID)
write.csv(all_sev_3, file.path("Bird_data", "processed_data", "260805_glmm_several.csv"))
# forest
all_forest_3 <- left_join(all_forest_2,year_join, by ="year") |> dplyr::select(-year) 
all_forest_3 <- all_forest_3|> rename(year = V2)|> dplyr::select(species, year, index, se, ZIP3_ID)
write.csv(all_forest_3, file.path("Bird_data", "processed_data", "260805_glmm_forest.csv"))
#-------------------------------------------------------------------------------
# NEST TYPE
#-------------------------------------------------------------------------------
# # CA
# all_CA <- unique(bird_CA$Species)
# all_CA_3 <-  bird_func3(bird_sample2, all_CA)
# # GC
# all_GC <- unique(bird_GC$Species)
# all_GC_3 <-  bird_func3(bird_sample2, all_GC)# 9 convergence issues
# # write.csv(all_GC_3, file="2401008_all_GC_index_occ.csv")
# # G
# all_G <- unique(bird_G$Species)
# all_G_3 <-  bird_func3(bird_sample2, all_G)# 5 convergence issues
# # write.csv(all_G_3, file="2401014_all_G_index_occ.csv")
# # OA
# all_OA_only<- unique(bird_OA$Species)
# all_OA_3only <-  bird_func3(bird_sample2, all_OA_only) # 1 convergence issues
# # write.csv(all_OA_3only, file="2401009_all_OA_only_index_occ.csv")
# # H
# all_H<- unique(bird_H$Species)
# all_H_2 <-  bird_func3(bird_sample2, all_H) # 2 convergence issues
# # write.csv(all_H_2, file="2401008_all_H_index_occ.csv")
#_________________
# MODEL just for example graph
# with pred_prob and se.fit
#_________________
# Function with GLMM model------------------------------------------------------
bird_func4 <- function(data, specs) {
  # empty list to store predictions
  filtered_data <- list()
  
  # Iteration for every species
  for (spec in specs) {
    # filter for current species
    spec.sub <- subset(data, Species == spec)
    
    # check if spec.sub is empty
    if (nrow(spec.sub) == 0) {
      next  
    }
    
    # Model
    mod_1 <- glmmTMB(prop_garden ~ Year + null_prop3 + (1 + Year | ZIP3_ID),
                     family = binomial(link = "logit"), weights = garden_tot, data = spec.sub)
    
    # new dataframe
    newdat <- expand.grid(
      ZIP3_ID = unique(spec.sub$ZIP3_ID),
      Year = unique(spec.sub$Year),
      Species = spec,
      garden_tot = mean(spec.sub$garden_tot, na.rm = TRUE),
      null_prop3 = mean(spec.sub$null_prop3, na.rm = TRUE),
      spec_no_garden = mean(spec.sub$spec_no_garden, na.rm = TRUE)
    )
    
    # variable types
    newdat$Year <- as.numeric(newdat$Year) 
    newdat$null_prop3 <- as.numeric(newdat$null_prop3) 
    
    # Prediction with standard error
    pred_results <- predict(mod_1, newdata = newdat, type = "response", se.fit = TRUE)
    
    # insert predictions to new dataframe
    newdat$pred_prob <- pred_results$fit
    newdat$se_fit <- pred_results$se
    filtered_data[[spec]] <- newdat[, c("ZIP3_ID", "Species", "Year", "pred_prob", "se_fit")]  }
  # add predictions to list
  final_result <- bind_rows(filtered_data)
  
  result2 <- subset(final_result, select = c(Species, Year, pred_prob, se_fit, ZIP3_ID))
  colnames(result2) <- c("species", "year", "pred_prob", "se_fit", "ZIP3_ID")
  
  # give back data
  return(result2)
}
# run function------------------------------------------------------------------
# one species 
one_spec <- unique("Gruenfink")
spec_3 <- bird_func4(bird_sample2, one_spec)

# subset for Graph, example ZIP code
test3 <- subset(spec_3, ZIP3_ID=="822" & species=="Gruenfink")
test3 |> group_by(ZIP3_ID, year, pred_prob, se_fit) |> summarize()-> test3.2

# GRAPH FOR METHOD PART AS EXAMPLE FOR ONE ZIP code area------------------------
bird_scale_sub <- subset(bird_sample2, Species=="Gruenfink" & ZIP3_ID=="822")
#bird_scale_sub |> dplyr::select(prop_garden)->bird_sub3

# rename
bird_scale_sub <- bird_scale_sub |> rename(year= Year)
# with prop_garden for raw data
join_index_berlin <- bird_scale_sub |> dplyr::select(prop_garden, year) |> left_join(test3.2, by="year")

# make year nice
year_join$year <- as.numeric(year_join$year)
join_index_berlin <- left_join(join_index_berlin,year_join, by ="year")

# plot raw data without index---------------------------------------------------
my_color <- c("Raw data" = "darkgrey",
              "Predicted data" = "darkgreen",
              "Standard Error" = "olivedrab")

gs <- ggplot(join_index_berlin, aes(x=V2)) +
  geom_line(aes(y=prop_garden, color="Raw data"), linewidth = 1) +
  geom_line(aes(y=pred_prob, color="Predicted data"), linewidth = 0.5) +
  # errorbar lines for sd
  geom_line(aes(y = pred_prob - se_fit, color = "Standard Error"), linetype = "dashed",  linewidth = 0.2) +
  geom_line(aes(y = pred_prob + se_fit, color = "Standard Error"), linetype = "dashed",  linewidth = 0.2) +
  theme_bw() +
  labs(fill = "Legend", color = "Bird data", x = "Year", y = "Proportion",
       title = "Greenfinch Population Trend",
       subtitle="Observed vs. Expected Trend in Berlin (2006-2021)") +
  
  scale_color_manual(values = my_color) +
  theme(
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.text = element_text(size = 10))
gs

ggsave("250729_Greenfich.png")

#-------------------------------------------------------------------------------
# MULTI SPECIES INDICATOR
#-------------------------------------------------------------------------------

# msi calculation
bird_func5 <- function(data) {
  
  res_msi <- NULL  # empty dataframe to store output
  trend_msi <- NULL
  # take ZIP3_ID
  unique_zip3 <- unique(data$ZIP3_ID)
  
  for (k in unique_zip3) {
    sub_zip <- subset(data, ZIP3_ID == k)
    # # remove column ZIP3_ID from data frame
    sub_zip$ZIP3_ID <- NULL
    
    # msi-function
    
    msi_c <- msi(sub_zip)
    
    # insert ZIP3_ID again
    msi_c$ZIP3_ID <- k
    
    # join results
    tmp <- msi_c$results
    tmp$ZIP3_ID <- k
    res_msi <- rbind(res_msi, tmp)
    
    tmp2 <- msi_c$trend
    tmp2$ZIP3_ID <- k
    trend_msi <- rbind(trend_msi, tmp2)
    
  }
  
  return(list(result1 = res_msi,trend1= trend_msi)) 
}

# run function------------------------------------------------------------------
all_spec_msi <- bird_func5(all_birds_2)

# Diet
grani_msi <- bird_func5(all_grani_3)
insecti_msi <- bird_func5(all_insect_3)
omni_msi <- bird_func5(all_omni_3)

# Habitat
urban_msi <- bird_func5(all_urban_3)
sev_msi <- bird_func5(all_sev_3)
forest_msi <- bird_func5(all_forest_3)


# # Nest type_________________________
# CA_msi <- bird_func5(all_CA_3)
# GC_msi <- bird_func5(all_GC_3)
# G_msi <- bird_func5(all_G_3)
# OA_msi_only <- bird_func5(all_OA_3only)
# H_msi <- bird_func5(all_H_2)

# take data tables--------------------------------------------------------------
# all species
all_res_msi <- all_spec_msi$result1
all_trend_msi <- all_spec_msi$trend1
all_res_msi_2021 <- all_res_msi |> 
  filter(year == "2021")

# Diet--------------------------------------------------------------------------
# granivore
grani_res_msi <-grani_msi$result1
grani_trend_msi <-grani_msi$trend1

# insectivore
insecti_res_msi <-insecti_msi$result1
insecti_trend_msi <-insecti_msi$trend1

# omnivore
omni_res_msi <-omni_msi$result1
omni_trend_msi <-omni_msi$trend1

# Habitat------------------------------------------------------------------------
# urban
urban_res_msi <-urban_msi$result1
urban_trend_msi<-urban_msi$trend1
# grassland
sev_res_msi <-sev_msi$result1
sev_trend_msi <-sev_msi$trend1
# forest
forest_res_msi <-forest_msi$result1
forest_trend_msi <-forest_msi$trend1

# # Nest type---------------------------------------------------------------------
# CA_res_msi <-CA_msi$result1
# CA_trend_msi<-CA_msi$trend1
# GC_res_msi <-GC_msi$result1
# GC_trend_msi<-GC_msi$trend1
# G_res_msi <-G_msi$result1
# G_trend_msi<-G_msi$trend1
# OA_only_res_msi <-OA_msi_only$result1
# OA_only_trend_msi<-OA_msi_only$trend1
# H_res_msi <-H_msi$result1
# H_trend_msi<-H_msi$trend1

# store data tables-------------------------------------------------------------
# all species
write.csv(all_res_msi, file.path("Results", "tables", "all_species_res_msi_260805.csv"))
#write.csv(all_trend_msi, file= "all_trend_msi_250630.csv")
#write.csv(all_res_msi_2021, file= "all_res_msi_2021_250630.csv")
# Diet
write.csv(grani_res_msi, file.path("Results", "tables", "grani_res_msi_260805.csv"))
#write.csv(grani_trend_msi, file= "grani_trend_msi_250709_2.csv")

write.csv(insecti_res_msi,  file.path("Results", "tables", "insecti_res_msi_260805.csv"))
#write.csv(insecti_trend_msi, file= "insecti_trend_msi_250529.csv")

write.csv(omni_res_msi,  file.path("Results", "tables", "omni_res_msi_260805.csv"))
#write.csv(omni_trend_msi, file= "omni_trend_msi_250530.csv")

# Habitat
write.csv(urban_res_msi, file.path("Results", "tables", "urban_res_msi_260805.csv"))
#write.csv(urban_trend_msi, file= "urban_trend_msi_250730.csv")
write.csv(sev_res_msi, file.path("Results", "tables", "sev_res_msi_260805.csv"))
#write.csv(sev_trend_msi, file= "sev_trend_msi_250730.csv")
write.csv(forest_res_msi, file.path("Results", "tables", "forest_res_msi_260805.csv"))
#write.csv(forest_trend_msi, file= "forest_trend_msi_250730.csv")

# Nest type
#write.csv(CA_res_msi, file= "CA_res_msi_250525.csv")
#write.csv(CA_trend_msi, file= "CA_trend_msi_250525.csv")
#write.csv(GC_res_msi, file= "GC_res_msi_250525.csv")
#write.csv(GC_trend_msi, file= "GC_trend_msi_250525.csv")
#write.csv(G_res_msi, file= "G_res_msi_250525.csv")
#write.csv(G_trend_msi, file= "G_trend_msi_250525.csv")
#write.csv(OA_only_res_msi, file= "OA_only_res_msi_250525.csv")
#write.csv(OA_only_trend_msi, file= "OA_only_trend_msi_250525.csv")
#write.csv(H_res_msi, file= "H_res_msi_250525.csv")
#write.csv(H_trend_msi, file= "H_trend_msi_250525.csv")