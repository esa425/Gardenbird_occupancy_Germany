library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(bayesplot)
library(compositions)
library(rstan)
#____________
# Data tables___________________________________________________________________
#____________
# all data

base_path <- file.path("Bird_data", "Processed_data")
#____________
# Data tables___________________________________________________________________
#____________
# all data
file1 <- file.path(base_path, "260730_bird_prop_landuse_ZIP3_final.csv")
bird_land <- read.csv(file1, sep = ",", dec = ".")
bird_land <- bird_land |> rename(garden_tot = total_participants)

# Combine classes 2, 6 to 11 into a single class "Other"------------------------
# sum up classes into one "Other" class
data_wide_combined <- bird_land |>
  mutate(
    Other = rowSums(across(c(lu2, lu6, lu7, lu8, lu9, lu10, lu11)), 
                    na.rm = TRUE)) |>
    dplyr::select(-lu2, -lu6, -lu7, -lu8, -lu9, -lu10, -lu11)

# round 3 digits

#select necessary columns
land_select <- data_wide_combined |> 
  group_by(ZIP3_ID, Year, lu1, lu3, lu4, lu5, Other) |> 
  summarize()
# filter rows with any zero values
# zero_rows <- land_select |> filter(if_any(everything(), ~ . == 0))

#Data adjustments---------------------------------------------------------------
#select year 2021
data_wide <- land_select|>
  filter(Year == "2021")

# Select the compositional columns
landuse_data <- data_wide[, c("lu1", "lu3", "lu4", "lu5", "Other" )]
# landuse_data<- landuse_data |> distinct()
# rename
colnames(landuse_data) <- c("urban", "agri", "grass", "forest", "other")

#-------------------------------------------------------------------------------
# Bayesian-Multiplicative replacement of zeros----------------------------------

# Bayesian-Multiplicative replacement
imputed_matrix <- zCompositions::cmultRepl(landuse_data)

imputed_matrix2<- imputed_matrix |> mutate(sum=rowSums(imputed_matrix))

#-------------------------------------------------------------------------------
# apply the CLR transformation--------------------------------------------------
clr_transformed <- clr(imputed_matrix)

# convert the CLR result back to a dataframe
clr_df <- as.data.frame(clr_transformed)
clr_df_n <-  clr_df 
colnames( clr_df_n) <- c("urban1", "agri1","grass1", "forest1")

# Combine with the zip names
result_df <- cbind(ZIP3_ID = data_wide$ZIP3_ID, clr_df)
#write.csv(result_df, file= "250730_clr_transformed_landuse_test.csv")

# back transform----------------------------------------------------------------
test_back<- clrInv(clr_transformed)
test_back <- as.data.frame(test_back)
# Combine with the zip names
result_df_back <- cbind(ZIP3_ID = data_wide$ZIP3_ID, test_back)

#write.csv(result_df_back, file= "250730_clr_backtransformed_landuse_test.csv")

#-------------------------------------------------------------------------------
#MODELS
#-------------------------------------------------------------------------------
base_path1 <- file.path("Results", "Tables" )
file2 <- file.path(base_path1,"all_species_res_msi_260805.csv")
file3 <- file.path(base_path1,"grani_res_msi_260805.csv")
file4 <- file.path(base_path1,"insecti_res_msi_260805.csv")
file5 <- file.path(base_path1,"omni_res_msi_260805.csv")
#_______________________
#ALL SPECIES
#_______________________

# join land use and msi
msi_all<- read.csv(file2, sep = ",", dec = ".")

# data for 2021
msi_all_2021 <- msi_all|> 
  filter(year=="2021")

# join data sets
join_land_msi_a <- left_join(msi_all_2021, result_df, by="ZIP3_ID")

# BRMS MODEL all species--------------------------------------------------------
library(Rcpp)
library(brms)
library(ggeffects)
library(gridExtra)
mod_brms_a <- brm(MSI | se(sd_MSI, sigma= TRUE) ~ agri + forest + grass + urban, 
                  data = join_land_msi_a,family = "gaussian")
get_prior(MSI | se(sd_MSI, sigma = TRUE) ~ agri + forest + grass + urban, data = join_land_msi_a, family = gaussian()) 
summary(mod_brms_a)
# save summary output
summary_all_df <- summary(mod_brms_a)$fixed
write.csv(summary_all_df, "Results/Summary_brms/260313_summary_all_model.csv", row.names = FALSE)
all <- read.csv("Results/Summary_brms/260313_summary_all_model.csv")

# Diagnostic plots
# Plot the model and store the plots in a list
plots_a <- plot(mod_brms_a, combined = FALSE, ask = FALSE)

# Save each plot
for (i in seq_along(plots_a)) {
  ggsave(
    filename = paste0("Results/figures/260313_brms_a_plot_", i, ".png"),
    plot = plots_a[[i]]
  )
}

pp_check(mod_brms_a, ndraws = 100) #does not look very nice, is this still ok? at least the summary values look fine

ggsave(filename = "Results/figures/160313_brms_all_spec_pp_check.png")


# Model assumption--------------------------------------------------------------
# Extract regression coefficients
coefficients_a <- as.data.frame(fixef(mod_brms_a))
coefficients_a2 <- coefficients_a|>
  mutate(Diet = "All_species") 
# Extract Bayesian R-squared
bayes_r2_all <- bayes_R2(mod_brms_a)

# Convert to data frame (if not already)
bayes_r2_df <- as.data.frame(bayes_r2_all)
# Save as CSV
write.csv(bayes_r2_df, "Results/Summary_brms/260213_bayes_r2_all.csv", row.names = FALSE)

# Get fitted values and residuals
fitted_values <- fitted(mod_brms_a)[, 1]
residuals_values <- residuals(mod_brms_a)[, 1]
covariat_urban <- as.vector(join_land_msi_a$urban)
covariat_agri <- as.vector(join_land_msi_a$agri)
covariat_forest <- as.vector(join_land_msi_a$forest)
covariat_grass <- as.vector(join_land_msi_a$grass)
res_urb <- cbind(residuals_values,covariat_urban)
res_agr<- cbind(residuals_values,covariat_agri)
res_forest<- cbind(residuals_values,covariat_forest)
res_grass<- cbind(residuals_values,covariat_grass)
qplot(fitted_values, residuals_values)
qplot(data = res_urb, x = covariat_urban, y = residuals_values)
qplot(data = res_urb, x = covariat_agri, y = residuals_values)
qplot(data = res_urb, x = covariat_forest, y = residuals_values)
qplot(data = res_urb, x = covariat_grass, y = residuals_values)

#_______________________
# GRANIVORE
#_______________________
msi_grani<- read.csv(file3, sep = ",", dec = ".")

# data for 2021
msi_grani_2021 <- msi_grani|> 
  filter(year=="2021")

# join data sets
join_land_msi <- left_join(msi_grani_2021, result_df, by="ZIP3_ID")

# BRMS MODEL granivore----------------------------------------------------------

mod_brms <- brm(MSI | se(sd_MSI, sigma= TRUE) ~ agri + forest + grass + urban,
                data = join_land_msi, family = "gaussian")

summary(mod_brms)
# save summary output
summary_grani_df <- summary(mod_brms)$fixed
write.csv(summary_grani_df, "Results/Summary_brms/260313_summary_grani_model.csv", row.names = FALSE)
grani <- read.csv("Results/Summary_brms/260313_summary_grani_model.csv")

# Diagnostic plots
# Plot the model and store the plots in a list
plots_g <- plot(mod_brms, combined = FALSE, ask = FALSE)

# Save each plot
for (i in seq_along(plots_g)) {
  ggsave(
    filename = paste0("Results/figures/260313_brms_g_plot_", i, ".png"),
    plot = plots_g[[i]]
  )
}

pp_check(mod_brms, ndraws = 100) #does not look very nice, is this still ok? at least the summary values look fine

ggsave(filename = "Results/figures/160313_brms_grani_spec_pp_check.png")

#Model assumption---------------------------------------------------------------
# Extract regression coefficients
coefficients_gr <- as.data.frame(fixef(mod_brms))
coefficients_gr2 <- coefficients_gr|>
  mutate(Diet = "Granivore")

# Extract Bayesian R-squared
bayes_r2_value <- bayes_R2(mod_brms)

# Convert to data frame (if not already)
bayes_r2_df_g <- as.data.frame(bayes_r2_value)
# Save as CSV
write.csv(bayes_r2_df_g, "Results/Summary_brms/260213_bayes_r2_grani.csv", row.names = FALSE)

# Get fitted values and residuals
fitted_values <- fitted(mod_brms)[, 1]
residuals_values <- residuals(mod_brms)[, 1]
covariat_urban <- as.vector(join_land_msi$urban)
covariat_agri <- as.vector(join_land_msi$agri)
covariat_forest <- as.vector(join_land_msi$forest)
res_urb <- cbind(residuals_values,covariat_urban)
res_agr<- cbind(residuals_values,covariat_agri)
res_forest<- cbind(residuals_values,covariat_forest)
ggplot2::qplot(fitted_values, residuals_values)
ggplot2::qplot(data = res_urb, x = covariat_urban, y = residuals_values)
ggplot2::qplot(data = res_urb, x = covariat_agri, y = residuals_values)
ggplot2::qplot(data = res_urb, x = covariat_forest, y = residuals_values)

#_______________________
# INSECTIVORE
#_______________________
msi_insecti<- read.csv(file4, sep = ",", dec = ".")

# data for 2021
msi_insecti_2021 <- msi_insecti|> 
  filter(year=="2021")

# join data sets
join_land_msi_i <- left_join(msi_insecti_2021, result_df, by="ZIP3_ID")

# BRMS MODEL insectivore--------------------------------------------------------
mod_brms_i <- brm(MSI | se(sd_MSI, sigma= TRUE) ~ agri + forest + grass + urban, 
                  data = join_land_msi_i, family = "gaussian")

summary(mod_brms_i)
# save summary output
summary_insecti_df <- summary(mod_brms_i)$fixed
write.csv(summary_insecti_df, "Results/Summary_brms/260313_summary_insecti_model.csv", row.names = FALSE)
insecti <- read.csv("Results/Summary_brms/260313_summary_insecti_model.csv")
# Diagnostic plots
# Plot the model and store the plots in a list
plots_i <- plot(mod_brms_i, combined = FALSE, ask = FALSE)

# Save each plot
for (i in seq_along(plots_i)) {
  ggsave(
    filename = paste0("Results/figures/260313_brms_i_plot_", i, ".png"),
    plot = plots_i[[i]]
  )
}

pp_check(mod_brms_i, ndraws = 100) #does not look very nice, is this still ok? at least the summary values look fine

ggsave(filename = "Results/figures/160313_brms_insecti_spec_pp_check.png")

# Model assumption--------------------------------------------------------------
# Extracting coefficients (fixef) for the model
coefficients_i <-as.data.frame(fixef(mod_brms_i))
coefficients_i2 <- coefficients_i|>
  mutate(Diet = "insectivore")

# Calculate Bayes R2 
bayes_r2_value_i <- bayes_R2(mod_brms_i)

# Convert to data frame 
bayes_r2_df_i <- as.data.frame(bayes_r2_value_i)
# Save as CSV
write.csv(bayes_r2_df_i, "Results/Summary_brms/260213_bayes_r2_insecti.csv", row.names = FALSE)

point_preds <- fitted(mod_brms_i)[, 1]
point_errs <- residuals(mod_brms_i)[, 1]
qplot(point_preds, point_errs)

# Get fitted values and residuals
covariat_urban_i <- as.vector(join_land_msi_i$urban)
covariat_agri_i <- as.vector(join_land_msi_i$agri)
covariat_forest_i <- as.vector(join_land_msi_i$forest)
res_urb_i <- cbind(point_errs,covariat_urban_i)
res_agr_i<- cbind(point_errs,covariat_agri_i)
res_forest_i<- cbind(point_errs,covariat_forest_i)
qplot(data = res_urb_i, x = covariat_urban_i, y = point_errs)
qplot(data = res_agr_i, x = covariat_agri_i, y = point_errs)
qplot(data = res_forest_i, x = covariat_forest_i, y = point_errs)

#_______________________
# OMNIVORE
#_______________________
msi_omni<- read.csv(file5, sep = ",", dec = ".")

# data for 2021
msi_omni_2021 <- msi_omni|> 
  filter(year=="2021")

# join data sets
join_land_msi_o <- left_join(msi_omni_2021, result_df, by="ZIP3_ID")

# BRMS MODEL omnivore-----------------------------------------------------------
mod_brms_o <- brm(MSI | se(sd_MSI, sigma= TRUE) ~ agri + forest + grass + urban, 
                  data = join_land_msi_o , family = "gaussian")

summary(mod_brms_o)
# save summary output
summary_omni_df <- summary(mod_brms_o)$fixed
write.csv(summary_omni_df, "Results/Summary_brms/260313_summary_omni_model.csv", row.names = FALSE)
omni <- read.csv("Results/Summary_brms/260313_summary_omni_model.csv")
# Diagnostic plots
# Plot the model and store the plots in a list
plots_o <- plot(mod_brms_o, combined = FALSE, ask = FALSE)

# Save each plot
for (i in seq_along(plots_o)) {
  ggsave(
    filename = paste0("Results/figures/260313_brms_o_plot_", i, ".png"),
    plot = plots_o[[i]]
  )
}

pp_check(mod_brms_o, ndraws = 100) #does not look very nice, is this still ok? at least the summary values look fine

ggsave(filename = "Results/figures/160313_brms_omni_spec_pp_check.png")

# model assessment---------------------------------------------------------------
coefficients_o <-  as.data.frame(fixef(mod_brms_o))
coefficients_o2 <- coefficients_o|>
  mutate(Diet = "Omnivore")     

bayes_r2_value_o <- bayes_R2(mod_brms_o)

# Convert to data frame 
bayes_r2_df_o <- as.data.frame(bayes_r2_value_o)
# Save as CSV
write.csv(bayes_r2_df_o, "Results/Summary_brms/260213_bayes_r2_omni.csv", row.names = FALSE)

point_preds <- fitted(mod_brms_o)[, 1]
point_errs <- residuals(mod_brms_o)[, 1]
qplot(point_preds, point_errs)

# Get fitted values and residuals
covariat_urban_o <- as.vector(join_land_msi_o$urban)
covariat_agri_o <- as.vector(join_land_msi_o$agri)
covariat_forest_o <- as.vector(join_land_msi_o$forest)
res_urb_o <- cbind(residuals_values,covariat_urban)
res_agr_o<- cbind(residuals_values,covariat_agri)
res_forest_o<- cbind(residuals_values,covariat_forest)
qplot(data = res_urb_o, x = covariat_urban_o, y = point_errs)
qplot(data = res_agr_o, x = covariat_agri_o, y = point_errs)
qplot(data = res_forest_o, x = covariat_forest_o, y = point_errs)

# # Zwei Modelle vergleichen
# library(loo)
# loomod1 <- loo(mod_brms_o)
# loomod2 <- loo(mod_brms_o2)
# loomod1 <- loo(mod_brms_o, save_pars = TRUE)
# pareto_k_values(loomod1)
# # Modellvergleich
# loomod1 <- loo(mod_brms_o, k_threshold = 0.9, save_pars = TRUE)
# 
# loo_compare(loomod1, loomod2)
# 
# waicmod1 <- waic(mod_brms_o)
# waicmod2 <- waic(mod_brms_o2)
# waic_compare(waicmod1, waicmod2)

#_______________________________________________________________________________
#plotting
#_______________________________________________________________________________
# Berlin example
msi_all<- read.csv(file2, sep = ",", dec = ".")

msi_berlin_all <- subset(msi_all, ZIP3_ID=="822" ) #822
msi_berlin_grani <- subset(msi_grani, ZIP3_ID=="822")

msi_berlin_insecti <- subset(msi_insecti, ZIP3_ID=="822")
msi_berlin_omni <- subset(msi_omni, ZIP3_ID=="822")

# check variables
msi_berlin_all$year <- as.character(msi_berlin_all$year)
msi_berlin_grani$year <- as.character(msi_berlin_grani$year)
msi_berlin_insecti$year <- as.character(msi_berlin_insecti$year)
msi_berlin_omni$year <- as.character(msi_berlin_omni$year)

my_colors1 <- c("granivore" = "darkgoldenrod4",
                "insectivore" = "darkorange",
                "omnivore" = "darkgrey",
                "all species" = "black")
# Define new labels for the levels of Type
new_labels1 <- c("granivore" = "Granivore (n = 8)",
                 "insectivore" = "Insectivore (n = 17)",
                 "omnivore" = "Omnivore (n = 10)",
                 "all species" = "All species (n = 35)")

gs <- ggplot() +
  
  geom_line(data = msi_berlin_all, aes(y = MSI, x = year, color = "all species", group = 1), linewidth = 1) +
  geom_line(data = msi_berlin_insecti, aes(y = MSI, x = year, color = "insectivore", group = 1), linewidth = 1) +
  geom_line(data = msi_berlin_omni, aes(y = MSI, x = year, color = "omnivore", group = 1), linewidth = 1) +
  geom_line(data = msi_berlin_grani, aes(y = MSI, x = year, color = "granivore", group = 1), linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.5) +  # Use geom_hline for horizontal lines
  theme_bw() +
  # ylim(50, 150) +
  labs(x = "Year", y = "MSI index",
       title = "MSI occupancy trends for different \n groups of species in Berlin (2006-2021)",
       color = "Species group") +
  scale_color_manual(values = my_colors1, labels = new_labels1) +  # Include new_labels here
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold")
  )

gs
ggsave(filename = "Results/figures/260313_Diet_Berlin.png", plot = gs, width = 15, height = 10, units = "cm")

#-------------------------------------------------------------------------------
#HABITAT
#-------------------------------------------------------------------------------
base_path2 <- file.path("Results", "tables" )
file_urb <- file.path(base_path2,"urban_res_msi_260316.csv")
file_sev <- file.path(base_path2,"sev_res_msi_260316.csv")
file_for <- file.path(base_path2,"forest_res_msi_260316.csv")

#_______________________
# urban
#_______________________
msi_urban<- read.csv(file_urb, sep = ",", dec = ".")

# data for 2021
msi_urban_2021 <- msi_urban|> 
  filter(year=="2021")

# join data sets
join_land_msi <- left_join(msi_urban_2021, result_df, by="ZIP3_ID")

# BRMS MODEL urban----------------------------------------------------------
library(Rcpp)
library(brms)
library(ggeffects)
library(gridExtra)

mod_brms_u <- brm(MSI | se(sd_MSI, sigma= TRUE) ~ agri + forest + grass + urban,
                  data = join_land_msi, family = "gaussian")

summary(mod_brms_u)
# save summary output
summary_urban_df <- summary(mod_brms_u)$fixed
write.csv(summary_urban_df, "Results/Summary_brms/260313_summary_urban_model.csv", row.names = FALSE)
urban <- read.csv("Results/Summary_brms/260313_summary_urban_model.csv")
# Diagnostic plots
# Plot the model and store the plots in a list
plots_u <- plot(mod_brms_u, combined = FALSE, ask = FALSE)

# Save each plot
for (i in seq_along(plots_u)) {
  ggsave(
    filename = paste0("Results/figures/260313_brms_u_plot_", i, ".png"),
    plot = plots_u[[i]]
  )
}

pp_check(mod_brms_u, ndraws = 100) #does not look very nice, is this still ok? at least the summary values look fine

ggsave(filename = "Results/figures/160313_brms_urban_spec_pp_check.png")
#Model assumption---------------------------------------------------------------

# Extract regression coefficients
coefficients_u <- as.data.frame(fixef(mod_brms_u))
coefficients_u2 <- coefficients_u|>
  mutate(Habitat = "urban")
#coefficients_u2 <- rownames_to_column(coefficients_u2, var = "Type") 

# Extract Bayesian R-squared
bayes_r2_value_u <- bayes_R2(mod_brms_u)

# Convert to data frame 
bayes_r2_df_u <- as.data.frame(bayes_r2_value_u)
# Save as CSV
write.csv(bayes_r2_df_u, "Results/Summary_brms/260213_bayes_r2_urban.csv", row.names = FALSE)

# Get fitted values and residuals
fitted_values <- fitted(mod_brms_u)[, 1]
residuals_values <- residuals(mod_brms_u)[, 1]
covariat_urban <- as.vector(join_land_msi$urban)
covariat_agri <- as.vector(join_land_msi$agri)
covariat_forest <- as.vector(join_land_msi$forest)
res_urb <- cbind(residuals_values,covariat_urban)
res_agr<- cbind(residuals_values,covariat_agri)
res_forest<- cbind(residuals_values,covariat_forest)
ggplot2::qplot(fitted_values, residuals_values)
ggplot2::qplot(data = res_urb, x = covariat_urban, y = residuals_values)
ggplot2::qplot(data = res_urb, x = covariat_agri, y = residuals_values)
ggplot2::qplot(data = res_urb, x = covariat_forest, y = residuals_values)

#_______________________
# Several
#_______________________
msi_sev<- read.csv(file_sev, sep = ",", dec = ".")

# data for 2021
msi_sev_2021 <- msi_sev|> 
  filter(year=="2021")

# join data sets
join_land_msi_sv <- left_join(msi_sev_2021, result_df, by="ZIP3_ID")

# BRMS MODEL several--------------------------------------------------------
mod_brms_sv <- brm(MSI | se(sd_MSI, sigma= TRUE) ~ agri + forest + grass + urban, 
                   data = join_land_msi_sv, family = "gaussian")

summary(mod_brms_sv)
# save summary output
summary__sev_df <- summary(mod_brms_sv)$fixed
write.csv(summary__sev_df, "Results/Summary_brms/260313_summary_sev_model.csv", row.names = FALSE)
several <- read.csv("Results/Summary_brms/260313_summary_sev_model.csv")
# Diagnostic plots
# Plot the model and store the plots in a list
plots_sv <- plot(mod_brms_sv, combined = FALSE, ask = FALSE)

# Save each plot
for (i in seq_along(plots_sv)) {
  ggsave(
    filename = paste0("Results/figures/260313_brms_sv_plot_", i, ".png"),
    plot = plots_sv[[i]]
  )
}

pp_check(mod_brms_sv, ndraws = 100) #does not look very nice, is this still ok? at least the summary values look fine

ggsave(filename = "Results/figures/160313_brms__sev_spec_pp_check.png")

# Model assumption--------------------------------------------------------------
# Extracting coefficients (fixef) for the model
coefficients_sv <-as.data.frame(fixef(mod_brms_sv))
coefficients_sv2 <- coefficients_sv|>
  mutate(Habitat = "several")
#coefficients_sv2 <- rownames_to_column(coefficients_sv2, var = "Type")

# Calculate Bayes R2 
bayes_r2_value_sv <- bayes_R2(mod_brms_sv)#["urban", "Estimate"]

# Convert to data frame 
bayes_r2_df_sv <- as.data.frame(bayes_r2_value_sv)
# Save as CSV
write.csv(bayes_r2_df_sv, "Results/Summary_brms/260213_bayes_r2_several.csv", row.names = FALSE)

point_preds <- fitted(mod_brms_sv)[, 1]
point_errs <- residuals(mod_brms_sv)[, 1]
qplot(point_preds, point_errs)

# Get fitted values and residuals
covariat_urban_sv <- as.vector(join_land_msi_sv$urban)
covariat_agri_sv <- as.vector(join_land_msi_sv$agri)
covariat_forest_sv <- as.vector(join_land_msi_sv$forest)
res_urb_sv <- cbind(point_errs,covariat_urban_sv)
res_agr_sv<- cbind(point_errs,covariat_agri_sv)
res_forest_sv<- cbind(point_errs,covariat_forest_sv)
qplot(data = res_urb_sv, x = covariat_urban_sv, y = point_errs)
qplot(data = res_agr_sv, x = covariat_agri_sv, y = point_errs)
qplot(data = res_forest_sv, x = covariat_forest_sv, y = point_errs)

#_______________________
# forest
#_______________________
msi_forest<- read.csv(file_for, sep = ",", dec = ".")

# data for 2021
msi_forest_2021 <- msi_forest|> 
  filter(year=="2021")

# join data sets
join_land_msi_f <- left_join(msi_forest_2021, result_df, by="ZIP3_ID")

# BRMS MODEL forestvore-----------------------------------------------------------
mod_brms_f <- brm(MSI | se(sd_MSI, sigma= TRUE) ~ agri + forest + grass + urban, 
                  data = join_land_msi_f , family = "gaussian")

summary(mod_brms_f)
# save summary output
summary_forest_df <- summary(mod_brms_f)$fixed
write.csv(summary_forest_df, "Results/Summary_brms/260313_summary_forest_model.csv", row.names = FALSE)
forest <- read.csv("Results/Summary_brms/260313_summary_forest_model.csv")
# Diagnostic plots
# Plot the model and store the plots in a list
plots_f <- plot(mod_brms_f, combined = FALSE, ask = FALSE)

# Save each plot
for (i in seq_along(plots_f)) {
  ggsave(
    filename = paste0("Results/figures/260313_brms_f_plot_", i, ".png"),
    plot = plots_f[[i]]
  )
}

pp_check(mod_brms_f, ndraws = 100) #does not look very nice, is this still ok? at least the summary values look fine

ggsave(filename = "Results/figures/160313_brms_forest_spec_pp_check.png")
# model assessment---------------------------------------------------------------
coefficients_f <-  as.data.frame(fixef(mod_brms_f))
coefficients_f2 <- coefficients_f|>
  mutate(Habitat = "forest")                         
#coefficients_f2 <- rownames_to_column(coefficients_f2, var = "Type")

bayes_r2_value_f <- bayes_R2(mod_brms_f)

# Convert to data frame 
bayes_r2_df_f <- as.data.frame(bayes_r2_value_f)
# Save as CSV
write.csv(bayes_r2_df_f, "Results/Summary_brms/260213_bayes_r2_forest.csv", row.names = FALSE)

point_preds <- fitted(mod_brms_f)[, 1]
point_errs <- residuals(mod_brms_f)[, 1]
qplot(point_preds, point_errs)

# Get fitted values and residuals
covariat_urban_f <- as.vector(join_land_msi_f$urban)
covariat_agri_f <- as.vector(join_land_msi_f$agri)
covariat_forest_f <- as.vector(join_land_msi_f$forest)
res_urb_f <- cbind(residuals_values,covariat_urban)
res_agr_f<- cbind(residuals_values,covariat_agri)
res_forest_f<- cbind(residuals_values,covariat_forest)
qplot(data = res_urb_f, x = covariat_urban_f, y = point_errs)
qplot(data = res_agr_f, x = covariat_agri_f, y = point_errs)
qplot(data = res_forest_f, x = covariat_forest_f, y = point_errs)
# 
#_______________________________________________________________________________
#plotting Habitat
#_______________________________________________________________________________
# Berlin
msi_berlin_urban <- subset(msi_urban, ZIP3_ID=="822")
msi_berlin_sev <- subset(msi_sev, ZIP3_ID=="822")
msi_berlin_forest <- subset(msi_forest, ZIP3_ID=="822")

#check variables
msi_berlin_urban$year <- as.character(msi_berlin_urban$year)
msi_berlin_sev$year <- as.character(msi_berlin_sev$year)
msi_berlin_forest$year <- as.character(msi_berlin_forest$year)

my_colors2 <- c("Settlements" = "darkgrey",
                "Several" = "yellowgreen",
                "Forest" = "darkgreen")
# Define new labels for the levels of Type
new_labels2 <- c("Settlements" = "Settlements (n = 8)",
                 "Several" = "Several habitats (n = 8)",
                 "Forest" = "Forest (n = 15)")

gh <- ggplot() +
  # Grassland with the ribbon
  #geom_ribbon(data = msi_berlin_grass, aes(x = year, ymin = MSI - sd_MSI, ymax = MSI + sd_MSI, fill = "Standard Error"), alpha = 1) +
  geom_line(data = msi_berlin_sev, aes(y = MSI, x = year, color = "Several", group = 1), linewidth = 1) +
  
  # Forest
  geom_line(data = msi_berlin_forest, aes(y = MSI, x = year, color = "Forest", group = 1), linewidth = 1) +
  
  # Urban
  geom_line(data = msi_berlin_urban, aes(y = MSI, x = year, color = "Settlements", group = 1), linewidth = 1) +
  
  # Customize the theme
  theme_bw() +
  #  scale_fill_manual(values = "darkorchid", name = "Error") + # Adjust fill scale
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.5) +
  labs(x = "Year", y = "MSI index",
       title = "MSI occupancy trends of different habitat \npreference groups in Berlin (2006-2021)", color = "Habitat preference") +
  scale_color_manual(values = my_colors2, labels = new_labels2) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold")
  )

gh

ggsave(filename = "Results/figures/260313_Habitat_Berlin.png", plot = gh, width = 15, height = 10, units = "cm")

#_______________________
# RESULT Model Plotting
#_______________________
#all species--------------------------------------------------------------------
library(tibble)
res_all <-coefficients_a2
# make row a column
res_all2 <- rownames_to_column(res_all, var = "Type")
res_all3  <- res_all2[-c(1), ]

#-------------------------------------------------------------------------------
# diet and all species
res_fixed <- rbind(coefficients_a2, coefficients_gr2, coefficients_i2, coefficients_o2)
# make row a column
res_fixed2  <- res_fixed[-c(1, 6, 11, 16), ]
res_fixed2 <- rownames_to_column(res_fixed2, var = "Type")
res_fixed2$Type <- gsub("[0-9]+$", "", res_fixed2$Type)

#-------------------------------------------------------------------------------
# Habitat
res_fixed_u <- rbind(coefficients_u2, coefficients_sv2, coefficients_f2)
res_fixed3  <- res_fixed_u[-c(1, 6, 11), ]

res_fixed3 <- rownames_to_column(res_fixed3, var = "Type")
res_fixed3$Type <- gsub("[0-9]+$", "", res_fixed3$Type)

# Habitat
bird_func8 <- function(data) {
  # Sort the data frame by Estimate in decreasing order
  data <- data[order(-data$Estimate), ]
  
  # Check the unique levels of Type
  unique_types <- unique(data$Type)
  print(unique_types)  # Debugging line
  my_colors <- c("agri" = "gold3",
                 "grass" = "yellowgreen",
                 "urban" = "coral3",
                 "forest" = "chartreuse4")
  # Define new labels for the levels of Type
  new_labels2 <- c("urban" = "Settlement (n = 8)",
                   "several" = "Several types (n = 8)",
                   "forest" = "Forest (n = 15)")
  # Create the plot
  g1 <- ggplot(data = data, aes(x = Estimate, y = Type, fill = Type)) +
    geom_col() +
    geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = .2) +
    theme_bw() +
    labs(x = "Slope estimates", y = "Land-use type",
         title = "Effect of land use on MSI occupancy trends (2006-2021)", 
         fill="Land-use types", subtitle = "Center Log Ratio (CLR) transformed land use") +
    theme(
      axis.title = element_text(face = "bold", size = 12),
      plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) +
    facet_wrap(. ~ Habitat, labeller = as_labeller(new_labels2)) +
    scale_fill_manual(values = my_colors, labels=c('Agriculture', 'Forest', 'Grassland', 'Urban area')) +
    scale_y_discrete(labels = c('Agriculture','Forest','Grassland', 'Urban area'))+
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") 
  
  # Print the plot
  print(g1)
}

bird_func8.1 <- function(data) {
  # Sortiere die Daten nach Habitat und Type
  data <- data[order(data$Type, data$Habitat), ]
  
  # Definiere Farben für die Habitat-Gruppen
  Habitat_colors <- c(
    "urban" = "darkgrey",
    "several" = "yellowgreen",
    "forest" = "darkgreen")
  
  # Definiere Labels für die Legende (mit Zahlenangabe)
  new_labels_Habitat <- c(
    "several" = "Several types (n = 8)",
    "urban" = "Settlement (n = 8)",
    "forest" = "Forest (n = 15)")
  
  # Definiere Labels für die x-Achse (ohne Zahlenangabe)
  new_labels_Habitat_x <- c(
    "several" = "Several",
    "urban" = "Settlements",
    "forest" = "Forest")
  
  # Definiere Labels für die Landuse-Types
  new_labels_type <- c(
    "agri" = "Agriculture",
    "grass" = "Grassland",
    "urban" = "Urban",
    "forest" = "Forest"
  )
  
  # Erstelle den Plot mit den Credible Intervallen (Q2.5 und Q97.5)
  g1 <- ggplot(data = data, aes(x = Habitat, y = Estimate, color = Habitat, shape = Habitat)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
    facet_wrap(. ~ Type, labeller = as_labeller(new_labels_type), ncol = 4) +
    theme_bw() +
    labs(
      x = "Habitat preference groups",
      y = "Slope estimates",
      title = "Effect of land use on MSI occupancy trends (2006-2021)",
      color = "Habitat preference groups",
      shape = "Habitat preference groups",
      subtitle = "Center Log Ratio (CLR) transformed land use"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(face = "bold", size = 12),
      plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      strip.text = element_text(face = "bold", size = 10)
    ) +
    scale_color_manual(values = Habitat_colors, labels = new_labels_Habitat) +
    scale_shape_manual(values = c(16, 17, 15)) +
    scale_x_discrete(labels = new_labels_Habitat_x) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 17, 15))), shape = "none") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ylim(-2, 1)  # Skaliert die y-Achse von -1 bis 1
  
  # Gib den Plot aus
  print(g1)
}


# Habitat run function----------------------------------------------------------
habitat_land <-  bird_func8(res_fixed3)
habitat_land2 <-  bird_func8.1(res_fixed3)
ggsave(filename = "Results/figures/260316_habitat_estimates_landuse.png", plot = habitat_land, width = 16, height = 12, units = "cm")
ggsave(filename = "Results/figures/260316_habitat3_estimates_trait.png", plot = habitat_land2, width = 20, height = 11, units = "cm")

#Diet---------------------------------------------------------------------------
bird_func9 <- function(data) {
  # Sort the data frame by Estimate in decreasing order
  data <- data[order(-data$Estimate), ]
  
  # Check the unique levels of Type
  unique_types <- unique(data$Type)
  print(unique_types)  # Debugging line
  my_colors <- c("agri" = "gold3",
                 "grass" = "yellowgreen",
                 "urban" = "coral3",
                 "forest" = "chartreuse4")
  # Define new labels for the levels of Type
  new_labels <- c("Granivore" = "Granivore (n = 10)",
                  "insectivore" = "Insectivore (n = 17)",
                  "Omnivore" = "Omnivore (n = 8)",
                  "All_species" = "All species (n = 35)" )
  new_labels2 <- c("Human modified" = "Human modified (n = 15)",
                   "Grassland" = "Grassland (n = 16)",
                   "Woodland" = "Woodland (n = 25)",
                   "Forest" = "Forest (n = 43)")
  # Create the plot
  g1 <- ggplot(data = data, aes(x = Estimate, y = Type, fill = Type)) +
    geom_col() +
    geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = .2) +
    theme_bw() +
    labs(x = "Slope estimates", y = "Land-use type",
         title = "Effect of land use on MSI occupancy trends (2006-2021)", 
         fill="Land-use types", subtitle = "Center Log Ratio (CLR) transformed land use") +
    theme(
      axis.title = element_text(face = "bold", size = 12),
      plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) +
    facet_wrap(. ~Diet, labeller = as_labeller(new_labels)) +
    scale_fill_manual(values = my_colors, labels=c('Agriculture', 'Forest', 'Grassland', 'Urban area')) +
    scale_y_discrete(labels = c('Agriculture','Forest','Grassland', 'Urban area'))+
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") 
  
  # Print the plot
  print(g1)
}
bird_func10 <- function(data) {
  # Sortiere die Daten nach Diet und Type
  data <- data[order(data$Type, data$Diet), ]
  
  # Definiere Farben für die Diet-Gruppen
  diet_colors <- c(
    "Granivore" = "darkgoldenrod4",
    "insectivore" = "darkorange",
    "Omnivore" = "darkgrey",
    "All_species" = "black")
  
  
  # Definiere Labels für die Legende (mit Zahlenangabe)
  new_labels_diet <- c(
    "Granivore" = "Granivore (n = 10)",
    "insectivore" = "Insectivore (n = 17)",
    "Omnivore" = "Omnivore (n = 8)",
    "All_species" = "All species (n = 35)"
  )
  
  # Definiere Labels für die x-Achse (ohne Zahlenangabe)
  new_labels_diet_x <- c(
    "Granivore" = "Granivore",
    "insectivore" = "Insectivore",
    "Omnivore" = "Omnivore",
    "All_species" = "All species"
  )
  
  # Definiere Labels für die Landuse-Types
  new_labels_type <- c(
    "agri" = "Agriculture",
    "grass" = "Grassland",
    "urban" = "Urban",
    "forest" = "Forest"
  )
  
  
  # Erstelle den Plot
  g1 <- ggplot(data = data, aes(x = Diet, y = Estimate, color = Diet, shape = Diet)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
    facet_wrap(. ~ Type, labeller = as_labeller(new_labels_type), ncol = 4) +
    theme_bw() +
    labs(
      x = "Diet groups",
      y = "Slope estimates",
      title = "Effect of land use on MSI occupancy trends (2006-2021)",
      color = "Diet groups",
      shape = "Diet groups",
      subtitle = "Center Log Ratio (CLR) transformed land use"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(face = "bold", size = 12),
      plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      strip.text = element_text(face = "bold", size = 10)
    ) +
    scale_color_manual(values = diet_colors, labels = new_labels_diet) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_x_discrete(labels = new_labels_diet_x) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 17, 15, 18))), shape = "none") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  
  # Gib den Plot aus
  print(g1)
}

# run function------------------------------------------------------------------
# all species
res_all3<- as.data.frame(res_all3)
all_species2 <- bird_func9(res_all3)

# Diet
diet_land2<-  bird_func9(res_fixed2)
diet_land3<-  bird_func10(res_fixed2)

ggsave(filename = "Results/figures/260313_all_spec_estimates_slope.png", plot = all_species2, width = 16, height = 12, units = "cm")
ggsave(filename = "Results/figures/260313_Diet_spec_estimates_landuse.png", plot = diet_land2, width = 18, height = 11, units = "cm")
ggsave(filename = "Results/figures/260313_Diet_spec_estimates_traits.png", plot = diet_land3, width = 18, height = 11, units = "cm")
#_______________________________________________________________________________
# Correlation between land use classes
#_______________________________________________________________________________
library(plotly)
library(GGally)
#citation("GGally")
#citation("plotly")
join_landuse_p  <- result_df_back|> dplyr::select(ZIP3_ID, urban, agri, grass, forest)

p1 <- ggpairs(join_landuse_p)
ggsave(filename = "Results/figures/260313_covariable_cor_untrans.png", plot = p1, width = 16, height = 12, units = "cm")

# transformed
join_landuse_p2<- result_df|> dplyr::select(ZIP3_ID,urban, agri, grass,forest)

p2 <- ggpairs(join_landuse_p2)
ggsave(filename = "Results/figures/260313_covariable_cor_trans.png", plot = p2, width = 16, height = 12, units = "cm")

#_______________________________________________________________________________
# zip size and MSI
#_______________________________________________________________________________
# All species
msi_all<- read.csv(file2, sep = ",", dec = ",")

msi_all_2021 <- msi_all|> 
  filter(year=="2021")

base_path2 <- file.path("Results", "tables")
file_area <- file.path(base_path2, file="250703_ZIP3_area.csv")
Zip3_size<- read.csv(file_area , sep = ",", dec = ",")

#join with msi data
zipsize_msi<- left_join(msi_all_2021, Zip3_size, by="ZIP3_ID" )

str(zipsize_msi)

zipsize_msi$Area_3<- as.numeric(zipsize_msi$Area_3)
zipsize_msi$MSI<- as.numeric(zipsize_msi$MSI)

ggplot(zipsize_msi, aes(x=Area_3, y=MSI))+
  geom_point()

zipsize_msi_p<- zipsize_msi|> dplyr::select(Area_3, MSI)
colnames(zipsize_msi_p) <- c("zip size", "MSI")
gp<- ggpairs(zipsize_msi_p)+
  theme_bw()
gp

ggsave(filename = "Results/figures/260313_correlation_MSI_size_all_spec.png", plot =gp, width = 11, height = 8, units = "cm")

# insectivore-------------------------------------------------------------------
zipsize_i<- read.csv(file_area, sep = ",", dec = ",")
msi_insecti<- read.csv(file4, sep = ",", dec = ".")
msi_insecti_2021 <- msi_insecti|> 
  filter(year=="2021")
join_zipsize_i <- left_join(msi_insecti_2021, zipsize_i, by="ZIP3_ID")
join_zipsize_i$Area_3<- as.numeric(join_zipsize_i$Area_3)
join_zipsize_i$MSI<- as.numeric(join_zipsize_i$MSI)
zipsize_msi_i<- join_zipsize_i|> dplyr::select(Area_3, MSI)
colnames(zipsize_msi_i) <- c("zip size", "MSI")
gi<- ggpairs(zipsize_msi_i)+
  theme_bw()
gi

ggsave(filename = "Results/figures/160313_correlation_MSI_size_insecti.png", plot =gi, width = 11, height = 8, units = "cm")


# granivore---------------------------------------------------------------------
msi_grani<- read.csv(file3, sep = ",", dec = ".")
msi_grani_2021 <- msi_grani|> 
  filter(year=="2021")
join_zipsize_g <- left_join(msi_grani_2021, zipsize_i, by="ZIP3_ID")
join_zipsize_g$Area_3<- as.numeric(join_zipsize_g$Area_3)
join_zipsize_g$MSI<- as.numeric(join_zipsize_g$MSI)
zipsize_msi_g<- join_zipsize_g|> dplyr::select(Area_3, MSI)
colnames(zipsize_msi_g) <- c("zip size", "MSI")
gv<- ggpairs(zipsize_msi_g)+
  theme_bw()
gv

ggsave(filename = "Results/figures/260313_correlation_MSI_size_grani.png", plot =gv, width = 11, height = 8, units = "cm")

# omnivore----------------------------------------------------------------------
msi_omni<- read.csv(file5, sep = ",", dec = ".")
msi_omni_2021 <- msi_omni|> 
  filter(year=="2021")
join_zipsize_o <- left_join(msi_omni_2021, zipsize_i, by="ZIP3_ID")
join_zipsize_o$Area_3<- as.numeric(join_zipsize_o$Area_3)
join_zipsize_o$MSI<- as.numeric(join_zipsize_o$MSI)
zipsize_msi_o<- join_zipsize_o|> dplyr::select(Area_3, MSI)
colnames(zipsize_msi_o) <- c("zip size", "MSI")
go<-ggpairs(zipsize_msi_o)+
  theme_bw()
go
ggsave(filename = "Results/figures/260313_correlation_MSI_size_omni.png", plot =go, width = 11, height = 8, units = "cm")

#-------------------------------------------------------------------------------
# Habitat
# Settlements
msi_urban<- read.csv(file = "urban_res_msi_250730.csv", sep = ",", dec = ".")
msi_urban_2021 <- msi_urban|>
  filter(year=="2021")
join_zipsize_urban <- left_join(msi_urban_2021, zipsize_i, by="ZIP3_ID")
join_zipsize_urban$Area_3<- as.numeric(join_zipsize_urban$Area_3)
join_zipsize_urban$MSI<- as.numeric(join_zipsize_urban$MSI)
zipsize_msi_urban<- join_zipsize_urban|> dplyr::select(Area_3, MSI)
colnames(zipsize_msi_urban) <- c("zip size", "MSI")
gu<- ggpairs(zipsize_msi_urban)+
  theme_bw()
gu

ggsave("250730_correlation_MSI_size_urban_hm.png", plot =gu, width = 11, height = 8, units = "cm")

# Several
msi_sev<- read.csv(file = "sev_res_msi_250730.csv", sep = ",", dec = ".")
msi_sev_2021 <- msi_sev|>
  filter(year=="2021")
join_zipsize_sev <- left_join(msi_sev_2021, zipsize_i, by="ZIP3_ID")
join_zipsize_sev$Area_3<- as.numeric(join_zipsize_sev$Area_3)
join_zipsize_sev$MSI<- as.numeric(join_zipsize_sev$MSI)
zipsize_msi_sev<- join_zipsize_sev|> dplyr::select(Area_3, MSI)
colnames(zipsize_msi_sev) <- c("zip size", "MSI")
gg<- ggpairs(zipsize_msi_sev)+
  theme_bw()
gg

ggsave("250730_correlation_MSI_size_several.png", plot =gg, width = 11, height = 8, units = "cm")

#forest
msi_forest<- read.csv(file = "forest_res_msi_250730.csv", sep = ",", dec = ".")
msi_forest_2021 <- msi_forest|>
  filter(year=="2021")
join_zipsize_f <- left_join(msi_forest_2021, zipsize_i, by="ZIP3_ID")
join_zipsize_f$Area_3<- as.numeric(join_zipsize_f$Area_3)
join_zipsize_f$MSI<- as.numeric(join_zipsize_f$MSI)
zipsize_msi_f<- join_zipsize_f|> dplyr::select(Area_3, MSI)
colnames(zipsize_msi_f) <- c("zip size", "MSI")
gf<- ggpairs(zipsize_msi_f)+
  theme_bw()
gf

ggsave("250730_correlation_MSI_size_forest.png", plot =gf, width = 11, height = 8, units = "cm")

