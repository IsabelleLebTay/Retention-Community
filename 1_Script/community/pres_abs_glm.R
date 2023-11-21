# install.packages('vegan')
# data(mite, package = "vegan")
# data("mite.env", package = "vegan")
library(tidyverse)
library(cmdstanr)
PATH_TO_CMDSTAN <- "C:/Users/ilebe/.cmdstan/cmdstan-2.31.0" # nolint
set_cmdstan_path(PATH_TO_CMDSTAN)
cmdstan_path()
cmdstan_version()
# Load the necessary library
library(readr)

# Read the CSV file
birds_abund <- read_csv(file.path("0_Data/Processed", "community_abundance_by_location.csv"))
head(birds_abund)
names(birds_abund)
# Identify the columns with the four-letter codes
# Assuming these are all the columns after 'longitude'
code_columns <- names(birds_abund)[5:ncol(birds)]

birds <- birds_abund
# Apply the transformation
birds[code_columns] <- lapply(birds[code_columns], function(x) as.integer(x > 0))

# View the first few rows of the modified data
head(birds)

getwd()
# birds <- read_csv("0. Data/Processed/July 29 results/presence_absence_per_site_mites.csv")
covariates <-read_csv("0_Data/Processed/merged_covariates.csv")


# Merge the dataframes on 'location'
merged_data <- merge(birds, covariates, by = "location", all = FALSE)
head(merged_data)
# Pivot the data to long format
birds_data_long <- merged_data %>%
  pivot_longer(ALFL:YRWA.x, names_to = "spp", values_to = "presabs")

# View the first few rows of the transformed data
head(birds_data_long)

# birds_data_long <- bind_cols(birds_env, birds) |> 
#   pivot_longer(OSFL:YRWA, names_to = "spp", values_to = "presabs")


birds_data_long |> 
  ggplot(aes(x = RETN_m2, y = presabs)) + 
  geom_point() + 
  stat_smooth(method = "glm", method.args = list(family = "binomial")) + 
  facet_wrap(~spp)

birds_data_long |> 
  ggplot(aes(x = Year_since_logging, y = presabs)) + 
  geom_point() + 
  stat_smooth(method = "glm", method.args = list(family = "binomial")) + 
  facet_wrap(~spp)


birds_many_glms <- birds_data_long |> 
  nest_by(spp) |> 
  mutate(logistic_regressions = list(
    glm(presabs ~ Year_since_logging,
        family = "binomial",
        data = data))) |> 
  mutate(coefs = list(broom::tidy(logistic_regressions)))

birds_many_glm_coefs <- birds_many_glms |> 
  select(-data, -logistic_regressions) |> 
  unnest(coefs)

birds_many_glm_coefs |> 
  ggplot(aes(x = estimate, y = spp,
             xmin = estimate - std.error,
             xmax = estimate + std.error)) + 
  geom_pointrange() + 
  facet_wrap(~term, scales = "free")


birds_many_glm_coefs |> 
  ggplot(aes(x = estimate)) + 
  geom_histogram(binwidth = .5) + 
  facet_wrap(~term, scales = "free")

# Shannon diverstiy
library(vegan)
# Calculate the Shannon diversity index
community_matrix <- birds_abund[, 6:ncol(birds_abund)]
community_matrix
shannon_indices <- diversity(community_matrix, index = "shannon")
shannon_indices
birds_abund$Shannon <- shannon_indices
head(birds_abund)
# Display the results
print(shannon_indices)

names(covariates)
selected_covariates <- covariates[, c("location", "percent_decid", "percent_mixed",
                                        "percent_spruce", "percent_pine",
                                        "RETN_m2", "RETN_perimeter_m",
                                        "Year_since_logging", "Veg_cat")]
birds_abund_covs <- merge(selected_covariates, birds_abund, by = "location", all = FALSE)
names(birds_abund_covs)

library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(bbmle) 
library(nlme)
library(ncf)
library(dplyr)
library(mgcv)

predictors_to_scale <- c('RETN_m2', 'Year_since_logging')

# Apply MinMax scaling to each predictor
birds_abund_covs[, predictors_to_scale] <- lapply(birds_abund_covs[, predictors_to_scale], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

diversity_glm <- glmmTMB(Shannon ~ RETN_m2 + Year_since_logging  + Veg_cat
                                    + (1|location),
                        data = birds_abund_covs,
                        family = gaussian)
summary(diversity_glm)
plot(simulateResiduals(diversity_glm))


par(mfrow=c(1,1))
names(birds_abund_covs)

divers_GAm <- gam(divers_resd ~ s(longitude.y, latitude.y), data = birds_abund_covs, family = gaussian)
plot(divers_GAm)
summary(divers_GAm)
autoXY <- fitted(divers_GAm) 

diversity_glm_autoXY <- glmmTMB(Shannon ~ RETN_m2 + Year_since_logging  + Veg_cat
                                    + autoXY
                                    + (1|location),
                        data = birds_abund_covs,
                        family = gaussian)

summary(diversity_glm_autoXY)
plot(simulateResiduals(diversity_glm_autoXY))

divers_resd_XY <- residuals(diversity_glm_autoXY)

corr <- spline.correlog(birds_abund_covs$longitude.y, birds_abund_covs$latitude.y, divers_resd, resamp = 99, type = "boot")
corrXY <- spline.correlog(birds_abund_covs$longitude.y, birds_abund_covs$latitude.y, divers_resd_XY, resamp = 99, type = "boot")


par(mfrow=c(1,2))
plot(corr, main='null model')
plot(corrXY, main='spatial model')

birds_abund_covs


## dbRDA

write.csv(birds_abund_covs, file.path("0_Data/Processed", "birds_abund_covs.csv"))
# Prepare species abundance data (assuming these are the last columns, adjust as needed)
species_data <- birds_abund_covs[, 14:ncol(birds_abund_covs)]  # Adjust the column indices as per your data
?decostand
new_sepcies_data <- decostand(species_data, method = "hellinger")

head(new_sepcies_data)
# Calculate the Bray-Curtis distance matrix
# distance_matrix <- vegdist(species_data, method = "bray")
names(birds_abund_covs)
# Environmental variables
env_data <- birds_abund_covs[, c("RETN_m2", "Year_since_logging", "percent_decid", "percent_mixed",
                                        "percent_spruce", "percent_pine" )]
env_data
# Running dbRDA
dbrda_result <- rda(new_sepcies_data ~ RETN_m2 + Year_since_logging + percent_decid
                                      + percent_spruce + percent_pine, data = env_data, distance="rlcr",
                                      comm = NULL)

install.packages(
  "ProcMod"
)

is_euclid(new_sepcies_data)
is.eucli

new_sepcies_data <- vegdist(new_sepcies_data, method = "rlcr")
?vegdist
# View the results
summary(dbrda_result)
plot(dbrda_result)
anova(dbrda_result, by = "margin")


adonis_result <- adonis(species_data ~ RETN_m2 + Year_since_logging + percent_decid
                                      + percent_spruce + percent_pine, data = env_data, distance="bray")
summary(adonis_result)
View(birds_abund_covs)

library(HillR)
# hill_taxa (q=0) will give richness