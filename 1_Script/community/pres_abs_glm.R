# Community

#1. Load packages----

library(tidyverse)
library(vegan)
library(readr)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(bbmle) 
library(nlme)
library(ncf)
library(dplyr)
library(mgcv)
library(hillR)
library(patchwork)

# library(cmdstanr)
# PATH_TO_CMDSTAN <- "C:/Users/ilebe/.cmdstan/cmdstan-2.31.0" # nolint
# set_cmdstan_path(PATH_TO_CMDSTAN)
# cmdstan_path()
# cmdstan_version()


#2 Read the CSV file----
birds_abund <- read_csv(file.path("0_Data/Processed", "community_abundance_by_location.csv"))
# head(birds_abund)
# names(birds_abund)

covariates <-read_csv("0_Data/Processed/merged_covariates.csv")

# __________________________________ Data Exploration __________________________________

#3 Logitic regressions on presence/absence----
# Identify the columns with the four-letter codes
# Assuming these are all the columns after 'longitude'
code_columns <- names(birds_abund)[5:ncol(birds_abund)]

birds <- birds_abund
# Apply the transformation
birds[code_columns] <- lapply(birds[code_columns], function(x) as.integer(x > 0))

# Merge the dataframes on 'location'
merged_data <- merge(birds, covariates, by = "location", all = FALSE)
head(merged_data)

ggplot(merged_data, aes(x = RETN_m2)) +
  geom_histogram()

# This data has a few really large retention pathces. Let;s get rid of those
pres_abs_covs_noLargePatches <- merged_data |>
  filter(RETN_m2 < 12000)

# Pivot the data to long format
birds_data_long <- pres_abs_covs_noLargePatches %>%
  pivot_longer(ALFL:PAWR, names_to = "spp", values_to = "presabs")

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

# __________________________________ Diversity Metrics __________________________________
#4 Shannon Diversity----

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

birds_abund_covs_noLargePatches <- birds_abund_covs |>
  filter(RETN_m2 < 12000)

predictors_to_scale <- c('RETN_m2', 'Year_since_logging')

# Apply MinMax scaling to each predictor
birds_abund_covs_noLargePatches[, predictors_to_scale] <- lapply(birds_abund_covs_noLargePatches[, predictors_to_scale], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

ggplot(birds_abund_covs_noLargePatches, aes(x = RETN_m2)) +
  geom_histogram()

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

#5 Richness and evenness----
# hill_taxa (q=0) will give richness

#5.1 Prepare the data-----
selected_covariates <- covariates[, c("location", "percent_decid", "percent_mixed",
                                        "percent_spruce", "percent_pine",
                                        "RETN_m2", "RETN_perimeter_m",
                                        "Year_since_logging", "Veg_cat")]
birds_abund_covs <- merge(selected_covariates, birds_abund, by = "location", all = FALSE)
# names(birds_abund_covs)

birds_abund_covs_noLargePatches <- birds_abund_covs |>
  filter(RETN_m2 < 12000)

species_data <- birds_abund_covs_noLargePatches[, 14:ncol(birds_abund_covs_noLargePatches)]  # Adjust the column indices as per your data

birds_abund_covs_noLargePatches$richness <- hill_taxa(comm= species_data, q=0)
birds_abund_covs_noLargePatches$evenness <- hill_taxa(comm= species_data, q=2)
birds_abund_covs_noLargePatches$evenness

## Cannot have an evenness when there are no species
community_covs_positives <- birds_abund_covs_noLargePatches |>
                                            filter(evenness != "Inf")
# species_data$BTNW
# CAWA <-birds_abund |>
#       filter(CAWA > 0)
# View(community_covs_scaled)

# Scale the predictors
community_covs_scaled <- community_covs_positives
predictors_to_scale <- c('RETN_m2', 'Year_since_logging')
community_covs_scaled[, predictors_to_scale] <- lapply(community_covs_scaled[, predictors_to_scale], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

comm_richness <- glmmTMB(richness ~ percent_decid + percent_pine + 
                            percent_spruce +
                            RETN_m2 + Year_since_logging+ I(Year_since_logging^2),
                    data = community_covs_scaled,
                    family=poisson)
summary(comm_richness)

community_covs_scaled$Veg_cat <- as.factor(community_covs_scaled$Veg_cat)
# birds_abund_covs_noLargePatches$Veg_cat <- relevel(birds_abund_covs_noLargePatches$Veg_cat, ref = "n")

comm_richness_interact <- glmmTMB(richness ~ RETN_m2 * Year_since_logging * Veg_cat,
                    data = community_covs_scaled,
                    family=poisson)                    
summary(comm_richness_interact)

gam_richness1 <- gam(richness ~ s(Year_since_logging),
                    data = community_covs_scaled,
                    family=poisson)   
summary(gam_richness1)
plot(gam_richness1)
AIC(gam_richness1)
gam_richness2 <- gam(richness ~ s(Year_since_logging) + RETN_m2,
                    data = community_covs_scaled,
                    family=poisson)    
summary(gam_richness2)
plot(gam_richness2)
AIC(gam_richness2)

gam_richness3 <- gam(richness ~ s(Year_since_logging) + RETN_m2 + Veg_cat,
                    data = community_covs_scaled,
                    family=poisson)    
summary(gam_richness3)
png(file.path("2_Outputs", "richness gam.png"), width = 400, height = 400)

plot(gam_richness3, residuals=FALSE, rug = FALSE, all.terms = FALSE, 
      ylab = "Effect of year since logging on Richness",
      xlab = "Year since logging, scaled from 0-22 years")
dev.off()
AIC(gam_richness3)

gam_richness4 <- gam(richness ~ s(Year_since_logging) + RETN_m2 * Veg_cat,
                    data = community_covs_scaled,
                    family=poisson)    
summary(gam_richness4)
plot(gam_richness4)
AIC(gam_richness4)

gam_richness5 <- gam(richness ~ s(Year_since_logging) + Veg_cat,
                    data = community_covs_scaled,
                    family=poisson)    
summary(gam_richness5)
plot(gam_richness5)
AIC(gam_richness5)

birds_abund_covs_noLargePatches |>
        ggplot(aes(x = RETN_m2, y = richness, color = Veg_cat)) +
        geom_point()
      
comm_evenness <- glmmTMB(evenness ~ percent_decid + percent_pine + 
                            percent_spruce +
                            RETN_m2 + Year_since_logging,
                    data = community_covs_scaled,
                    family=gaussian)
summary(comm_evenness)

png(file.path("2_Outputs", "evenness gam.png"), width = 400, height = 400)

gam_evenness <- gam(evenness ~ s(Year_since_logging),
                    data = community_covs_scaled,
                    family=gaussian)    
summary(gam_evenness)
plot(gam_evenness,
      ylab = "Effect of year since logging on Richness",
      xlab = "Year since logging, scaled from 0-22 years")
dev.off()
AIC(gam_evenness)

gam_evenness2 <- gam(evenness ~ s(Year_since_logging) + RETN_m2,
                    data = community_covs_scaled,
                    family=gaussian)    
summary(gam_evenness2)
plot(gam_evenness2)
AIC(gam_evenness2)

gam_evenness3 <- gam(evenness ~ s(Year_since_logging) + RETN_m2 + Veg_cat,
                    data = community_covs_scaled,
                    family=gaussian)    
summary(gam_evenness3)
plot(gam_evenness3)
AIC(gam_evenness3)

gam_evenness4 <- gam(evenness ~ s(Year_since_logging) + RETN_m2 * Veg_cat,
                    data = community_covs_scaled,
                    family=gaussian)    
summary(gam_evenness4)
plot(gam_evenness4)
AIC(gam_evenness4)

# Extract fitted values
community_covs_scaled$fitted_evenness <- fitted(comm_evenness)

# Create the plot
ggplot(community_covs_scaled, aes(x = Year_since_logging, y = fitted_evenness)) +
  geom_line() +  # or geom_line() if you prefer a line plot
  xlab("Year Since Logging") +
  ylab("Fitted Evenness") +
  ggtitle("Fitted Evenness over Years Since Logging")

community_covs_scaled |>
        ggplot(aes(x = RETN_m2, y = evenness, color = Veg_cat)) +
        geom_point()

comm_evenness_interact <- glmmTMB(evenness ~  RETN_m2 * Year_since_logging * Veg_cat,
                    data = birds_abund_covs_positives,
                    family=gaussian)
summary(comm_evenness_interact)

png(file.path("2_Outputs", "richness and evenness.png"), width = 1000, height = 1000)
# par(mfrow=c(2, 2))
# Create a simple plot showing how evenness changes with age
p1 <- ggplot(community_covs_scaled, aes(x = Year_since_logging, y = evenness)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "gaussian"))

# Create a simple plot showing how evenness changes with age
p2 <- ggplot(community_covs_scaled, aes(x = Year_since_logging, y = richness)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"))

# Combine the plots side by side
combined_plot <- p1 + p2 +
  plot_layout(widths = c(1, 1), heights = c(1,1))

# Print the combined plot
combined_plot

dev.off()
# Create a plot with the effect of year_since_harvest holding all other variables constant

# Create a new data frame for predictions
predict_data <- data.frame(
  percent_decid = mean(community_covs_scaled$percent_decid, na.rm = TRUE),
  percent_pine = mean(community_covs_scaled$percent_pine, na.rm = TRUE),
  percent_spruce = mean(community_covs_scaled$percent_spruce, na.rm = TRUE),
  RETN_m2 = mean(community_covs_scaled$RETN_m2, na.rm = TRUE),
  Year_since_logging = seq(min(community_covs_scaled$Year_since_logging, na.rm = TRUE), 
                           max(community_covs_scaled$Year_since_logging, na.rm = TRUE), length.out = 100)
)

# Get predictions
predict_data$predicted_evenness <- predict(comm_evenness, newdata = predict_data, type = "response")

# Create the plot with raw data and the prediction line
community_covs_scaled %>%
  ggplot(aes(x = Year_since_logging, y = evenness)) +
  geom_point() +
  geom_line(data = predict_data, aes(x = Year_since_logging, y = predicted_evenness), color = "blue") +
  # geom_ribbon(data = predict_data, aes(ymin = predicted_evenness - 1.96 * se, ymax = predicted_evenness + 1.96 * se), alpha = 0.2) +
  xlab("Year Since Logging") +
  ylab("Evenness")

BTNW <- birds_abund_covs_positives |>
                      filter(BTNW > 0)
View(BTNW)
write.csv(INF, file.path("0_Data/Processed", 'sites with no detections & no bad weather.csv'))

#6 Distance-based redundancy analysis, continuous predictors----
## dbRDA
# write.csv(birds_abund_covs_noLargePatches, file.path("0_Data/Processed", "birds_abund_covs_noLargePatches.csv"))
# Prepare species abundance data (assuming these are the last columns, adjust as needed)
# View(community_covs)

new_species_data <- decostand(select(community_covs_scaled, CCSP:PAWR), method = "hellinger")
# ?decostand
head(new_species_data)
# Calculate the Bray-Curtis distance matrix
# distance_matrix <- vegdist(species_data, method = "bray")
names(birds_abund_covs)
# Environmental variables

env_data <- community_covs_scaled[, c("RETN_m2", "Year_since_logging", "percent_decid", "percent_mixed",
                                        "percent_spruce", "percent_pine", "Veg_cat")]

# Reset to default single panel plot setting
par(mfrow=c(1,1))
dev.off()
# Running dbRDA
rda_result <- rda(new_species_data ~ RETN_m2 + Year_since_logging + percent_decid
                                      + percent_spruce + percent_pine, data = env_data, distance="bray",
                                      comm = NULL)

dbrda_result <- dbrda(new_species_data ~ RETN_m2 + Year_since_logging + percent_decid
                                      + percent_spruce + percent_pine, data = env_data, distance="bray",
                                      comm = NULL)
?dbrda
rda_model <- rda(new_species_data ~ RETN_m2 + Year_since_logging + Veg_cat, data = env_data)
View(community_covs_scaled)

# View the results
summary(rda_result)
plot(rda_model)
png(file.path("2_Outputs", "dbrda plot.png"), width = 500, height = 500)
plot(dbrda_result)
dev.off()
anova(dbrda_result, by = "margin")

png(file.path("2_Outputs", "dbrda plot colour.png"), width = 500, height = 500)

# Add vectors for explanatory variables
# Define a color for each variable
vector_colors <- c("RETN_m2" = "red", "Year_since_logging" = "blue", "percent_decid" = "green", 
                   "percent_spruce" = "orange", "percent_pine" = "purple")
# plot(dbrda_result)
# Use ordiplot() to create a base plot
ordiplot(dbrda_result, display = "sites")

# Determine a scaling factor for arrow lengths
scale_factor <- 4  # Adjust this value as needed to match the length of arrows in the default plot

# Add scaled vectors with colors
for (var in names(vector_colors)) {
    vectors <- scores(dbrda_result, display = "bp", choices = 1:2)
    if (var %in% rownames(vectors)) {
        arrows(0, 0, vectors[var, 1] * scale_factor, vectors[var, 2] * scale_factor, col = vector_colors[var], length = 0.1)
        text(vectors[var, 1] * scale_factor, vectors[var, 2] * scale_factor, labels = var, col = vector_colors[var], pos = 3)
    }
}
dev.off()

#7 RDA with discrete groupings of retention amount----
# Read the CSV file containing the species community matrix and site covariates
community_covs <- read.csv(file.path("0_Data/Processed","birds_abund_covs_noLargePatches.csv"))

# community_covs$RETN_cat <- ifelse(community_covs$RETN_m2 == 0, 
#                         0, 
#                         cut(community_covs$RETN_m2, 
#                             breaks = c(min(community_covs$RETN_m2[community_covs$RETN_m2 > 0]), 
#                                        median(community_covs$RETN_m2[community_covs$RETN_m2 > 0]), 
#                                        max(community_covs$RETN_m2)), 
#                             include.lowest = TRUE, 
#                             labels = c("1-50%", "51-100%")))

# create_RETN_categories <- function(data, size_breaks) {
#     # Ensure size_breaks is a numeric vector and sorted
#     size_breaks <- sort(as.numeric(size_breaks))
#     print(size_breaks)
    
#     # Create a breaks vector that starts with 0 and then includes the provided size_breaks
#     size_breaks <- c(size_breaks, max(data$RETN_m2))
#     print(size_breaks)
#     # Create labels for the breaks
#     labels <- c("0%", paste0(seq_along(size_breaks), "-",
#                              c(size_breaks[-1], "100%")))
#     print(labels)
#     # Create the RETN_cat column
#     data$RETN_cat <- cut(data$RETN_m2, 
#                          breaks = size_breaks, 
#                          include.lowest = TRUE)
    
#     # Handle the special case for 0
#     data$RETN_cat[data$RETN_m2 == 0] <- "0%"

#     return(data)
# }

# community_covs <- create_RETN_categories(community_covs, c(25, 50, 75))

species_data <- select(community_covs, CCSP:PAWR)  # include only the columns for species

community_covs$evenness <- hill_taxa(comm= species_data, q=2)
## Cannot have an evenness when there are no species
community_covs_positives <- community_covs |>
                                            filter(evenness != "Inf")

predictors_to_scale <- c('RETN_m2', 'Year_since_logging')

# predictors_to_scale <- c('RETN_m2')

# Apply MinMax scaling to each predictor
community_covs_scaled <- community_covs_positives
community_covs_scaled[, predictors_to_scale] <- lapply(community_covs[, predictors_to_scale], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

# community_covs_logs <- community_covs
# community_covs_logs$RETN_m2 <- lapply(community_covs$RETN_m2, function(x) {
#   (log(x+1))
# })
# Create the RETN_cat column
community_covs_scaled <- community_covs_scaled %>%
  mutate(RETN_cat = case_when(
    RETN_m2 == 0 ~ 0,
    RETN_m2 > 0 & RETN_m2 <= 0.25 ~ 1,
    RETN_m2 > 0.25 & RETN_m2 <= 0.50 ~ 2,
    RETN_m2 > 0.50 & RETN_m2 <= 0.75 ~ 3,
    RETN_m2 > 0.75 ~ 4
  ))
community_covs_scaled$RETN_cat = as.factor(community_covs_scaled$RETN_cat)

# Plotting the histogram of RETN_cat
# ggplot(community_covs_scaled, aes(x = RETN_cat)) +
#   geom_histogram(stat = "count") +
#   # scale_x_continuous(breaks = 0:4) +
#   xlab("RETN Category") +
#   ylab("Frequency")

# write.csv(community_covs_scaled, file.path("0_Data/Processed", "scaled_hellinger_community.csv"))
# Change the dataframe to scaled

species_data <- select(community_covs_scaled, CCSP:PAWR)  # Adjust the column indices as per your data

# Hellinger transformation of the species counts

hellinger_species_data <- decostand(select(community_covs_scaled, CCSP:PAWR), method = "hellinger")
# Prepare the environmental data
env_data <- community_covs_scaled[, c("RETN_cat", "Year_since_logging", "Veg_cat")]

# Perform the RDA
rda_model <- rda(hellinger_species_data ~ RETN_cat 
                  + Year_since_logging + Veg_cat, 
                  data = env_data)
# View(community_covs_positives)
# Ensure the data frame has appropriate row names
rownames(community_covs_positives) <- as.character(1:nrow(community_covs_positives))
plot(rda_model)

summary(rda_model)
anova(rda_model, by = "margin")
RsquareAdj(rda_model)

#7 Single plot RDA----
# Filter the RDA results for year_since_logging = 1 or 2
years_1_2_filtered <- community_covs_positives[community_covs_positives$Year_since_logging %in% c(1, 2, 3, 4, 5), ]

# Check for NA or infinite values in RETN_m2 and remove them if necessary
years_1_2_filtered <- na.omit(years_1_2_filtered)
years_1_2_filtered <- years_1_2_filtered[is.finite(years_1_2_filtered$RETN_m2), ]

# Ensure that the row names match between the RDA scores and the filtered data
rownames(years_1_2_filtered) <- rownames(community_covs_positives)[rownames(community_covs_positives) %in% rownames(years_1_2_filtered)]

# Get ordination scores
ordination_scores <- scores(rda_model, display = "sites")

# Adjust row names to match numeric row names in years_1_2_filtered
adjusted_row_names <- as.numeric(gsub("row", "", rownames(ordination_scores)))

# Match scores with the filtered data
matched_scores <- ordination_scores[adjusted_row_names %in% rownames(years_1_2_filtered), ]

# Check if the number of scores matches the number of group labels
if (nrow(matched_scores) == nrow(years_1_2_filtered)) {
  # Plot the RDA with ellipses for the categories of RETN_m2
  plot(rda_model, type = "n")  # Plot the RDA points
  # Draw ellipses around groups, ensure groups are factors
  ordiellipse(matched_scores, groups = factor(years_1_2_filtered$RETN_cat), 
              display = "species",
              label = TRUE,
              )
} else {
  stop("The number of ordination scores does not match the number of group labels.")
}

#8 Several RDA subplots----

rda_model <- rda(hellinger_species_data ~ RETN_cat 
                  + Year_since_logging + Veg_cat, 
                  data = env_data)
# Get ordination scores
ordination_scores <- scores(rda_model, display = "sites")
# Define the year ranges for subsetting
# year_ranges <- list(`1-4` = 1:4, `8-12` = 8:12, `18-22` = 18:22, `16-22` = 20:22)

year_ranges <- list(`1-4` = 1:4, `8-12` = 8:12, `18-22` = 18:22)
# Set up the file to save the plot
dev.off()
png(file.path("2_Outputs", "rda plots by year categories, 4 patch sizes.png"), width = 1000, height = 1000)

# Setup the plotting area for a 2x2 layout
par(mfrow=c(2, 2))

# Loop through the year ranges to create plots

subset_years <- community_covs_positives |>
  filter(Year_since_logging == 8)
nrow(subset_years)
class(community_covs_positives$Year_since_logging)
unique(community_covs_positives$Year_since_logging)
dev.off()

# Setup the plotting area for a 2x2 layout

png(file.path("2_Outputs", "rda plots by year categories, 5 patch sizes.png"), width = 500, height = 500)
par(mfrow=c(2, 2))
for (i in seq_along(year_ranges)) {
    # Reverse the Yeear_since_logging to their originl unscaled integers
    community_covs_scaled$Year_since_logging <- community_covs_positives$Year_since_logging
    # current_year_range <- year_ranges[[i]]
    years_filtered <- subset(community_covs_scaled, Year_since_logging %in% year_ranges[[i]])
    # print(community_covs_scaled$Year_since_logging)
    # print(years_filtered)

    # Ensure no NA or infinite values in the filtered data
    years_filtered <- na.omit(years_filtered)
    years_filtered <- years_filtered[is.finite(years_filtered$RETN_cat), ]
    
    # Adjust the row names as done before
    adjusted_row_names <- as.numeric(gsub("row", "", rownames(ordination_scores)))
    # print(adjusted_row_names)
    # Match scores with the filtered data
    matched_scores <- ordination_scores[adjusted_row_names %in% rownames(years_filtered), ]
# print(matched_scores)    
    years_filtered$RETN_cat <- as.factor(years_filtered$RETN_cat)
    col_palette <- palette()[years_filtered$RETN_cat]
    print(col_palette) # fixed
# print(col_palette)
      # Create the plot for the current year range
    if (nrow(matched_scores) == nrow(years_filtered)) {
      print(matched_scores)
        # Plot the RDA with ellipses for the categories of RETN_m2
        
        plot(rda_model, type = "n", main = paste("Years", names(year_ranges)[i]))
        points(matched_scores, pch = 19, cex = 0.5, col=unique(col_palette))
        ordiellipse(matched_scores, groups = factor(years_filtered$RETN_cat),
                    # conf = .95,
                    label = FALSE, col = unique(col_palette))

        legend('topright', legend=unique(years_filtered$RETN_cat), col=unique(col_palette), pch = 16,
                title = "Retention level", bty = "n")

    } else {
        stop(paste("The number of ordination scores does not match the number of group labels for the range", names(year_ranges)[i]))
    }
}

# For the fourth subplot, use the full dataset and focus on Veg_cat for ellipses
# Ensure no NA or infinite values in the full data
# data_full <- na.omit(community_covs_positives)
# data_full <- data_full[is.finite(data_full$RETN_m2), ]
# View(data_full)
adjusted_row_names <- as.numeric(gsub("row", "", rownames(ordination_scores)))
matched_scores_full <- ordination_scores[adjusted_row_names %in% rownames(community_covs_positives), ]

plot(rda_model, type = "n", main = "All sites")

# unique_veg_cats <- unique(data_full$Veg_cat)
community_covs_positives$Veg_cat <- as.factor(community_covs_positives$Veg_cat)
col_palette <- palette()[community_covs_positives$Veg_cat]

# print(unique(col_palette))
points(matched_scores_full, pch = 19, cex = 0.5, col=unique(col_palette))

ordiellipse(matched_scores_full, groups = factor(community_covs_positives$Veg_cat), 
            label = FALSE,
            col = unique(col_palette)) 

# Add a custom legend for ellipses for Veg_cat
legend("topright", legend = unique(community_covs_positives$Veg_cat), col=unique(col_palette), pch = 16, 
       title = "Veg Categories", bty = "n")

# Close the device to save the file
dev.off()

# Reset to default single panel plot setting
par(mfrow=c(1,1))
