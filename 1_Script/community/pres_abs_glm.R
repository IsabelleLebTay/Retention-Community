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
birds <- read_csv("community_abundance_by_location.csv")

# Identify the columns with the four-letter codes
# Assuming these are all the columns after 'longitude'
code_columns <- names(birds)[5:ncol(birds)]

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
head(birds_data_long)
birds

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

## In STAN
getwd()
all_species_unpooled <- cmdstan_model(
  stan_file = "1. Script/STAN_scripts/all_species_unpooled.stan", 
  pedantic = TRUE)
?set_cmdstan_path
all_species_unpooled
