---
title: calculate_functional_diversity
date: 2023-11-28
type: reference
tags: " #code/R "
description: "Code for calculating functional diversity in R"
---
**Original Script by Dr. Brendan Casey**

Methods by:
Laliberté, E.; Legendre, P.; Shipley, B. FD: Measuring Functional Diversity from Multiple Traits, and Other Tools for Functional Ecology. R Package Version 1.0-12. 2014. Available online: <https://cran.r-project.org/web/packages/FD>.

Trait data can be accessed in R using https://github.com/RS-eco/traitdata.

```R
#Get functional traits from the traitdata package
install.packages("remotes")
remotes::install_github("RS-eco/traitdata")
library(traitdata)

# view glossary of trait data objects
t<-trait_glossary

# get bird traits
el<-elton_birds
```


Calculate functional diversity using the `FD` package in R:

```R
#Setup ----

##Load packages----
library(FD)
library(tidyverse)

##Import data----
### Load dataframe with functional traits----
#data frame with a species column and columns for traits. E.g.:
#|species|diet.inv|diet.vend|diet.vect|diet.seed|forestStrat.groud|
load("0_data/manual/bird/bd_traits_full_2023-03-20.rData")

### Load dataframe with site level detection data----
# dataframe should have a site column, year or date column, and columns each species and their detections:
# |site|year|ALFL|AMBI|AMCO|AMCR|AMKE|AMPI|...|

load("/Volumes/Projects/harvestSeverity_birds/0_data/manual/bird/bd_wide_2023-03-20.rData")

##////////////////////////////////////////////////////////////////

#Set up traits dataframe ----
##Keep only traits of interest----
bd_tr<-bd_traits_full%>%
  dplyr::select(c("SPEC", "Diet.Inv", "Diet.Vend",
                  "Diet.Vect", "Diet.Vfish", "Diet.Vunk",
                  "Diet.Scav", "Diet.Fruit", "Diet.Nect",
                  "Diet.Seed", "Diet.PlantO", "Diet.5Cat",
                  "ForStrat.watbelowsurf", "ForStrat.wataroundsurf", "ForStrat.ground",
                  "ForStrat.understory", "ForStrat.midhigh", "ForStrat.canopy",
                  "ForStrat.aerial", "PelagicSpecialist", "Nocturnal",
                  "BodyMass.Value"))%>%
  arrange(SPEC)

## set species codes as rownames in trait dataframe----
traits<- bd_tr[,-1]
rownames(traits) <- bd_tr[,1]

##////////////////////////////////////////////////////////////////

# Setup detection matrix----
abund<-x
##remove site and year fields----
#you should have a dataframe with only species detection columns. E.g:
#|ALFL|AMBI|AMCO|AMCR|AMKE|AMPI|...|
abund1<-x[-c(1:2)]

##covert dataframe to matrix----
abund2<-data.matrix(head(abund1))

##////////////////////////////////////////////////////////////////

#Calculate functional diversity metrics with FD:dbFD----
## Calculate functional diversity metrics with FD:dbFD----
fd<- FD::dbFD(traits, abund2)

## Bind functional diversity metrics with station data
bd_funcDiv<-cbind(abund[1:2], fd)
save(bd_funcDiv, file=paste0("0_data/manual/bird/bd_FuncDiv_", Sys.Date(), ".rData"))

```
