##### UNDP Fragility Model
# Written by David Hammond 22 July 2021
#
# This script takes preformatted data, downloaded from the sources 
# in the UNDP Fragility Model Report. This data is pre-imputed using:
#
# 1) Interpolation for missing years
# 2) knn imputation for missing data (except for SDG 5.2 which get regional averages)
# 3) These are then inverted to ensure the polarity of each indicator is in the same direction
# 4) These are then banded between 0 and 1
#
# This script takes that data, creates a PCA weighting scheme and calculates the final scores
# The output is written to the results.xlsx file
#######

library(tidyverse)
filter <- dplyr::filter
select <- dplyr::select
summarise <- dplyr::summarise
imputed = readRDS("imputed.rds")
source("pca-functions.R")

#Calculate mean banded scores for the domains
tmp = imputed %>% dplyr::group_by(domain, id, concept, geocode, country, year) %>%
        dplyr::summarise(value = mean(banded)) %>%
        ungroup() 
# reformat the data for PCA
tmp = tmp %>% 
        dplyr::filter(year == max(year)) %>% dplyr::select(geocode, concept, value) %>%
        spread(concept, value) %>% as.data.frame()
# calculate PCA
x = get_pca(tmp)
# Extract weights
y = x$weights
y = y %>% dplyr::rename(concept = variablename)
# Select max year and join weights
tmp = imputed %>% dplyr::group_by(domain, id, concept, geocode, country, year) %>%
        dplyr::summarise(value = mean(banded)) %>%
        ungroup()  %>% 
        dplyr::filter(year == max(year)) %>% 
    dplyr::select(geocode, concept, value) %>% left_join(y)

# Calculate weighted score
score = tmp %>% mutate(fragility = value * weight) %>%
        group_by(geocode) %>% dplyr::summarise(fragility = sum(fragility)/sum(weight))

# Create output table
tmp = imputed %>% dplyr::group_by(domain, id, concept, geocode, country, year) %>%
        dplyr::summarise(value = mean(banded)) %>%
        ungroup() 
    

wide = tmp %>% dplyr::filter(year == max(year)) %>%
        dplyr::select(geocode, country, concept, value) %>%
        tidyr::spread(concept, value)
wide = wide %>% left_join(score)
wide = wide %>% relocate(geocode, country, fragility) %>%
        arrange(desc(fragility)) 
wide[,-c(1:2)] = apply(wide[,-c(1,2)], 2, round, 2)
wide = wide %>% select(-country) %>% left_join(hammond::hcountryinfo %>% select(geocode, country)) %>%
    relocate(geocode, country)
rio::export(wide, "results.xlsx")

