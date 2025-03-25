# Summary Figures for March 2024 Report
rm(list=ls())
graphics.off()

#Figures of drivers by flow and impacts and NCP

#load libraries
library(ggplot2)
library(tidyverse)
library(forcats)
library(ggalluvial)
library(dplyr)
library(plyr)
library(cowplot)
library(data.table)

"%notin%" <- Negate("%in%")

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- fread("data/008_preppedata_forsynthesis.csv")
impact.group <- fread("data/data_cleaning/impact_group.csv")

## reshape data 
driver_data = melt(reshaped_data,
                   id.vars=c("ID_DOI_by_Flow", "X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                   measure.vars = patterns(driver="^driver."),
                   variable.name = "driver")[value==TRUE,][,value:=NULL][,driver:=gsub(pattern="^driver.(*)", replacement="\\1",driver)]

impact_data = melt(reshaped_data,
                   id.vars=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                   measure.vars = patterns(driver="^X2.12.Impact"),
                   variable.name = "impact",
                   value.name="direction")[direction!="",][,impact:=gsub(pattern="^X2.12.Impact..(*)", replacement="\\1",impact)]

#condense and clean
impact_data = impact_data[impact != "None"]
impact_data = impact_data[direction != 'No direction mentioned']

#merge with the groupings for which direction "we want" for impact
impact_data = merge(impact_data,
                    impact.group,
                    by="impact",
                    allow.cartesian=TRUE)
#change names
impact_data = impact_data[impact =="Indigenous.Knowledge", impact := "Indigenous Knowledge"]
impact_data = impact_data[impact =="Land.Use.Restore", impact := "Habitat Restoration"]
impact_data = impact_data[impact =="Land.Use.Loss", impact := "Habitat Loss"]

## combined datasets
driver_impact_data = merge(driver_data,
                           impact_data,
                           by="ID_DOI_by_Flow",
                           allow.cartesian=TRUE)
#condense and clean
driver_impact_data = driver_impact_data[direction != 'No direction mentioned']
driver_impact_data = driver_impact_data[impact != "None"]

# data on altered flow 
altered_flow_data =  melt(reshaped_data,
                          id.vars=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                          measure.vars = patterns(driver="^X2.7.Altered.Flow.."),
                          variable.name = "altered_flow",
                          value.name="alteration")[alteration!="",][,altered_flow:=gsub(pattern="^X2.7.Altered.Flow..(*)", replacement="\\1",altered_flow)]
#view the data
table(altered_flow_data$altered_flow)
table(altered_flow_data$alteration)

driver_flow_impact = merge(driver_impact_data,
                           altered_flow_data,
                           by="ID_DOI_by_Flow",
                           allow.cartesian=TRUE)
#NCP data
NCP_data = melt(reshaped_data,
                id.vars=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                measure.vars = patterns(driver="^X2.16.NCP.ES.."),
                variable.name = "ncp",
                value.name="ncp_direction")[ncp_direction!="",][,ncp:=gsub(pattern="^X2.16.NCP.ES..(*)", replacement="\\1",ncp)]
NCP_data = NCP_data[ncp != "None"]
NCP_data = NCP_data[ncp_direction != "No direction mentioned"]

#combine driver impact and NCP data
driver_impact_ncp = merge(driver_impact_data,
                          NCP_data,
                          by="ID_DOI_by_Flow",
                          allow.cartesian=TRUE)

table(driver_impact_ncp$ncp_direction)

#Human well-being dataset
hwb_data = melt(reshaped_data,
                id.vars=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                measure.vars = patterns(driver="^X2.20.Well.being...."),
                variable.name = "hwb",
                value.name="hwb_direction")[hwb_direction!="",][,hwb:=gsub(pattern="^X2.20.Well.being..(*)", replacement="\\1",hwb)]

hwb_data = hwb_data[hwb_direction != 'No direction mentioned']
hwb_data = hwb_data[hwb != 'None']

#combine driver impact and hwb data
driver_impact_hwb = merge(driver_impact_data,
                          hwb_data,
                          by="ID_DOI_by_Flow",
                          allow.cartesian=TRUE)

##########################################
##### Basic Summary Stat Figures ########
########################################

## Summary Stats on the Flow and Subflow:
subflowcount <- reshaped_data %>%
  ggplot(aes(x = fct_infreq(X2.2.Subtype), fill = X2.1.Flow.Type)) +
  geom_bar() + theme_minimal() +
  labs(x = "Subflow Type",
       title = "Count of Papers by Subflow Type",
       y = "Count") + coord_flip() +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", "NA" = "white"))
subflowcount <-subflowcount +labs(fill ="Flow Type")
subflowcount
jpeg(file="subflowcount.jpeg")


####################################################
#### Figures summarizing Drivers ##################
###################################################
## Get the top 10 drivers for future analyses and for the simplified count figure
#for the alluvial, reduce the drivers to ones with more than 5 counts
table(driver_impact_data$driver, driver_impact_data$impact)

# for this visuals for now - group : 
 # "extreme.weather" and "hurricanes" and heat.waves
 #   Hurricanes,  natural.disasters 
driver_data = driver_data[driver == "Hurricanes", driver := "Extreme Event"]
driver_data = driver_data[driver =="extreme.weather", driver := "Extreme Event"]
driver_data = driver_data[driver =="heat.waves", driver := "Extreme Event"]
driver_data = driver_data[driver =="natural.disasters", driver := "Extreme Event"]

#clean a couple of things
driver_data$driver <- gsub("\\.", " ", driver_data$driver)
driver_data$driver <- gsub("\\  ", " ", driver_data$driver)
driver_data$driver <- gsub("\\c ", "c", driver_data$driver)

driver_count <- ggplot(driver_data, aes(x = fct_infreq(driver))) +
  geom_bar() +  theme_minimal() + coord_flip() +
  labs(
    title = "Count of drivers surveyed in the review",
    x = "Driver",
    y = "Count")
driver_count

#list to drop from figures:
#this was still too busy so doing the top ~10 drivers here:
  # data <- driver_impact_data %>%
  #   filter(`driver` %notin% c("freshwater.temperature.change","snow.pack..depth.and.hardness.",
  #                             "remove emissions.concentration.change","C02.concentration",
  #                             "frazzle..land.ice..change", "water.availability",
  #                             "surface.water.change", "freshwater.chemistry.change",
  #                             "emissions.concentration.change",
  #                             "extreme.weather", "snow.pack.change",
  #                             "Ocean.currents", "permafrost.melt",
  #                             "Seawater.chemistry", "Hurricanes"))
# table(data$driver)

data <- driver_impact_data
# for this visuals for now - group : 
# "extreme.weather" and "hurricanes" and heat.waves
# Hurricanes,  natural.disasters 
driver_impact_data = driver_impact_data[driver == "Hurricanes", driver := "Extreme Event"]
driver_impact_data = driver_impact_data[driver =="extreme.weather", driver := "Extreme Event"]
driver_impact_data = driver_impact_data[driver =="heat.waves", driver := "Extreme Event"]
driver_impact_data = driver_impact_data[driver =="natural.disasters", driver := "Extreme Event"]

## flow alterations first
# calculate frequencies
tab <- table(data$driver)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
d_s <- subset(data, driver %in% top10)
# order factor levels
d_s$driver <- factor(d_s$driver, levels = rev(top10))

# calculate frequencies
tab <- table(data$impact)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
i_s <- subset(data, impact %in% top10)
# order factor levels
i_s$impact <- factor(i_s$impact, levels = rev(top10))

#clean a couple of things
i_s$driver <- gsub("\\.", " ", i_s$driver)
i_s$driver <- gsub("\\  ", " ", i_s$driver)
i_s$driver <- gsub("\\c ", "c", i_s$driver)
i_s$impact <- gsub("\\.", " ", i_s$impact)

# counts on driver data
driver_count_simplified <- ggplot(i_s, aes(x = fct_infreq(driver))) +
  geom_bar() +  theme_minimal() + coord_flip() +
  labs(
    title = "Count of drivers per flow",
    x = "Driver",
    y = "Count")
driver_count_simplified

jpeg(file="driver_count_simplified.jpeg")
Fig1 <- plot_grid(driver_count_simplified,subflowcount)
Fig1
ggsave("figures/for_report/Fig1.pdf", width = 30,  height = 10,  units = "cm")

#To do below:
# Alluvial connecting to Driver to Impact by flow

##################################################################################
# BD Impacts ###################################################
##################################################################
Impacts <- ggplot(impact_data, aes(x = fct_infreq(impact), fill = direction)) +
  geom_bar() +  theme_minimal() +
  facet_grid(~impact.group) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Biodiversity Impact",
    x = "Impact to Biodiversity",
    y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip()
Impacts

Impacts_by_flow <- Impacts + facet_wrap(~X2.1.Flow.Type, scales = "free") 
Impacts_by_flow

Impacts_by_subflow <- Impacts + facet_wrap(~X2.2.Subtype, scales = "free") 
Impacts_by_subflow
# write.csv(impact_data, "data/impact_data_for_Anna.csv")

#############################################################
### NCP Impacts ###########################################
#########################################################
NCP_data$ncp <- gsub("\\.", " ", NCP_data$ncp)

ncp <- ggplot(NCP_data, aes(x = fct_infreq(ncp), fill = ncp_direction)) +
  geom_bar(position= "stack") +
  coord_flip() + theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex hange" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Impacts to Nature's Contribution to People (NCP)",
    x = "NCP",
    y = "Count",
    fill = "Impact Direction")
ncp

# NCP impacts by flow type
ncp_by_flow <- ncp + facet_wrap(~X2.1.Flow.Type, scales = "free")
ncp_by_flow

# NCP impacts by subflow type
ncp_by_subflow <- ncp + facet_wrap(~X2.2.Subtype, scales = "fixed")
ncp_by_subflow

#out of curiosity just to for range shifts
table(NCP_data$X2.2.Subtype)
ncp_rangeshift = NCP_data[X2.2.Subtype == "range-shift", ]
ncp_rangeshift$ncp <- gsub("\\.", " ", ncp_rangeshift$ncp)

ncp_range <- ggplot(ncp_rangeshift, aes(x = fct_infreq(ncp), fill = ncp_direction)) +
  geom_bar(position= "stack") +
  coord_flip() + theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex hange" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Impacts of Range Shifts to Nature's Contribution to People (NCP)",
    x = "NCP",
    y = "Count",
    fill = "Impact Direction")
ncp_range


#############################################################
### HWB Impacts ###########################################
#########################################################

