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
                           by= c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),  #"ID_DOI_by_Flow",
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
                           by= c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"), #"ID_DOI_by_Flow",
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
                          by= c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"), 
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
                          by= c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"), 
                          allow.cartesian=TRUE)

##########################################
##### Basic Summary Stat Figures ########
########################################

reshaped_data$V1 = NULL
reshaped_data$X = NULL
## Summary Stats on the Flow and Subflow:
subflowcount <- reshaped_data %>%
  ggplot(aes(x = fct_infreq(X2.2.Subtype), fill = X2.1.Flow.Type)) +
  geom_bar() + theme_minimal() +
  labs(x = "Subflow Type",
       title = "Count of Papers by Subflow Type",
       y = "Count") + coord_flip() +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", "NA" = "white"))
subflowcount
subflowcount <-subflowcount + labs(fill ="Flow Type")
subflowcount
jpeg(file="subflowcount.jpeg")


# by flow and calculate percents for the report
#do as % :
reshaped_data$V1 = NULL
reshaped_data$V1 = NULL
reshaped_data$X = NULL

flow_percent <- reshaped_data %>%
  group_by(X2.1.Flow.Type) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
flow_percent

ggplot(flow_percent, aes(as.factor(X2.1.Flow.Type), prop)) +
  geom_col(aes(fill = as.factor(X2.1.Flow.Type))) +
  labs( title = "Proportion of Flow Type",
        x = "Flow Type",
        y = "Proportion of Entries") +
  theme_minimal() + theme(legend.position="none") +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", "NA" = "white"))

subflow_perc  <- reshaped_data %>%
  group_by(X2.2.Subtype) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))

subflow_perc 

write.csv(subflow_perc, "subflow_type_percent.csv")

# calculate % of papers that contained more than one flow
multiflow <- reshaped_data %>%
  group_by(Another.Flow.) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
multiflow

#calc ecosystem type
ecosystem <- reshaped_data %>%
  group_by(X2.14.Ecosystem.Type) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
ecosystem

#substantially more cleaning is needed to do anything by ecosystem type:
ecotype <- ggplot(reshaped_data, aes(x = fct_infreq(X2.14.Ecosystem.Type)), fill = X2.1.Flow.Type) +
  geom_bar() + theme_minimal() +
  labs(x = "Ecosystem Type",
       title = "Count of Papers by Ecosystem Type",
       y = "Count") + coord_flip() +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", "NA" = "white"))
ecotype <- ecotype + labs(fill ="Flow Type")
ecotype

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

#calc ecosystem type
driver_perc <- driver_data %>%
  group_by(driver) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(driver_perc) 

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
driver_impact_data = driver_impact_data[driver == "Hurricanes", driver := "Extreme Events"]
driver_impact_data = driver_impact_data[driver =="extreme.weather", driver := "Extreme Events"]
driver_impact_data = driver_impact_data[driver =="heat.waves", driver := "Extreme Events"]
driver_impact_data = driver_impact_data[driver =="natural.disasters", driver := "Extreme Events"]


driver_impact_data$driver <- gsub("\\.", " ", driver_impact_data$driver)
#grouping extreme events 
driver_extremesgrouped_count <- ggplot(driver_impact_data, aes(x = fct_infreq(driver))) +
  geom_bar() +  theme_minimal() + coord_flip() +
  labs(
    title = "Count of drivers surveyed in the review",
    x = "Driver",
    y = "Count")
driver_extremesgrouped_count

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

#What are the top ten impacts reported?
data <- impact_data
# do for top subflow groups
# calculate frequencies
tab <- table(data$impact)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
d_s <- subset(data, impact %in% top10)

# order factor levels
d_s$impact <- factor(d_s$impact, levels = rev(top10))
list(unique(d_s$impact))

#What are the top ten Subflow types?
# calculate frequencies
tab <- table(data$X2.2.Subtype)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
top5 <- tail(names(tab_s), 5)
# subset of data frame
i_s <- subset(data, X2.2.Subtype %in% top10)
i_s <- subset(data, X2.2.Subtype %in% top5)
# order factor levels
i_s$X2.2.Subtype <- factor(i_s$X2.2.Subtype, levels = rev(top10))
i_s$X2.2.Subtype <- factor(i_s$X2.2.Subtype, levels = rev(top5))
list(unique(i_s$X2.2.Subtype))

#clean a couple of things
i_s$impact <- gsub("\\.", " ", i_s$impact)

Impacts_top10_subflow <- ggplot(i_s, aes(x = fct_infreq(impact), fill = direction)) +
  geom_bar() +  theme_minimal() +
  facet_grid(~X2.2.Subtype) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Biodiversity Impact",
    x = "Impact to Biodiversity",
    y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip()
Impacts_top10_subflow  

#############################################################
### NCP Impacts ###########################################
#########################################################
NCP_data$ncp <- gsub("\\.", " ", NCP_data$ncp)

ncp <- ggplot(NCP_data, aes(x = fct_infreq(ncp), fill = ncp_direction)) +
  geom_bar(position= "stack") +
  coord_flip() + theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey")) +
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
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Impacts of Range Shifts to Nature's Contribution to People (NCP)",
    x = "NCP",
    y = "Count",
    fill = "Impact Direction")
ncp_range

# grab the subflows determined above as the top ten
list(unique(i_s$X2.2.Subtype))
top_subflow <- list(unique(i_s$X2.2.Subtype))

reduced_ncp_data <- NCP_data %>%
  filter(`X2.2.Subtype` %notin% top_subflow)

length(unique(reduced_ncp_data$X2.2.Subtype))

# NCP impacts by subflow
# drop trade ,wind, snow melt?
reduced_ncp_data <- NCP_data %>%
  filter(`X2.2.Subtype` %notin% c("trade","wind", "snow melt runoff"))

reduced_ncp_data$ncp <- gsub("\\.", " ", reduced_ncp_data$ncp)

ncp_by_subflow2 <- ggplot(reduced_ncp_data, aes(x = fct_infreq(ncp), fill = ncp_direction)) +
  geom_bar(position= "stack") +
  coord_flip() + theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey")) +
  theme_minimal() + labs(
    title = "Impacts to Nature's Contribution to People (NCP)",
    x = "NCP",
    y = "Count",
    fill = "Impact Direction") + facet_wrap(~X2.2.Subtype, scales = "fixed")
ncp_by_subflow2

#############################################################
### HWB Impacts ###########################################
#########################################################

hwb <- ggplot(hwb_data, aes(x = fct_infreq(hwb), fill = hwb_direction)) +
  geom_bar(position= "stack") +  coord_flip() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  theme_minimal() + labs(
    title = "Impacts to human well-being",
    x = "Human Well-being Dimension",
    size = "Count",
    fill = "Impact Direction")
hwb

# Combined Figure with NCP impacts:
Fig2 <- plot_grid(ncp,hwb)
plot_grid(ncp,hwb)

#broken out by flow
hwb + facet_wrap(~X2.1.Flow.Type, scales = "fixed")
#broken out by subflow
hwb + facet_wrap(~X2.2.Subtype, scales = "fixed")

table(hwb_data$hwb)
# Cohesion    Justice    Other Relational    Welfare 
# 18         21          7         31         76 
table(hwb_data$X2.2.Subtype, hwb_data$hwb_direction)

#calc % - this is overall but should be done by HWB dimension
hwb_perc <- hwb_data %>%
  group_by(hwb, hwb_direction) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(hwb_perc) 

#cohesion 
cohesion = hwb_data[hwb == "Cohesion",]
cohension_perc <- cohesion %>%
  group_by(hwb_direction) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(cohension_perc) 

#cohesion by flow
cohesion = hwb_data[hwb == "Cohesion",]
cohension_perc <- cohesion %>%
  group_by(hwb_direction, X2.1.Flow.Type) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(cohension_perc) 

#welfare
welfare = hwb_data[hwb == "Welfare",]
welfare_perc <- welfare %>%
  group_by(hwb_direction) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(welfare_perc) 

#welfare by flow
welfare = hwb_data[hwb == "Welfare",]
welfare_perc <- welfare %>%
  group_by(hwb_direction,  X2.1.Flow.Type) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(welfare_perc) 

# Relational 
relational = hwb_data[hwb == "Relational",]
relational_perc <- relational %>%
  group_by(hwb_direction) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(relational_perc) 
#relational by flow
relational = hwb_data[hwb == "Relational",]
relational_perc <- relational %>%
  group_by(hwb_direction, X2.1.Flow.Type) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(relational_perc) 

#Justice
Justice = hwb_data[hwb == "Justice",]
Justice_perc <- Justice %>%
  group_by(hwb_direction) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(Justice_perc) 

#Justice by flow
Justice = hwb_data[hwb == "Justice",]
Justice_perc <- Justice %>%
  group_by(hwb_direction, X2.1.Flow.Type) %>%
  dplyr::summarise(n = n()) %>%
  mutate(prop = n / sum(n))
print(Justice_perc) 

############################################################################################
### DO NCP and HWB Impacts FOR TOP SUBFLOWS ONLY ###########################################
#######################################################################################
#do for the top subflows only




#############################################################
### ALTERED FLOW SUMMARIES ###########################################
#########################################################
head(altered_flow_data)

table(altered_flow_data$altered_flow)
table(altered_flow_data$alteration)

alter <- ggplot(altered_flow_data, aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  facet_wrap(~X2.1.Flow.Type, scales = "fixed") +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  labs(
    title = "Changes to Flow",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")
alter
#this doesnt have the more aggregated physical categories that were fixed
alter + facet_wrap(~X2.2.Subtype, scales = "fixed")

#do for just biotic 
bioalter <- ggplot(altered_flow_data[X2.1.Flow.Type == "Biotic",], aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  facet_wrap(~X2.1.Flow.Type, scales = "fixed") +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  labs(
    title = "Changes to Biotic Flows",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")
bioalter
bioalter + facet_wrap(~X2.2.Subtype)

#do for just physical 

#* drop ocean and wind, and combine glacial and snow melt for the figure
#* 
phys_altered_flow_data = altered_flow_data[X2.1.Flow.Type == "Physical",]
phys_altered_flow_data = phys_altered_flow_data[X2.2.Subtype == "glacial melt", X2.2.Subtype := "glacial and snow melt"]
phys_altered_flow_data = phys_altered_flow_data[X2.2.Subtype == "snow melt runoff", X2.2.Subtype := "glacial and snow melt"]

#drop wind and current
phys_altered_flow_data = phys_altered_flow_data[X2.2.Subtype != "wind",]
phys_altered_flow_data = phys_altered_flow_data[X2.2.Subtype != "ocean current",]
  
physalter <- ggplot(phys_altered_flow_data, aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  facet_wrap(~X2.1.Flow.Type, scales = "fixed") +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  labs(
    title = "Changes to Physical Flows",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")
physalter
physalter + facet_wrap(~X2.2.Subtype)

# do for human

#do for just human
humanalter <- ggplot(altered_flow_data[X2.1.Flow.Type == "Human movement",], aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  facet_wrap(~X2.1.Flow.Type, scales = "fixed") +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  labs(
    title = "Changes to Human movement Flows",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")
humanalter
humanalter + facet_wrap(~X2.2.Subtype)

#do for just socio
socioalter <- ggplot(altered_flow_data[X2.1.Flow.Type == "Sociocultural",], aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  facet_wrap(~X2.1.Flow.Type, scales = "fixed") +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  labs(
    title = "Changes to Sociocultural Flows",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")
socioalter
socioalter + facet_wrap(~X2.2.Subtype)

###########################################################################################
### Do figures on each flow type #############################################################################
#######################################################################################

# do for just range shift 
ggplot(altered_flow_data[X2.2.Subtype == "range-shift", ], aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  labs(
    title = "Changes to Range Shifts",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")

# vs migration
ggplot(altered_flow_data[X2.2.Subtype == "migration", ], aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  labs(
    title = "Changes to Animal Migrations",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")

###########################################################################################
### Do figures by top driver(s) #############################################################################
#######################################################################################




###########################################################################################
### COMBINATIONS and COUNTS  - HEAT MAPS AND ALLUVIALS ###########################################
#######################################################################################

###########################################################################################
### PREP DATA  ###########################################
#######################################################################################
#get combos https://stackoverflow.com/questions/15475832/count-number-of-time-combination-of-events-appear-in-dataframe-columns-ext
### NOW NEED TO GET COMBINATIONS and COUNTS 

ncp_rangeshift = NCP_data[X2.2.Subtype == "range-shift", ]

#*** TO DO FOR TOP SUBFLOWS
head(driver_flow_impact)
driver_flow_impact[,count_driver_alteredflow:=.N, by=.(driver, altered_flow)]
summary(driver_flow_impact$count_driver_alteredflow)

driver_flow_impact[,count_driver_alteration:=.N, by=.(driver, altered_flow,  alteration)]
summary(driver_flow_impact$count_driver_alteration)

driver_flow_impact[,count_driver_bd_impactdirection:=.N, by=.(driver, impact, direction)]


#PREP DATA: to do the alluvials, we need fewer categories

# calculate driver frequencies
tab <- table(driver_flow_impact$driver)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10_driver <- tail(names(tab_s), 10)
top5_driver <- tail(names(tab_s), 5)
# subset of data frame
d_s <- subset(driver_flow_impact, driver %in% top10_driver)
d_s <- subset(driver_flow_impact, driver %in% top5_driver)
# order factor levels
d_s$driver <- factor(d_s$driver, levels = rev(top10_driver))
d_s$driver <- factor(d_s$driver, levels = rev(top5_driver))

# calculate impact frequencies
tab <- table(driver_flow_impact$impact)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10_impact <- tail(names(tab_s), 10)
#extract top 5
top5_impact <- tail(names(tab_s), 5)
#extract top 7
top7_impact <- tail(names(tab_s), 7)
# subset of data frame
i_s <- subset(driver_flow_impact, impact %in% top10_impact)
i_s <- subset(driver_flow_impact, impact %in% top5_impact)
i_s <- subset(driver_flow_impact, impact %in% top7_impact)

# order factor levels
i_s$impact <- factor(i_s$impact, levels = rev(top5_impact))
i_s$impact <- factor(i_s$impact, levels = rev(top10_impact))
i_s$impact <- factor(i_s$impact, levels = rev(top7_impact))

#clean a couple of things
i_s$driver <- gsub("\\.", " ", i_s$driver)
i_s$driver <- gsub("\\  ", " ", i_s$driver)
i_s$driver <- gsub("\\c ", "c", i_s$driver)
i_s$impact <- gsub("\\.", " ", i_s$impact)

#view lists
top10_impact
top5_impact
top10_driver
top5_driver
top7_impact

## subset the data to make the alluvials
d_s <- subset(driver_flow_impact, driver %in% top10_driver)
d_s <- subset(d_s, impact %in% top7_impact)
d_s$driver <- gsub("\\.", " ", d_s$driver)


##### HEAT MAPS #######################
# driver by flow counts for all
ggplot(driver_flow_impact, aes(driver, altered_flow, fill= count_driver_alteredflow)) + 
  geom_tile() +
  scale_size_continuous(range = c(1, 8), 
                        limits = c(0, 300), 
                        breaks=c(1, 75, 150, 225, 300)) + 
  scale_fill_gradient(low="white", high="blue") + coord_flip()

ggplot(driver_flow_impact, aes(x = driver, y = altered_flow, size = count_driver_alteredflow)) +
  geom_point(color = "blue", alpha = 0.7) +  # Use points to represent combinations
  scale_size_continuous(range = c(1, 8), 
                        limits = c(0, 300), 
                        breaks=c(1, 75, 150, 225, 300)) +   # Adjust size range for better visibility
  labs(
    title = "Interaction Between Altered Flows and Impacts",
    x = "Altered Flow",
    y = "Impact",
    size = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )

# FOR USE IN REPORT #
#with directionality of how flow is altered
driver_alter <-  ggplot(d_s, aes(x = driver, y = altered_flow, size = count_driver_alteration, color = alteration)) +
  geom_point(alpha = 0.4) +  # Add points with alpha transparency
  facet_wrap(~alteration, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 8), 
                        limits = c(0, 300), 
                        breaks=c(1, 75, 150, 225, 300)) + # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "NoChange" = "grey70")) + 
  labs(
    title = "Driver alteration of flow",
    x = "Driver",
    y = "Impact",
    size = "Count",
    color = "Impact Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
driver_alter

driver_alter +  scale_size_continuous(range = c(1, 5)) + facet_wrap(~X2.1.Flow.Type, scales = "fixed")

# for range shifts only
driver_alter <-  ggplot(d_s[X2.2.Subtype == "range-shift",], aes(x = driver, y = altered_flow, size = count_driver_alteration, color = alteration)) +
  geom_point(alpha = 0.4) +  # Add points with alpha transparency
  facet_wrap(~alteration, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 8), 
                        limits = c(0, 300), 
                        breaks=c(1, 75, 150, 225, 300)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "NoChange" = "grey70")) + 
  labs(
    title = "Driver alteration of range shifts",
    x = "Driver",
    y = "Impact",
    size = "Count",
    color = "Impact Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
driver_alter

#for knowledge transfer only
driver_alter <-  ggplot(d_s[X2.2.Subtype == "knowledge transfer",], aes(x = driver, y = altered_flow, size = count_driver_alteration, color = alteration)) +
  geom_point(alpha = 0.4) +  # Add points with alpha transparency
  facet_wrap(~alteration, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 8), 
                        limits = c(0, 300), 
                        breaks=c(1, 75, 150, 225, 300)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "NoChange" = "grey70")) + 
  labs(
    title = "Driver alteration of knowledge transfer",
    x = "Driver",
    y = "Impact",
    size = "Count",
    color = "Impact Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
driver_alter

#FOR DISEASE
driver_alter <-  ggplot(d_s[X2.2.Subtype == "disease spread",], aes(x = driver, y = altered_flow, size = count_driver_alteration, color = alteration)) +
  geom_point(alpha = 0.4) +  # Add points with alpha transparency
  facet_wrap(~alteration, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 8), 
                        limits = c(0, 300), 
                        breaks=c(1, 75, 150, 225, 300)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "NoChange" = "grey70")) + 
  labs(
    title = "Driver alteration of disease spread",
    x = "Driver",
    y = "Impact",
    size = "Count",
    color = "Impact Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
driver_alter
driver_alter + facet_wrap(~driver)

#p + geom_point(aes(shape = factor(cyl)))
driver_impact <-  ggplot(i_s, aes(x = driver, y = altered_flow, size = count_driver_alteration, color = alteration)) +
  geom_point(alpha = 0.4) +  # Add points with alpha transparency
  facet_wrap(~alteration, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 5)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "NoChange" = "grey70")) + 
  labs(
    title = "Driver alteration of flow",
    x = "Driver",
    y = "Impact",
    size = "Count",
    color = "Impact Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
driver_impact

#do for connections between driver-->altered flow--> biodiv impact
driver_flow_impact[,count_driver_altflow_bdimpact:=.N, by=.(driver, altered_flow, impact)]
summary(driver_flow_impact$count_driver_altflow_bdimpact)

driver_flow_impact[,count_driver_impact:=.N, by=.(driver, impact)]

driver_flow_impact %>%
  ggplot(aes(driver, impact)) +
  geom_raster(aes(fill = count_driver_impact))

driver_flow_impact[,count_alteredflow_impact:=.N, by=.(altered_flow, impact)]

driver_flow_impact %>%
  ggplot(aes(altered_flow, impact)) +
  geom_raster(aes(fill = count_alteredflow_impact), na.rm = TRUE) + 
  facet_wrap(~X2.1.Flow.Type, scales = "fixed")

driver_flow_impact %>%
  ggplot(aes(altered_flow, impact, color = direction, size = count_driver_alteration)) +
  geom_point(aes(fill = count_alteredflow_impact), na.rm = TRUE) + 
  facet_wrap(~X2.1.Flow.Type, scales = "fixed")


###########################################################################################
### BD and NCP impact heat maps ############################################################################
#######################################################################################

# need to clean up the driver names - show fewer in the main and put the missing ones in the SI?
driver_flow_impact$driver <- gsub("\\.", " ", driver_flow_impact$driver)



driver_bdimpact <-  ggplot(driver_flow_impact, aes(x = driver, y = impact, size = count_driver_impact, color = direction)) +
  geom_point(alpha = 0.4) +  # Add points with alpha transparency
  facet_wrap(~direction, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 8), 
                        limits = c(0, 300), 
                        breaks=c(1, 75, 150, 225, 300)) +  # Adjust size range and breaks
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey70")) + 
  labs(
    title = "Driver-Biodiversity Impact",
    x = "Driver",
    y = "Biodiversity Impact",
    size = "Count",
    color = "Impact Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
driver_bdimpact

driver_bdimpact + facet_wrap(~impact.group, scale = fixed)


#for just range shifts
driver_bdimpact <-  ggplot(driver_flow_impact[X2.2.Subtype == "range-shift",], aes(x = driver, y = impact, size = count_driver_impact, color = direction)) +
  geom_point(alpha = 0.4) +  # Add points with alpha transparency
  facet_wrap(~direction, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 8), 
                        limits = c(0, 300), 
                        breaks=c(1, 75, 150, 225, 300)) +  # Adjust size range and breaks
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey70")) + 
  labs(
    title = "Driver-Biodiversity Impact",
    x = "Driver",
    y = "Biodiversity Impact",
    size = "Count",
    color = "Impact Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
driver_bdimpact

###########################################################################################
### ALLUVIALS #############################################################################
#######################################################################################

## Make Figures ## 
# all drivers and impacts but for Biotic 
ggplot(data = driver_flow_impact[X2.1.Flow.Type == "Biotic", ],
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  # scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", 'NA' = 'white')) +
  geom_stratum(alpha=.75) +
  geom_text(stat = "stratum", min.y = .75,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.15)) + labs(fill = "Flow Type") + theme_void()

ggplot(data = driver_flow_impact,
       aes(axis1 = driver, axis2 = altered_flow, axis3 = impact,
           y = count_driver_altflow_bdimpact)) +
  scale_x_discrete(limits = c("Driver", "Altered Flow", "Impact"), expand = c(.2, .05)) +
  xlab("Causal sequence") +
  geom_alluvium(aes(fill = direction)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Driver impacts to biodiversity through altering flow") #,
          # "stratified by demographics and survival")

#### #### #### #### #### #### ####
### ALLUVIALS ON SUBSET #### ####
#### #### #### #### #### #### ####

#try from: https://medium.com/@arnavsaxena96/all-about-alluvial-diagrams-21da1505520b
ggplot(d_s, aes(axis1 = driver, axis2 = altered_flow, axis3 = impact), weight = count_driver_altflow_bdimpact )+
  geom_alluvium(aes(fill=as.factor(direction))) +
  geom_stratum(decreasing = TRUE,  min.y = 20) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),size = 2,discern=TRUE) 
# + stat_alluvium(geom = "flow", lode.guidance = "forward",
#                 width = 30) 

flourish <- d_s %>% 
select(driver, altered_flow, impact, count_driver_alteration, count_driver_altflow_bdimpact, count_driver_impact, 
       alteration, direction, count_alteredflow_impact, X2.1.Flow.Type)
write.csv(flourish, "data_for_flourish.csv")

# df <- df %>% 
#   dplyr::group_by(stock, newspaper, status) %>% 
#   summarise(n = n()) 

#try from: https://medium.com/@arnavsaxena96/all-about-alluvial-diagrams-21da1505520b
driver_to_impact <- ggplot(d_s, aes(axis1 = driver, axis2 = altered_flow, axis3 = impact))+
  geom_alluvium(aes(fill=as.factor(direction))) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),size = 3,discern=TRUE) +  theme_void() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey70")) 
driver_to_impact + facet_wrap(~X2.1.Flow.Type, scale = "free")

# Alluvial connecting to Driver to Impact by flow
driver_to_impact <- ggplot(d_s, aes(axis1 = driver, axis2 = impact))+
  geom_alluvium(aes(fill=as.factor(direction))) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),size = 3,discern=TRUE) +  theme_void() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey70")) 

driver_to_impact + facet_wrap(~X2.1.Flow.Type, scale = "free")



# to do Alluvial diagram (as the one above) showing the
# links and weight of connections among climate change drivers,
# type of flow changes, and human well-being dimensions.



#extract unique combinations
driver_to_impact

driver_to_impact = driver_flow_impact[,count_driver_alteredflow_impact_direction:=.N, by=.(driver, altered_flow, impact, direction)]

driver_to_impact_subtype = driver_flow_impact[,count_driver_alteredflow_impact_direction_sub:=.N, by=.(X2.2.Subtype,driver, altered_flow, impact, direction)]


extract = driver_to_impact[unique(count_driver_alteredflow_impact_direction)]
write.csv(extract, "unique_driver_flow_impact_paths.csv")

extract_subtype = driver_to_impact[unique(count_driver_alteredflow_impact_direction_sub)]

