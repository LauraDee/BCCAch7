#Figures of drivers by flow and impacts and NCP

#load libraries
library(ggplot2)
library(tidyverse)
library(forcats)
library(ggalluvial)
library(dplyr)
library(plyr)
library(data.table)

"%notin%" <- Negate("%in%")

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- fread("data/008_preppedata_forsynthesis.csv")

driver_data = melt(reshaped_data,
                 id.vars=c("ID_DOI_by_Flow", "X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                 measure.vars = patterns("^driver."),
                 variable.name = "driver")[value==TRUE,][,value:=NULL][,driver:=gsub(pattern="^driver.(*)", replacement="\\1",driver)]

impact_data = melt(reshaped_data,
                   id.vars=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                   measure.vars = patterns("^X2.12.Impact"),
                   variable.name = "impact",
                   value.name="direction")[direction!="",][,impact:=gsub(pattern="^X2.12.Impact..(*)", replacement="\\1",impact)]

driver_impact_data = merge(impact_data,
                           driver_data,
                           by=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                           allow.cartesian=TRUE)

#condense and clean
impact_data = impact_data[impact != "None"]
impact_data = impact_data[direction != 'No direction mentioned']
impact_data = impact_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]
table(driver_impact_data$direction)

altered_flow_data =  melt(reshaped_data,
       id.vars=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
       measure.vars = patterns("^X2.7.Altered.Flow.."),
       variable.name = "altered_flow",
       value.name="alteration")[alteration!="",][,altered_flow:=gsub(pattern="^X2.7.Altered.Flow..(*)", replacement="\\1",altered_flow)]

#view the data
table(altered_flow_data$altered_flow)
table(altered_flow_data$alteration)
altered_flow_data = altered_flow_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]

driver_flow_impact = merge(driver_impact_data,
                          altered_flow_data,
                          by=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                          allow.cartesian=TRUE)

#NCP data
NCP_data = melt(reshaped_data,
                              id.vars=c("ID_DOI_by_Flow"),
                              measure.vars = patterns("^X2.16.NCP.ES.."),
                              variable.name = "ncp",
                              value.name="ncp_direction")[ncp_direction!="",][,ncp:=gsub(pattern="^X2.16.NCP.ES..(*)", replacement="\\1",ncp)]
NCP_data = NCP_data[ncp != "None"]
NCP_data = NCP_data[ncp_direction != "No direction mentioned"]

# driver_impact_data = merge(driver_data,
#                            impact_data,
#                            by="ID_DOI_by_Flow",
#                            allow.cartesian=TRUE)
#condense and clean
driver_impact_data = driver_impact_data[X2.1.Flow.Type =="'Trade (transport of goods and services)'", X2.1.Flow.Type := "Human movement"]
driver_impact_data = driver_impact_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]
driver_impact_data = driver_impact_data[direction != 'No direction mentioned']
table(driver_impact_data$direction)
driver_impact_data = driver_impact_data[impact != "None"]

driver_impact_ncp = merge(driver_impact_data,
                          NCP_data,
                          by=c("ID_DOI_by_Flow"),
                          allow.cartesian=TRUE)

table(driver_impact_ncp$ncp_direction)

#Human well-being dataset
hwb_data = melt(reshaped_data,
                id.vars=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                measure.vars = patterns("^X2.20.Well.being...."),
                variable.name = "hwb",
                value.name="hwb_direction")[hwb_direction!="",][,hwb:=gsub(pattern="^X2.20.Well.being..(*)", replacement="\\1",hwb)]
table(hwb_data)
hwb_data = hwb_data[hwb_direction != 'No direction mentioned']
hwb_data = hwb_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]
hwb_data = hwb_data[hwb != 'None']

##################################################################################
# BD Impacts ###################################################
##################################################################
Impacts <- ggplot(impact_data, aes(x = impact, fill = direction)) +
  geom_bar() +  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Biodiversity Impact",
    x = "Impact to Biodiversity",
    y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip()
Impacts

Impacts_by_flow <- ggplot(impact_data, aes(x = impact, fill = direction)) +
  geom_bar() +  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  facet_wrap(~X2.1.Flow.Type, scales = "free") +
  labs(
    title = "Biodiversity Impact",
    x = "Impact to Biodiversity",
    y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip()
Impacts_by_flow

# for the top subflow categories - to do impacts by"
Impacts_by_subflow <- ggplot(impact_data, aes(x = impact, fill = direction)) +
  geom_bar() +  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  facet_wrap(~X2.2.Subtype, scales = "free") +
  labs(
    title = "Biodiversity Impact",
    x = "Impact to Biodiversity",
    y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip()
Impacts_by_subflow

####################################################
#### Figures on  Drivers################
###################################################
# counts on driver data
driver_count <- ggplot(driver_data, aes(x = driver)) +
  geom_bar() +  theme_minimal() + coord_flip() +
  labs(
    title = "Count of drivers surveyed in the review",
    x = "Driver",
    y = "Count")
driver_count

# count number of observations by driver here, then remove the ones with less than X
table(driver_data$driver)

####################################################
#### Figures on BD Impacts and Drivers################
###################################################

#for the alluvial, reduce the drivers to ones with more than 5 counts
table(driver_impact_data$driver, driver_impact_data$impact)

#list to drop from figures:
#this was still too busy so doing the top ~10 drivers here:
data <- driver_impact_data %>%
  filter(`driver` %notin% c("freshwater.temperature.change","snow.pack..depth.and.hardness.",
                                "remove emissions.concentration.change","C02.concentration",
                                "frazzle..land.ice..change", "water.availability",
                                "surface.water.change", "freshwater.chemistry.change",
                                "emissions.concentration.change",
                                "extreme.weather", "snow.pack.change",
                            "Ocean.currents", "permafrost.melt",
                            "Seawater.chemistry", "Hurricanes"))
table(data$driver)
length(unique(data$driver)) #17

# for this visual - could group : "extreme.weather", hurricaines.

##################################################################################
# Alluvial: Driver and BD Impacts ###################################################
###################################################
ggplot(data = data,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
 # facet_wrap(~X2.1.Flow.Type, scales = "free") +
  scale_fill_manual(values = c("Increase" = "dodgerblue4", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.05)) +
  theme_classic()

# Alluvial connecting to Driver to Impact by flow
#not that informative because so many more of the papers are biotic

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
driver_count_simplified <- ggplot(i_s, aes(x = driver)) +
  geom_bar() +  theme_minimal() + coord_flip() +
  labs(
    title = "Count of drivers per flow",
    x = "Driver",
    y = "Count")
driver_count_simplified


# count number of observations by driver here, then remove the ones with less than X
table(driver_data$driver)

ggplot(data = i_s,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = driver)) +
  scale_fill_manual(values = c("darkslategray3", "darkslategray4", "antiquewhite3", "gold", "deepskyblue",
                               "darkolivegreen1", "darkolivegreen3", "aquamarine","cornflowerblue","darkblue","darkorange1")) +
  geom_stratum(alpha=.75) +
  geom_text(stat = "stratum", min.y = .75,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.15)) +
  theme_void()

#flow type to impact
ggplot(data = i_s,
       aes(axis1 = X2.1.Flow.Type, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = X2.1.Flow.Type)) +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", 'NA' = 'white')) +
  geom_stratum(alpha=.75) +
  geom_text(stat = "stratum", min.y = .75,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.15)) + labs(fill = "Flow Type") + 
  theme_void()

#flow type to impact by direction of impact
ggplot(data = i_s,
       aes(axis1 = X2.1.Flow.Type, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  #scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", 'NA' = 'white')) +
  geom_stratum(alpha=.75) +
  geom_text(stat = "stratum", min.y = .75,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.15)) + labs(fill = "Flow Type") + 
  theme_void()

#flow type to driver
ggplot(data = i_s,
       aes(axis1 = X2.1.Flow.Type, axis2 = driver)) + #, y = freq
  geom_alluvium(aes(fill = X2.1.Flow.Type)) +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta")) +
  geom_stratum(alpha=.75) +
  geom_text(stat = "stratum", min.y = .75,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.15)) +
  theme_void()

#####################
### NCP figures ######
#####################

# NCP impacts
ncp <- ggplot(NCP_data, aes(x = ncp, fill = ncp_direction)) +
  geom_bar(position= "stack") +   coord_flip() + theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey"))
ncp

# NCP impacts by flow type
# ncp_by_flow <- ncp + facet_wrap(~X2.1.Flow.Type, scales = "fixed")
# ncp_by_flow
# ncp + facet_wrap(~X2.2.Subtype, scales = "fixed")

#now read in the data with NCP and drivers in one dataframe:

# ncp + facet_wrap(~driver, scales = "fixed")

# all driver categories (too many)
ggplot(driver_impact_ncp, aes(x = ncp, fill = ncp_direction)) +
  geom_bar(position= "stack") +
  facet_wrap(~driver, scales = "fixed") +   coord_flip() + theme_minimal() +
 scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey"))

# reduce driver categories and re-plot

# calculate frequencies
tab <- table(driver_impact_ncp$ncp)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
n_s <- subset(driver_impact_ncp, ncp %in% top10)
# order factor levels
n_s$ncp <- factor(n_s$ncp, levels = rev(top10))

# calculate frequencies
tab <- table(n_s$impact)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
i_s <- subset(n_s, impact %in% top10)
# order factor levels
i_s$impact <- factor(i_s$impact, levels = rev(top10))

#clean a couple of things
i_s$driver <- gsub("\\.", " ", i_s$driver)
i_s$driver <- gsub("\\  ", " ", i_s$driver)
i_s$driver <- gsub("\\c ", "c", i_s$driver)
i_s$impact <- gsub("\\.", " ", i_s$impact)
i_s$ncp <- gsub("\\.", " ", i_s$ncp)

#Alluvials
ggplot(data = driver_impact_ncp,
       aes(axis1 = impact, axis2 = ncp)) + #, y = freq
  geom_alluvium(aes(fill = ncp_direction)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey"))

#colleen's simplified alluvial for ncps and impacts
ggplot(data = i_s,
       aes(axis1 = impact, axis2 = ncp)) + #, y = freq
  geom_alluvium(aes(fill = impact)) +
  geom_stratum(alpha=.75) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.15)) +
  theme_void() +
  scale_fill_manual(values = c("chartreuse", "chartreuse3","chartreuse4", 'darksalmon', 'darkorchid4','deepskyblue2',
                               'dimgray','deepskyblue4','darkgreen','gold'))

tab <- table(driver_impact_ncp$driver)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
d_s <- subset(driver_impact_ncp, driver %in% top10)
# order factor levels
d_s$driver <- factor(d_s$driver, levels = rev(top10))
# calculate frequencies
tab <- table(d_s$impact)


#clean a couple of things
d_s$driver <- gsub("\\.", " ", d_s$driver)
d_s$driver <- gsub("\\  ", " ", d_s$driver)
d_s$driver <- gsub("\\c ", "c", d_s$driver)
d_s$impact <- gsub("\\.", " ", d_s$impact)
d_s$ncp <- gsub("\\.", " ", d_s$ncp)


#colleen's simplified alluvial for ncps and drivers
ggplot(data = d_s,
       aes(axis1 = driver, axis2 = ncp)) + #, y = freq
  geom_alluvium(aes(fill = driver)) +
  geom_stratum(alpha=.75) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.15)) +
  theme_void() +
  scale_fill_manual(values = c("darkslategray3", "darkslategray4", "antiquewhite3", "gold", "deepskyblue",
                               "darkolivegreen1", "darkolivegreen3", "aquamarine","cornflowerblue","darkorange1"))


#driver to NCP alluvial (all drivers)
ggplot(data = d_s,
       aes(axis1 = driver, axis2 = ncp)) + #, y = freq
  geom_alluvium(aes(fill = ncp_direction)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey"))

#driver to NCP alluvial (all drivers)
ggplot(data = d_s,
       aes(axis1 = ncp_direction, axis2 = ncp)) + #, y = freq
  geom_alluvium(aes(fill = ncp_direction)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey"))


###########################################
### HWB PLOTS ##############################
########################################################
# to do Alluvial diagram (as the one above) showing the
# links and weight of connections among climate change drivers,
# type of flow changes, and human well-being dimensions.

hwb <- ggplot(hwb_data, aes(x = hwb, fill = hwb_direction)) +
  geom_bar(position= "stack") +  coord_flip() +
  labs(
    title = "Impacts to human well-being",
    x = "Human Well-being Dimension",
    size = "Count",
    fill = "Impact Direction") +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex Change" = "goldenrod1", "No change (measured)" = "grey"))

hwb

#broken out by flow
hwb + facet_wrap(~X2.1.Flow.Type, scales = "fixed")
#broken out by subflow
hwb + facet_wrap(~X2.2.Subtype, scales = "fixed")


hwb_alluv <- ggplot(data = hwb_data,
       aes(axis1 = X2.2.Subtype, axis2 = hwb)) + #, y = freq
  geom_alluvium(aes(fill = hwb_direction)) +
  #scale_fill_manual(values = c("Biotic" = "bisque2", "Physical" = "deepskyblue1", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("direction", "altered flow"),
                   expand = c(0.15, 0.05)) +
  theme_void()

hwb_alluv
hwb_alluv + facet_wrap(~X2.1.Flow.Type, scales = "free")
  
###########################################
## ALTERED FLOW ##########################
###########################################

## altered flow by flow type
altered_flow_data
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
#this doesnt have the more aggregated phsyical categories that were fixed
alter + facet_wrap(~X2.2.Subtype, scales = "fixed")

#sparse for some of the categories by promising for migration, disease spread and range-shift
ggplot(altered_flow_data, aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  theme_minimal() +
  facet_wrap(~X2.2.Subtype, scales = "fixed") +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1")) +
  labs(
    title = "Changes to Flow",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")

ggplot(data = driver_flow_impact,
       aes(axis1 = driver, axis2 = altered_flow)) + #, y = freq
  geom_alluvium(aes(fill = X2.1.Flow.Type)) +
  scale_fill_manual(values = c("Biotic" = "bisque2", "Physical" = "deepskyblue1", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

ggplot(data = driver_flow_impact,
       aes(axis1 = direction, axis2 = altered_flow)) + #, y = freq
  geom_alluvium(aes(fill = X2.1.Flow.Type)) +
  scale_fill_manual(values = c("Biotic" = "bisque2", "Physical" = "deepskyblue1", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("direction", "altered flow"),
                   expand = c(0.15, 0.05)) +
  theme_void()

## flow alterations first
# calculate frequencies
tab <- table(driver_flow_impact$altered_flow)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
d_s <- subset(driver_flow_impact, altered_flow %in% top10)
# order factor levels
d_s$altered_flow <- factor(d_s$altered_flow, levels = rev(top10))


ggplot(data = driver_flow_impact,
       aes(axis1 = X2.1.Flow.Type, axis2 = altered_flow)) + #, y = freq
  geom_alluvium(aes(fill = X2.1.Flow.Type)) +
  scale_fill_manual(values = c("Biotic" = "bisque2", "Physical" = "deepskyblue1", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta")) +
  geom_stratum(alpha=0.6) +
  geom_text(stat = "stratum",min.y=.75,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("direction", "altered flow"),
                   expand = c(0.15, 0.05)) +
  theme_void()

## drivers first
# calculate frequencies
tab <- table(driver_flow_impact$driver)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
d_s <- subset(driver_flow_impact, driver %in% top10)
# order factor levels
d_s$driver <- factor(d_s$driver, levels = rev(top10))


ggplot(data = d_s,
       aes(axis1 = X2.1.Flow.Type, axis2 = driver)) + #, y = freq
  geom_alluvium(aes(fill = X2.1.Flow.Type)) +
  scale_fill_manual(values = c("Biotic" = "bisque2", "Physical" = "deepskyblue1", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta")) +
  geom_stratum(alpha=0.6) +
  geom_text(stat = "stratum", min.y=.75,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("driver", "X2.1.Flow.Type"),
                   expand = c(0.15, 0.05)) +
  theme_void()

### do for just biotic
biotic_driver_flow_impact = driver_flow_impact[driver_flow_impact$X2.1.Flow.Type == 'Biotic',]
unique(biotic_driver_flow_impact$X2.1.Flow.Type)

biotic_driver_flow_impact[,count := .N, by = 'impact']
biotic_driver_flow_impact[,count := .N, by = 'driver']

## drivers first
# calculate frequencies
tab <- table(biotic_driver_flow_impact$driver)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
d_s <- subset(biotic_driver_flow_impact, driver %in% top10)
# order factor levels
d_s$driver <- factor(d_s$driver, levels = rev(top10))
# calculate frequencies
tab <- table(d_s$altered_flow)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
a_s <- subset(d_s, altered_flow %in% top10)
# order factor levels
a_s$altered_flow <- factor(a_s$altered_flow, levels = rev(top10))

#clean a couple of things
a_s$driver <- gsub("\\.", " ", a_s$driver)
a_s$driver <- gsub("\\  ", " ", a_s$driver)
a_s$driver <- gsub("\\c ", "c", a_s$driver)
a_s$impact <- gsub("\\.", " ", a_s$impact)
a_s$ncp <- gsub("\\.", " ", a_s$ncp)


ggplot(data = a_s,
      aes(axis1 = altered_flow, axis2 = X2.2.Subtype)) + #, y = freq
  geom_alluvium(aes(fill = altered_flow)) +
  #scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum(alpha=0.75) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.15)) +
  theme_void()

ggplot(data = a_s,
       aes(axis1 = altered_flow, axis2 = driver)) + #, y = freq
  geom_alluvium(aes(fill = altered_flow)) +
  #scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum(alpha=0.75) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.15)) +
  theme_void()

ggplot(data = a_s,
       aes(axis1 = driver, axis2 = X2.2.Subtype)) + #, y = freq
  geom_alluvium(aes(fill = driver)) +
  scale_fill_manual(values = c("darkslategray3", "darkslategray4", "antiquewhite3", "gold", "deepskyblue",
                               "darkolivegreen1", "darkolivegreen3", "aquamarine","cornflowerblue","darkorange1")) +
  geom_stratum(alpha=0.75) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.15)) +
  theme_void()

# calculate frequencies
tab <- table(a_s$impact)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
i_s <- subset(a_s, impact %in% top10)
# order factor levels
i_s$impact <- factor(i_s$impact, levels = rev(top10))


ggplot(data = i_s,
       aes(axis1 = impact, axis2 = X2.2.Subtype)) + #, y = freq
  geom_alluvium(aes(fill = impact)) +
  scale_fill_manual(values = c("chartreuse", "chartreuse3","chartreuse4", 'darksalmon', 'darkorchid4','deepskyblue2',  ###colors need to be updated
                               'dimgray','deepskyblue4','darkgreen','gold')) +
  geom_stratum(alpha=0.75) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.15)) +
  theme_void()


## impacts second
# calculate frequencies
tab <- table(d_s$impact)
# sort
tab_s <- sort(tab)
# extract 10 most frequent nationalities
top10 <- tail(names(tab_s), 10)
# subset of data frame
i_s <- subset(d_s, impact %in% top10)
# order factor levels
i_s$impact <- factor(i_s$impact, levels = rev(top10))

# drivers <- c("Air temperature directional change", "Climate change generic", "Drought", "Air temperature variability", "Precipitation directional change",
#              "Precipitation variability", "Wildfires", "Sea temperature change", "Sea ice area change", "Ocean currents")
# impacts <- c("Abundance", 'Composition', 'Connectivity', 'Disease', 'Genetics', 'Invasion', 'Land Use Loss', 'Loss', 'Richness', 'Trophic')
# directions <- c('Increase', 'Decrease', 'Complex change', 'No change (measured)', 'No direction mentioned')

# i_s$driver <- factor(i_s$driver, levels = drivers, labels = drivers)
# i_s$impact <- factor(i_s$impact, levels = impacts, labels = impacts)
# i_s$direction <- factor(i_s$direction, levels = directions, labels = directions)

#clean a couple of things
i_s$driver <- gsub("\\.", " ", i_s$driver)
i_s$driver <- gsub("\\  ", " ", i_s$driver)
i_s$driver <- gsub("\\c ", "c", i_s$driver)
i_s$impact <- gsub("\\.", " ", i_s$impact)

ggplot(data = i_s,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum() +
  geom_text(stat = "stratum", min.y=.75,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()


# plot
ggplot(data = biotic_driver_flow_impact,
       aes(axis1 = impact, axis2 = altered_flow)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("direction", "altered flow"),
                   expand = c(0.15, 0.05)) +
  theme_void()



r <- ggplot(data = to_graph_data,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  #scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("direction", "altered flow"),
                   expand = c(0.15, 0.05)) +
  theme_void()
r
ggsave('test.png',
       width = unit(20, 'in'), height = unit(20, 'in'))
