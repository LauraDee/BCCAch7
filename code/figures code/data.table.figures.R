#Figures of drivers by flow and impacts and NCP

#https://www.cedricscherer.com/2023/10/26/yet-another-how-to-on-labelling-bar-graphs-in-ggplot2/
# https://www.sthda.com/english/articles/32-r-graphics-essentials/132-plot-grouped-data-box-plot-bar-plot-and-more/#grouped-categorical-variables

#load libraries
library(ggplot2)
library(tidyverse)
library(forcats)
library(ggalluvial)
library(data.table) #load last

"%notin%" <- Negate("%in%")

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- fread("data/007_output_interventions.csv")
impact.group <- fread("data/data_cleaning/impact_group.csv")

#clean up Flow and Subflow types
reshaped_data = reshaped_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]
#group:
 # groundwater &  groundwater flow
 # sea ice change & sea ice retreat
# stream flow and river flow to  surface water quantity
reshaped_data = reshaped_data[X2.2.Subtype =="groundwater flow", X2.2.Subtype := "groundwater"]
reshaped_data = reshaped_data[X2.2.Subtype =="sea ice change", X2.2.Subtype := "sea ice retreat"]
reshaped_data = reshaped_data[X2.2.Subtype =="stream flow", X2.2.Subtype := "surface water quantity"]
reshaped_data = reshaped_data[X2.2.Subtype =="river flow", X2.2.Subtype := "surface water quantity"]
# make a combined nutrient and sediment flow
reshaped_data = reshaped_data[X2.2.Subtype =="sediment flow", X2.2.Subtype := "nutrient and sediment flow"]
reshaped_data = reshaped_data[X2.2.Subtype =="nutrient flow", X2.2.Subtype := "nutrient and sediment flow"]
#rename wind flow to just wind
# rename glacial melt and discharge to just glacial melt
reshaped_data = reshaped_data[X2.2.Subtype =="wind flow", X2.2.Subtype := "wind"]
reshaped_data = reshaped_data[X2.2.Subtype =="glacial melt and discharge", X2.2.Subtype := "glacial melt"]

#check how well this condensed
phys = reshaped_data[X2.1.Flow.Type =="Physical",]
table(phys$X2.2.Subtype)

#COULD group as the nutrient, sediment and polluation all as water quality??
phys = reshaped_data[X2.1.Flow.Type =="Physical",]
table(phys$X2.2.Subtype)

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
impact_data = impact_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]

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
driver_impact_data = driver_impact_data[X2.1.Flow.Type.x =="'Trade (transport of goods and services)'", X2.1.Flow.Type := "Human movement"]
driver_impact_data = driver_impact_data[X2.1.Flow.Type.x =="Trade", X2.1.Flow.Type := "Human movement"]
driver_impact_data = driver_impact_data[direction != 'No direction mentioned']
table(driver_impact_data$direction)
driver_impact_data = driver_impact_data[impact != "None"]

altered_flow_data =  melt(reshaped_data,
       id.vars=c("ID_DOI_by_Flow","X2.1.Flow.Type", "X2.2.Subtype","DOI"),
       measure.vars = patterns(driver="^X2.7.Altered.Flow.."),
       variable.name = "altered_flow",
       value.name="alteration")[alteration!="",][,altered_flow:=gsub(pattern="^X2.7.Altered.Flow..(*)", replacement="\\1",altered_flow)]

#view the data
table(altered_flow_data$altered_flow)
table(altered_flow_data$alteration)
altered_flow_data = altered_flow_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]

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
table(hwb_data)
hwb_data = hwb_data[hwb_direction != 'No direction mentioned']
hwb_data = hwb_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]
hwb_data = hwb_data[hwb != 'None']

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

Impacts_by_flow <- ggplot(impact_data, aes(x = fct_infreq(impact), fill = direction)) +
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

write.csv(impact_data, "data/impact_data_for_Anna.csv")

# for the top subflow categories - to do impacts by"
Impacts_by_subflow <- ggplot(impact_data, aes(x = impact, fill = direction)) +
  geom_bar() +  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple", "No change (measured)" = "lightblue")) +
  facet_wrap(~X2.2.Subtype, scales = "fixed") +
  labs(
    title = "Biodiversity Impact",
    x = "Impact to Biodiversity",
    y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip()
Impacts_by_subflow



####################################################
#### Figures on  Drivers ################
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
    title = "Count of drivers per Flow",
    x = "Driver",
    y = "Count")
driver_count_simplified

jpeg(file="driver_count_simplified.jpeg")
Fig1 <- plot_grid(driver_count_simplified,subflowcount)
ggsave("figures/for_report/Fig1.pdf", width = 30,  height = 10,  units = "cm")


## Alluvials ###

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
                   expand = c(0.15, 0.15)) +
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
 # scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple")) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

## by flow type
biodata = data[X2.1.Flow.Type.x == "Biotic", ]
physdata = data[X2.1.Flow.Type.x == "Physical", ]
sociodata = data[X2.1.Flow.Type.x == "Sociocultural", ]
humandata = data[X2.1.Flow.Type.x == "Human movement", ]

# Alluvial connecting to Driver to Impact by flow

#biotic
ggplot(data = biodata,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  # facet_wrap(~X2.1.Flow.Type, scales = "free") +
  # scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple")) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

#physical
ggplot(data = physdata,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  # facet_wrap(~X2.1.Flow.Type, scales = "free") +
  # scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple")) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

#human
ggplot(data = humandata,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  # facet_wrap(~X2.1.Flow.Type, scales = "free") +
  # scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple")) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

#socio
ggplot(data = sociodata,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = direction)) +
  # facet_wrap(~X2.1.Flow.Type, scales = "free") +
  # scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple")) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

# Alluvial connecting to Driver to Impact by flow
#not that informative because so many more of the papers are biotic
ggplot(data = data,
       aes(axis1 = driver, axis2 = impact)) + #, y = freq
  geom_alluvium(aes(fill = X2.1.Flow.Type)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Driver", "Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

#####################
### NCP figures ######
#####################

NCP_data$ncp <- gsub("\\.", " ", NCP_data$ncp)

# NCP impacts
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

ncp_by_subflow <- ncp + facet_wrap(~X2.2.Subtype, scales = "fixed")
ncp_by_subflow

# NCP impacts by subflow
# drop trade ,wind, snow melt?
reduced_ncp_data <- NCP_data %>%
  filter(`X2.2.Subtype` %notin% c("trade","wind", "snow melt runoff"))

reduced_ncp_data$ncp <- gsub("\\.", " ", reduced_ncp_data$ncp)

ncp_by_subflow2 <- ggplot(reduced_ncp_data, aes(x = fct_infreq(ncp), fill = ncp_direction)) +
  geom_bar(position= "stack") +
  coord_flip() + theme_minimal() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex hange" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Impacts to Nature's Contribution to People (NCP)",
    x = "NCP",
    y = "Count",
    fill = "Impact Direction") + facet_wrap(~X2.2.Subtype, scales = "fixed")
ncp_by_subflow2


#####################
### NCP + Driver figures ######
#####################

#now read in the data with NCP and drivers in one dataframe:
ncp + facet_wrap(~driver, scales = "fixed")

# all driver categories (too many)
ggplot(driver_impact_ncp, aes(x = ncp, fill = ncp_direction)) +
  geom_bar(position= "stack") +
  facet_wrap(~driver, scales = "fixed") +   coord_flip() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey"))

# reduce driver categories and re-plot
ncp_data <- driver_impact_ncp %>%
  filter(`driver` %notin% c("freshwater.temperature.change","snow.pack..depth.and.hardness.",
                            "remove emissions.concentration.change","C02.concentration",
                            "frazzle..land.ice..change", "water.availability",
                            "surface.water.change", "freshwater.chemistry.change",
                            "emissions.concentration.change",
                            "extreme.weather", "snow.pack.change",
                            "Ocean.currents", "permafrost.melt",
                            "Seawater.chemistry", "Hurricanes"))

ggplot(ncp_data, aes(x = ncp, fill = ncp_direction)) +
  geom_bar(position= "stack") +
  facet_wrap(~driver, scales = "fixed") +   coord_flip() +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Count and direction of Impacts to NCP by Driver",
    x = "NCP",
    y = "Count",
    fill = "Impact Direction") +  theme_minimal()

#Alluvials
# Relationship btwn BD and NCP impacts, where the stratum is the NCP direction
# of impacts
ggplot(data = driver_impact_ncp,
       aes(axis1 = impact, axis2 = ncp)) + #, y = freq
  geom_alluvium(aes(fill = ncp_direction)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

#driver to NCP alluvial (all drivers)
ggplot(data = driver_impact_ncp,
       aes(axis1 = driver, axis2 = ncp)) + #, y = freq
  geom_alluvium(aes(fill = ncp_direction)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()


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
    x = "Human Well-being Impact",
    size = "Count",
    fill = "Impact Direction")
hwb
#broken out by flow
hwb + facet_wrap(~X2.1.Flow.Type, scales = "fixed")
#broken out by subflow
hwb + facet_wrap(~X2.2.Subtype, scales = "fixed")

## altered flow by flow type
altered_flow_data
table(altered_flow_data$altered_flow)
table(altered_flow_data$alteration)

ggplot(altered_flow_data, aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  facet_wrap(~X2.1.Flow.Type, scales = "fixed") +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Changes to Flow",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")

#sparse for some of the categories by promising for migration, disease spread and range-shift
ggplot(altered_flow_data, aes(x = altered_flow, fill = alteration)) +
  geom_bar(position= "stack") +
  coord_flip() +
  facet_wrap(~X2.2.Subtype, scales = "fixed") +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex change" = "goldenrod1", "No change (measured)" = "grey")) +
  labs(
    title = "Changes to Flow",
    x = "Changes to Flow",
    size = "Count",
    fill = "Impact Direction")

ggplot(data = driver_flow_impact,
       aes(axis1 = driver, axis2 = altered_flow)) + #, y = freq
  geom_alluvium(aes(fill = X2.1.Flow.Type.x)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("BD Impact", "NCP Impact"),
                   expand = c(0.15, 0.05)) +
  theme_void()

ggplot(data = driver_flow_impact,
       aes(axis1 = driver, axis2 = altered_flow)) + #, y = freq
  geom_alluvium(aes(fill = X2.2.Subtype.x)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("driver", "altered flow"),
                   expand = c(0.15, 0.05)) +
  theme_void()



### do for just Biotic
biotic = driver_flow_impact[X2.1.Flow.Type.x == "Biotic",]

ggplot(data = biotic,
       aes(axis1 = driver, axis2 = altered_flow)) + #, y = freq
  geom_alluvium(aes(fill = X2.2.Subtype.x)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("driver", "altered flow"),
                   expand = c(0.15, 0.05)) +
  theme_void()
