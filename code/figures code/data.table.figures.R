#Figures of drivers by flow and impacts and NCP

#load libraries
library(ggplot2)
library(tidyverse)
library(data.table)
library(forcats)
library(ggalluvial)

"%notin%" <- Negate("%in%")

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- fread("data/007_output_interventions.csv")

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
table(driver_impact_data$direction)

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
                              id.vars=c("ID_DOI_by_Flow"),
                              measure.vars = patterns(driver="^X2.16.NCP.ES.."),
                              variable.name = "ncp",
                              value.name="ncp_direction")[ncp_direction!="",][,ncp:=gsub(pattern="^X2.16.NCP.ES..(*)", replacement="\\1",ncp)]
NCP_data = NCP_data[ncp != "None"]
NCP_data = NCP_data[ncp_direction != "No direction mentioned"]

driver_impact_data = merge(driver_data, 
                           impact_data, 
                           by="ID_DOI_by_Flow",
                           allow.cartesian=TRUE)
#condense and clean 
driver_impact_data = driver_impact_data[X2.1.Flow.Type =="'Trade (transport of goods and services)'", X2.1.Flow.Type := "Human movement"]
driver_impact_data = driver_impact_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]
driver_impact_data = driver_impact_data[direction != 'No direction mentioned']
table(driver_impact_data$direction)
driver_impact_data = driver_impact_data[impact != "None"]

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
Impacts <- ggplot(impact_data, aes(x = impact, fill = direction)) +
  geom_bar() +  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple", "No change (measured)" = "lightblue")) +
  labs(
    title = "Biodiversity Impact",
    x = "Impact to Biodiversity",
    y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip() 
Impacts

Impacts_by_flow <- ggplot(impact_data, aes(x = impact, fill = direction)) +
  geom_bar() +  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple", "No change (measured)" = "lightblue")) +
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
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple", "No change (measured)" = "lightblue")) +
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
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple")) +
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

# NCP impacts 
ncp <- ggplot(NCP_data, aes(x = ncp, fill = ncp_direction)) +
  geom_bar(position= "stack") +   coord_flip() +
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex Change" = "purple", "No change (measured)" = "lightblue"))
ncp

# NCP impacts by flow type
#ncp_by_flow <- ncp + facet_wrap(~X2.1.Flow.Type, scales = "fixed") 
ncp_by_flow
ncp + facet_wrap(~X2.2.Subtype, scales = "fixed") 

#now read in the data with NCP and drivers in one dataframe:

ncp + facet_wrap(~driver, scales = "fixed") 

# all driver categories (too many)
ggplot(driver_impact_ncp, aes(x = ncp, fill = ncp_direction)) +
  geom_bar(position= "stack") + 
  facet_wrap(~driver, scales = "fixed") +   coord_flip() +
 scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex Change" = "purple", "No change (measured)" = "lightblue"))

# reduce driver categories and re-plot



#Alluvials
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
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple")) +
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
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex change" = "purple")) +
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




### do for just biotic 
biotic_driver_flow_impact = driver_flow_impact