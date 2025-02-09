## Reclassify Subflow Types and ReplaceEntries
# Feb 9 2025

# ****TO DO STIL BEFORE THIS STEP -- #need to figure out what to do with the FLOW itself that have been changed***
library(tidyr)
library(dplyr)
library(data.table)

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/data")
docs <- read.csv("cleaned_4_byFlow.csv")

#load look up tables 
#need to figure out what to do with the FLOW itself that have been changed
biotic <- read.csv("./data_cleaning/bioticFlowSubtype_lookup.csv")
socio <- read.csv("./data_cleaning/socioculturalFlowSubtype_lookup.csv")
phys <-  read.csv("./data_cleaning/physicalFlowSubtype_lookup.csv")

#Read in new entries that will be appended and assigned new IDs
# replace <- read.csv 

# in the look up files, probably need to first remove the ones removed in the 004 step to avoid confusion
phys <- phys %>% filter(Citation != "Chen, 2011, Journal of Sustainable Development", 
                        Citation != "Nielsen, 2015, Global Change Biology", 
                        Citation != "Vaddey, 2010, Watershed Management",
                        Citation != "Jenkins et al., 2013, Advances in Parasitology",
                        Citation != "Dube et al., 2012, INTEGRATED ENVIRONMENTAL ASSESSMENT AND MANAGEMENT", #Mabel checking
                        Citation != "Covich et al., 1997, Hydrological Processes",
                        Citation != "de la Fontaine,  2018, Ecology",
                        Citation != "Perry, 2007, Climate Change 2007") 

biotic <- biotic %>% filter(Citation != "Noyes, 2009, Environment International",
                            Citation != "de la Fontaine,  2018, Ecology",
                            Citation != "Perry, 2007, Climate Change 2007") 

socio <- socio %>% filter(Citation != "Shin et al., 2021, Global Change Biology")                    

#merge subflows into a single datafile
subflows <- merge(socio, phys, all.y = T)
subflows <- merge(biotic, subflows,  all.y = T)

# Merge the new subflows into the main dataset
updated_docs <- merge(subflows, docs, by = "ID_DOI_by_Flow", all.y = T)
glimpse(updated_docs)

#clean entries so all of the range shifts are written the same
#updated_docs[X2.2.Subtype.NEW == "range shifts", X2.2.Subtype.NEW := "Range shift"]
#updated_docs[X2.2.Subtype.NEW == "range shift", X2.2.Subtype.NEW := "Range shift"]

# Add in the new entries entirely for: 
  # de la Fontaine,  2018, Ecology
  # Costa, D., 2021, Journal of Great Lakes Research
  # Shin et al., 2021, Global Change Biology

write.csv(updated_docs, "cleaned_5_byFlow.csv")
