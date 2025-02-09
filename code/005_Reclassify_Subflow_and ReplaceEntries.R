## Reclassify Subflow Types and ReplaceEntries
# Feb 9 2025

# ****TO DO STIL BEFORE THIS STEP -- #need to figure out what to do with the FLOW itself that have been changed***

library(tidyr)
library(dplyr)

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

# Merge the new subflows into the main dataset
updated_docs <- merge(socio, docs, by = "ID_DOI_by_Flow")
updated_docs <- merge(phys, docs, by = "ID_DOI_by_Flow")
updated_docs <- merge(biotic, docs, by = "ID_DOI_by_Flow")
glimpse(updated_docs)

# Add in the new entries entirely: 
  # de la Fontaine,  2018, Ecology
  # Costa, D., 2021, Journal of Great Lakes Research
  # Shin et al., 2021, Global Change Biology


