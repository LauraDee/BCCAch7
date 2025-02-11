## Reclassify Subflow Types and ReplaceEntries
# Feb 9 2025
rm(list = ls())

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
glimpse(biotic)
glimpse(phys)
glimpse(socio)

#Read in new entries that will be appended and assigned new IDs
# we will need to add all of the same columns since the main data has the extra management ones, or we should do it this in an earlier step
# replace_socio <- read.csv()

#combine subflows look ups into a single datafile - **this didnt work**
identical(names(biotic), names(phys))
names(phys) <- names(biotic)
identical(names(biotic), names(socio))
names(socio) <- names(biotic)

subflows <- rbind(biotic, phys)
subflows <- rbind(socio, biotic)

# in the look up files, probably need to first remove the ones removed in the 004 step to avoid confusion
subflows <- subflows %>% filter(Citation != "Chen, 2011, Journal of Sustainable Development",
                        Citation != "Nielsen, 2015, Global Change Biology",
                        Citation != "Vaddey, 2010, Watershed Management",
                        Citation != "Jenkins et al., 2013, Advances in Parasitology", #Enrique recoding
                        Citation != "Dube et al., 2012, INTEGRATED ENVIRONMENTAL ASSESSMENT AND MANAGEMENT", #Mabel checking
                        Citation != "Covich et al., 1997, Hydrological Processes", #Mabel recoding
                        Citation != "de la Fontaine,  2018, Ecology", #Colleen recoded
                        Citation != "Noyes, 2009, Environment International", #Becky recoding
                        Citation != "Perry, 2007, Climate Change 2007",
                        Citation != "Shin et al., 2021, Global Change Biology") #Kyle recoded

# Merge the new subflow look ups into the main dataset
updated_docs <- merge(subflows, docs, by = "ID_DOI_by_Flow", all.y = T)
glimpse(updated_docs)

#clean entries so all of the range shifts are written the same
#updated_docs[X2.2.Subtype.NEW == "range shifts", X2.2.Subtype.NEW := "Range shift"]
#updated_docs[X2.2.Subtype.NEW == "range shift", X2.2.Subtype.NEW := "Range shift"]

# Add in the new entries entirely for:
  # de la Fontaine,  2018, Ecology
  # Costa, D., 2021, Journal of Great Lakes Research
  # Shin et al., 2021, Global Change Biology

#waiting on new entries for:
 # "Noyes, 2009, Environment International"
 #"Jenkins et al., 2013, Advances in Parasitology"
 # "Covich et al., 1997, Hydrological Processes"

write.csv(updated_docs, "cleaned_5_byFlow.csv")
