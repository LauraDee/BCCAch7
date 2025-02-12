## Reclassify Subflow Types and ReplaceEntries
# Feb 9 2025
rm(list = ls())

# ****TO DO STIL BEFORE THIS STEP -- #need to figure out what to do with the FLOW itself that have been changed***
library(tidyr)
library(dplyr)
library(data.table)

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/data")
docs <- read.csv("cleaned_4_byFlow.csv")
#nrow(docs) #217
#clean up extra columns
docs$X.2 <- NULL
docs$X.1 <- NULL
docs$X <- NULL

#Read in new entries that will be appended and assigned new IDs
# we will need to add all of the same columns since the main data has the extra management ones, or we should do it this in an earlier step
replace_socio <- read.csv("./data_cleaning/Kyle_RecodeSubflows-Feb102025.csv", header = T)
replace_socio$X.2 <- NULL
replace_socio$X.1 <- NULL
replace_socio$X <- NULL

# replace_socio has 129 columns and docs has only 99

#combine docs with Kyle's entries:
rbind(docs, replace_socio) #different column numbers so this wont work and needs to be done earlier

#load look up tables
#need to figure out what to do with the FLOW itself that have been changed
biotic <- read.csv("./data_cleaning/bioticFlowSubtype_lookup.csv")
socio <- read.csv("./data_cleaning/socioculturalFlowSubtype_lookup.csv")
phys <-  read.csv("./data_cleaning/physicalFlowSubtype_lookup.csv")

#combine subflows look ups into a single datafile - **this didnt work** so I coerced the names to be the same
glimpse(biotic)
glimpse(phys)
glimpse(socio)
identical(names(biotic), names(socio))
identical(names(biotic), names(phys))
names(phys) <- names(biotic)

#rebind the flow look ups
subflows <- rbind(biotic, phys)
subflows <- rbind(subflows, socio)
nrow(subflows) #164 

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
nrow(subflows) #142

# Merge the new subflow look ups into the main dataset
updated_docs <- merge(subflows, docs, by = "ID_DOI_by_Flow", all.y = T)
glimpse(updated_docs)

updated_docs2 <- merge(subflows, docs, by = "ID_DOI_by_Flow")
nrow(updated_docs) #242
nrow(updated_docs2) #142

#should the merge by all.y??? looks like some subtypes not replicated if so...
# need to put in the subtype.y for the subtype.new for the ones that were left the same for some?
#check which:

updated_docs <- merge(subflows, docs, by = "ID_DOI_by_Flow", all.y = T)
#nrow(updated_docs) #242 

updated_docs2 <- merge(subflows, docs, by = "ID_DOI_by_Flow") 
nrow(updated_docs2) #142

#clean up extra columns and names
updated_docs$DOI.y <- updated_docs$DOI
updated_docs$DOI.x <- NULL
updated_docs$Citation.y <- updated_docs$Citation
updated_docs$Citation.x <- NULL
ID_DOI_by_FlowEntry.x <- NULL

#clean entries so all of the range shifts are written the same
docs <- as.data.table(updated_docs)
docs[X2.2.Subtype.NEW == "range shifts", X2.2.Subtype.NEW := "Range shift"]
docs[X2.2.Subtype.NEW == "range shift", X2.2.Subtype.NEW := "Range shift"]

# Add in the new entries entirely for:
  # de la Fontaine,  2018, Ecology
  # Costa, D., 2021, Journal of Great Lakes Research
  # Shin et al., 2021, Global Change Biology

#waiting on new entries for:
 # "Noyes, 2009, Environment International"
 #"Jenkins et al., 2013, Advances in Parasitology"
 # "Covich et al., 1997, Hydrological Processes"

write.csv(updated_docs, "cleaned_5_byFlow.csv")
