docs <- read.csv("data/003_output_byFlow.csv")

biotic <- read.csv("data/data_cleaning/bioticFlowSubtype_lookup.csv")
socio <- read.csv("data/data_cleaning/socioculturalFlowSubtype_lookup.csv")
phys <-  read.csv("data/data_cleaning/physicalFlowSubtype_lookup.csv")
human <-  read.csv("data/data_cleaning/lookuptable_humamovement.csv")



## Reclassify human movement
lookup_table <- human %>%
  distinct(sub_original, sub_new)
df1_updated <- docs %>%
  left_join(lookup_table, by = c("X2.2.Subtype" = "sub_original")) %>%
  mutate(X2.2.Subtype = if_else(is.na(sub_new), X2.2.Subtype, sub_new)) %>%
  select(-sub_new) 

## Reclassify socio
df2_updated <- df1_updated %>%
  left_join(socio %>% dplyr::select("X2.2.Subtype.NEW", "ID_DOI_by_Flow"), by = c("ID_DOI_by_Flow" = "ID_DOI_by_Flow")) %>%
  mutate(X2.2.Subtype = if_else(is.na(X2.2.Subtype.NEW), X2.2.Subtype, X2.2.Subtype.NEW)) %>%
  select(-X2.2.Subtype.NEW) 
glimpse(df2_updated)

## Reclassify physical
glimpse(phys)
df3_updated <- df2_updated %>%
  left_join(phys %>% dplyr::select("X2.1.Flow.Type.NEW","X2.2.Subtype.NEW", "ID_DOI_by_Flow"), by = c("ID_DOI_by_Flow" = "ID_DOI_by_Flow")) %>%
  mutate(X2.1.Flow.Type = if_else(is.na(X2.1.Flow.Type.NEW), X2.1.Flow.Type, X2.1.Flow.Type.NEW)) %>%
  mutate(X2.2.Subtype = if_else(is.na(X2.2.Subtype.NEW), X2.2.Subtype, X2.2.Subtype.NEW)) %>%
  select(-X2.2.Subtype.NEW,-X2.1.Flow.Type.NEW) 
glimpse(df3_updated)

## Reclassify biotic
glimpse(biotic)
df4_updated <- df3_updated %>%
  left_join(biotic %>% dplyr::select("X2.1.Flow.Type.NEW","X2.2.Subtype.NEW", "ID_DOI_by_Flow"), by = c("ID_DOI_by_Flow" = "ID_DOI_by_Flow")) %>%
  mutate(X2.1.Flow.Type = if_else(is.na(X2.1.Flow.Type.NEW), X2.1.Flow.Type, X2.1.Flow.Type.NEW)) %>%
  mutate(X2.2.Subtype = if_else(is.na(X2.2.Subtype.NEW), X2.2.Subtype, X2.2.Subtype.NEW)) %>%
  select(-X2.2.Subtype.NEW,-X2.1.Flow.Type.NEW) 
glimpse(df4_updated)
df4_updated %>% filter(ID_DOI_by_Flow==60)




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
  # de la Fontaine,  2018, Ecology - in the form
  # Costa, D., 2021, Journal of Great Lakes Research - Hilary sent Laura
  # Shin et al., 2021, Global Change Biology - in "./data_cleaning/Kyle_RecodeSubflows-Feb102025.csv"

#waiting on new entries for:
 # "Noyes, 2009, Environment International"
 # "Jenkins et al., 2013, Advances in Parasitology"
 # "Covich et al., 1997, Hydrological Processes"

write.csv(updated_docs, "cleaned_5_byFlow.csv")

#Read in new entries that will be appended and assigned new IDs
# we will need to add all of the same columns since the main data has the extra management ones, or we should do it this in an earlier step
replace_socio <- read.csv("./data_cleaning/Kyle_RecodeSubflows-Feb102025.csv", header = T)
replace_socio$X.2 <- NULL
replace_socio$X.1 <- NULL
replace_socio$X <- NULL

#read in Hilary and Colleen's

# replace_socio has 129 columns and docs has only 99

#combine docs with Kyle's entries:
rbind(docs, replace_socio) #different column numbers so this wont work and needs to be done earlier
#****NEED HELP HERE *****
#
