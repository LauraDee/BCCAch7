## Remove entries that have been checked, and ones that need to be replaced entirely
# Flag and remove papers that need to be recoded.
# Feb 9 2025
docs <- read.csv("data/reshaped_6_recoded.csv")

#list of entire papers to remove  forever
 # “Nielsen, 2015, Global Change Biology”,  “Vaddey, 2010, Watershed Management”,
 # “Chen, 2011, Journal of Sustainable Development”
 # Perry, 2007, Climate Change 2007 which is the IPCC??? check.

doc2 <- docs %>% filter(Citation != "Chen, 2011, Journal of Sustainable Development",
                        Citation != "Nielsen, 2015, Global Change Biology",
                        Citation != "Vaddey, 2010, Watershed Management",
                        Citation != "Perry, 2007, Climate Change 2007")

# list of papers to remove that need to be recoded:
list_to_recode <- c("Jenkins et al., 2013, Advances in Parasitology",
                    "Noyes, 2009, Environment International",
                    "Dube et al., 2012, INTEGRATED ENVIRONMENTAL ASSESSMENT AND MANAGEMENT",
                    "Covich et al., 1997, Hydrological Processes")

doc2 <- doc2 %>% filter(Citation != "Jenkins et al., 2013, Advances in Parasitology",
                        Citation != "Noyes, 2009, Environment International",
                        Citation != "Dube et al., 2012, INTEGRATED ENVIRONMENTAL ASSESSMENT AND MANAGEMENT", #Mabel checking
                        Citation != "Covich et al., 1997, Hydrological Processes") # #Mabel checking


# list of subflow entries (by_flow) to remove:

 ##** We need to think about how to do this if some have been recoded in the form
 ##*# then if we reload the data and reassign the #s - will this still work?
subflow_to_remove <-  doc2 %>% filter(ID_DOI_by_Flow != "95",
                                      ID_DOI_by_Flow != "69",
                                      ID_DOI_by_Flow != "135") # this is Dube et al 2012 that says River and groundwater flows along with sediment and pollunts and lists Biotic entry
 ###Dube et al., 2012, INTEGRATED ENVIRONMENTAL ASSESSMENT AND MANAGEMENT" needs to be recoded.

# list of entire papers to remove that have been recoded and need to be replaced:
  # de la Fontaine,  2018, Ecology
  # Costa, D., 2021, Journal of Great Lakes Research
  # Shin et al., 2021, Global Change Biology
doc2 <- doc2 %>% filter(Citation != "de la Fontaine,  2018, Ecology",
                        Citation != "Costa, D., 2021, Journal of Great Lakes Research",
                        Citation != "Shin et al., 2021, Global Change Biology") # #Mabel checking

## list of papers with at least one major flow that was reclassified and needs to be replaced
# or just have the Flow entry changed*** HOW TO BEST DO THIS?
 # Hartig, 2021, Journal of Great Lakes Research, DOI_by_Flow #95 changed to Biotic (range shift)

dim(docs)
dim(doc2)
write.csv(doc2, "reshaped_7_cleaned.csv")



