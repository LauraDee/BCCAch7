docs <- read.csv("data/003_output_byFlow.csv") #before it was called reshaped_3_byFlow

biotic <- read.csv("data/data_cleaning/bioticFlowSubtype_lookup.csv")
socio <- read.csv("data/data_cleaning/socioculturalFlowSubtype_lookup.csv")
#phys <-  read.csv("data/data_cleaning/physicalFlowSubtype_lookup.csv")
human <-  read.csv("data/data_cleaning/lookuptable_humamovement.csv")
phys <-  read.csv("data/data_cleaning/physicalFlowSubtype_lookup_CRM.csv")


## Reclassify human movement
lookup_table <- human %>%
  distinct(sub_original, sub_new)
df1_updated <- docs %>%
  left_join(lookup_table, by = c("X2.2.Subtype" = "sub_original")) %>%
  mutate(X2.2.Subtype = if_else(is.na(sub_new), X2.2.Subtype, sub_new)) %>%
  select(-sub_new)
glimpse(df1_updated) 


## Reclassify socio
# ## Sanity Check: DOIs must match
# socio %>% select(ID_DOI_by_Flow,DOI) %>% tail(3)
# ids <- socio %>% select(ID_DOI_by_Flow) %>% tail(3)
# df1_updated$DOI[which(df1_updated$ID_DOI_by_Flow %in% ids$ID_DOI_by_Flow)]

df2_updated <- df1_updated %>%
  left_join(socio %>% dplyr::select("X2.2.Subtype.NEW", "ID_DOI_by_Flow"), by = c("ID_DOI_by_Flow" = "ID_DOI_by_Flow")) %>%
  mutate(X2.2.Subtype = if_else(is.na(X2.2.Subtype.NEW), X2.2.Subtype, X2.2.Subtype.NEW)) %>%
  select(-X2.2.Subtype.NEW) 
glimpse(df2_updated)

levels(factor(df1_updated$X2.2.Subtype[which(df1_updated$X2.1.Flow.Type=="Sociocultural")]))
levels(factor(df2_updated$X2.2.Subtype[which(df2_updated$X2.1.Flow.Type=="Sociocultural")]))
## Reclassify physical

## Sanity Check: DOIs must match
# phys %>% select(ID_DOI_by_Flow,DOI) %>% tail(3)
# ids <- phys %>% select(ID_DOI_by_Flow) %>% tail(3)
# df2_updated$DOI[which(df2_updated$ID_DOI_by_Flow %in% ids$ID_DOI_by_Flow)]

df3_updated <- df2_updated %>%
  left_join(phys %>% dplyr::select("X2.1.Flow.Type.NEW","X2.2.Subtype.NEW", "ID_DOI_by_Flow"), by = c("ID_DOI_by_Flow" = "ID_DOI_by_Flow")) %>%
  mutate(X2.1.Flow.Type = if_else(is.na(X2.1.Flow.Type.NEW), X2.1.Flow.Type, X2.1.Flow.Type.NEW)) %>%
  mutate(X2.2.Subtype = if_else(is.na(X2.2.Subtype.NEW), X2.2.Subtype, X2.2.Subtype.NEW)) %>%
  select(-X2.2.Subtype.NEW,-X2.1.Flow.Type.NEW) 

levels(factor(df2_updated$X2.2.Subtype[which(df2_updated$X2.1.Flow.Type=="Physical")]))
levels(factor(df3_updated$X2.2.Subtype[which(df3_updated$X2.1.Flow.Type=="Physical")]))
levels(factor(phys$X2.2.Subtype.NEW[which(phys$X2.1.Flow.Type.NEW=="Physical")]))





## Reclassify biotic
## Sanity Check: DOIs must match
# biotic %>% select(ID_DOI_by_Flow,DOI) %>% tail(4)
# ids <- biotic %>% select(ID_DOI_by_Flow) %>% tail(4)
# df3_updated$DOI[which(df3_updated$ID_DOI_by_Flow %in% ids$ID_DOI_by_Flow)]

df4_updated <- df3_updated %>%
  left_join(biotic %>% dplyr::select("X2.1.Flow.Type.NEW","X2.2.Subtype.NEW", "ID_DOI_by_Flow"), by = c("ID_DOI_by_Flow" = "ID_DOI_by_Flow")) %>%
  mutate(X2.1.Flow.Type = if_else(is.na(X2.1.Flow.Type.NEW), X2.1.Flow.Type, X2.1.Flow.Type.NEW)) %>%
  mutate(X2.2.Subtype = if_else(is.na(X2.2.Subtype.NEW), X2.2.Subtype, X2.2.Subtype.NEW)) %>%
  select(-X2.2.Subtype.NEW,-X2.1.Flow.Type.NEW) %>%
  mutate(X2.1.Flow.Type = ifelse(X2.1.Flow.Type=="Trade (transport of goods and services)","Trade",X2.1.Flow.Type))
  levels(factor(df4_updated$X2.1.Flow.Type))


## list_removed remove, list_recode recode
## list not in subflow phys


levels(factor(df3_updated$X2.2.Subtype[which(df3_updated$X2.1.Flow.Type=="Biotic")]))
levels(factor(df4_updated$X2.2.Subtype[which(df4_updated$X2.1.Flow.Type=="Biotic")]))
levels(factor(df4_updated$X2.2.Subtype[which(df4_updated$X2.1.Flow.Type=="Biotic")]))



levels1 <- levels(factor(df4_updated$X2.2.Subtype[df4_updated$X2.1.Flow.Type == "Physical"]))
levels2 <- levels(factor(phys$X2.2.Subtype.NEW[phys$X2.1.Flow.Type.NEW == "Physical"]))
# Find levels present in one but not the other
non_matching_levels1 <- setdiff(levels1, levels2)
# Identify rows with non-matching levels in the first data frame
non_matching_rows_df4 <- df4_updated[df4_updated$X2.1.Flow.Type == "Physical" & df4_updated$X2.2.Subtype %in% non_matching_levels1, ]
list_nomatching_phys <- non_matching_rows_df4 %>% select(Citation,X2.1.Flow.Type,X2.2.Subtype,ID_DOI_by_Flow)


levels1 <- levels(factor(df4_updated$X2.2.Subtype[df4_updated$X2.1.Flow.Type == "Biotic"]))
levels2 <- levels(factor(biotic$X2.2.Subtype.NEW[biotic$X2.1.Flow.Type.NEW == "Biotic"]))
# Find levels present in one but not the other
non_matching_levels1 <- setdiff(levels1, levels2)
# Identify rows with non-matching levels in the first data frame
non_matching_rows_df4 <- df4_updated[df4_updated$X2.1.Flow.Type == "Biotic" & df4_updated$X2.2.Subtype %in% non_matching_levels1, ]
list_nomatching_biotic <- non_matching_rows_df4 %>% select(Citation,X2.1.Flow.Type,X2.2.Subtype)

list_nomatching_biotic
list_nomatching_phys
list_recode <- df4_updated %>% select(Citation,X2.1.Flow.Type,X2.2.Subtype) %>% filter(X2.1.Flow.Type=="Recode")
list_remove <- df4_updated %>% select(Citation,X2.1.Flow.Type,X2.2.Subtype) %>% filter(X2.1.Flow.Type=="Remove")


list_remove_handpicked <- c("Chen, 2011, Journal of Sustainable Development","Nielsen, 2015, Global Change Biology","Vaddey, 2010, Watershed Management", "Perry, 2007, Climate Change 2007")
list_recode_handpicked <- c("Jenkins et al., 2013, Advances in Parasitology",
                    "Noyes, 2009, Environment International",
                    "Covich et al., 1997, Hydrological Processes", "de la Fontaine,  2018, Ecology")

dois_byflow_to_remove <- c(95,69)


all_removed <- df4_updated %>% filter(Citation %in% list_nomatching_biotic$Citation|
                      ID_DOI_by_Flow %in% list_nomatching_phys$ID_DOI_by_Flow|
                      X2.1.Flow.Type %in% c("Recode","Remove")|
                      Citation %in% list_remove_handpicked|
                      ID_DOI_by_Flow %in% dois_byflow_to_remove|
                      Citation %in% list_recode_handpicked)


# df4_updated <- df4_updated %>% filter(Citation %notin% list_nomatching_biotic$Citation, 
#                       Citation %notin% list_nomatching_phys$Citation, 
#                       X2.1.Flow.Type %notin% c("Recode","Remove"), 
#                       Citation %notin% list_remove_handpicked, 
#                       Citation %notin% list_recode_handpicked)


df4_updated <- df4_updated %>% mutate(X2.1.Flow.Type = ifelse(X2.1.Flow.Type=="Trade (transport of goods and services)","Trade",X2.1.Flow.Type))


levels(factor(df4_updated$X2.1.Flow.Type))

write.csv(df4_updated, 'data/004_output_Reclassified.csv')
write.csv(all_removed , 'data/999_removed_papers.csv')
