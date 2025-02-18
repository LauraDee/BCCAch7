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

## Reclassify biotic
## Sanity Check: DOIs must match
biotic %>% select(ID_DOI_by_Flow,DOI) %>% tail(4)
ids <- biotic %>% select(ID_DOI_by_Flow) %>% tail(4)
df3_updated$DOI[which(df3_updated$ID_DOI_by_Flow %in% ids$ID_DOI_by_Flow)]

df4_updated <- df3_updated %>%
  left_join(biotic %>% dplyr::select("X2.1.Flow.Type.NEW","X2.2.Subtype.NEW", "ID_DOI_by_Flow"), by = c("ID_DOI_by_Flow" = "ID_DOI_by_Flow")) %>%
  mutate(X2.1.Flow.Type = if_else(is.na(X2.1.Flow.Type.NEW), X2.1.Flow.Type, X2.1.Flow.Type.NEW)) %>%
  mutate(X2.2.Subtype = if_else(is.na(X2.2.Subtype.NEW), X2.2.Subtype, X2.2.Subtype.NEW)) %>%
  select(-X2.2.Subtype.NEW,-X2.1.Flow.Type.NEW) 

df4_updated <- df4_updated %>% mutate(X2.1.Flow.Type = ifelse(X2.1.Flow.Type=="Trade (transport of goods and services)","Trade",X2.1.Flow.Type))
levels(factor(df4_updated$X2.1.Flow.Type))
list(unique(df4_updated$X2.2.Subtype))
write.csv(df4_updated, 'data/004_output_Reclassified.csv')
