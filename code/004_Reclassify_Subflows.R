docs <- read.csv("data/reshaped_3_byFlow.csv")

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
write.csv(df4_updated, 'data/reshaped_4_Reclassified.csv')
