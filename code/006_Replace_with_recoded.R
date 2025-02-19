### Replace recoded entries

reshaped_data_drivers <- read.csv("data/005_output_drivers.csv")
levels(factor(reshaped_data_drivers$X2.1.Flow.Type))
reshaped_data_drivers <- reshaped_data_drivers %>% dplyr::select(-c(X,X.3,X.2,X.1))

# we will need to add all of the same columns since the main data has the extra management ones, or we should do it this in an earlier step
replace_socio <- read.csv("data/data_cleaning/Kyle_RecodeSubflows-Feb102025.csv", header = T)
replace_socio <- replace_socio %>% dplyr::select(-c(X,X.2,X.1))
glimpse(replace_socio)
dim(replace_socio)
dim(reshaped_data_drivers)
identical(colnames(replace_socio), colnames(reshaped_data_drivers))


ids <- c(replace_socio$ID_DOI_by_Flow)
citation_socio <- c(replace_socio$Citation)
reshaped_data_drivers <- reshaped_data_drivers %>% filter(ID_DOI_by_Flow %notin% ids)
dim(reshaped_data_drivers)
reshaped_data_drivers <- rbind(reshaped_data_drivers,replace_socio %>% filter(!is.na(ID_DOI_by_Flow)))
dim(reshaped_data_drivers)
# replace_socio has 129 columns and docs has only 99


#COSTA RECODED
replace_costa <- read.csv("data/data_cleaning/Brumberg_BCCA_revisions_costa_recode.csv", header = T)
dim(replace_costa)
costa_original <- reshaped_data_drivers %>% filter(ID_DOI_by_Flow %in% replace_costa$ID_DOI_by_Flow)
costa_original <- rbind(costa_original,costa_original)
costa_original[,1:99] <- replace_costa[,1:99]
dim(costa_original)
dim(reshaped_data_drivers)
ids <- c(costa_original$ID_DOI_by_Flow)
reshaped_data_drivers <- reshaped_data_drivers %>% filter(ID_DOI_by_Flow %notin% ids)
dim(reshaped_data_drivers)
reshaped_data_drivers <- rbind(reshaped_data_drivers,costa_original %>% filter(!is.na(ID_DOI_by_Flow)))
dim(reshaped_data_drivers)




replace_dube <- read.csv("data/data_cleaning/reshaped_3_byFlow_Dubefix_recode.csv")
replace_dube$ID_DOI_by_Flow
dim(replace_dube)
dube_original <- reshaped_data_drivers %>% filter(ID_DOI_by_Flow %in% replace_dube$ID_DOI_by_Flow)
dube_original$ID_DOI_by_Flow
dim(dube_original)
names(dube_original)
names(replace_dube)
dube_original[,1:99] <- replace_dube[,1:99]
dim(dube_original)
dim(reshaped_data_drivers)
ids <- c(dube_original$ID_DOI_by_Flow)
reshaped_data_drivers <- reshaped_data_drivers %>% filter(ID_DOI_by_Flow %notin% ids)
dim(reshaped_data_drivers)
reshaped_data_drivers <- rbind(reshaped_data_drivers,dube_original %>% filter(!is.na(ID_DOI_by_Flow)))
dim(reshaped_data_drivers)

#reshaped_3_byFlow_Dubefix_recode.csv
#replace_Dube <- read.csv("data/data_cleaning/reshaped_3_byFlow_Dubefix_recode.csv", header = T)



write.csv(reshaped_data_drivers, "data/006_output_recoded.csv")

