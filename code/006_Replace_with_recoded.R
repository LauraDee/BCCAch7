### Replace recoded entries

reshaped_data_drivers <- read.csv("data/005_output_drivers.csv")
reshaped_data_drivers <- reshaped_data_drivers %>% dplyr::select(-c(X,X.2,X.1,X.3))

# we will need to add all of the same columns since the main data has the extra management ones, or we should do it this in an earlier step
replace_socio <- read.csv("data/data_cleaning/Kyle_RecodeSubflows-Feb102025.csv", header = T)
replace_socio <- replace_socio %>% dplyr::select(-c(X,X.2,X.1))

dim(replace_socio)
dim(reshaped_data_drivers)
identical(colnames(replace_socio), colnames(reshaped_data_drivers))


ids <- c(replace_socio$ID_DOI_by_Flow)
'%notin%' <- Negate('%in%')
reshaped_data_drivers <- reshaped_data_drivers %>% filter(ID_DOI_by_Flow %notin% ids)
dim(reshaped_data_drivers)
reshaped_data_drivers <- rbind(reshaped_data_drivers,replace_socio %>% filter(!is.na(ID_DOI_by_Flow)))
dim(reshaped_data_drivers)
# replace_socio has 129 columns and docs has only 99


write.csv(reshaped_data_drivers, "data/006_output_recoded.csv")
