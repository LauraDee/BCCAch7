### Replace recoded entries

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
