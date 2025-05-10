## Systematic review ## Extract Papers full list
# filter papers by each flow type when there are multiple tags
# Laura Dee - May9 2025

#Close graphics and clear local memory
graphics.off()
rm(list=ls())

setwd('~/Documents/GitHub/BCCAch7/data/SystematicReviewPapers/CovidenceData/')

library(splitstackshape)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

"%notin%" <- Negate("%in%")

biotic_v1 = fread('biotic_sample_nov1extraction.csv', header = T)
biotic_full_v1 = fread('biotic_FULL_nov1extraction.csv', header = T)
doc = fread('november1codingextraction.csv', header=T)
final = fread('review_462040_included_csv_20250424044203.csv', header = T)

#fir  st identify the biotic papers that weren't sampled in the first round
#and print that list
# compare biotic csv - biotic_full_v1 vs biotic_v1
# 
# subset(b, !(y %in% a$x))

biotic_v1_doi <- list(biotic_v1$DOI)
biotic_full_v1_doi <- list(biotic_full_v1$DOI)
length(unique(biotic_v1$DOI)) #185
length(unique(biotic_full_v1$DOI)) #231
biotic <- subset(biotic_full_v1, !(DOI %in% biotic_v1$DOI))
length(unique(biotic$DOI)) #check this should be 46!

write.csv(biotic, "remainingbiotic_nov1_papers.csv")

#then identify the new papers added since the first Covidence extraction
# compare docs to the final 
# subset(b, !(y %in% a$x))

data = subset(final, !(DOI %in% doc$DOI))

#############################################
##### assign those papers to the groups #####
#############################################
# check this to make sure code is working - docs=final
#column names for each tag
docs = data
tags = unique(trimws(unlist(strsplit(docs$Tags, split=";"))))
for(tag in tags) {
  set(docs, j=tag, value=F)
  set(docs, i=grep(tag, docs$Tags), j=tag, value=T)
}
setcolorder(docs, 
            neworder=c("Sociocultural",
                       "Human movement",
                       "Biophysical",
                       "Biotic",
                       "IK",
                       "Intervention",
                       "Potentially relevant",
                       "save for other sections",
                       "Transboundary unclear",
                       "Original study",
                       "Perspective paper"),
            after = "Tags")

#remain column names with spaces
docs$Human_movement = docs$`Human movement`
docs$Original_study = docs$`Original study`
docs$`Original study` = NULL
docs$`Human movement` = NULL

#write out full reshaped data file 
# write.csv(docs, "full_extractedpapers_May92025.csv")
write.csv(docs, "new_only_extractedpapers_May92025.csv")

#Before filtering the data, write out these files for other teams to look at:
intervention = docs[Intervention == "TRUE",]
write.csv(intervention, "intervention_May9_extraction.csv")

# IK = docs[IK == "TRUE",]
# write.csv(IK, "IK_May9extraction.csv")

transboundary_unclear = docs[`Transboundary unclear` == "TRUE",]
write.csv(transboundary_unclear, "transboundary_unclear_May9.csv")

#write out original study files to check whether they should be excluded
og = docs[Original_study == "TRUE",] 
write.csv(og, "og_check_finalmay9.csv")

## Now filter to allocate per group 
#numbers of tags per flow type (including tags in multiple types 
# this number drops considerably below)
table(docs$Sociocultural) #82 originally, now 83 where is the missing one (10 co-tagged with human movement)
table(docs$Biotic) #additional 58...? 392 .... (with 27 co-tagged with sociocultural)
table(docs$Biophysical) #151 - final is additional 24
table(docs$Human_movement)  #51 

# counts of co-tags out of curiosity 
docs[, socio_human :=  Sociocultural == "TRUE" & Human_movement == "TRUE",]
docs[, socio_biotic :=  Sociocultural == "TRUE" & Biotic == "TRUE",]
table(docs$socio_human) #10 are co-tagged so we can allocate these to one person
table(docs$socio_biotic) #27 are co-tagged with biotic! 
docs[, socio_physical :=  Sociocultural == "TRUE" & Biophysical == "TRUE",]
table(docs$socio_physical) #only 5 that are also physical 
docs[, human_biotic :=  Human_movement == "TRUE" & Biotic == "TRUE",]
table(docs$human_biotic) # 9 with human movement and biotic 

#write out papers with any sociocultural tag
socio = docs[Sociocultural == "TRUE",]
socio = socio[, socio_human :=  Sociocultural == "TRUE" & Human_movement == "TRUE",]
write.csv(socio, "sociopapers_May9extraction.csv")

# Next step: exclude sociocultural papers then subset to ANY with human-movement
docs2 = docs[Sociocultural == "FALSE",] 
human = docs2[Human_movement == "TRUE",]
table(human$Human_movement) #41 
write.csv(human, "human_May9extraction.csv")

# Next step: exclude sociocultural (already done above) and human movement, then filter with ANY physical flow
docs3 = docs[Human_movement == "FALSE",]
physical = docs[Biophysical == "TRUE",]
table(physical$Biophysical) #112  --> #142 --> 30 left. in the final... but it shoudlnt include original so this isnt working
#how many left are physical co-tagged with biotic?
physical[, biotic_physical :=  Biotic == "TRUE" & Biophysical == "TRUE",]
table(physical$biotic_physical) #31 vs 35  ... 4 in final co-tagged. 
write.csv(physical, "physical_May9extraction.csv")

# Next step: exclude all but biotic then write out biotic
biotic2 = docs[Biophysical == "FALSE",] 
biotic2 = biotic[Biotic == "TRUE",]
head(biotic2)
table(biotic2$Biotic) #256 --> 54 in the new that wasnt in the old


#combine biotic2 and biotic -- need to expand to add tags to biotic
docs = biotic
tags = unique(trimws(unlist(strsplit(docs$Tags, split=";"))))
for(tag in tags) {
  set(docs, j=tag, value=F)
  set(docs, i=grep(tag, docs$Tags), j=tag, value=T)
}
setcolorder(docs, 
            neworder=c("Sociocultural",
                       "Human movement",
                       "Biophysical",
                       "Biotic",
                       "IK",
                       "Intervention",
                       "Potentially relevant",
                       "save for other sections",
                       "Transboundary unclear",
                       "Original study",
                       "Perspective paper"),
            after = "Tags")

#remain column names with spaces
docs$Human_movement = docs$`Human movement`
docs$Original_study = docs$`Original study`
docs$`Original study` = NULL
docs$`Human movement` = NULL

head(docs)

#combine the remaining biotic papers that havent been screened yet
biotic_final <- rbind(docs, biotic2, fill = TRUE)
length(unique(biotic_final$DOI)) #100 left

write.csv(biotic_final, "biotic_May9extraction.csv")
table(biotic_final$`Transboundary unclear`)

