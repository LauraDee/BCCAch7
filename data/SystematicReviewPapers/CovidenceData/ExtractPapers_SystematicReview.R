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

#first identify the biotic papers that weren't sampled in the first round
#and print that list
# compare biotic csv - biotic_full_v1 vs biotic_v1
# 
# d_s <- subset(driver_impact_ncp, driver %in% top10)
biotic_v1_doi <- list(biotic_v1$DOI)
biotic_full_v1_doi <- list(biotic_full_v1$DOI)
length(unique(biotic_v1$DOI)) #185
length(unique(biotic_full_v1$DOI)) #231

biotic <- subset(biotic_full_v1, DOI == biotic_v1_doi)

length(unique(biotic$DOI)) #231 but this should be 46!

#then identify the new papers added since the first Covidence extraction
# compare docs to the final 
doc_DOI <- list(doc$DOI)

data <- final %>% 
 filter(DOI) %notin% doc_DOI

# assign those papers to the groups


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
# write.csv(docs, "extractedpapers_May92025.csv")

#Before filtering the data, write out these files for other teams to look at:
intervention = docs[Intervention == "TRUE",]
write.csv(intervention, "intervention_May9_extraction.csv")
IK = docs[IK == "TRUE",]
write.csv(IK, "IK_May9extraction.csv")

#write out original study files to check whether they should be excluded
og = docs[Original_study == "TRUE",] 
write.csv(og, "og_check_final.csv")

## Now filter to allocate per group 
#numbers of tags per flow type (including tags in multiple types 
# this number drops considerably below)
table(docs$Sociocultural) #82 (10 co-tagged with human movement)
table(docs$Biotic) #320 (with 26 co-tagged with sociocultural)
table(docs$Biophysical) #121
table(docs$Human_movement)  #51 

# counts of co-tags out of curiosity 
docs[, socio_human :=  Sociocultural == "TRUE" & Human_movement == "TRUE",]
docs[, socio_biotic :=  Sociocultural == "TRUE" & Biotic == "TRUE",]
table(docs$socio_human) #10 are co-tagged so we can allocate these to one person
table(docs$socio_biotic) #26 are co-tagged with biotic! 
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
docs3 = docs2[Human_movement == "FALSE",]
physical = docs3[Biophysical == "TRUE",]
table(physical$Biophysical) #112 
#how many left are physical co-tagged with biotic?
physical[, biotic_physical :=  Biotic == "TRUE" & Biophysical == "TRUE",]
table(physical$biotic_physical) #31 co-tagged. 
write.csv(physical, "physical_May9extraction.csv")

# Next step: exclude all but biotic then write out biotic
biotic = docs3[Biophysical == "FALSE",] 
biotic = biotic[Biotic == "TRUE",]
head(biotic)
table(biotic$Biotic) #256 
write.csv(biotic, "biotic_May9extraction.csv")

### Randomly sample either 50% of remaining biotic or n= 200
# https://dplyr.tidyverse.org/reference/sample_n.html 
nrow(biotic) #256
set.seed(7)
biotic.sample = sample_n(biotic, 200, replace = FALSE)
#test  
nrow(biotic.sample)

# could filter these 4 out first before sampling -
table(biotic.sample$Original_study) # 2 are original study
table(biotic.sample$`Perspective paper`) # 2 are perspective paper
#OR:
#biotic.50percent = sample_frac(biotic, 0.25, replace = FALSE)
