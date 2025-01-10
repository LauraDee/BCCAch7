#subflow look up and fix - Physical FLow
graphics.off()
rm(list=ls())

library(tidyr)
library(reshape2)
library(data.table)
library(readxl)
library(dplyr)

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/data")
phys_new <- fread("data_cleaning/PHYSICALTEAM_subflowmerge.csv")
# phys_old <- fread("data_cleaning/reshaped_v2_drivers_jan8.csv")
# the current reshaped master file with the older names: 
phys_old <- fread("reshaped_4_drivers.csv")

phys_new = phys_new[Excluded == "No", ]
phys_old = phys_old[Excluded == "No", ]
head(phys_new)
head(phys_old)
colnames(phys_old)

phys_old = phys_old[Physical == "TRUE", ]
# phys_old = phys_old["2.1.Flow.Type" == "Physical"]
phys_new = phys_new[Physical == "TRUE", ]
table(phys_new$Physical)
table(phys_old$Physical)

#df1 %>% select(A, B, E)

#phys_new = subset(phys_new, select=c("Citation", "DOI", "2.1 Flow Type", "Task", "2.2 Subtype"))
phys_new = subset(phys_new, select=c("Citation", "DOI", "2.1 Flow Type", "2.2 Subtype"))
phys_new$"2.2.Subtype" <- phys_new$"2.2 Subtype"
phys_old = subset(phys_old, select=c("Citation", "DOI", "2.1.Flow.Type",  "2.2.Subtype"))

phys.fix = merge(phys_new, phys_old, by = c("Citation", "DOI"), all.y = TRUE, all.x = TRUE, allow.cartesian = TRUE)
head(phys.fix)

phys.fix$'2.2.Subtype.NEW' <- phys.fix$'2.2.Subtype.x'
phys.fix$'2.2.Subtype.OLD' <- phys.fix$'2.2.Subtype.y'

write.csv(phys.fix, "data_cleaning/physicalFlowSubtype_lookup3.csv")


