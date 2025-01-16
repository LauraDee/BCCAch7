# Summary Stats

graphics.off()
rm(list=ls())

library(ggplot2)
library(data.table)

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/data")
#reshaped_data <- read.csv("reshaped_3_byFlow.csv")
data <- fread("reshaped_4_drivers.csv")
glimpse(data)
data$V1 = NULL
data$V1 = NULL

#check to make sure we removed excluded papers
table(data$Excluded)
length(unique(data$Citation)) #175 papers, and 233?? or 221 unique flows

# get the number of *entries* per flow type 
table(data$`2.1.Flow.Type`)

# get the number of unique papers per flow type 
#** to fix to filter to unique
# table(data$`2.1.Flow.Type`)


#how many papers have multiple flows?
table(data$Another.Flow.)  #47 papers do!

#check the flow entries and IDs
length(data$ID_DOI_by_Flow) #233
length(data$ID_DOI_by_FlowEntry) #221 IDs but 233 rows -- this must be numbers of flows? which is the diffrences
# Is that right? i'm not sure these IDs should be the same number... something isnt right? 
nrow(data) # #233 

#DRIVERS
## What is the count of each driver ?
table(data$'driver:Hurricanes') #10 flow entries w/ hurricanes
table(data$'driver:Drought') #55 on drought
table(data$'driver:Sea level rise') #11 


## What is the count of each driver by flow type?

biotic.data = data['2.1.Flow.Type' == "Biotic",]

table(biotic.data$'driver:Hurricanes') #10 flow entries w/ hurricanes
table(biotic.data$'driver:Drought') #55 on drought
table(biotic.data$'driver:Sea level rise') #11 
