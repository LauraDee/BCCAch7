library(tidyr)
library(dplyr)

docs <- read.csv("data/reshaped_2_byFlowEntry.csv")
# lAuras
setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/data")
docs <- read.csv("reshaped_2_byFlowEntry.csv")

names(docs) <- gsub("^X(\\d)", "\\1", names(docs))
glimpse(docs)
#column names for each tag


# Assuming the column name is `flow_type` (adjust it if necessary)
reshaped_data <- docs %>%
  separate_rows(`2.1.Flow.Type`, sep = ", ") # Split the rows based on the comma and space

glimpse(reshaped_data)
dim(docs)
dim(reshaped_data)

reshaped_data$ID_DOI_by_Flow <- seq(1:dim(reshaped_data)[1])
max(reshaped_data$ID_DOI_by_FlowEntry)
write.csv(reshaped_data, 'data/reshaped_3_byFlow.csv')
#laura: write.csv(reshaped_data, 'reshaped_3_byFlow.csv')