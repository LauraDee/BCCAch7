docs <- read.csv("data/reshaped_2_byFlowEntry.csv")
names(docs) <- gsub("^X(\\d)", "\\1", names(docs))

reshaped_data <- docs %>%
  separate_rows(`2.1.Flow.Type`, sep = ", ") # Split the rows based on the comma and space

dim(docs)
dim(reshaped_data)

reshaped_data$ID_DOI_by_Flow <- seq(1:dim(reshaped_data)[1])
max(reshaped_data$ID_DOI_by_FlowEntry)
reshaped_data %>% filter(ID_DOI_by_FlowEntry==123) %>% select("Citation") ## Sanity check, this should give you: "1 Sivakumar, B.; 2011; Hydrological Sciences Journal" as of Feb 14, 2025
write.csv(reshaped_data, 'data/reshaped_3_byFlow.csv')
