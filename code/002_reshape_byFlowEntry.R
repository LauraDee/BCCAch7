data_rev <- read.csv("data/001_output_flowtypes.csv")
data_rev <- data_rev %>% rename(Citation = 'X1..What.is.the.short.citation.of.the.paper...Author..Year..Journal.')
data_rev <- data_rev %>% filter(Citation != "TEST",Citation != "test" )
dim(data_rev)
### Step 1: Reshape by DOI - Flow (START)
  # Identify repeating columns
  base_names <- gsub("^.*?(2\\.[0-9]+).*", "\\1", names(data_rev))
  place_rep <- which(base_names %in% c("2.1"))
  differences <- diff(place_rep)

  # Ensure indices are equally spaced
  if (length(unique(differences)) != 1) {
    stop("Indices are not equally spaced.")
  }

  # Preallocate a matrix for reshaped data
  total_rows <- nrow(data_rev) * length(place_rep[-length(place_rep)])  # Estimate total rows
  reshaped_data_matrix <- matrix(NA, nrow = total_rows, ncol = 97)  # 98 columns + index

  # Fill the reshaped matrix
  current_row <- 1
  for (i in seq_along(place_rep[-length(place_rep)])) {
    start_idx <- place_rep[i]
    end_idx <- place_rep[i + 1] - 1
    
    # Select columns for the current group
    flow_entry_group <- data_rev[, c(start_idx:end_idx)]
    flow_entry_group[flow_entry_group == ""] <- NA
    # Identify rows with at least one non-NA value
    rows_with_data <- which(rowSums(!is.na(flow_entry_group)) > 0)
    if (length(rows_with_data) == 0) {
        next
      }

    current_group <- data_rev[rows_with_data, c(1:(place_rep[1]-1), start_idx:end_idx, (place_rep[10] + differences[1]+1):ncol(data_rev))]
    

    # Add an index column
    current_group_with_index <- cbind(as.matrix(current_group), i)
    
    # Add to the reshaped matrix
    rows_to_add <- nrow(current_group_with_index)
    reshaped_data_matrix[current_row:(current_row + rows_to_add - 1), ] <- current_group_with_index
    current_row <- current_row + rows_to_add
  }

  # Convert matrix to data frame and assign column names
  rows_with_data <- which(rowSums(!is.na(reshaped_data_matrix)) > 0)
  reshaped_data <- as.data.frame(reshaped_data_matrix[rows_with_data,])
  colnames(reshaped_data) <- c(colnames(data_rev)[c(1:(place_rep[2]-1), (place_rep[10] + differences[1]+1):ncol(data_rev))], "index")
  colnames(reshaped_data) <- short_names <- c(
    "ID_DOI", 
    "Timestamp", 
    "Email", 
    "Citation", 
    "DOI", 
    "Has_Flow", 
    "Its_Transboundary", 
    "Elaboration", 
    "Human_Socio_Flows", 
    "Exclude_Paper", 
    "Excluded", 
    "Excluded_Insights", 
    "2.1 Flow Type", 
    "2.2 Subtype", 
    "2.3 Flow Text", 
    "2.4 Climate Driver", 
    "2.5 Trigger Location", 
    "2.6 Flow Change Text", 
    "2.7 Altered Flow: Quantity", 
    "2.7 Altered Flow: Direction", 
    "2.7 Altered Flow: Location", 
    "2.7 Altered Flow: Distance", 
    "2.7 Altered Flow: Timing", 
    "2.7 Altered Flow: Frequency", 
    "2.7 Altered Flow: Duration", 
    "2.7 Altered Flow: Speed", 
    "2.7 Altered Flow: Feedbacks", 
    "2.7 Altered Flow: Mechanism", 
    "2.7 Altered Flow: Other", 
    "2.8 Other Description", 
    "2.9 Flow Origin/Destination", 
    "2.10 Cited Paper DOI?", 
    "2.11 Biodiversity Text", 
    "2.12 Impact: None", 
    "2.12 Impact: Abundance", 
    "2.12 Impact: Richness", 
    "2.12 Impact: Loss", 
    "2.12 Impact: Disease", 
    "2.12 Impact: Invasion", 
    "2.12 Impact: Composition", 
    "2.12 Impact: Genetics", 
    "2.12 Impact: Land Use Loss", 
    "2.12 Impact: Land Use Restore", 
    "2.12 Impact: Urbanization", 
    "2.12 Impact: Connectivity", 
    "2.12 Impact: Trophic", 
    "2.12 Impact: Indigenous Knowledge", 
    "2.12 Impact: Management", 
    "2.12 Impact: Other", 
    "2.13 Biodiversity Elaboration", 
    "2.14 Ecosystem Type", 
    "2.15 Species Impacted", 
    "2.16 NCP/ES: None", 
    "2.16 NCP/ES: Habitat", 
    "2.16 NCP/ES: Pollination", 
    "2.16 NCP/ES: Air Quality", 
    "2.16 NCP/ES: Climate", 
    "2.16 NCP/ES: Acidification", 
    "2.16 NCP/ES: Freshwater", 
    "2.16 NCP/ES: Water Quality", 
    "2.16 NCP/ES: Soil Protection", 
    "2.16 NCP/ES: Hazards", 
    "2.16 NCP/ES: Organisms", 
    "2.16 NCP/ES: Energy", 
    "2.16 NCP/ES: Food/Feed", 
    "2.16 NCP/ES: Materials", 
    "2.16 NCP/ES: Medicinal", 
    "2.16 NCP/ES: Learning", 
    "2.16 NCP/ES: Experiences", 
    "2.16 NCP/ES: Identities", 
    "2.16 NCP/ES: Options", 
    "2.16 NCP/ES: Other", 
    "2.17 NCP List", 
    "2.18 NCP Text", 
    "2.19 NCP Impact Location", 
    "2.20 Well-being: None", 
    "2.20 Well-being: Welfare", 
    "2.20 Well-being: Justice", 
    "2.20 Well-being: Relational", 
    "2.20 Well-being: Cohesion", 
    "2.20 Well-being: Other", 
    "2.21 Well-being List", 
    "2.22 Inequality Explanation", 
    "2.23 Flow Summary", 
    "Response Mentioned?", 
    "Response Text", 
    "Another Flow?", 
    "Indigenous Nations Text", 
    "Indigenous Info Text", 
    "Review Notes", 
    "Additional Notes", 
    "Physical", 
    "Movement", 
    "Trade", 
    "Sociocultural", 
    "Biotic", 
    "Index"
  )

  reshaped_data$ID_DOI_by_FlowEntry <- seq(1:dim(reshaped_data)[1])

  # Check the reshaped data
  cat("Dimensions of reshaped data:", dim(reshaped_data), "\n")
  cat("Unique values in index column:", levels(factor(reshaped_data$Index)), "\n")
  cat("Unique DOIs:", length(levels(factor(reshaped_data$ID_DOI))), "\n")
  cat("Unique DOIs by Flow Entry:", length(levels(factor(reshaped_data$ID_DOI_by_FlowEntry))), "\n")
dim(reshaped_data)  

write.csv(reshaped_data, "data/002_output_byFlowEntry.csv")


# ### Step 1: Reshape by DOI - Flow (END)
