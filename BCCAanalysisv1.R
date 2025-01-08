### Explore BCCA coding responses Laura Jan 7 2024

library(splitstackshape)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(readxl)
library(dplyr)

# Define folder paths
main_folder <- "C:/Users/basti/Documents/GitHub/BCCAch7"
data_folder <- file.path(main_folder, "data")

data_rev <- read.csv(list.files(path = data_folder, pattern = "\\.csv$", full.names = TRUE)[1])

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
    current_group <- data_rev[, c(1:(place_rep[1]-1), start_idx:end_idx, (place_rep[10] + differences[1]+1):ncol(data_rev))]
    
    # Add an index column
    current_group_with_index <- cbind(as.matrix(current_group), i)
    
    # Add to the reshaped matrix
    rows_to_add <- nrow(current_group_with_index)
    reshaped_data_matrix[current_row:(current_row + rows_to_add - 1), ] <- current_group_with_index
    current_row <- current_row + rows_to_add
  }

  # Convert matrix to data frame and assign column names
  reshaped_data <- as.data.frame(reshaped_data_matrix)
  colnames(reshaped_data) <- c(colnames(data_rev)[c(1:(place_rep[2]-1), (place_rep[10] + differences[1]+1):ncol(data_rev))], "index")
  colnames(reshaped_data) <- short_names <- c(
    "ID", 
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


  # Check the reshaped data
  cat("Dimensions of reshaped data:", dim(reshaped_data), "\n")
  cat("Unique values in index column:", levels(factor(reshaped_data$Index)), "\n")
  glimpse(reshaped_data)


### Step 1: Reshape by DOI - Flow (END)

### Step 2 - Clean up other names  (START)

docs = reshaped_data
## need to remove test paper flows - find and drop
glimpse(docs)
docs <- docs %>% filter(Citation %notin% c("TEST","test"))

table(docs$Excluded) #177 no, 229 yes
docs$ExcludeReason <- docs$'7. Is there any reason to exclude this paper? If so, explain.'


docs$Flow1 <- docs$'2.1. What type of flow is it?'
table(docs$`2.1 Flow Type`)
glimpse(docs)
setDT(docs) 
#Identify subtypes 
table(docs$`2.2 Subtype`)

## need to harmonize the category names
#cover[local_provenance =="Naturalised", local_provenance:="INT"]
print(unique(docs$`2.2 Subtype`))
docs[`2.2 Subtype` == "test flow", `2.2 Subtype` := "TEST"]
docs[`2.2 Subtype` == "river flow", `2.2 Subtype` := "River flow"]
docs[`2.2 Subtype` == "River Flow", `2.2 Subtype` := "River flow"]
docs[`2.2 Subtype` == "range-shift", `2.2 Subtype` := "Range-shift"]
docs[`2.2 Subtype` == "range shift", `2.2 Subtype` := "Range-shift"]
docs[`2.2 Subtype` == "Range shift", `2.2 Subtype` := "Range-shift"]
docs[`2.2 Subtype` == "range shifts of fisheries species", `2.2 Subtype` := "Range-shift"]
docs[`2.2 Subtype` == "Range shift of algal species northward that affects fish and shellfish and produces a toxin that is harmful when consumed. The sreener mentioned island examples but they are not relevant. However, Canada is not mentioned in the source paper.", `2.2 Subtype` := "Range-shift"]
docs[`2.2 Subtype` == "range expansion", `2.2 Subtype` := "Range expansion"]
docs[`2.2 Subtype` == "range-expansion", `2.2 Subtype` := "Range expansion"]
docs[`2.2 Subtype` == "disease-spread", `2.2 Subtype` := "Disease spread"]
docs[`2.2 Subtype` == "disease spread", `2.2 Subtype` := "Disease spread"]
docs[`2.2 Subtype` == "migration", `2.2 Subtype` := "Migration"]
docs[`2.2 Subtype` == "migration, displacement", `2.2 Subtype` := "Displacement; Migration"]
docs[`2.2 Subtype` == "Fire", `2.2 Subtype` := "wildfire"]
docs[`2.2 Subtype` == "Disease spread, range shift", `2.2 Subtype` := "Range shift; Disease spread"]
docs[`2.2 Subtype` == "range shift, disease spread" , `2.2 Subtype` := "Range shift; Disease spread"]
docs[`2.2 Subtype` == "disease spread, range shift", `2.2 Subtype` := "Range shift; Disease spread"]
docs[`2.2 Subtype` == "dispersal", `2.2 Subtype` := "Dispersal"]
docs[`2.2 Subtype` == "governance", `2.2 Subtype` := "Governance"]

unique.flow.type <- print(unique(docs$`2.2 Subtype`))
write.csv(unique.flow.type, "flowsubtype_unaggregated.csv")

# #ones we should decide on -- groups:
# "Implied range shift"  
# ## these include the impacts, I assume we want to cut the impacts from here but we should
# ## make sure the form captured these impacts elsewhere:
#   "range shift causing hybridization" 
#   "range shifts, population density increases"  

# "Climate-induced migration"   
# "Change in migration dynamics"

# # sea ice retreat vs sea ice

# "Disease spread, range shift"                                                                                                                                                                                                                      
# "range shift, disease spread"                                                                                                                                                                                                                      
# "disease spread, range shift" 

# # to discuss and decide on:
#   "Range shift and Invasion because of it" 
#   "Range shift of a invasive species"   
#   "range shift, introductions"  
#   "Invasion"  
#   "invasive species range shift"  

# #*** should this be rechecked bc this seems like multiple flow types in this paper?
#   #*"range shift, changes in freshwater run-offs, increase of tourism, agriculture,  and other human activities in the North region"
#   # ""Glacial retreat;; range shift" 
  
#print subtype by flow
glimpse(docs)
biotic = docs[Biotic == "TRUE",]
print(unique(biotic$`2.2 Subtype`))
unique.biotic.flow.type <- print(unique(biotic$`2.2 Subtype`))
write.csv(unique.biotic.flow.type, "bioticflowsubtype_unaggregated.csv")

physical = docs[Physical == "TRUE",]
print(unique(physical$`2.2 Subtype`))
unique.phys.flow.type <- print(unique(physical$`2.2 Subtype`))
write.csv(unique.phys.flow.type, "physflowsubtype_unaggregated.csv")

sociocultural = docs[Sociocultural == "TRUE",]
print(unique(sociocultural$`2.2 Subtype`))
unique.socio.flow.type <- print(unique(sociocultural$`2.2 Subtype`))
write.csv(unique.socio.flow.type, "socioflowsubtype_unaggregated.csv")

docs$Human_movement <- docs$'Human movement'
docs$trade <- docs$'Trade (transport of goods and services)'

human = docs[Human_movement == "TRUE",]
print(unique(human$`2.2 Subtype`))
unique.human.flow.type <- print(unique(human$`2.2 Subtype`))
write.csv(unique.human.flow.type, "humanflowsubtype_unaggregated.csv")

trade = docs[trade == "TRUE",]
print(unique(trade$`2.2 Subtype`)) # 0 
length(trade)

# ** BERNIE HERE -- make this a column where each flow-driver combo is a row
# Drivers -*** need to figure out what to do with the others **** 
docs$Driver <- docs$'2.4 What is the climate driver(s) or trigger(s)? Note, that the driver may not be described as below--choose the closest option(s)'
print(unique(docs$Driver))

#column names for each driver
tags = unique(trimws(unlist(strsplit(docs$Driver, split=","))))

for(tag in tags) {
  set(docs, j=tag, value=F)
  set(docs, i=grep(tag, docs$Driver), j=tag, value=T)
}

setcolorder(docs, 
            neworder=c("Climate change (generic)",
            "Air temperature directional change",
            "Air temperature variability", 
            "Precipitation directional change",
            "Precipitation variability",
            "Drought", "Sea temperature change", 
            "Sea ice area change", "Sea level rise", 
            "Ocean currents",
            "Seawater chemistry", 
            "Wildfires", "Floods", "Hurricanes",
      #the other the others written in for Drivers that need to be harmonizing
            "permafrost melt; sea ice decline",
            "water availability" , "extreme weather" ,                                            
            "freshwater temperature and chemistry change",              
           "Land ice cover (glaciers)",                                  
           "C02 concentration",                                           
          "Increased frequency of extreme weather events" ,              
           "Extreme weather",                                            
          "Water temperature directional change",                     
          "ice cover duration",                                         
           "streamflow patterns",                                      
          "freshwater salinization",                                 
       "additional natural disasters ", "extreme weather events" , "changes in wind pattersn",  
      "this paper does not mention temp or precip change explicitly", 
      "only habitat change"  , "snow pack (depth and hardness)" ,
      "El Nino Southern Oscillation (ENSO)", 
      "Permafrost thawing" , "Snow cover directional change", 
      "Melting permafrost" , "increased UV radiation", 
      "snow cover loss.",  "River water temperature; hydrology", 
      "winter storms", "Riverine discharge change", "heat waves", 
      "Natural disasters broadly" , "Snowpack reduction","melting permafrost",
      "la la la"), after = "Driver")

## Flow change
docs$QuantityChange <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Quantity/Amount]'
table(docs$QuantityChange)
docs$DirectionChange <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Direction (select complex)]'
table(docs$DirectionChange)
docs$LocationChange <-docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Location (select complex)]'
table(docs$LocationChange)
docs$DistanceChange <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Distance]'
table(docs$DistanceChange)

docs$TimingChange <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Timing (earlier = increase, later = decrease)]'
table(docs$TimingChange)
docs$FreqChange <-docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Frequency]'
table(docs$FreqChange)

docs$DurationChange <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Duration or intensity]'
table(docs$DurationChange)

docs$SpeedChange <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Speed]'
table(docs$SpeedChange)

docs$FeedbackInteractions <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Feedbacks or interactions (select complex)]'
table(docs$FeedbackInteractions)

docs$MechanismsChange <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Mechanism of transfer (select complex)]'
docs$OtherChange <- docs$'2.7. MANDATORY: Which characteristic(s) of flow is altered? [Other]'
table(docs$OtherChange)
table(docs$MechanismsChange)


docs$NCPlist <- docs$'2.17 If applicable - list the NCP/ES in the papers words, separated with semi-colons (e.g., agricultural yield; water quality; carbon sequestration)'
print(unique(docs$NCPlist))
docs$HWBlist <- docs$'2.21 If applicable - list the indicators of human well-being in the papers words, separated with semi-colons (e.g., food sovereignty; aesthetic values; mental health)'
print(unique(docs$HWBlist))
