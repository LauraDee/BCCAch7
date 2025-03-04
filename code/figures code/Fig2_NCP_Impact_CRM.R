Fig2_NCP_Impact.r

#load libraries
graphics.off()
rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(ggalluvial)
#devtools::install_github("erblast/easyalluvial") #https://erblast.github.io/easyalluvial/
library(easyalluvial)

#setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/data")
reshaped_data <- read.csv("reshaped_4_drivers.csv")
glimpse(reshaped_data)

#How many entries had noNCPs (sum of all of the columns)
table(reshaped_data$X2.16.NCP.ES..None)
23 + 18 + 39  # = 80 which means 153 entries had one or more NCP
# The figure has a max of 30 and it looks like 153 had some entries so max % of papers total is ~19.6%

#Let's look
table(reshaped_data$X2.16.NCP.ES..Experiences)
table(reshaped_data$X2.16.NCP.ES..Habitat)
table(reshaped_data$X2.16.NCP.ES..Pollination)
table(reshaped_data$X2.16.NCP.ES..Air.Quality)
table(reshaped_data$X2.16.NCP.ES..Climate)
table(reshaped_data$X2.16.NCP.ES..Acidification)
table(reshaped_data$X2.16.NCP.ES..Freshwater)
table(reshaped_data$X2.16.NCP.ES..Water.Quality)
table(reshaped_data$X2.16.NCP.ES..Soil.Protection)
table(reshaped_data$X2.16.NCP.ES..Hazards)
#etc.
#review impacts
table(reshaped_data$X2.12.Impact..None)
table(reshaped_data$X2.12.Impact..Abundance)
table(reshaped_data$X2.12.Impact..Richness)
table(reshaped_data$X2.12.Impact..Loss)
table(reshaped_data$X2.12.Impact..Disease) 
table(reshaped_data$X2.12.Impact..Invasion)
table(reshaped_data$X2.12.Impact..Composition)
table(reshaped_data$X2.12.Impact..Genetics)
table(reshaped_data$X2.12.Impact..Land.Use.Loss)
table(reshaped_data$X2.12.Impact..Land.Use.Restore)
table(reshaped_data$X2.12.Impact..Urbanization)
table(reshaped_data$X2.12.Impact..Connectivity)  
table(reshaped_data$X2.12.Impact..Trophic)
table(reshaped_data$X2.12.Impact..Indigenous.Knowledge)
table(reshaped_data$X2.12.Impact..Management)
table(reshaped_data$X2.12.Impact..Other)

table(reshaped_data$driver.Drought) #178 FALSE, 55 TRUE
table(reshaped_data$driver.Climate.change..generic.)
table(reshaped_data$driver.Sea.level.rise)
table(reshaped_data$driver.Hurricanes)


# Select and reshape relevant columns
flow_columns <- names(reshaped_data)[grepl("2.7.Altered.Flow.", names(reshaped_data))]
impact_columns <- names(reshaped_data)[grepl("2.12.Impact.", names(reshaped_data))]
driver_columns <-  names(reshaped_data)[grepl("driver.", names(reshaped_data))]
hwb_cols <-names(reshaped_data)[grepl("2.20.Well.being.", names(reshaped_data))]
NCP_cols <-names(reshaped_data)[grepl("X2.16.NCP.ES.", names(reshaped_data))]

#Remove the No NCP entry which is "X2.16.NCP.ES..None"  
NCP_cols <- NCP_cols[-1]

#remove none for X2.20.Well.being..None
hwb_cols <- hwb_cols[-1]

# Filter relevant columns for Altered Flow and Impact
"%notin%" <- Negate("%in%")

interaction_data <- reshaped_data %>% filter(`Citation` %notin% c("TEST","test","Test")) %>% 
  select(all_of(c(flow_columns, impact_columns, driver_columns, hwb_cols, NCP_cols))) %>%
  mutate(row_id = row_number())  %>%
  filter(!if_all(-row_id, ~ .x == ""))
glimpse(interaction_data)

# Generate all possible combinations of flows and NCP
combinations <- expand.grid(
  Flow = flow_columns,
  NCP = NCP_cols,
  stringsAsFactors = FALSE
)

# Count occurrences of each combination in the data
combination_counts <- combinations %>%
  rowwise() %>% 
  mutate(
    count = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] != "",
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Convert to a dataframe for easy viewing
combination_counts_df <- as.data.frame(combination_counts)

# Preview the result
glimpse(combination_counts_df)

# Create the plot of paper counts by combination
ggplot(combination_counts_df, aes(x = Flow, y = NCP, size = count)) +
  geom_point(color = "blue", alpha = 0.7) +  # Use points to represent combinations
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range for better visibility
  labs(
    title = "Interaction Between Altered Flows and NCPs",
    x = "Altered Flow",
    y = "NCP",
    size = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )

# Count occurrences for each NCP direction 
combination_counts_by_NCP <- combinations %>%
  rowwise() %>%
  mutate(
    Increase = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Increase",
      na.rm = TRUE
    ),
    Decrease = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Decrease",
      na.rm = TRUE
    ),
    Complex = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Complex Change",
      na.rm = TRUE
    ),
    NotConsidered = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "",
                        na.rm = TRUE
    ),
    NoChangeMeasured = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] =='No change (measured)',
                           na.rm = TRUE
    ),
    NotMeasured = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] =='No direction mentioned',
                      na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Increase, Decrease, Complex, NotConsidered, NoChangeMeasured, NotMeasured), names_to = "NCPDirection", values_to = "count") %>%
  ungroup()

#check & write out the summaries here - note this is just of altered flows..
# .. .but doesn't say which flows or which drivers
head(combination_counts_by_NCP)
#write.csv(combination_counts_by_NCP, "NCPsummarydata.csv")

#check 
table(combination_counts_by_NCP$NCPDirection, combination_counts_by_NCP$count)

#Remove the NCPDirection == "NotConsidered" rows -
combination_counts_by_NCP_filtered <- combination_counts_by_NCP %>%
  filter(NCPDirection != "NotConsidered")
#check 
head(combination_counts_by_NCP_filtered) # yay it worked!

# remove not measured too
combination_counts_by_NCP_filtered <- combination_counts_by_NCP_filtered %>%
  filter(NCPDirection != "NotMeasured")

# remove the blank NCP entries for all NCP columns
# Filter out rows with count == 0
combination_counts_by_NCP_filtered <- combination_counts_by_NCP_filtered  %>%
  filter(count > 0)

# Plot with facets by NCP direction excluding entries with NO NCPs mentioned  and zero counts
ggplot(combination_counts_by_NCP_filtered, aes(x = Flow, y = NCP, size = count, color = NCPDirection)) +
  geom_point(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCPDirection, scales = "free") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey70", 
                                NotMeasured = "black")) +
  labs(
    title = "Interaction Between Altered Flows and NCPs by NCP Direction",
    x = "Altered Flow",
    y = "NCP",
    size = "Count",
    color = "NCP Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )

### NOTE: The figure has a max count of ~30;
# it looks like 153 had some entries so max % of papers total is ~19.6%
# 1 paper is 0.65% of papers. 5 papers is 3.27%

## Plots of basic NCP impacts by NCP
ggplot(combination_counts_by_NCP_filtered, aes(x = NCPDirection, fill = NCPDirection)) +
  geom_bar(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCP, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod3", "NoChangeMeasured" = "grey70", 
                               "NotMeasured" = "black")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"))

ggplot(combination_counts_by_NCP_filtered, aes(x = count, fill = NCP)) +
  geom_bar(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCPDirection, scales = "fixed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"))

#stacked bar plot  
ggplot(combination_counts_by_NCP_filtered, aes(x = count, fill = NCPDirection)) +
  geom_bar(alpha = 0.7, position = "stack") +  # Add points with alpha transparency
  facet_wrap(~NCP, scales = "fixed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"))

ggplot(combination_counts_by_NCP_filtered, aes(x = NCP, fill = NCPDirection)) +
  geom_bar(alpha = 0.7, position = "stack") +  # Add points with alpha transparency
  #facet_wrap(~NCP, scales = "free") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod3", "NoChangeMeasured" = "grey70")) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"))


# Plots of which flow change are having which NCP changes

# create an alluvial:
is_alluvia_form(as.data.frame(combination_counts_by_NCP_filtered), axes = 1:5, silent = TRUE)

ggplot(as.data.frame(combination_counts_by_NCP_filtered),
       aes(y = count, axis1 = Flow, axis2 = NCP)) +
  geom_alluvium(aes(fill = NCPDirection), width = 1/5) +
  geom_stratum(width = 1/4, fill = 'white', color = "grey80") +
  geom_text(stat = "stratum", color='black', size=4, aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Altered Flow", "NCP"), expand = c(.05, .05)) +
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey70")) +
  #scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("NCP impacts, by Altered Flow") +
  theme_minimal() +
  theme(legend.position = "right",
        title = element_text(size = 15),
        legend.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=20))




#### do for each subtype ####

#load libraries
graphics.off()
rm(list=ls())

library(ggplot2)

#setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/data")
reshaped_data <- read.csv("reshaped_4_drivers.csv")
glimpse(reshaped_data)

#How many entries had noNCPs (sum of all of the columns)
table(reshaped_data$X2.16.NCP.ES..None)

#BIOTIC
biotic_combo_NCP <- reshaped_data %>%
  filter(Biotic == "TRUE")

#Let's look
table(biotic_combo_NCP$X2.16.NCP.ES..Experiences)
table(biotic_combo_NCP$X2.16.NCP.ES..Habitat)
table(biotic_combo_NCP$X2.16.NCP.ES..Pollination)
table(biotic_combo_NCP$X2.16.NCP.ES..Air.Quality)
table(biotic_combo_NCP$X2.16.NCP.ES..Climate)
table(biotic_combo_NCP$X2.16.NCP.ES..Acidification)
table(biotic_combo_NCP$X2.16.NCP.ES..Freshwater)
table(biotic_combo_NCP$X2.16.NCP.ES..Water.Quality)
table(biotic_combo_NCP$X2.16.NCP.ES..Soil.Protection)
table(biotic_combo_NCP$X2.16.NCP.ES..Hazards)
#etc.

# Select and reshape relevant columns
flow_columns <- names(biotic_combo_NCP)[grepl("2.7.Altered.Flow.", names(biotic_combo_NCP))]
impact_columns <- names(biotic_combo_NCP)[grepl("2.12.Impact.", names(biotic_combo_NCP))]
driver_columns <-  names(biotic_combo_NCP)[grepl("driver.", names(biotic_combo_NCP))]
hwb_cols <-names(biotic_combo_NCP)[grepl("2.20.Well.being.", names(biotic_combo_NCP))]
NCP_cols <-names(biotic_combo_NCP)[grepl("X2.16.NCP.ES.", names(biotic_combo_NCP))]

#Remove the No NCP entry which is "X2.16.NCP.ES..None"  
NCP_cols <- NCP_cols[-1]

# Filter relevant columns for Altered Flow and Impact
"%notin%" <- Negate("%in%")

interaction_data <- biotic_combo_NCP %>% filter(`Citation` %notin% c("TEST","test","Test")) %>% 
  select(all_of(c(flow_columns, impact_columns, driver_columns, hwb_cols, NCP_cols))) %>%
  mutate(row_id = row_number())  %>%
  filter(!if_all(-row_id, ~ .x == ""))
glimpse(interaction_data)

# Generate all possible combinations of flows and NCP
combinations <- expand.grid(
  Flow = flow_columns,
  NCP = NCP_cols,
  stringsAsFactors = FALSE
)

# Count occurrences of each combination in the data
combination_counts <- combinations %>%
  rowwise() %>% 
  mutate(
    count = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] != "",
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Convert to a dataframe for easy viewing
combination_counts_df <- as.data.frame(combination_counts)

# Preview the result
glimpse(combination_counts_df)

# Create the plot of paper counts by combination
ggplot(combination_counts_df, aes(x = Flow, y = NCP, size = count)) +
  geom_point(color = "blue", alpha = 0.7) +  # Use points to represent combinations
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range for better visibility
  labs(
    title = "Interaction Between Altered Flows and NCPs",
    x = "Altered Flow",
    y = "NCP",
    size = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )

# Count occurrences for each NCP direction 
combination_counts_by_NCP <- combinations %>%
  rowwise() %>%
  mutate(
    Increase = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Increase",
      na.rm = TRUE
    ),
    Decrease = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Decrease",
      na.rm = TRUE
    ),
    Complex = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Complex Change",
      na.rm = TRUE
    ),
    NotConsidered = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "",
                        na.rm = TRUE
    ),
    NoChangeMeasured = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] =='No change (measured)',
                           na.rm = TRUE
    ),
    NotMeasured = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] =='No direction mentioned',
                      na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Increase, Decrease, Complex, NotConsidered, NoChangeMeasured, NotMeasured), names_to = "NCPDirection", values_to = "count") %>%
  ungroup()

#check & write out the summaries here - note this is just of altered flows..
# .. .but doesn't say which flows or which drivers
head(combination_counts_by_NCP)
#write.csv(combination_counts_by_NCP, "NCPsummarydata.csv")

#check 
table(combination_counts_by_NCP$NCPDirection, combination_counts_by_NCP$count)

#Remove the NCPDirection == "NotConsidered" rows -
combination_counts_by_NCP_filtered <- combination_counts_by_NCP %>%
  filter(NCPDirection != "NotConsidered")

#check 
head(combination_counts_by_NCP_filtered) # yay it worked!

# remove not measured too
combination_counts_by_NCP_filtered <- combination_counts_by_NCP_filtered %>%
  filter(NCPDirection != "NotMeasured")

# remove the blank NCP entries for all NCP columns
# Filter out rows with count == 0
combination_counts_by_NCP_filtered <- combination_counts_by_NCP_filtered  %>%
  filter(count > 0)

# Plot with facets by NCP direction excluding entries with NO NCPs mentioned  and zero counts
bioticNCP <- ggplot(combination_counts_by_NCP_filtered, aes(x = Flow, y = NCP, size = count, color = NCPDirection)) +
  geom_point(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCPDirection, scales = "free") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey", 
                                NotMeasured = "black")) +
  labs(
    title = "Biotic Flows: Interaction Between Altered Flows and NCPs by NCP Direction",
    x = "Altered Flow",
    y = "NCP",
    size = "Count",
    color = "NCP Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey70", linetype = "dotted")
  )
bioticNCP

## Plots of basic NCP impacts by NCP
ggplot(combination_counts_by_NCP_filtered, aes(x = NCPDirection, fill = NCPDirection)) +
  geom_bar(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCP, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey", 
                               "NotMeasured" = "black")) +
  labs(
    title = "Biotic Flows: NCPs Direction",
    x = "Effect",
    y = "Count",
    size = "Count",
    color = "NCP Direction"  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted") )

#basic bar plot counting the NCP entry counts by direction

bioticNCP <- ggplot(combination_counts_by_NCP_filtered, aes(x = NCP, fill = NCPDirection)) +
  geom_bar(alpha = 0.7, position = "stack") +  # Add points with alpha transparency
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey"))
bioticNCP

# PHYSICAL
phys_combo_NCP <- reshaped_data %>%
  filter(Physical == "TRUE")

#Let's look
table(phys_combo_NCP$X2.16.NCP.ES..Experiences)
table(phys_combo_NCP$X2.16.NCP.ES..Habitat)
table(phys_combo_NCP$X2.16.NCP.ES..Pollination)
table(phys_combo_NCP$X2.16.NCP.ES..Air.Quality)
table(phys_combo_NCP$X2.16.NCP.ES..Climate)
table(phys_combo_NCP$X2.16.NCP.ES..Acidification)
table(phys_combo_NCP$X2.16.NCP.ES..Freshwater)
table(phys_combo_NCP$X2.16.NCP.ES..Water.Quality)
table(phys_combo_NCP$X2.16.NCP.ES..Soil.Protection)
table(phys_combo_NCP$X2.16.NCP.ES..Hazards)
#etc.

# Select and reshape relevant columns
flow_columns <- names(phys_combo_NCP)[grepl("2.7.Altered.Flow.", names(phys_combo_NCP))]
impact_columns <- names(phys_combo_NCP)[grepl("2.12.Impact.", names(phys_combo_NCP))]
driver_columns <-  names(phys_combo_NCP)[grepl("driver.", names(phys_combo_NCP))]
hwb_cols <-names(phys_combo_NCP)[grepl("2.20.Well.being.", names(phys_combo_NCP))]
NCP_cols <-names(phys_combo_NCP)[grepl("X2.16.NCP.ES.", names(phys_combo_NCP))]

#Remove the No NCP entry which is "X2.16.NCP.ES..None"  
NCP_cols <- NCP_cols[-1]

# Filter relevant columns for Altered Flow and Impact
"%notin%" <- Negate("%in%")

interaction_data <- phys_combo_NCP %>% filter(`Citation` %notin% c("TEST","test","Test")) %>% 
  select(all_of(c(flow_columns, impact_columns, driver_columns, hwb_cols, NCP_cols))) %>%
  mutate(row_id = row_number())  %>%
  filter(!if_all(-row_id, ~ .x == ""))
glimpse(interaction_data)

# Generate all possible combinations of flows and NCP
combinations <- expand.grid(
  Flow = flow_columns,
  NCP = NCP_cols,
  stringsAsFactors = FALSE
)

# Count occurrences of each combination in the data
combination_counts <- combinations %>%
  rowwise() %>% 
  mutate(
    count = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] != "",
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Convert to a dataframe for easy viewing
combination_counts_df <- as.data.frame(combination_counts)

# Preview the result
glimpse(combination_counts_df)

# Create the plot of paper counts by combination
ggplot(combination_counts_df, aes(x = Flow, y = NCP, size = count)) +
  geom_point(color = "blue", alpha = 0.7) +  # Use points to represent combinations
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range for better visibility
  labs(
    title = "Interaction Between Altered Flows and NCPs",
    x = "Altered Flow",
    y = "NCP",
    size = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )

# Count occurrences for each NCP direction 
combination_counts_by_NCP <- combinations %>%
  rowwise() %>%
  mutate(
    Increase = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Increase",
      na.rm = TRUE
    ),
    Decrease = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Decrease",
      na.rm = TRUE
    ),
    Complex = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Complex Change",
      na.rm = TRUE
    ),
    NotConsidered = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "",
                        na.rm = TRUE
    ),
    NoChangeMeasured = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] =='No change (measured)',
                           na.rm = TRUE
    ),
    NotMeasured = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] =='No direction mentioned',
                      na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Increase, Decrease, Complex, NotConsidered, NoChangeMeasured, NotMeasured), names_to = "NCPDirection", values_to = "count") %>%
  ungroup()

#check & write out the summaries here - note this is just of altered flows..
# .. .but doesn't say which flows or which drivers
head(combination_counts_by_NCP)
#write.csv(combination_counts_by_NCP, "NCPsummarydata.csv")

#check 
table(combination_counts_by_NCP$NCPDirection, combination_counts_by_NCP$count)

#Remove the NCPDirection == "NotConsidered" rows -
combination_counts_by_NCP_filtered <- combination_counts_by_NCP %>%
  filter(NCPDirection != "NotConsidered")
#check 
head(combination_counts_by_NCP_filtered) # yay it worked!

# remove the blank NCP entries for all NCP columns
# Filter out rows with count == 0
combination_counts_by_NCP_filtered <- combination_counts_by_NCP_filtered  %>%
  filter(count > 0)

# remove not measured too
combination_counts_by_NCP_filtered <- combination_counts_by_NCP_filtered %>%
  filter(NCPDirection != "NotMeasured")




# Plot with facets by NCP direction excluding entries with NO NCPs mentioned  and zero counts
physNCP <- ggplot(combination_counts_by_NCP_filtered, aes(x = Flow, y = NCP, size = count, color = NCPDirection)) +
  geom_point(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCPDirection, scales = "free") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey", 
                                NotMeasured = "black")) +
  labs(
    title = "Physicalc Flows: Interaction Between Altered Flows and NCPs by NCP Direction",
    x = "Altered Flow",
    y = "NCP",
    size = "Count",
    color = "NCP Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
physNCP

#scale fixed
physNCP <- ggplot(combination_counts_by_NCP_filtered, aes(x = Flow, y = NCP, size = count, color = NCPDirection)) +
  geom_point(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCPDirection, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey", 
                                NotMeasured = "black")) +
  labs(
    title = "Physicalc Flows: Interaction Between Altered Flows and NCPs by NCP Direction",
    x = "Altered Flow",
    y = "NCP",
    size = "Count",
    color = "NCP Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
physNCP

## Plots of basic NCP impacts by NCP
ggplot(combination_counts_by_NCP_filtered, aes(x = NCPDirection, fill = NCPDirection)) +
  geom_bar(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCP, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey", 
                               "NotMeasured" = "black")) +
  labs(
    title = "Physical Flows: NCPs Direction",
    x = "Effect",
    y = "Count",
    size = "Count",
    color = "NCP Direction"  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted") )


ggplot(combination_counts_by_NCP_filtered, aes(x = count, fill = NCP)) +
  geom_bar(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCPDirection, scales = "fixed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted") )


# *** Human movement - how to combine with trade in this step?? *** not sure if this worked..
human_combo_NCP <- reshaped_data %>%
  filter(Movement=="TRUE")

#Let's look
table(human_combo_NCP$X2.16.NCP.ES..Experiences)
table(human_combo_NCP$X2.16.NCP.ES..Habitat)
table(human_combo_NCP$X2.16.NCP.ES..Pollination)
table(human_combo_NCP$X2.16.NCP.ES..Air.Quality)
table(human_combo_NCP$X2.16.NCP.ES..Climate)
table(human_combo_NCP$X2.16.NCP.ES..Acidification)
table(human_combo_NCP$X2.16.NCP.ES..Freshwater)
table(human_combo_NCP$X2.16.NCP.ES..Water.Quality)
table(human_combo_NCP$X2.16.NCP.ES..Soil.Protection)
table(human_combo_NCP$X2.16.NCP.ES..Hazards)
#etc.

# Select and reshape relevant columns
flow_columns <- names(human_combo_NCP)[grepl("2.7.Altered.Flow.", names(human_combo_NCP))]
impact_columns <- names(human_combo_NCP)[grepl("2.12.Impact.", names(human_combo_NCP))]
driver_columns <-  names(human_combo_NCP)[grepl("driver.", names(human_combo_NCP))]
hwb_cols <-names(human_combo_NCP)[grepl("2.20.Well.being.", names(human_combo_NCP))]
NCP_cols <-names(human_combo_NCP)[grepl("X2.16.NCP.ES.", names(human_combo_NCP))]

#Remove the No NCP entry which is "X2.16.NCP.ES..None"  
NCP_cols <- NCP_cols[-1]

# Filter relevant columns for Altered Flow and Impact
"%notin%" <- Negate("%in%")

interaction_data <- human_combo_NCP %>% filter(`Citation` %notin% c("TEST","test","Test")) %>% 
  select(all_of(c(flow_columns, impact_columns, driver_columns, hwb_cols, NCP_cols))) %>%
  mutate(row_id = row_number())  %>%
  filter(!if_all(-row_id, ~ .x == ""))
glimpse(interaction_data)

# Generate all possible combinations of flows and NCP
combinations <- expand.grid(
  Flow = flow_columns,
  NCP = NCP_cols,
  stringsAsFactors = FALSE
)

# Count occurrences of each combination in the data
combination_counts <- combinations %>%
  rowwise() %>% 
  mutate(
    count = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] != "",
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Convert to a dataframe for easy viewing
combination_counts_df <- as.data.frame(combination_counts)

# Preview the result
glimpse(combination_counts_df)

# Create the plot of paper counts by combination
ggplot(combination_counts_df, aes(x = Flow, y = NCP, size = count)) +
  geom_point(color = "blue", alpha = 0.7) +  # Use points to represent combinations
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range for better visibility
  labs(
    title = "Interaction Between Altered Flows and NCPs",
    x = "Altered Flow",
    y = "NCP",
    size = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )

# Count occurrences for each NCP direction 
combination_counts_by_NCP <- combinations %>%
  rowwise() %>%
  mutate(
    Increase = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Increase",
      na.rm = TRUE
    ),
    Decrease = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Decrease",
      na.rm = TRUE
    ),
    Complex = sum(
      interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "Complex Change",
      na.rm = TRUE
    ),
    NotConsidered = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] == "",
                        na.rm = TRUE
    ),
    NoChangeMeasured = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] =='No change (measured)',
                           na.rm = TRUE
    ),
    NotMeasured = sum(interaction_data[[Flow]] != "" & interaction_data[[NCP]] =='No direction mentioned',
                      na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Increase, Decrease, Complex, NotConsidered, NoChangeMeasured, NotMeasured), names_to = "NCPDirection", values_to = "count") %>%
  ungroup()

#check & write out the summaries here - note this is just of altered flows..
# .. .but doesn't say which flows or which drivers
head(combination_counts_by_NCP)
#write.csv(combination_counts_by_NCP, "NCPsummarydata.csv")

#check 
table(combination_counts_by_NCP$NCPDirection, combination_counts_by_NCP$count)

#Remove the NCPDirection == "NotConsidered" rows -
combination_counts_by_NCP_filtered <- combination_counts_by_NCP %>%
  filter(NCPDirection != "NotConsidered")
#check 
head(combination_counts_by_NCP_filtered) # yay it worked!

# remove the blank NCP entries for all NCP columns
# Filter out rows with count == 0
combination_counts_by_NCP_filtered <- combination_counts_by_NCP_filtered  %>%
  filter(count > 0)

# Plot with facets by NCP direction excluding entries with NO NCPs mentioned  and zero counts
humanNCP <- ggplot(combination_counts_by_NCP_filtered, aes(x = Flow, y = NCP, size = count, color = NCPDirection)) +
  geom_point(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCPDirection, scales = "free") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey", 
                                NotMeasured = "black")) +
  labs(
    title = "Human movement/trade Flows Flows: Interaction Between Altered Flows and NCPs by NCP Direction",
    x = "Altered Flow",
    y = "NCP",
    size = "Count",
    color = "NCP Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )
humanNCP

## Plots of basic NCP impacts by NCP
ggplot(combination_counts_by_NCP_filtered, aes(x = NCPDirection, fill = NCPDirection)) +
  geom_bar(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCP, scales = "fixed") +  # Create facets for each NCP direction
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_fill_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1", "NoChangeMeasured" = "grey", 
                               "NotMeasured" = "black")) +
  labs(
    title = "Human movement/trade Flows: NCPs Direction",
    x = "Effect",
    y = "Count",
    size = "Count",
    color = "NCP Direction"  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted") )

#basic bar plot counting the NCP entry counts by direction
ggplot(combination_counts_by_NCP_filtered, aes(x = count, fill = NCP)) +
  geom_bar(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~NCPDirection, scales = "fixed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted") )






## BERNIE -- not sure what this is? It didnt work for me -- 
unique_flows <- interaction_data %>%
  select(starts_with("X2.7")) %>%
  unlist() %>%
  unique() %>%
  na.omit()

unique_NCPs <- interaction_data %>%
  select(starts_with("X2.16")) %>%
  unlist() %>%
  unique() %>%
  na.omit()

# Print unique values to ensure we understand the data structure
print(unique_flows)
print(unique_NCPs)

# Count occurrences for each combination
combination_counts_by_NCP <- combinations %>%
  rowwise() %>%
  mutate(
    count = sum(
      apply(interaction_data, 1, function(row) {
        row[[Flow]] != "" && row[[NCP]] != "" && 
          row[[Flow]] == Flow && row[[NCP]] == NCP
      })
    ),
    FlowDirection = case_when(
      grepl("Increase", Flow) ~ "Increase",
      grepl("Decrease", Flow) ~ "Decrease",
      grepl("Complex change", Flow) ~ "Complex change",
      TRUE ~ NA_character_
    ),
    NCPDirection = case_when(
      grepl("Increase", NCP) ~ "Increase",
      grepl("Decrease", NCP) ~ "Decrease",
      grepl("Complex change", NCP) ~ "Complex change"
    )
  ) %>%
  filter(!is.na(FlowDirection) & !is.na(NCPDirection)) %>%  # Exclude undefined combinations
  ungroup()

# Plot with corrected counts
ggplot(combination_counts_by_NCP, aes(
  x = Flow, y = NCP, size = count, color = NCPDirection, shape = FlowDirection)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~NCPDirection, scales = "free") +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(values = c("Increase" = "dodgerblue3", "Decrease" = "deeppink3", "Complex" = "goldenrod1")) +
  scale_shape_manual(values = c("Increase" = 16, "Decrease" = 17, "Complex" = 15)) +
  labs(
    title = "Interaction Between Altered Flows and NCPs by NCP Direction",
    x = "Altered Flow",
    y = "NCP",
    size = "Count",
    color = "NCP Direction",
    shape = "Flow Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )



