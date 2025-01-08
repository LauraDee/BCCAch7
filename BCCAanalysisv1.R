### Explore BCCA coding responses Laura Jan 7 2024

library(splitstackshape)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

graphics.off()
rm(list=ls())

setwd('~/Downloads/')
# docs =fread('BCCAresponsesJan7.csv', header=T)
docs = fread('newdocjan7.csv' ,header = T)
# had to delete "the eg xxxx" after the additional natural disasters to get the code to work
# one said " "Hurricanes are mentioned" but Hurricane is a category!

head(docs)
colnames(docs)

## need to remove test paper flows - find and drop
docs$Citation <- docs$'1. What is the short citation of the paper? (Author, Year, Journal)'
docs[Citation == "test", Citation := "TEST"]
docs = docs[Citation != "TEST",]

# filter to papers included after seeing how many were excluded 
docs$Exclude <- docs$'8. Are you excluding this paper from further screening?'
table(docs$Exclude) #177 no, 229 yes
docs$ExcludeReason <- docs$'7. Is there any reason to exclude this paper? If so, explain.'

docs = docs[Exclude == "No",]

#Identify flow types 
docs$Flow1 <- docs$'2.1. What type of flow is it?'
table(docs$Flow1)

#Identify subtypes 
docs$Flow1Type <- docs$'2.2 What is the sub-type of flow (e.g., river flow, ocean current, range-shift, etc.)?'
table(docs$Flow1Type)

## need to harmonize the category names
#cover[local_provenance =="Naturalised", local_provenance:="INT"]
print(unique(docs$Flow1Type))
docs[Flow1Type == "test flow", Flow1Type := "TEST"]
docs[Flow1Type == "river flow", Flow1Type := "River flow"]
docs[Flow1Type == "River Flow", Flow1Type := "River flow"]
docs[Flow1Type == "range-shift", Flow1Type := "Range-shift"]
docs[Flow1Type == "range shift", Flow1Type := "Range-shift"]
docs[Flow1Type == "Range shift", Flow1Type := "Range-shift"]
docs[Flow1Type == "range shifts of fisheries species", Flow1Type := "Range-shift"]
docs[Flow1Type == "Range shift of algal species northward that affects fish and shellfish and produces a toxin that is harmful when consumed. The sreener mentioned island examples but they are not relevant. However, Canada is not mentioned in the source paper.", Flow1Type := "Range-shift"]
docs[Flow1Type == "range expansion", Flow1Type := "Range expansion"]
docs[Flow1Type == "range-expansion", Flow1Type := "Range expansion"]
docs[Flow1Type == "disease-spread", Flow1Type := "Disease spread"]
docs[Flow1Type == "disease spread", Flow1Type := "Disease spread"]
docs[Flow1Type == "migration", Flow1Type := "Migration"]
docs[Flow1Type == "migration, displacement", Flow1Type := "Displacement; Migration"]
docs[Flow1Type == "Fire", Flow1Type := "wildfire"]
docs[Flow1Type == "Disease spread, range shift", Flow1Type := "Range shift; Disease spread"]
docs[Flow1Type == "range shift, disease spread" , Flow1Type := "Range shift; Disease spread"]
docs[Flow1Type == "disease spread, range shift", Flow1Type := "Range shift; Disease spread"]
docs[Flow1Type == "dispersal", Flow1Type := "Dispersal"]
docs[Flow1Type == "governance", Flow1Type := "Governance"]

unique.flow.type <- print(unique(docs$Flow1Type))
write.csv(unique.flow.type, "flowsubtype_unaggregated.csv")

#ones we should decide on -- groups:
"Implied range shift"  
## these include the impacts, I assume we want to cut the impacts from here but we should
## make sure the form captured these impacts elsewhere:
  "range shift causing hybridization" 
  "range shifts, population density increases"  

"Climate-induced migration"   
"Change in migration dynamics"

# sea ice retreat vs sea ice

"Disease spread, range shift"                                                                                                                                                                                                                      
"range shift, disease spread"                                                                                                                                                                                                                      
"disease spread, range shift" 

# to discuss and decide on:
  "Range shift and Invasion because of it" 
  "Range shift of a invasive species"   
  "range shift, introductions"  
  "Invasion"  
  "invasive species range shift"  

#*** should this be rechecked bc this seems like multiple flow types in this paper?
  #*"range shift, changes in freshwater run-offs, increase of tourism, agriculture,  and other human activities in the North region"
  # ""Glacial retreat;; range shift" 
  
#print subtype by flow
biotic = docs[Biotic == "TRUE",]
print(unique(biotic$Flow1Type))
unique.biotic.flow.type <- print(unique(biotic$Flow1Type))
write.csv(unique.biotic.flow.type, "bioticflowsubtype_unaggregated.csv")

physical = docs[Physical == "TRUE",]
print(unique(physical$Flow1Type))
unique.phys.flow.type <- print(unique(physical$Flow1Type))
write.csv(unique.phys.flow.type, "physflowsubtype_unaggregated.csv")

sociocultural = docs[Sociocultural == "TRUE",]
print(unique(sociocultural$Flow1Type))
unique.socio.flow.type <- print(unique(sociocultural$Flow1Type))
write.csv(unique.socio.flow.type, "socioflowsubtype_unaggregated.csv")

docs$Human_movement <- docs$'Human movement'
docs$trade <- docs$'Trade (transport of goods and services)'

human = docs[Human_movement == "TRUE",]
print(unique(human$Flow1Type))
unique.human.flow.type <- print(unique(human$Flow1Type))
write.csv(unique.human.flow.type, "humanflowsubtype_unaggregated.csv")

trade = docs[trade == "TRUE",]
print(unique(trade$Flow1Type)) # 0 
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

