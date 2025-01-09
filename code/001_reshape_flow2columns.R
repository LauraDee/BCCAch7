install.packages("splitstackshape")
library(splitstackshape)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
setwd('C:\\Users\\basti\\Documents\\GitHub\\BCCAch7')
docs <- fread('data/v3form.csv', header=T)


#setwd('~/Downloads/')


#main_folder <- "~/Documents/GitHub/BCCAch7/data/"
#data_folder <- file.path(main_folder, "data")
#docs <- fread('v3form.csv', header=T)


#column names for each tag
tags = unique(trimws(unlist(strsplit(docs$'2.1. What type of flow is it?', split=", "))))
for(tag in tags) {
  set(docs, j=tag, value=F)
  set(docs, i=grep(tag, docs$'2.1. What type of flow is it?'), j=tag, value=T)
}

setcolorder(docs, 
            neworder=c("Sociocultural",
                       "Human movement",
                       "Biophysical",
                       "Biotic"
                      ),
            after = "2.1. What type of flow is it?")
colnames(docs)
write.csv(docs, 'data/reshaped_1_flowtypes.csv')
